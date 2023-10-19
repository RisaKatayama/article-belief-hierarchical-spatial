function [pmat_td, pmat_gr] = TM_hierarchical_noreest(sbj_id, mode, beta, delta, gamma, datdir)

%% data load
game = [];
dirt = []; % south:1, east:2, west:3, north:4
resp = [];  % left:1, forward:2, right:3, 4: listen
tigroar = [];

varnames = {'game','dirt','tigroar','resp'};

Nses = 4;
for s = 1:Nses
    dat = load([datdir '/s' sbj_id '_' num2str(mode) '_' num2str(s) '.mat']);
    
    game = [game dat.game+3*(s-1)];
    dirt = [dirt dat.dirt];
    resp = [resp dat.resp];
    tigroar = [tigroar dat.tigroar];
end

% exclude no-response trials
Nvar = length(varnames);
for v = 1:Nvar
    eval([varnames{v} '=' varnames{v} '(~isnan(resp));']);
end

%% POMDP
dir_act=[3,4,2,1; 1,3,4,2; 4,2,1,3; 2,1,3,4];
dir_tigdir = [2,1,3,0; 3,2,0,1; 1,0,2,3; 0,3,1,2];
[maze, tigdirt, MAZE] = makemaze();
Ngame = max(game);

Ngrid = 16;
alpha = 0.85;

pmat_td = [];
pmat_gr = [];

for g = 1:Ngame
    dirt_g = dirt(game==g);
    tigroar_g = tigroar(game==g);
    resp_g = resp(game==g);
    ntrl = sum(game==g);
    % not include goal trial
    
    pmat_td_g = NaN(ntrl,3);
    pmat_gr_g = NaN(ntrl,16);
    
    gr_cand = 1:Ngrid;
    td_cand = zeros(1,length(gr_cand));
    for i = 1:length(gr_cand)
        td_cand(i) = dir_tigdir(dirt_g(1),tigdirt(i));
    end

    p_gr = ones(1,length(gr_cand))/Ngrid;
    p_td_gr = zeros(3,length(gr_cand));
    for i = 1:length(gr_cand)
        if td_cand(i)~=0
            tmp = ones(1,3)*(gamma/3);
            tmp(td_cand(i)) = 1-gamma;
            p_td_gr(:,gr_cand(i)) = tmp/sum(tmp);
        else
            p_td_gr(:,gr_cand(i)) = 1/3;
        end
    end

    p_td = sum(p_td_gr.*repmat(p_gr,3,1),2)'/sum(sum(p_td_gr.*repmat(p_gr,3,1),2)); % marginalize
    pmat_td_g(1,:) = p_td;
    pmat_gr_g(1,:) = p_gr;

    p_gr_ts = p_gr;

    for t = 1:ntrl
        if t < ntrl
            %% Listen trial (no grid transition)
            if resp_g(t) == 4
                pred_td = find(p_td==max(p_td));
                
                % tiger door inference update
                lik_td = ones(1,3)*(1-alpha)/2;
                lik_td(tigroar_g(t)) = alpha;
                
                n_p_td = p_td.*(lik_td.^delta);
                n_p_td = n_p_td/sum(n_p_td);
                map_td = find(n_p_td==max(n_p_td));
                
                % grid inference updating based on the predicted tiger door
                n_p_gr = zeros(1,length(gr_cand));
                for i = 1:length(gr_cand)
                    if ismember(td_cand(i),map_td)
                        n_p_gr(i) = p_gr(i)*((1-gamma)*beta+gamma*(1-beta));
                    elseif td_cand(i)~=0
                        n_p_gr(i) = p_gr(i)*((1-gamma)*(1-beta)+gamma*beta);
                    else
                        n_p_gr(i) = p_gr(i)*gamma;
                    end
                end
                n_p_gr = n_p_gr/sum(n_p_gr);
                
            %% Move trial
            else
                n_p_gr = zeros(1,length(gr_cand));
                ntd_cand = zeros(1,length(gr_cand));
                n_p_td_gr = zeros(3,length(gr_cand));
                
                for i = 1:length(gr_cand)
                    ntd_cand(i) = dir_tigdir(dirt_g(t+1),tigdirt(i));
                end
                
                act = dir_act(dirt_g(t),resp_g(t));
                for i = 1:length(gr_cand)
                    ngr = maze(gr_cand(i),act);
                    adj = MAZE(gr_cand(i),act);
                    
                    if ~isnan(ngr)
                        n_p_gr(gr_cand==ngr) = p_gr(i);
                        tmp = ones(1,3)*(gamma/3);
                        tmp(ntd_cand(gr_cand==ngr)) = 1-gamma;
                        n_p_td_gr(:,gr_cand==ngr) = tmp/sum(tmp);
                    else
                        n_p_gr(gr_cand==adj) = p_gr(i)*gamma;
                        n_p_td_gr(:,gr_cand==adj) = 1/3;
                    end
                end
                n_p_gr = n_p_gr/sum(n_p_gr);
                p_gr_ts = n_p_gr;
                
                n_p_td = sum(n_p_td_gr.*repmat(n_p_gr,3,1),2)'/sum(sum(n_p_td_gr.*repmat(n_p_gr,3,1),2));

                td_cand = ntd_cand;
            end

            pmat_td_g(t+1,:) = n_p_td;
            pmat_gr_g(t+1,:) = n_p_gr;

            p_td = n_p_td;
            p_gr = n_p_gr;
        end
    end

    pmat_gr = [pmat_gr; pmat_gr_g];
    pmat_td = [pmat_td; pmat_td_g];
    reest = [reest reest_g];
end
end


function [maze, tigdirt, MAZE] = makemaze()

% [N, W, E, S] adjacent grid ID, NaN = tiger door
maze = [  13,   4, NaN,   5;  14, NaN,   3,   6;  15,   2,   4, NaN;  16,   3,   1, NaN;...
           1,   8, NaN,   9;   2, NaN,   7,  10; NaN,   6,   8,  11; NaN,   7,   5,  12;...
           5,  12,  10, NaN;   6,   9, NaN,  14;   7, NaN,  12,  15;   8,  11,   9, NaN;...
         NaN,  16,  14,   1;  10,  13, NaN,   2;  11, NaN,  16,   3; NaN,  15,  13,   4];

tigdirt = 5 - (find(isnan(maze'))' - (0:15)*4);

MAZE = [  13,   4,   2,   5;  14,   1,   3,   6;  15,   2,   4,   7;  16,   3,   1,   8;...
           1,   8,   6,   9;   2,   5,   7,  10;   3,   6,   8,  11;   4,   7,   5,  12;...
           5,  12,  10,  13;   6,   9,  11,  14;   7,  10,  12,  15;   8,  11,   9,  16;...
           9,  16,  14,   1;  10,  13,  15,   2;  11,  14,  16,   3;  12,  15,  13,   4];
     
end