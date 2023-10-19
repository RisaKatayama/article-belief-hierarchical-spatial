function [bres, mres, param] = TM_param_fitting(model)

tic;
fprintf('===== start =====\n');

datdir = '';

Nsbj = 20;
Nitr = 150;

bres.pmat_td = cell(1,Nsbj);
bres.pmat_gr = cell(1,Nsbj);
mres.pmat_td = cell(1,Nsbj);
mres.pmat_gr = cell(1,Nsbj);


param.beta = zeros(1,Nsbj);
param.delta = zeros(1,Nsbj);
param.epsilon = zeros(1,Nsbj);
param.gamma = zeros(1,Nsbj);

for sbj_id = 1:Nsbj
    res_name_b = ['s' num2str(sbj_id) '_1_' model '_res.mat'];
    res_name_m = ['s' num2str(sbj_id) '_0_' model '_res.mat'];
    switch model
        case 'topdown'
            gamma = optimizableVariable('gamma', [0 0.3]);
            optimizefunc = @(X) (calc_evidence(sbj_id, X, model, 1, datdir));
            optresults = bayesopt(optimizefunc, gamma, 'MaxObjectiveEvaluations', Nitr, 'NumSeedPoints', 128, 'Verbose', 0, 'PlotFcn', []);

            gamma_opt = optresults.XAtMinObjective.gamma;
            
            [pmat_td, pmat_gr] = TM_topdown(sbj_id, 1, gamma_opt, datdir);
            bres.pmat_td{sbj_id} = pmat_td;
            bres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_b ' pmat_td pmat_gr']);

            [pmat_td, pmat_gr] = TM_topdown(sbj_id, 0, gamma_opt, datdir);
            mres.pmat_td{sbj_id} = pmat_td;
            mres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_m ' pmat_td pmat_gr']);

            param.gamma(sbj_id) = gamma_opt;
        
        case 'parallel'
            delta = optimizableVariable('delta', [1 3]);
            gamma = optimizableVariable('gamma', [0 0.3]);
            optimizefunc = @(X) (calc_evidence(sbj_id, X, model, 1, datdir));
            optresults = bayesopt(optimizefunc, [delta,gamma], 'MaxObjectiveEvaluations', Nitr, 'NumSeedPoints', 128, 'Verbose', 0, 'PlotFcn', []);

            delta_opt = optresults.XAtMinObjective.delta;
            gamma_opt = optresults.XAtMinObjective.gamma;
            
            [pmat_td, pmat_gr] = TM_parallel(sbj_id, 1, delta_opt, gamma_opt, datdir);
            bres.pmat_td{sbj_id} = pmat_td;
            bres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_b ' pmat_td pmat_gr']);
            
            [pmat_td, pmat_gr] = TM_parallel(sbj_id, 0, delta_opt, gamma_opt, datdir);
            mres.pmat_td{sbj_id} = pmat_td;
            mres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_m ' pmat_td pmat_gr']);

            param.delta(sbj_id) = delta_opt;
            param.gamma(sbj_id) = gamma_opt;

        case 'hierarchical'
            beta = optimizableVariable('beta', [0.5 0.999]);
            delta = optimizableVariable('delta', [1 3]);
            gamma = optimizableVariable('gamma', [0 0.3]);
            epsilon = optimizableVariable('epsilon', [0 1]);
            optimizefunc = @(X) (calc_evidence(sbj_id, X, model, 1, datdir));
            optresults = bayesopt(optimizefunc, [beta,delta,epsilon,gamma], 'MaxObjectiveEvaluations', Nitr, 'NumSeedPoints', 128, 'Verbose', 0, 'PlotFcn', []);
            
            beta_opt = optresults.XAtMinObjective.beta;
            delta_opt = optresults.XAtMinObjective.delta;
            epsilon_opt = optresults.XAtMinObjective.epsilon;
            gamma_opt = optresults.XAtMinObjective.gamma;
            
            [pmat_td, pmat_gr] = TM_hierarchical(sbj_id, 1, beta_opt, delta_opt, epsilon_opt, gamma_opt, datdir);
            bres.pmat_td{sbj_id} = pmat_td;
            bres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_b ' pmat_td pmat_gr']);

            [pmat_td, pmat_gr] = TM_hierarchical(sbj_id, 0, beta_opt, delta_opt, epsilon_opt, gamma_opt, datdir);
            mres.pmat_td{sbj_id} = pmat_td;
            mres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_m ' pmat_td pmat_gr']);
            
            param.beta(sbj_id) = beta_opt;
            param.delta(sbj_id) = delta_opt;
            param.epsilon(sbj_id) = epsilon_opt;
            param.gamma(sbj_id) = gamma_opt;

        case 'hierarchical_noreest'
            beta = optimizableVariable('beta', [0.5 0.999]);
            delta = optimizableVariable('delta', [1 3]);
            gamma = optimizableVariable('gamma', [0 0.3]);
            optimizefunc = @(X) (calc_evidence(sbj_id, X, model, 1, datdir));
            optresults = bayesopt(optimizefunc, [beta,delta,gamma], 'MaxObjectiveEvaluations', Nitr, 'NumSeedPoints', 128, 'Verbose', 0, 'PlotFcn', []);
            
            beta_opt = optresults.XAtMinObjective.beta;
            delta_opt = optresults.XAtMinObjective.delta;
            gamma_opt = optresults.XAtMinObjective.gamma;
            
            [pmat_td, pmat_gr] = TM_hierarchical_noreest(sbj_id, 1, beta_opt, delta_opt, gamma_opt, datdir);
            bres.pmat_td{sbj_id} = pmat_td;
            bres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_b ' pmat_td pmat_gr']);

            [pmat_td, pmat_gr] = TM_hierarchical_noreest(sbj_id, 0, beta_opt, delta_opt, gamma_opt, datdir);
            mres.pmat_td{sbj_id} = pmat_td;
            mres.pmat_gr{sbj_id} = pmat_gr;
            %eval(['save ' datdir '/' res_name_m ' pmat_td pmat_gr']);
            
            param.beta(sbj_id) = beta_opt;
            param.delta(sbj_id) = delta_opt;
            param.gamma(sbj_id) = gamma_opt;            
    end
    fprintf('%2d ',sbj_id);
end
end


function nle = calc_evidence(sbj_id, param, model, mode, datdir)

switch model
    case 'topdown'
        [pmat_td, pmat_gr] = TM_topdown(sbj_id, mode, param.gamma, datdir);

    case 'parallel'
        [pmat_td, pmat_gr] = TM_parallel(sbj_id, mode, param.delta, param.gamma, datdir);

    case 'hierarchical'
        [pmat_td, pmat_gr] = TM_hierarchical(sbj_id, mode, param.beta, param.delta, param.epsilon, param.gamma, datdir);

    case 'hierarchical_noreest'
        [pmat_td, pmat_gr] = TM_hierarchical_noreest(sbj_id, mode, param.beta, param.delta, param.gamma, datdir);
end

resp = [];
esttd = [];
estgr = [];
cf_esttd = [];
cf_estgr = [];
for s = 1:4
    dat = load([datdir '/s' num2str(sbj_id) '_' num2str(mode) '_' num2str(s) '.mat']);
    resp = [resp data.resp];
    esttd = [esttd data.esttigloc];
    estgr = [estgr data.estroom];
    cf_esttd = [cf_esttd data.conf_esttigloc];
    cf_estgr = [cf_estgr data.conf_estroom];
end
esttd = esttd(~isnan([NaN resp(1:end-1)])); % exclude no-responce trial
estgr = estgr(~isnan([NaN resp(1:end-1)]));
cf_esttd = cf_esttd(~isnan([NaN resp(1:end-1)]));
cf_estgr = cf_estgr(~isnan([NaN resp(1:end-1)]));

include = (~isnan(esttd.*cf_esttd.*estgr.*cf_estgr))&((esttd.*cf_esttd.*estgr.*cf_estgr)~=0); % exclude no prediction/confidence response trials

lik_esttd = sum(pmat_td(include,:).*onehotencode(esttd(include),1,'ClassNames',1:3)',2);
lik_estgr = sum(pmat_gr(include,:).*onehotencode(estgr(include),1,'ClassNames',1:Ngrid)',2);

eta_td = 1/log(3);
eta_gr = 1/log(Ngrid);

nle = eta_td*(-sum(log(lik_esttd))) + eta_gr*(-sum(log(lik_estgr)));

end