library(coin)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(pracma)


dat.dir = ''
load(paste0(dat.dir,'data.RData'))
cpalette = c('#bebebe','#ff9900','#99ff00','#ff3333','#3366ff','#481567','#1f968b','#fde725')


ns = max(data$sbjno)
maxtrl = 12
maxlistrl = 6
maxmovtrl = 6
include = !is.na(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr)&(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr!=0)
n = length(include)


prp.lis = array(NaN, dim=c(ns,maxtrl))
for (i in 1:ns){
for (t in 1:maxtrl){
    rsp = data$resp[data$sbjno==i&c(include[2:n],FALSE)]
    trl = data$trial[data$sbjno==i&c(include[2:n],FALSE)]
    prp.lis[i,t] = sum(rsp==4&trl==t)/sum(trl==t)
}
}

## Fig 2a ####
dat = data.frame(prp=array(t(prp.lis)), trl=factor(rep(c(1:maxtrl))))
dat = dat[!is.na(dat$prp),]
fig2a = ggplot(data=dat, aes(x=trl, y=prp, group=trl)) +
    stat_boxplot(geom='errorbar',size=0.6,width=0.4) +
    geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
    scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,0.2),name="Proportion of Listen trial") +
    scale_x_discrete(name="Trial number in a single game\n") +
    theme_classic()
fig2a


prp.lis.cfs = array(NaN, dim=c(ns,2,2)) # [sbj x cftd(l,h) x cfgr(l,h)]
for (i in 1:ns){
    rsp = data$resp[data$sbjno==i&include]
    cftd = data$conf.esttd[data$sbjno==i&include]
    cfgr = data$conf.estgr[data$sbjno==i&include]
    for (ct in 1:2){
        for (cg in 1:2){
            prp.lis.cfs[i,ct,cg] = sum(rsp==4&cftd==ct&cfgr==cg)/sum(cftd==ct&cfgr==cg)
        }
    }
}

## Fig 2b ####
dat = data.frame(prp=array(aperm(prp.lis.cfs,c(2,3,1))), cfs=factor(rep(c('tg','Tg','tG','TG')),levels=c('tg','Tg','tG','TG')))
dat = dat[!is.na(dat$prp),]
  
fig2b = ggplot(data=dat, aes(x=cfs, y=prp, group=cfs)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,1.05), breaks=seq(0,1,0.2), name="Proportion of Listen trial") +
        scale_x_discrete(labels=c('t/g','T/g','t/G','T/G'), name="Confidence levels\nin trial t-1") +
        theme_classic()
fig2b


cfs.trl.lis = array(NaN, dim=c(ns,maxlistrl,2)) # [sbj x trl x (td,gr)]
for (i in 1:ns){
    for (t in 1:maxlistrl){
        cftd = data$conf.esttd[data$sbjno==i&data$predlistrl==t&include]
        cfgr = data$conf.estgr[data$sbjno==i&data$predlistrl==t&include]
        cfs.trl.lis[i,t,1] = sum(cftd==2)/length(cftd)
        cfs.trl.lis[i,t,2] = sum(cfgr==2)/length(cfgr)
    }
}
cfs.trl.mov = array(NaN, dim=c(ns,maxmovtrl,2)) # [sbj x trl x (td,gr)]
for (i in 1:ns){
    for (t in 1:maxmovtrl){
        cftd = data$conf.esttd[data$sbjno==i&data$predmovtrl==t&include]
        cfgr = data$conf.estgr[data$sbjno==i&data$predmovtrl==t&include]
        cfs.trl.mov[i,t,1] = sum(cftd==2)/length(cftd)
        cfs.trl.mov[i,t,2] = sum(cfgr==2)/length(cfgr)
    }
}

## Fig 2c ####
dat = data.frame(cf=array(aperm(cfs.trl.lis,c(2,1,3))), trl=factor(rep(c(1:maxlistrl),ns*2)),
                 typ=factor(c(rep('td',ns*maxlistrl),rep('gr',ns*maxlistrl)),levels=c('td','gr')))
dat = dat[!is.na(dat$cf),]

fig2cl = ggplot(data=dat, aes(x=trl, y=cf, fill=typ, group=interaction(trl,typ), dodge=interaction(trl,typ))) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4, position=position_dodge(width=0.75)) +
        geom_boxplot(outlier.shape=3, outlier.size=1.2) +
        scale_fill_manual(values=cpalette[2:3]) +
        scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,0.2),name="Averaged confidence level") +
        scale_x_discrete(name="Listen trial number\nin a single game") +
        theme_classic()
fig2cl

dat = data.frame(cf=array(aperm(cfs.trl.mov,c(2,1,3))), trl=factor(rep(c(1:maxmovtrl),ns*2)),
                 typ=factor(c(rep('td',ns*maxmovtrl),rep('gr',ns*maxmovtrl)),levels=c('td','gr')))
dat = dat[!is.na(dat$cf),]

fig2cm = ggplot(data=dat, aes(x=trl, y=cf, fill=typ, group=interaction(trl,typ), dodge=interaction(trl,typ))) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4, position=position_dodge(width=0.75)) +
        geom_boxplot(outlier.shape=3, outlier.size=1.2) +
        scale_fill_manual(values=cpalette[2:3]) +
        scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,0.2),name="Averaged confidence level") +
        scale_x_discrete(name="Move trial number\nin a single game") +
        theme_classic()
fig2cm


prp.cfs.lis = array(NaN, dim=c(ns,2,2))
prp.cfs.mov = array(NaN, dim=c(ns,2,2))
for (i in 1:ns){
    cftd = data$conf.esttd[data$sbjno==i&include]
    cfgr = data$conf.estgr[data$sbjno==i&include]
    rsp = data$resp[data$sbjno==i&include]
    for (ct in 1:2){
        for(cg in 1:2){
            prp.cfs.lis[i,ct,cg] = nansum(cftd==ct&cfgr==cg&rsp==4)/nansum(rsp==4)
            prp.cfs.mov[i,ct,cg] = nansum(cftd==ct&cfgr==cg&rsp!=4)/nansum(rsp!=4)
        }
    }
}

## Fig 2d ####
dat = data.frame(prp=array(aperm(prp.cfs.lis,c(2,3,1))), cfs=factor(rep(c('tg','Tg','tG','TG'),ns),levels=c('tg','Tg','tG','TG')))
dat = dat[!is.na(dat$prp),]

fig2dl = ggplot(data=dat, aes(x=cfs, y=prp, group=cfs)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,0.2),name="Proportion of trial in each condition") +
        scale_x_discrete(labels=c('t/g','T/g','t/G','T/G'),name="Confidence levels") +
        theme_classic()
fig2dl

dat = data.frame(prp=array(aperm(prp.cfs.mov,c(2,3,1))), cfs=factor(rep(c('tg','Tg','tG','TG'),ns),levels=c('tg','Tg','tG','TG')))
dat = dat[!is.na(dat$prp),]

fig2dm = ggplot(data=dat, aes(x=cfs, y=prp, group=cfs)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,0.2),name="Proportion of trial in each condition") +
        scale_x_discrete(labels=c('t/g','T/g','t/G','T/G'),name="Confidence levels") +
        theme_classic()
fig2dm


tr.mat.lis = array(NaN, dim=c(ns,4,4))
tr.mat.mov = array(NaN, dim=c(ns,4,4))
for (i in 1:ns){
    cftd.t0 = data$conf.esttd[data$sbjno==i&c(include[2:n],NaN)]
    cfgr.t0 = data$conf.estgr[data$sbjno==i&c(include[2:n],NaN)]
    cftd.t1 = c(data$conf.esttd[2:n],NaN)[data$sbjno==i&c(include[2:n],NaN)]
    cfgr.t1 = c(data$conf.estgr[2:n],NaN)[data$sbjno==i&c(include[2:n],NaN)]
    rsp = data$resp[data$sbjno==i&c(include[2:n],NaN)]

    cfs.t0 = (cftd.t0-1) + (cfgr.t0-1)*2 + 1 # tg = 1, Tg = 2, tG = 3, TG = 4
    cfs.t1 = (cftd.t1-1) + (cfgr.t1-1)*2 + 1
    tmp.cfs.t0 = cfs.t0
    tmp.cfs.t1 = cfs.t1
    rsp = rsp[!is.na(cfs.t0*cfs.t1)&cfs.t0>0&cfs.t1>0]
    tmp.cfs.t0 = tmp.cfs.t0[!is.na(cfs.t0*cfs.t1)&cfs.t0>0&cfs.t1>0]
    tmp.cfs.t1 = tmp.cfs.t1[!is.na(cfs.t0*cfs.t1)&cfs.t0>0&cfs.t1>0]
    cfs.t0 = tmp.cfs.t0
    cfs.t1 = tmp.cfs.t1

    for (c0 in 1:4){
        for (c1 in 1:4){
            tr.mat.lis[i,c0,c1] = sum(cfs.t0==c0&cfs.t1==c1&rsp==4)/sum(cfs.t0==c0&rsp==4)
            tr.mat.mov[i,c0,c1] = sum(cfs.t0==c0&cfs.t1==c1&rsp!=4)/sum(cfs.t0==c0&rsp!=4)
        }
    }
}

## Fig 2e ####
med.tr.mat.lis = apply(tr.mat.lis,c(2,3),nanmedian)
cft0.mat = array(rep(c('tg','Tg','tG','TG'),4),dim=c(4,4))
cft1.mat = t(array(rep(c('tg','Tg','tG','TG'),4),dim=c(4,4)))
dat = data.frame(prp=array(med.tr.mat.lis), cft0=factor(array(cft0.mat),levels=c('TG','tG','Tg','tg')),
                 cft1=factor(array(cft1.mat),levels=c('tg','Tg','tG','TG')))

fig2el = ggplot(data=dat, aes(x=cft1, y=cft0, fill=prp)) + 
        geom_tile(color='black') +
        scale_fill_gradient2(low=cpalette[6], mid=cpalette[7], high=cpalette[8], midpoint=0.5,
                             guide=guide_colourbar(title=NULL, frame.colour="black", frame.linewidth=0.8/.pt, ticks.colour='black')) +
        scale_x_discrete(expand=expansion(c(0.25,0.25)), labels=c('t/g','T/g','t/G','T/G'), name="Confidence levels in trial t") +
        scale_y_discrete(expand=expansion(c(0.25,0.25)), labels=c('T/G','t/G','T/g','t/g'),name="Confidence levels in trial t-1") +
        theme_classic()
fig2el

med.tr.mat.mov = apply(tr.mat.mov,c(2,3),nanmedian)
dat = data.frame(prp=array(med.tr.mat.mov), cft0=factor(array(cft0.mat),levels=c('TG','tG','Tg','tg')),
                 cft1=factor(array(cft1.mat),levels=c('tg','Tg','tG','TG')))

fig2em = ggplot(data=dat, aes(x=cft1, y=cft0, fill=prp)) + 
        geom_tile(color='black') +
        scale_fill_gradient2(low=cpalette[6], mid=cpalette[7], high=cpalette[8], midpoint=0.5,
                             guide=guide_colourbar(title=NULL, frame.colour="black", frame.linewidth=0.8/.pt, ticks.colour='black')) +
        scale_x_discrete(expand=expansion(c(0.25,0.25)), labels=c('t/g','T/g','t/G','T/G'), name="Confidence levels in trial t") +
        scale_y_discrete(expand=expansion(c(0.25,0.25)), labels=c('T/G','t/G','T/g','t/g'),name="Confidence levels in trial t-1") +
        theme_classic()
fig2em