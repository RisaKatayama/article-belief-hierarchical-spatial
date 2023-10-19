library(coin)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(pracma)
library(R.matlab)
library(bmsR)


dat.dir = ''
mres.dir = '/Users/katayama-r/Documents/TigerMaze/scripts/model'
load(paste0(dat.dir,'data.RData'))
cpalette = c('#bebebe','#ff9900','#99ff00','#ff3333','#3366ff')

ns = max(data$sbjno)
maxtrl = 12
maxlistrl = 6
maxmovtrl = 6
include = !is.na(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr)&(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr!=0)
n = length(include)

pmat.td = c()
pmat.gr = c()
for (i in 1:ns){
    bres = readMat(paste0(mres.dir,'/behavioural/s',i,'_1_hierarchical_res.mat'))
    pmat.td = rbind(pmat.td,bres$pmat.esttd)
    pmat.gr = rbind(pmat.gr,bres$pmat.estgr)
}
for (i in 1:ns){
    bres = readMat(paste0(mres.dir,'/scanning/s',i,'_0_hierarchical_res.mat'))
    pmat.td = rbind(pmat.td,bres$pmat.esttd)
    pmat.gr = rbind(pmat.gr,bres$pmat.estgr)
}
HI = list(pmat.td=pmat.td, pmat.gr=pmat.gr)

epy.HI = data.frame(esttd = -apply(HI$pmat.td*log(HI$pmat.td),1,nansum),
                    estgr = -apply(HI$pmat.gr*log(HI$pmat.gr),1,nansum))


epytd.cf = array(NaN, dim=c(ns,2))
epygr.cf = array(NaN, dim=c(ns,2))
for (i in 1:ns){
    for (c in 1:2){
        epytd.cf[i,c] = nanmean(epy.HI$esttd[data$sbjno==i&data$conf.esttd==c&include])
        epygr.cf[i,c] = nanmean(epy.HI$estgr[data$sbjno==i&data$conf.estgr==c&include])
    }
}

## Fig 3b ####
dat = data.frame(epy=array(t(epytd.cf)), cf=factor(rep(c('l','h'),ns),levels=c('l','h')))
dat = dat[!is.na(dat$epy),]

fig3b.td = ggplot(data=dat, aes(x=cf, y=epy, group=cf)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),name="Entropy of TD inference") +
        scale_x_discrete(labels=c("Low","High"),name='TD prediction\nconfidence in trial t') +
        theme_classic()
fig3b.td

dat = data.frame(epy=array(t(epygr.cf)), cf=factor(rep(c('l','h'),ns),levels=c('l','h')))
dat = dat[!is.na(dat$epy),]

fig3b.gr = ggplot(data=dat, aes(x=cf, y=epy, group=cf)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.4,0.4),name="Entropy of GR inference") +
        scale_x_discrete(labels=c("Low","High"),name='GR prediction\nconfidence in trial t') +
        theme_classic()
fig3b.gr


epys.trl.lis = array(NaN, dim=c(ns,maxlistrl,2))
for (i in 1:ns){
    for (t in 1:maxlistrl){
        epys.trl.lis[i,t,1] = nanmean(epy.HI$esttd[data$sbjno==i&data$predlistrl==t&include])
        epys.trl.lis[i,t,2] = nanmean(epy.HI$estgr[data$sbjno==i&data$predlistrl==t&include])
    }
}
epys.trl.mov = array(NaN, dim=c(ns,maxmovtrl,2))
for (i in 1:ns){
    for (t in 1:maxlistrl){
        epys.trl.mov[i,t,1] = nanmean(epy.HI$esttd[data$sbjno==i&data$predmovtrl==t&include])
        epys.trl.mov[i,t,2] = nanmean(epy.HI$estgr[data$sbjno==i&data$predmovtrl==t&include])
    }
}

## Fig 3c ####
dat = data.frame(epy=array(aperm(epys.trl.lis,c(2,1,3))),trl=factor(rep(1:maxlistrl,ns*2)),
                 typ=factor(c(rep('td',ns*maxlistrl),rep('gr',ns*maxlistrl)),levels=c('td','gr')))
dat = dat[!is.na(dat$epy),]

fig3cl = ggplot(data=dat, aes(x=trl, y=epy, fill=typ, group=interaction(trl,typ), dodge=interaction(trl,typ))) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4, position=position_dodge(width=0.75)) +
        geom_boxplot(outlier.shape=3, outlier.size=1.2) +
        scale_fill_manual(values=cpalette[2:3]) +
        scale_y_continuous(limits=c(0,3),breaks=seq(0,3,1),labels=scales::number_format(accuracy=0.1),
                            name="Entropy of GR inference",sec.axis=sec_axis(~./2,name="Entropy of TD inference")) +
        scale_x_discrete(name="Listen trial number\nin a single game") +
        theme_classic()
fig3cl

dat = data.frame(epy=array(aperm(epys.trl.mov,c(2,1,3))),trl=factor(rep(1:maxmovtrl,ns*2)),
                 typ=factor(c(rep('td',ns*maxmovtrl),rep('gr',ns*maxmovtrl)),levels=c('td','gr')))
dat = dat[!is.na(dat$epy),]

fig3cm = ggplot(data=dat, aes(x=trl, y=epy, fill=typ, group=interaction(trl,typ), dodge=interaction(trl,typ))) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4, position=position_dodge(width=0.75)) +
        geom_boxplot(outlier.shape=3, outlier.size=1.2) +
        scale_fill_manual(values=cpalette[2:3]) +
        scale_y_continuous(limits=c(0,3),breaks=seq(0,3,1),labels=scales::number_format(accuracy=0.1),
                            name="Entropy of GR inference",sec.axis=sec_axis(~./2,name="Entropy of TD inference")) +
        scale_x_discrete(name="Move trial number\nin a single game") +
        theme_classic()
fig3cm


epytd.act = array(NaN, dim=c(ns,2))
epygr.act = array(NaN, dim=c(ns,2))
for (i in 1:ns){
    epytd.act[i,1] = nanmean(epy.HI$esttd[data$sbjno==i&data$resp==4&include])
    epytd.act[i,2] = nanmean(epy.HI$esttd[data$sbjno==i&data$resp!=4&include])
    epygr.act[i,1] = nanmean(epy.HI$estgr[data$sbjno==i&data$resp==4&include])
    epygr.act[i,2] = nanmean(epy.HI$estgr[data$sbjno==i&data$resp!=4&include])
}

## Fig 3d ####
dat = data.frame(epy=array(t(epytd.act)), act=factor(rep(c('l','m'),ns),levels=c('l','m')))
dat = dat[!is.na(dat$epy),]

fig3d.td = ggplot(data=dat, aes(x=act, y=epy, group=act)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,1.2), breaks=seq(0,1.2,0.2), name="Prior entropy of TD inference") +
        scale_x_discrete(labels=c("Listen","Move"), name='Action in trial t') +
        theme_classic()
fig3d.td

dat = data.frame(epy=array(t(epygr.act)), act=factor(rep(c('l','m'),ns),levels=c('l','m')))
dat = dat[!is.na(dat$epy),]

fig3d.gr = ggplot(data=dat, aes(x=act, y=epy, group=act)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        scale_y_continuous(limits=c(0,2.5), breaks=seq(0,2.5,0.5), name="Prior entropy of GR inference") +
        scale_x_discrete(labels=c("Listen","Move"), name='Action in trial t') +
        theme_classic()
fig3d.gr


cor.epys = array(NaN, dim=c(ns,2)) # [sbj x act(lis, mov)]
for (i in 1:ns){
    w = cor.test(epy.HI$esttd[data$sbjno==i&data$predlistrl>0&include],
                 epy.HI$estgr[data$sbjno==i&data$predlistrl>0&include])
    cor.epys[i,1] = w$estimate
    w = cor.test(epy.HI$esttd[data$sbjno==i&data$predmovtrl>0&include],
                 epy.HI$estgr[data$sbjno==i&data$predmovtrl>0&include])
    cor.epys[i,2] = w$estimate
}

## Fig 3e left ####
sidx = 6
dat = data.frame(epytd=c(epy.HI$esttd[data$sbjno==sidx&data$predlistrl>0&include],epy.HI$esttd[data$sbjno==sidx&data$predmovtrl>0&include]),
                 epygr=c(epy.HI$estgr[data$sbjno==sidx&data$predlistrl>0&include],epy.HI$estgr[data$sbjno==sidx&data$predmovtrl>0&include]),
                 act=factor(c(rep('l',sum(data$sbjno==sidx&data$predlistrl>0&include)),rep('m',sum(data$sbjno==sidx&data$predmovtrl>0&include))),levels=c('l','m')))

fig3e1 = ggplot(data=dat, aes(x=epytd, y=epygr, group=act, color=act)) +
        geom_point(shape=16, size=1.5, alpha=0.8) +
        geom_smooth(method="lm", se=FALSE, size=1, alpha=1) +
        scale_color_manual(values=cpalette[4:5], labels=c('Listen','Move')) +
        scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1.2,0.4), name='Entropy of TD inference') +
        scale_y_continuous(limits=c(0,3), breaks=seq(0,3,1), name='Entropy of GR inference') +
        theme_classic()
fig3e1

## Fig 3e right ####
dat = data.frame(r=array(t(cor.epys)), act=factor(rep(c('l','m'),ns),levels=c('l','m')))

fig3e2 = ggplot(data=dat, aes(x=act, y=r, fill=act, group=act)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.25) +
        geom_boxplot(outlier.shape=3, outlier.size=1.2, width=0.6) +
        scale_fill_manual(values=cpalette[4:5], labels=c('Listen','Move')) +
        scale_y_continuous(limits=c(0,1.15), breaks=seq(0,1,0.2), name="Correlation of entropy between\nTD and GR inference") +
        scale_x_discrete(labels=c('Listen','Move'), name='Action in trial t') +
        theme_classic()
fig3e2