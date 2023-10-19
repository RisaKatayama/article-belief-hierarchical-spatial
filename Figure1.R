library(coin)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(pracma)


dat.dir = ''
load(paste0(dat.dir,'data.RData'))
cpalette = c('#bebebe','#ff9900','#99ff00','#ff3333','#3366ff')


ns = max(data$sbjno)
include = !is.na(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr)&(data$esttd*data$conf.esttd*data$estgr*data$conf.estgr!=0)


acc.td = array(NaN, dim=c(ns,1))
acc.gr = array(NaN, dim=c(ns,1))
for (i in 1:ns){
    td = data$tigdoor[data$sbjno==i&include]
    esttd = data$esttd[data$sbjno==i&include]
    gr = data$grid[data$sbjno==i&include]
    estgr = data$estgr[data$sbjno==i&include]
    acc.td[i] = sum(td==esttd)/length(td)
    acc.gr[i] = sum(gr==estgr)/length(gr)
}


acc.td.cf = array(NaN, dim=c(ns,2))
acc.gr.cf = array(NaN, dim=c(ns,2))
for (i in 1:ns){
    td = data$tigdoor[data$sbjno==i&include]
    esttd = data$esttd[data$sbjno==i&include]
    cftd = data$conf.esttd[data$sbjno==i&include]
    gr = data$grid[data$sbjno==i&include]
    estgr = data$estgr[data$sbjno==i&include]
    cfgr = data$conf.estgr[data$sbjno==i&include]
    for (c in 1:2) {
        acc.td.cf[i,c] = sum(td==esttd&cftd==c)/sum(cftd==c)
        acc.gr.cf[i,c] = sum(gr==estgr&cfgr==c)/sum(cfgr==c)
    }
}

## Fig 1c ####
dat = data.frame(acc=array(t(acc.td.cf)), cf=factor(rep(c('h','l'))))
fig1c.td = ggplot(data=dat, aes(x=cf, y=acc, group=cf)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        geom_hline(yintercept=1/3, linetype="dashed") +
        scale_y_continuous(limits=c(0,1.2),breaks=seq(0,1,0.2),name="TD prediction accuracy in trial t") +
        scale_x_discrete(labels=c("Low","High"),name='TD prediction\nconfidence in trial t') +
        theme_classic()
fig1c.td

dat = data.frame(acc=array(t(acc.gr.cf)), cf=factor(rep(c('h','l'))))
fig1c.gr = ggplot(data=dat, aes(x=cf, y=acc, group=cf)) +
        stat_boxplot(geom='errorbar', size=0.6, width=0.4) +
        geom_boxplot(fill=cpalette[1], outlier.shape=3, outlier.size=1.2) +
        geom_hline(yintercept=1/16, linetype="dashed") +
        scale_y_continuous(limits=c(0,1.2),breaks=seq(0,1,0.2),name="GR prediction accuracy in trial t") +
        scale_x_discrete(labels=c("Low","High"),name='GR prediction\nconfidence in trial t') +
        theme_classic()
fig1c.gr