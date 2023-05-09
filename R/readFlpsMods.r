library(rstan)
#library(tidyverse)
library(dplyr)
library(ggh4x)
library(GGally)
library(lme4)
library(ggplot2)
library(tikzDevice)
library(tidyr)
library(ggh4x)




print(load('fittedModels/classicPSlogit.RData'))
load('fittedModels/flpsRasch1.RData')
load('fittedModels/flps2plStan2.RData')
load('fittedModels/grm2.RData')

raschsumm=summary(flpsRasch1)
raschsumm=raschsumm$summary
tplsumm <- summary(flps2pl)
tplsumm <- tplsumm$summary
grmsumm <- summary(fit)
grmsumm <- grmsumm$summary
classicsumm <- summary(psObs)$summary



### check convergence
rbind(
    data.frame(rhat=raschsumm[,'Rhat'],model='rasch'),
    data.frame(rhat=tplsumm[,'Rhat'],model='2PL'),
    data.frame(rhat=grmsumm[,'Rhat'],model='GRM'),
    data.frame(rhat=classicsumm[,'Rhat'],model='Classic')   
    )%>%
    group_by(model)%>%mutate(lab=c(paste(n(),'parameters\n',sum(rhat>1.1),'>1.1\n',sum(rhat>1.01),'>1.01'),rep(NA,length(rhat)-1)))%>%
    ggplot(aes(rhat))+geom_histogram(bins=50)+geom_vline(xintercept=c(1.01,1.1))+
    facet_wrap(~model)+geom_label(aes(x=1.03,y=2000,label=lab))
#    geom_label(aes(x=1.03,y=1750,label=over1.1))+
#    geom_label(aes(x=1.03,y=1500,label=over1.01))
ggsave('plots/rhats.pdf')    

raschStudEff <- raschsumm[paste0('studEff[',seq(sdat$nstud),']'),'mean']
raschProbEff <- raschsumm[paste0('probEff[',seq(sdat$nprob),']'),'mean']


modelOrd=c('Classic','Rasch','2PL','GRM')

pd <- bind_rows(
    tibble(est=c(raschStudEff,raschProbEff),par=c(rep('\\eta_T',sdat$nstud),rep('diff.',sdat$nprob)),model='Rasch'),
    tibble(est=tplsumm[startsWith(rownames(tplsumm),'alpha['),'mean'],par='\\eta_T',model='2PL'),
    tibble(est=tplsumm[startsWith(rownames(tplsumm),'beta['),'mean']+tplsumm['mu_beta','mean'],par='diff.',model='2PL'),
    tibble(est=tplsumm[startsWith(rownames(tplsumm),'gamma['),'mean'],par='disc.',model='2PL'),
    tibble(est=grmsumm[startsWith(rownames(grmsumm),'alpha['),'mean'],par='\\eta_T',model='GRM'),
    tibble(est=grmsumm[paste0('beta[',1:sdat$nprob,',1]'),'mean'],par='d_1',model='GRM'),
    tibble(est=grmsumm[paste0('beta[',1:sdat$nprob,',2]'),'mean'],par='d_2',model='GRM'),
    tibble(est=grmsumm[paste0('gamma[',1:sdat$nprob,']'),'mean'],par='disc.',model='GRM'))%>%
    mutate(model=factor(model,levels=modelOrd))
    
#pdf('plots/studEffs.pdf')

pStud <- with(sdat,
  vapply(1:nstud,function(i) mean(firstTry[studentM==i]),1.1))



p=pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n(),model=paste0("\"",model,"  \"*eta"))%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
bind_cols(`logit(prop.correct)`=qlogis(pStud))%>%
select(-id)%>%
filter(is.finite(`logit(prop.correct)`))%>%
pivot_longer(cols=-`\"Rasch  \"*eta`)%>%
ggplot(aes(`\"Rasch  \"*eta`,value))+geom_point()+
facet_wrap(~name,labeller=label_parsed)+
xlab(bquote(Rasch~~eta))


rhoDat=pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n(),model=paste0("\"",model,"  \"*eta"))%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
bind_cols(`logit(prop.correct)`=qlogis(pStud))%>%
select(-id)%>%
filter(is.finite(`logit(prop.correct)`))%>%
pivot_longer(cols=-`\"Rasch  \"*eta`)%>%
group_by(name)%>%
summarize(rho=round(cor(`\"Rasch  \"*eta`,value),2))



lab=as.character(latex2exp::TeX(paste0("$\\rho=$",rhoDat$rho)))#expression(rho == 3))
rhoDat$lab=lab
p+geom_text(data=rhoDat,aes(x=-1,y=3,label=lab),parse=TRUE)+
ylab(NULL)
ggsave("plots/rhoCompare.png",height=3,width=6)


#dev.off()

tikz('plots/measurementPars.tex',width=6.5,height=3,standAlone=TRUE)
ggplot(pd,aes(par,est))+geom_violin()+geom_jitter(alpha=0.2)+geom_boxplot(width=0.1,outlier.shape=NA)+
    facet_grid2(~model,scales='free_x',space="free_x")+
    scale_x_discrete(name=NULL,labels=c(
      "\\eta_T"="$\\eta_T$",
      "d_2"="$d_2$",
      "d_1"="$d_1$"))+   
    #labels=c("\\eta_T"=expression(eta),"d_1"=expression(d[1]),"d_2"=expression(d[2])))+
    theme(text = element_text(size = 12)) +
    ylab("Post. Means") 
dev.off()


tplDraws=rstan::extract(flpsRasch1)

### for "multImp" and "trtEff"
set.seed(613)
U <- tplDraws$studEff
Usamp <- U[sample(1:nrow(U),1000),]

### for sampleSizeEta & etaDiff
draw <- 1000
U <- U[,sort(unique(sdat$studentM))]
eta <- U[draw,]
etasd <- apply(U,2,sd)

### for "usageModel"
sdEta <- sqrt(mean(apply(tplDraws$studEff,1,var)))
Eeta <- colMeans(tplDraws$studEff)

#tplDraws$studEff <- Usamp


#save(tplDraws,draw,eta,etasd,sdEta,Eeta,summMain,Usamp,file='output/smallMain.RData')



##################################################
### eta vs outcomes
##################################################

drawsObs <- rstan::extract(psObs)

drawMb <- which.min(abs(drawsObs$b1-mean(drawsObs$b1)))

plotDatObs <- with(sdatObs,
  data.frame(
    Y=c(Yt,Yc),
    mbar=c(propT,drawsObs$propC[drawMb,]),
    Z=c(rep(1,nstudT),rep(0,nstudC))
  )
)

plotDatObs$treat <- ifelse(plotDatObs$Z==1,'Treatment','Control')
plotDatObs$slope <- (drawsObs$a1[drawMb]+ifelse(plotDatObs$treat=='Control',0,drawsObs$b1[drawMb]))#/pooledSD
plotDatObs <- within(
  plotDatObs,
  int <- mean(Y[treat=='Control'])-
          mean(slope[treat=='Control'])*mean(mbar[treat=='Control'])+
          ifelse(treat=='Control',0,drawsObs$b0[drawMb])
)

#plotDatObs <- within(plotDatObs, int <- int-( mean(int+slope*mbar)-mean(Y)))
plotDatObs <- plotDatObs[order(plotDatObs$treat),]
plotDatObs$treat2 <- plotDatObs$treat
plotDatObs$model='Classic'

etaYdatFun=function(model,sdat,modelName){
  tplDraws=rstan::extract(model)
  drawMb <- which.min(abs(tplDraws$b1-mean(tplDraws$b1)))

  plotDat=with(sdat,
    data.frame(
      Y=Y,
      mbar=if('alpha'%in%names(tplDraws)) tplDraws$alpha[drawMb,] else tplDraws$studEff[drawMb,],
      treat=ifelse(Z==1,'Treatment','Control'),
      slope=tplDraws$a1[drawMb]+ifelse(Z==0,0,tplDraws$b1[drawMb]),
      model=modelName
    ) 
  )
  plotDat <- within(
    plotDat,
    int <- mean(Y[treat=='Control'])-
          mean(slope[treat=='Control'])*mean(mbar[treat=='Control'])+
          ifelse(treat=='Control',0,tplDraws$b0[drawMb])
)

  plotDat[order(plotDat$treat),]
}

etaYdat=bind_rows(
  plotDatObs,
  etaYdatFun(flpsRasch1,sdat,'Rasch'),
  etaYdatFun(flps2pl,sdat,'2PL'),
  etaYdatFun(fit,sdat,'GRM')
)

etaYdat$model=factor(etaYdat$model,levels=modelOrd)

tikz(file = "plots/etaYModel.tex",
  standAlone = T,
  width  = 6, height  = 6)
print(
  ggplot(etaYdat,aes(mbar,Y,fill=treat,group=treat,color=treat))+geom_point(size=1)+
      geom_abline(aes(intercept=int,slope=slope,color=treat),size=2)+
    scale_colour_manual(values=c('red','blue'))+
    labs(group=NULL,fill=NULL,alpha=NULL)+xlab('$\\eta_T$')+
    ylab('Posttest Score')+theme(legend.position='top',text=element_text(size=15))+
    guides(
      color = guide_legend(title=NULL,override.aes=list(alpha=1,size=3),keywidth=3),
      linetype=guide_legend(title=NULL,keywidth=1,override.aes=list(size=1))
    )+
    facet_wrap(~model,scales="free_x")
  )#override.aes=list(size=2)))

dev.off()




###### 