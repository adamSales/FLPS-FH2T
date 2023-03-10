library(rstan)
#library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(tikzDevice)
library(tidyr)




load('fittedModels/classicPS.RData')
load('fittedModels/flpsRasch1.RData')
load('fittedModels/flps2plStan2.RData')
load('fittedModels/grm2.RData')

raschsumm=summary(flpsRasch1)
raschsumm=raschsumm$summary
tplsumm <- summary(flps2pl)
tplsumm <- tplsumm$summary
grmsumm <- summary(fit)
grmsumm <- grmsumm$summary



### check convergence
rbind(
    data.frame(rhat=raschsumm[,'Rhat'],model='rasch'),
    data.frame(rhat=tplsumm[,'Rhat'],model='2PL'),
    data.frame(rhat=grmsumm[,'Rhat'],model='GRM'))%>%
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
    
pdf('plots/studEffs.pdf')

pStud <- with(sdat,
  vapply(1:nstud,function(i) mean(firstTry[studentM==i]),1.1))


pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n())%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
bind_cols(prop.correct=pStud)%>%
select(-id)%>%
pairs()
dev.off()

nProbDat=pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n())%>%
ungroup()%>%
bind_rows(tibble(id=1:sdat$nstud,est=pStud,par='\\hat{p}',model='Classic'))%>%
group_by(model)%>%
mutate(nProb=vapply(1:sdat$nstud,function(i) sum(sdat$studentM==i),1))%>%
filter(nProb>0)

nProbDat%>%
mutate(fac=factor(paste0(model,' (',par,')'),levels=paste0(modelOrd,' (',c('\\hat{p}',rep('\\eta_T',3)),')')))%>%
ggplot(aes(nProb,est))+geom_point()+facet_wrap(~fac,scales="free_y")+ theme(text = element_text(size = 30))   
ggsave('plots/nprobWorked.pdf',width=6.3,height=4)

nProbDat%>%
summarize(rho=cor(nProb,est,method='spearman'))

studAvgDiff=vapply(1:sdat$nstud,function(i) mean(raschProbEff[sdat$prob[sdat$studentM==i]]),1.1)
studAvgDat=
pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n())%>%
ungroup()%>%
bind_rows(tibble(id=1:sdat$nstud,est=pStud,par='\\hat{p}',model='Classic'))%>%
group_by(model)%>%
mutate(avgDiff=studAvgDiff)%>%
filter(is.finite(avgDiff))

studAvgDat%>%
mutate(fac=factor(paste0(model,' (',par,')'),levels=paste0(modelOrd,' (',c('\\hat{p}',rep('\\eta_T',3)),')')))%>%
ggplot(aes(avgDiff,est))+geom_point()+geom_smooth(method="lm")+facet_wrap(~fac,scales="free_y") + theme(text = element_text(size = 30))  
ggsave('plots/avgDiff.pdf',width=6.3,height=4)

studAvgDat%>%
summarize(rho=cor(avgDiff,est,method='spearman'))


pd%>%
filter(par=="disc.")%>%
droplevels()%>%
group_by(model)%>%
mutate(id=1:n())%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
select(-id)%>%
ggplot(aes(GRM,`2PL`))+geom_point()+ theme(text = element_text(size = 30))  
ggsave('plots/disc.pdf')


pdf('plots/diff.pdf')

pProb= with(sdat,
  vapply(1:nprob,function(i) mean(firstTry[prob==i]),1.1))


pd%>%
filter(par=="diff.")%>%
droplevels()%>%
group_by(model)%>%
mutate(id=1:n())%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
select(-id)%>%
bind_cols(prop.correct=pProb)%>%pairs()
dev.off()


ggplot(pd,aes(par,est))+geom_violin()+geom_jitter(alpha=0.2)+geom_boxplot(width=0.1,outlier.shape=NA)+
    facet_wrap(~model,scales='free_x')+
    scale_x_discrete(labels=c("\\eta_T"=expression(eta[T]),"d_1"=expression(d[1]),"d_2"=expression(d[2])))+
    theme(text = element_text(size = 30))  
ggsave('plots/measurementPars.pdf')


#######################################################
# change parameterization
# for 2pl prob. intercept= -diff*disc

pd2=pd
pd2$est[pd2$par=='diff.'&pd2$model=='2PL']=
  with(rstan::extract(flps2pl),
       colMeans(gamma*(sweep(beta,1,mu_beta,"+"))))

pd2$est[pd2$par=='d_1'&pd2$model=='GRM']=
  with(rstan::extract(fit),
       colMeans(gamma*beta[,,1]))

pd2$est[pd2$par=='d_2'&pd2$model=='GRM']=
  with(rstan::extract(fit),
       colMeans(gamma*beta[,,2]))

pd2$par[pd2$par=='diff.'] = 'd'
pd2$par[pd2$par=='disc.'] = 'a'

ggplot(pd2,aes(par,est))+geom_violin()+geom_jitter(alpha=0.2)+geom_boxplot(width=0.1,outlier.shape=NA)+
    facet_wrap(~model,scales='free_x')+
    scale_x_discrete(labels=c("\\eta_T"=expression(eta[T]),"d_1"=expression(d[1]),"d_2"=expression(d[2])))+
    theme(text = element_text(size = 30))+labs(x=NULL,y="Point Estimates")  
ggsave('plots/measurementParsSlopeInt.pdf')






betaY=
bind_rows(
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(summObs[grep('betaY',rownames(summObs)),]),
  model='obs'
  ),
    tibble(terms=c(colnames(sdat$X)),
  as.data.frame(raschsumm[grep('betaY',rownames(raschsumm)),]),
  model='rasch'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(tplsumm[grep('by',rownames(tplsumm)),]),
  model='2pl'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(grmsumm[grep('by',rownames(grmsumm)),]),
  model='grm'
  )
)%>%mutate(lhs='Y')

betaU=
bind_rows(
  tibble(terms=c('Int',colnames(sdat$X)),
  as.data.frame(summObs[grep('betaU',rownames(summObs)),]),
  model='obs'
  ),
    tibble(terms=c(colnames(sdat$X)),
  as.data.frame(raschsumm[grep('betaU',rownames(raschsumm)),]),
  model='rasch'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(tplsumm[grep('bu',rownames(tplsumm)),]),
  model='2pl'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(grmsumm[grep('bu',rownames(grmsumm)),]),
  model='grm'
  )
)%>%mutate(lhs='U')


bind_rows(betaU,betaY)%>%
filter(!startsWith(terms,'as.factor(teach)'))%>%
ggplot(aes(terms,mean,ymin=mean-2*sd,ymax=mean+2*sd,color=model))+
geom_point(position=position_dodge(width=0.5))+
geom_errorbar(position=position_dodge(width=0.5),width=0)+
geom_hline(yintercept=0)+
facet_wrap(~lhs,scale="free_x")+
coord_flip()+ theme(text = element_text(size = 30))  
ggsave("plots/coef.pdf")





###############3
### summaries from main model
#################

draws=rstan::extract(flpsRasch1)

### for "multImp" and "trtEff"
set.seed(613)
U <- draws$studEff
Usamp <- U[sample(1:nrow(U),1000),]

### for sampleSizeEta & etaDiff
draw <- 1000
U <- U[,sort(unique(sdat$studentM))]
eta <- U[draw,]
etasd <- apply(U,2,sd)

### for "usageModel"
sdEta <- sqrt(mean(apply(draws$studEff,1,var)))
Eeta <- colMeans(draws$studEff)

#draws$studEff <- Usamp


#save(draws,draw,eta,etasd,sdEta,Eeta,summMain,Usamp,file='output/smallMain.RData')


##############################
#### Potential Outcomes Plot
###########################

a0 <- rnorm(length(draws$a1),mean(sdat$Y[sdat$Z==0]),sd(sdat$Y[sdat$Z==0])/sqrt(sum(sdat$Z==0)))
a1 <- draws$a1
b0 <- draws$b0
b1 <- draws$b1

studEff95 <- quantile(Usamp,c(0.025,0.975))
xx <- seq(studEff95[1],studEff95[2],length=100)
Yc <- outer(a1,xx)
Yc <- sweep(Yc,1,a0,'+')

YcUp <- apply(Yc,2,function(x) quantile(x,0.975))
YcDown <- apply(Yc,2,function(x) quantile(x,0.025))


Yt <- outer(a1+b1,xx)
Yt <- sweep(Yt,1,a0+b0,'+')
YtUp <- apply(Yt,2,function(x) quantile(x,0.975))
YtDown <- apply(Yt,2,function(x) quantile(x,0.025))

pdf('plots/potentialOutcomes.pdf',width=6,height=6)
curve(mean(a0)+mean(a1)*x,from=min(xx), to=max(xx),lwd=2,col='red',xlab=expression(eta[T]),ylab=expression(paste('E[',Y[Z],'|',eta[T],']',sep='')),ylim=range(c(YtDown,YcDown,YtUp,YcUp)),cex.lab=1.25)

curve(mean(a0)+mean(b0)+(mean(b1)+mean(a1))*x,add=TRUE,lwd=2,col='blue')
polygon(c(xx,rev(xx)),c(YcUp,rev(YcDown)),col=adjustcolor('red',0.1))
polygon(c(xx,rev(xx)),c(YtUp,rev(YtDown)),col=adjustcolor('blue',0.1))

legend('topleft',legend=c(expression(Y[C]),expression(Y[T])),col=c('red','blue'),lwd=2)
dev.off()

############################
######## main effect plot
#########################


pdMod <- function(mod,row=1,column=1,func){
    draws <- rstan::extract(mod)
    if('alpha'%in%names(draws)) draws$studEff=draws$alpha
    samp <- seq(1,length(draws$b1),length=1000)
    Usamp <- draws$studEff[samp,]
    iqr <- apply(Usamp,1,IQR)
    studEff95 <- quantile(Usamp,c(0.025,0.975))
    Usamp[Usamp<studEff95[1] | Usamp>studEff95[2]] <- NA
    trtEff <- sweep(sweep(Usamp,1,draws$b1[samp],'*'),1,draws$b0[samp],'+')



    if(missing(func)){
        func <- function(x) mean(draws$b0)+mean(draws$b1)*x
        knownTruth <- FALSE
    } else knownTruth <- TRUE
    truth <- curve(func,from=studEff95[1],to=studEff95[2],n=length(samp)/3)
    avg <- curve(mean(draws$b0)+x*mean(draws$b1),
                 from=studEff95[1],to=studEff95[2],n=length(samp)/3)
    postDraw <- curve(mean(draws$b0)+x*mean(draws$b1),
                      from=studEff95[1],to=studEff95[2],n=length(samp)-length(truth$x)-length(avg$x))
    x <- c(postDraw$x,truth$x,avg$x)
    y <- c(postDraw$y,truth$y,avg$y)
    if(knownTruth) truthOrAvg <- c(rep('Posterior\nDraws',length(postDraw$x)),rep('True\nEffect',length(truth$x)),rep('Posterior\nAverage',length(avg$x))) else
     truthOrAvg <- c(rep('Posterior\nDraws',length(postDraw$x)),rep('Posterior\nAverage',length(avg$x)+length(truth$x)))

#    if(knownTruth) title <- paste('True Effe

    pd <- data.frame(b0=draws$b0[samp],b1=draws$b1[samp],id=1:length(samp),row=row,column=column,xmin=studEff95[1],xmax=studEff95[2],ymin=min(trtEff,na.rm=T),ymax=max(trtEff,na.rm=T),x=x,y=y,
                     truthOrAvg=truthOrAvg,
                     iqr=iqr)
    pd
}

effectDat=bind_rows(
  #pdMod(psObs)%>%mutate(model='Classic')#,
  pdMod(flpsRasch1)%>%mutate(model='Rasch'),  
  pdMod(flps2pl)%>%mutate(model='2PL'),
  pdMod(fit)%>%mutate(model='GRM')
)%>%mutate(model=factor(model,levels=modelOrd))

pdRasch <- 
## effectDat <- within(effectDat,
## {
##     b0 <- b0/pooledSD
##     b1 <- b1/pooledSD*iqr
##     xmin <- xmin/mean(iqr)
##     xmax <- xmax/mean(iqr)
##     ymin <- ymin/pooledSD
##     ymax <- ymax/pooledSD
## }
## )
tikz('figure/mainEffects.tex', standAlone=T,
     width=6,height=5)
#print(

  ggplot(effectDat)+
    geom_abline(aes(intercept=b0,slope=b1,group=id),color='red')+
    coord_cartesian(xlim=c(min(effectDat$xmin),max(effectDat$xmax)),
                    ylim=c(min(effectDat$ymin),max(effectDat$ymax)),expand=FALSE)+
    geom_line(aes(x=x,y=y,group=truthOrAvg,linetype=truthOrAvg,color=truthOrAvg,alpha=truthOrAvg),size=1.5)+
              geom_hline(yintercept=0)+
    xlab('$\\eta_T$')+ylab('$\\hat{\\tau}(\\eta_T)$')+
    labs(group=NULL,color=NULL,linetype=NULL)+
    scale_color_manual(values=c('black','red','black'))+scale_linetype_manual(values=c('solid','solid','dotted'))+
    scale_alpha_manual(values=c(1,0,1),guide=FALSE)+theme(legend.position='top')+
    theme(text=element_text(size=15),legend.key.width=unit(.5,'in'))+facet_wrap(~model)



dev.off()
setwd('figure'); tools::texi2dvi('mainEffects.tex', pdf = T, clean = T); setwd('..')



##################################################
### eta vs outcomes
##################################################

sdatObs <- with(sdat,
                list(
                  ncov=ncov,
                  nstudT=sum(Z),
                  nstudC=nstud-sum(Z),
                  propT=vapply(1:nstud,function(i) mean(firstTry[studentM==i]),.3)[Z==1],
                  Xt=X[Z==1,],
                  Xc=X[Z==0,],
                  Yt=Y[Z==1],
                  Yc=Y[Z==0]))

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
  draws=rstan::extract(model)
  drawMb <- which.min(abs(draws$b1-mean(draws$b1)))

  plotDat=with(sdat,
    data.frame(
      Y=Y,
      mbar=if('alpha'%in%names(draws)) draws$alpha[drawMb,] else draws$studEff[drawMb,],
      treat=ifelse(Z==1,'Treatment','Control'),
      slope=draws$a1[drawMb]+ifelse(Z==0,0,draws$b1[drawMb]),
      model=modelName
    ) 
  )
  plotDat <- within(
    plotDat,
    int <- mean(Y[treat=='Control'])-
          mean(slope[treat=='Control'])*mean(mbar[treat=='Control'])+
          ifelse(treat=='Control',0,draws$b0[drawMb])
)

  plotDat[order(plotDat$treat),]
}

etaYdat=bind_rows(
  plotDatObs,
  etaYdatFun(flpsRasch1,sdat,'Rasch'),
  etaYdatFun(flps2pl,sdat,'2PL'),
  etaYdatFun(fit,sdat,'GRM')
)



tikz(file = "figure/etaYModel.tex",
  standAlone = T,
  width  = 6, height  = 6)
print(
  ggplot(etaYdat,aes(mbar,Y,fill=treat,group=treat,color=treat))+geom_point(size=1)+
      geom_abline(aes(intercept=int,slope=slope,color=treat),size=2)+
    scale_colour_manual(values=c('red','blue'))+
    labs(group=NULL,fill=NULL,alpha=NULL)+xlab('$\\bar{m}_T$')+
    ylab('Posttest Score')+theme(legend.position='top',text=element_text(size=15))+
    guides(
      color = guide_legend(title=NULL,override.aes=list(alpha=1,size=3),keywidth=3),
      linetype=guide_legend(title=NULL,keywidth=1,override.aes=list(size=1))
    )+
    facet_wrap(~model,scales="free_x")
  )#override.aes=list(size=2)))

dev.off()




###### 