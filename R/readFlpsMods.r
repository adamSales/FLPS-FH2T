library(rstan)
library(tidyverse)
library(lme4)
library(huxtable)
library(broom)
library(flextable)




load('fittedModels/classicPS.RData')
load('fittedModels/flpsRasch1.RData')
summ=summary(flpsRasch1)
summ=summ$summary

drawsRasch <- rstan::extract(flpsRasch1)

pdf('plots/flpsRasch1/rhats.pdf')
hist(summ[,'Rhat'])
dev.off()

summObs <- summary(psObs)$summary
drawsObs <- rstan::extract(psObs)

betaUobs <- summObs[grep('betaU',rownames(summObs)),]
rownames(betaUobs) <- colnames(sdat$X)
betaUobs <- cbind(betaUobs,p=apply(drawsObs$betaU,2,function(x) 2*min(mean(x<0),mean(x>0))))
betaUobs <- betaUobs[!grepl('as.factor(teach)',rownames(betaUobs),fixed=TRUE),]
class(betaUobs) <- 'stanSumm'

betaU <- summ[grep('betaU',rownames(summ)),]
rownames(betaU) <- colnames(sdat$X)
betaU <- cbind(betaU,p=apply(draws$betaU,2,function(x) 2*min(mean(x<0),mean(x>0))))
betaU <- betaU[!grepl('as.factor(teach)',rownames(betaU),fixed=TRUE),]
class(betaU) <- 'stanSumm'

huxreg(betaUobs,betaU)%>%
  as_flextable()%>%
  save_as_docx(path='tables/betaU.docx')

betaY <- summ[grep('betaY',rownames(summ)),]
rownames(betaY) <- colnames(sdat$X)
betaY <- cbind(betaY,p=apply(draws$betaU,2,function(x) 2*min(mean(x<0),mean(x>0))))


print(load('fittedModels/psMod1.RData'))
betaUmle=lme4::fixef(psMod1)

pdf('plots/flpsRasch1/bayesVsMLEbetaU.pdf')
covNamesCompare=intersect(names(betaUmle),rownames(betaU))[-1]
plot(betaUmle[covNamesCompare],
     betaU[covNamesCompare,'mean']/Xsds[covNamesCompare])
abline(0,1)

covNamesCompare=covNamesCompare[!grepl('teach',covNamesCompare)]
plot(betaUmle[covNamesCompare],
     betaU[covNamesCompare,'mean']/Xsds[covNamesCompare],main='no teach FE')
abline(0,1)

dev.off()

round(summ[-grep('Eff|beta',rownames(summ)),c(1:4,8:10)],3)

print(load('fittedModels/raschMod.RData'))

load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

### first try flps
studDat1=studDat%>%
  filter(StuID%in%flpsDat$StuID,!is.na(Scale.Score7))%>%
  arrange(StuID)%>%
  mutate(
    stud=as.numeric(as.factor(StuID))
  )

### flps dat only IDs that are in studDat1 & new stud id
flpsDat1=right_join(flpsDat,select(studDat1,StuID,stud))%>%
    mutate(prob=as.numeric(as.factor(probPart)))

mleEta=ranef(rasch)$StuID[match(as.character(studDat1$StuID),rownames(ranef(rasch)$StuID)),]#[sdat$Z==1],]

pdf('etaCompare.pdf')
plot(mleEta,summ[paste0('studEff[',1:sdat$nstud,']'),1])#[sdat$Z==1])
abline(0,1)

plot(mleEta,summ[paste0('studEff[',1:sdat$nstud,']'),1],xlim=c(-3,3),ylim=c(-3,3))
abline(0,1)
dev.off()

which(summ[paste0('studEff[',1:sdat$nstud,']'),1][sdat$Z==1]< -3) ### only one problem worked

### ctl and trt etas
eta0=summ[paste0('studEff[',which(sdat$Z==0),']'),]
eta1=summ[paste0('studEff[',which(sdat$Z==1),']'),]

eta=as.data.frame(summ[startsWith(rownames(summ),'studEff'),])
eta$Z=sdat$Z

eta%>%ggplot(aes(as.factor(Z),mean))+geom_boxplot()
ggsave('plots/studEffByZ.jpg')

eta%>%ggplot(aes(as.factor(Z),sd))+geom_boxplot()
ggsave('plots/studEffByZsd.jpg')

eta$ftd <- sdat$X%*%betaU[,'mean']

eta[-587,]%>%ggplot(aes(ftd,mean))+geom_point()+geom_smooth(method='lm',se=FALSE)+facet_wrap(~Z,scales='free')
ggsave('plots/etaByXbetaByZ.jpg',width=10,height=5)


#### problem effects?
raschProb=ranef(rasch)$probPart
probCW=distinct(flpsDat1,prob,probPart)%>%arrange(prob)
pdf('plots/probEffsVSmle.pdf')
plot(raschProb[match(probCW$probPart,rownames(raschProb)),],
     summ[paste0('probEff[',probCW$prob,']'),'mean'])
abline(0,1)
dev.off()



###############3
### summaries from main model
#################


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

pdMain <- pdMod(flpsRasch1)
## pdMain <- within(pdMain,
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
print(ggplot(pdMain)+
    geom_abline(aes(intercept=b0,slope=b1,group=id),color='red')+
    coord_cartesian(xlim=c(min(pdMain$xmin),max(pdMain$xmax)),
                    ylim=c(min(pdMain$ymin),max(pdMain$ymax)),expand=FALSE)+
    geom_line(aes(x=x,y=y,group=truthOrAvg,linetype=truthOrAvg,color=truthOrAvg,alpha=truthOrAvg),size=1.5)+
    xlab('$\\eta_T$')+ylab('$\\hat{\\tau}(\\eta_T)$')+
    labs(group=NULL,color=NULL,linetype=NULL)+
    scale_color_manual(values=c('black','red','black'))+scale_linetype_manual(values=c('solid','solid','dotted'))+
    scale_alpha_manual(values=c(1,0,1),guide=FALSE)+theme(legend.position='top')+
    theme(text=element_text(size=15),legend.key.width=unit(.5,'in')))
dev.off()
setwd('figure'); tools::texi2dvi('mainEffects.tex', pdf = T, clean = T); setwd('..')




summGrm <- summary(fitGrm)$summ
parNames <- strsplit(rownames(summGrm),'[',fixed=TRUE)%>%
  map_chr(~.[1])

smallPars <- names(table(parNames)[table(parNames)<49])
Rhats <- summGrm[!parNames%in%smallPars,'Rhat']%>%
  split(factor(parNames[!parNames%in%smallPars]),drop=TRUE)

iwalk(Rhats,~hist(.x,main=.y))

map(Rhats,~head(sort(.,dec=T)))

traceplot(fitGrm,par='betaU')
traceplot(fitGrm,par='betaY')
traceplot(fitGrm,par=names(head(sort(Rhats$lambda,dec=T))))
traceplot(fitGrm,par=names(head(sort(Rhats$tau,dec=T))))
