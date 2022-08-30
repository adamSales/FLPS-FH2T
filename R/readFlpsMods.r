library(rstan)
library(tidyverse)
library(lme4)

load('fittedModels/flpsRasch1.RData')
summ=summary(flpsRasch1)
summ=summ$summary

pdf('plots/flpsRasch1/rhats.pdf')
hist(summ[,'Rhat'])
dev.off()

betaU <- summ[grep('betaU',rownames(summ)),]
rownames(betaU) <- colnames(sdat$X)

betaY <- summ[grep('betaY',rownames(summ)),]
rownames(betaY) <- colnames(sdat$X)

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
