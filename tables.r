library(tidyverse)
#library(rstan)
library(splines)
library(huxtable)
library(flextable)


source('R/tableFuncs.r')
#options(mc.cores = 8)
#rstan_options(auto_write = TRUE)

load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

probPartDat1 <- filter(probPartDat,StuID%in%flpsDat$StuID,condition=='ASSISTments')

probPartDat1%>%summarize(across(c(ProblemSet,ProblemID,probPart),n_distinct))


hist(table(sdat$studentM))
summary(as.vector(table(sdat$studentM)))
sd(as.vector(table(sdat$studentM)))

summary(probPartDat1$nerr)
summary(probPartDat1$nhint)

mean(probPartDat1$nerr==0)
mean(probPartDat1$firstTry)


### load in fitted models

print(load('fittedModels/grm2.RData'))
fitGrm <- fit
grmDraws <- rstan::extract(fit)

print(load('fittedModels/classicPS.RData'))
drawsObs <- rstan::extract(psObs)

print(load('fittedModels/flpsRasch1.RData'))
raschDraws <- rstan::extract(flpsRasch1)

print(load('fittedModels/flps2plStan2.RData'))
tplDraws <- rstan::extract(flps2pl)

load('data/sdatSimp.RData')

coefNames <- c(
  "beta0"="a0",
  "omega"="a1",
  "tau0"="b0",
  "tau1"="b1",
  Male="MALE"
  )


## fix "lambda_adj" for gpcm model
cent <- colMeans(sdat$X)
scl <- apply(sdat$X,2,function(x) if(length(unique(x))==2) max(x)-min(x) else sd(x)*2)

gpcmDraws$lambda <- sweep(gpcmDraws$lambda_adj[,-1],2,scl,"*")
## check:
all(gpcmDraws$lambda[1,]==unname(gpcmDraws$lambda_adj[1,-1]*scl))

huxreg(
  Classical=do.call("coefSumm",getStuff(drawsObs,sdatObs,"U")),
  Rasch=do.call("coefSumm",getStuff(raschDraws,sdat,"U")),
  `2pl`=do.call("coefSumm",getStuff(tplDraws,sdat,"U")),
  #do.call("coefSumm",getStuff(grmDraws,sdat,"Y")),
  GRM=do.call("coefSumm",getStuff(grmDraws,sdat,"U")) ,statistics=NULL)%>%
  quick_docx(file='tables/usageReg.docx')
  #,coefs=coefNames)


huxreg(
  Classical=do.call("coefSumm",getStuff(drawsObs,sdatObs,"Y")),
  Rasch=do.call("coefSumm",getStuff(raschDraws,sdat,"Y")),
  `2pl`=do.call("coefSumm",getStuff(tplDraws,sdat,"Y")),
  #do.call("coefSumm",getStuff(grmDraws,sdat,"Y")),
  GRM=do.call("coefSumm",getStuff(grmDraws,sdat,"Y")) ,statistics=NULL)%>%
  quick_docx(file='tables/outcomeReg.docx')


### Table 1
vars = as.data.frame(sdat$X)%>%
  dplyr::select(-starts_with("as.factor(teach)"))%>%
  mutate(
    Z=sdat$Z,
    Treatment=factor(ifelse(Z==1,'Immediate','Delayed')),
    `Race/Ethnicity`=factor(
      ifelse(`raceHispanic/Latino`>0,"Hispanic/Latino",
      ifelse(raceAsian>0,"Asian",
      ifelse(raceOther>0,"Other","White"))),
      levels=c("White","Asian","Hispanic/Latino","Other")),
      Accelerated=acceleratedTRUE>0,
      EIP=EIP>0,
      ESOL=ESOL>0,
      Gifted=GIFTED>0,
      IEP=IEP>0,
      `Days Absent (6th Grd)`=`sqrt(AbsentDays6)`^2,
      `Avg. Time on Tasks (Pretest)`=exp(`log(pre.avg_time_on_tasks)`),
      Sex=ifelse(MALE>0,"Male","Female"),
      Posttest=sdat$Y)%>%
    rename(
      `5th Grd State Test`=Scale.Score5,
      Pretest=pre.total_math_score)



