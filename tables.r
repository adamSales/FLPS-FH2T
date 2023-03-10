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
print(load('fittedModels/gpcm.RData'))
fitGpcm <- fit
gpcmDraws <- rstan::extract(fitGpcm)

print(load('fittedModels/grm.RData'))
fitGrm <- fit
grmDraws <- rstan::extract(fit)

print(load('fittedModels/classicPS.RData'))
drawsObs <- rstan::extract(psObs)

print(load('fittedModels/flpsRasch1.RData'))
raschDraws <- rstan::extract(flpsRasch1)

print(load('fittedModels/flps2pl.RData'))
tplDraws <- rstan::extract(flps2pl)

load('data/sdatSimp.RData')

coefNames <- c(
  "beta0"="a0",
  "omega"="a1",
  "tau0"="b0",
  "tau1"="b1",
  Male="MALE",
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
  GPCM=do.call("coefSumm",getStuff(gpcmDraws,sdat,"U")) ,statistics=NULL)#,coefs=coefNames)


huxreg(
  Classical=do.call("coefSumm",getStuff(drawsObs,sdatObs,"Y")),
  do.call("coefSumm",getStuff(raschDraws,sdat,"Y")),
  do.call("coefSumm",getStuff(tplDraws,sdat,"Y")),
  #do.call("coefSumm",getStuff(grmDraws,sdat,"Y")),
  do.call("coefSumm",getStuff(gpcmDraws,sdat,"Y")) ,statistics=NULL,coefs=coefNames)

%>%
  quick_docx(file='tables/outcomeReg.docx')
