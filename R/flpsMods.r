library(tidyverse)
library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

### first try flps
studDat1=studDat%>%
    filter(StuID%in%flpsDat$StuID,!is.na(Scale.Score7))%>%
    group_by(teach)%>%
    mutate(teachTrt=n_distinct(rdm_condition))%>%
    ungroup()%>%
    filter(teachTrt>1)%>%select(-teachTrt)%>%
  arrange(StuID)%>%
  mutate(
    stud=as.numeric(as.factor(StuID))
  )

### flps dat only IDs that are in studDat1 & new stud id
flpsDat1=flpsDat%>%
    filter(Z==1)%>%
    inner_join(select(studDat1,StuID,stud))

sdat=with(flpsDat1,
          list(
            studentM=stud,
            prob=as.numeric(as.factor(probPart)),
            firstTry=firstTry))
sdat$X=model.matrix(
  ~MALE+race+as.factor(teach)+accelerated+
    Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
    IEP+AbsentDays6+pre.avg_time_on_tasks+
    pre_MA_total_score+pre_MSE_total_score+
    pre_PS_tasks_total_score,
  data=studDat1)

Xsds=apply(sdat$X[,-1],2,sd)

sdat$X[,-1]=scale(sdat$X[,-1])
sdat$Z=ifelse(studDat1$rdm_condition=='ASSISTments',1,0)
sdat$Y=studDat1$Scale.Score7

sdat$nprobWorked=nrow(flpsDat1)
sdat$ncov=ncol(sdat$X)
sdat$nstud=nrow(studDat1)
sdat$nprob=max(sdat$prob)

flpsRasch1=stan('R/rasch1lev.stan',data=sdat,chains=8,iter=5000,warmup=4000,thin=2)
save(flpsRasch1,sdat,Xsds,file='fittedModels/flpsRasch1.RData')
