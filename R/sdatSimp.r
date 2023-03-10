library(tidyverse)
#library(rstan)
library(splines)

#options(mc.cores = 8)
#rstan_options(auto_write = TRUE)

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

Ypsd=studDat1%>%ungroup()%>%group_by(rdm_condition)%>%
    summarize(v=var(Scale.Score7),n=n())%>%
    ungroup()%>%
    summarize(pVar=sum(v*(n-1))/(sum(n)-2),psd=sqrt(pVar))%>%pull(psd)

### flps dat only IDs that are in studDat1 & new stud id
flpsDat1=flpsDat%>%
    filter(Z==1)%>%
    inner_join(select(studDat1,StuID,stud))

sdat=with(flpsDat1,
          list(
            studentM=stud,
            prob=as.numeric(as.factor(probPart)),
            firstTry=firstTry,
            feedbackOrd=feedbackOrd))
sdat$X=model.matrix(
  ~MALE+race+as.factor(teach)+accelerated+
    Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
    IEP+sqrt(AbsentDays6)+log(pre.avg_time_on_tasks)+
    pre_MA_total_score+pre_MSE_total_score+
    pre_PS_tasks_total_score,
  data=studDat1)

sdat$X <- sdat$X[,-1]

### check histograms
par(mfrow=c(2,4))
HH <- apply(sdat$X[,apply(sdat$X,2,n_distinct)>2],2,hist)
par(mfrow=c(1,1))

Xsds=apply(sdat$X,2,sd)

sdat$X=scale(sdat$X)
sdat$Z=ifelse(studDat1$rdm_condition=='ASSISTments',1,0)
sdat$Y=studDat1$Scale.Score7/Ypsd

sdat$nprobWorked=nrow(flpsDat1)
sdat$ncov=ncol(sdat$X)
sdat$nstud=nrow(studDat1)
sdat$nprob=max(sdat$prob)

save(sdat,file='data/sdatSimp.RData')
