library(tidyverse)
library(lubridate)
library(lme4)
library(missForest)
library(rstan)
library(arm)                                        #actDat0=read_csv('data/IES_action_logs_with_hints.csv')
library(splines)

load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

mean(studDat$StuID%in%actDat$StuID)

actDat$condition=studDat$rdm_condition[match(actDat$StuID,studDat$StuID)]

xtabs(~action+condition,actDat)
dim(actDat)

plid=actDat%>%filter(!is.na(StuID))%>%group_by(StuID,ProblemID)%>%summarize(n=n_distinct(problem_log_id))
table(plid$n)



### do students work problems in order w/i problem sets?
for(PS in unique(probPartDat$ProblemSet)){
  ps=probPartDat%>%
    ungroup()%>%
    filter(condition=='ASSISTments', ProblemSet==PS)%>%
    arrange(ProblemOrder)%>%
    select(StuID,ProblemOrder,n)%>%
    pivot_wider(names_from=ProblemOrder,values_from=n,values_fn=sum,values_fill = 0)

  ordVi=numeric(ncol(ps)-2)
  for(i in 3:ncol(ps))
    ordVi[i-2]=if(sum(ps[,i-1]==0)>0) sum(ps[ps[,i-1]==0,i]>0) else 0

  cat(PS,' ',sum(ordVi,na.rm=TRUE),'\n')
}
### YES



probPartDat%>%
  filter(condition=='BAU')%>%
  xtabs(~everCorrect+nerr,data=.)
## anomalies:

inner_join(actDat,filter(probPartDat,condition=='BAU',nerr>1))%>%
  as.data.frame()

inner_join(actDat,filter(probPartDat,condition=='BAU',nerr>0,everCorrect))%>%
  as.data.frame()
#### these are anomalous, but it probably has to do with "resume",
## i.e. student put answer, took break, put another answer.


probPartDat%>%filter(!is.na(condition))%>%
  group_by(StuID,condition)%>%
  summarize(
    across(nerr:firstTry,mean,na.rm=TRUE))%>%
  group_by(condition)%>%
  summarize(
    across(-StuID,mean,na.rm=TRUE))

probPartDat%>%filter(condition=='ASSISTments')%>%
  xtabs(~everCorrect+bottomOut,data=.)

probPartDat%>%filter(condition=='ASSISTments',isTRUE(bottomOut))%>%
  xtabs(~nhint+Hints,data=.)

probPartDat%>%filter(condition=='ASSISTments')%>%
  group_by(ProblemID,Subproblem)%>%
  summarize(nHint=n_distinct(Hints),diff=max(nhint)-Hints[1],.groups = 'drop')%>%
  select(nHint,diff)%>%map(table)

### Hints variable isn't what it's cut out for

probPartDat$probPart=paste0(probPartDat$ProblemID,'-',probPartDat$Subproblem)

probPartDat%>%
  group_by(ProblemID,Subproblem)%>%
  summarize(nstud=n_distinct(StuID),n=n())%>%
  walk(~print(range(.)))%>%
  filter(n>nstud)

probPartDat%>%
  filter(condition=='ASSISTments')%>%
  group_by(ProblemID,Subproblem)%>%
  summarize(nstud=n_distinct(StuID),n=n())%>%
  walk(~print(range(.)))%>%
  filter(n>nstud)


### mean correctness & # problems
studDatT=probPartDat%>%
  filter(condition=='ASSISTments')%>%
  group_by(StuID)%>%
  summarize(
    nprob=n(),
    nprob2=n_distinct(probPart),
    correctness=mean(firstTry,na.rm=TRUE))

all.equal(studDatT$nprob,studDatT$nprob2)

### duplicate entries or...
## aaa=probDat%>%
##   filter(condition=='ASSISTments')%>%
##   group_by(problem_id,StuID)%>%
##   summarize(
##     correct=mean(firstTry,na.rm=TRUE),
##     n=n(),
##     starts=n_distinct(timeStart),
##     cors=n_distinct(correct),
##     errs=n_distinct(nerr),
##     hint=n_distinct(nhint))

## table(aaa$correct)
## table(aaa$starts)
## table(aaa$n)
## table(aaa$errs)
## table(aaa$hint)

### weirdness. for now, just count each problem_id as worked once
### UPDATE: WE FIGURED IT OUT the duplicate problems are of the sort "how do you feel about math" "what computer are u using" etc.
### TASK: Remove non-content problems (mean(correct)<1)


## probDat=probDat%>%
##   group_by(StuID,problem_id,condition)%>%
##   summarize(across(-c(timeStart,problem_log_id),mean,na.rm=TRUE))


hist(studDatT$nprob)
hist(studDatT$correctness)

sum(studDatT$nprob==0)
sum(studDatT$correctness==0)
sum(studDatT$correctness==1)

filter(studDatT,correctness%in%c(0,1))

studDatT%>%
  ggplot(aes(nprob,correctness))+geom_point()+geom_smooth()

hist(qlogis(studDatT$correctness))

#### let's try fitting some models

### rasch model
rasch=glmer(firstTry~(1|StuID)+(1|probPart),
            family=binomial,data=probPartDat,
            subset=condition=='ASSISTments')
save(rasch,file='fittedModels/raschMod.RData')

raschML=glmer(firstTry~as.factor(ProblemSet)+(1|StuID)+(1|ProblemID/probPart),
            family=binomial,data=probPartDat,
            subset=condition=='ASSISTments')
save(raschML,file='fittedModels/raschMLMod.RData')

anova(raschML,rasch)

raschPS=glmer(firstTry~as.factor(ProblemSet)+(1|StuID),
            family=binomial,data=probPartDat,
            subset=condition=='ASSISTments')
save(raschML,file='fittedModels/raschPS.RData')




etaHat=ranef(rasch,condVar=TRUE)
etaHat=etaHat$StuID
names(etaHat)='etaHat'
etaHat$vv=attr(etaHat,'postVar')[1,1,]
etaHat$StuID=as.numeric(rownames(etaHat))

etaHatML=ranef(raschML,condVar=TRUE)
etaHatML=etaHatML$StuID
names(etaHatML)='etaHat'
etaHatML$vv=attr(etaHatML,'postVar')[1,1,]
etaHatML$StuID=as.numeric(rownames(etaHatML))

plot(etaHat$etaHat,etaHatML$etaHat)
plot(etaHat$vv,etaHatML$vv)

studDatT=full_join(studDatT,etaHatML)


ggplot(studDatT,aes(correctness,etaHat))+geom_point()+geom_smooth()
ggplot(studDatT,aes(nprob,etaHat))+geom_point()+geom_smooth()
ggplot(studDatT,aes(nprob,1/vv))+geom_point()+geom_smooth()

studDatT%>%pivot_longer(c(etaHat,correctness),names_to='measure',values_to='value')%>%
  mutate(measure=ifelse(measure=='etaHat','Estimated Ability','% Correct 1st Try'))%>%
  ggplot(aes(nprob,value))+geom_point()+geom_smooth()+facet_wrap(~measure,scales="free_y")+xlab('# of Problems Completed')

ease=ranef(rasch)$probID
names(ease)[1]='ease'
ease$probPart=rownames(ease)

probPartDat$difficulty= -ease$ease[match(probPartDat$probPart,ease$probPart)]

### how does ease vary with order within problem set?
## by problem part
probPartDat%>%
  group_by(ProblemSet,PartOrder)%>%
  summarize(difficulty=mean(difficulty))%>%
  mutate(ps=factor(paste('Prob Set',ProblemSet),levels=paste('Prob Set',1:11)))%>%
  ggplot(aes(PartOrder,difficulty))+geom_point()+geom_smooth()+facet_wrap(~ps)
ggsave('plots/difficultyByOrderWithinPS.jpg')

probPartDat%>%
  group_by(ProblemSet)%>%
  mutate(ProblemOrder=as.numeric(as.factor(ProblemOrder)))%>%
  distinct(ProblemSet,ProblemOrder,difficulty)%>%
  group_by(ProblemSet,ProblemOrder)%>%
  summarize(difficulty=mean(difficulty))%>%
  mutate(ps=factor(paste('Prob Set',ProblemSet),levels=paste('Prob Set',1:11)))%>%
  ggplot(aes(ProblemOrder,difficulty))+geom_point()+geom_smooth()+facet_wrap(~ps)

probPartDat%>%ungroup()%>%
  mutate(ProblemSet=as.numeric(as.factor(ProblemSet)))%>%
  distinct(ProblemSet,probPart,difficulty)%>%
  group_by(ProblemSet)%>%
  summarize(difficulty=mean(difficulty))%>%
  ggplot(aes(ProblemSet,difficulty))+geom_point()+geom_smooth()+ylab("Avg. Difficulty")+
  scale_x_continuous(breaks=1:n_distinct(probPartDat$ProblemSet),minor_breaks=NULL)
ggsave('plots/avgDifficultyByPS.jpg')


### how about ability
probPartDat%>%
  ungroup()%>%
  filter(condition=='ASSISTments')%>%
  inner_join(etaHat)%>%
  group_by(ProblemSet)%>%
  mutate(ProblemOrder=as.numeric(as.factor(ProblemOrder)))%>%
  group_by(ProblemSet,ProblemOrder)%>%
  summarize(ability=mean(etaHat))%>%
  ggplot(aes(ProblemOrder,ability))+geom_point()+geom_smooth()+facet_wrap(~ProblemSet) + scale_x_continuous(breaks=seq(1,26,5),minor_breaks=NULL)+ylab('Average Ability of Students Working Problem')
ggsave('plots/avgAbilityByOrderWithinPS.jpg')


probPartDat%>%
  ungroup()%>%
  mutate(ProblemSet=as.integer(as.factor(ProblemSet)))%>%
  filter(condition=='ASSISTments')%>%
  inner_join(etaHat)%>%
  distinct(ProblemSet,StuID,etaHat)%>%
  group_by(ProblemSet)%>%
  summarize(ability=mean(etaHat))%>%
  ggplot(aes(ProblemSet,ability))+geom_point()+geom_smooth()+
  scale_x_continuous(breaks=1:9,minor_breaks=NULL)+
  ylab('Average Ability of Students Working PS')
ggsave('plots/avgAbilityByPS.jpg')


difficultyStud=probPartDat%>%group_by(StuID,condition)%>%summarize(difficulty=mean(difficulty))
difficultyStud%>%group_by(condition)%>%summarize(difficulty=mean(difficulty))

studDatT$difficulty=difficultyStud$difficulty[match(studDatT$StuID,difficultyStud$StuID)]

studDatT%>%pivot_longer(c(etaHat,correctness),names_to='measure',values_to='value')%>%ggplot(aes(difficulty,value))+geom_point()+geom_smooth()+facet_wrap(~measure,scales="free_y")

reML=ranef(raschML)
easeML=reML[['probPart:ProblemID']]
ids=strsplit(rownames(easeML),'-|:',fixed=FALSE)
easeML$ProblemID=map_chr(ids,~.[1])
easeML$probPart=map_chr(ids,~paste0(.[1],'-',.[2]))
names(easeML)[1]='ease'
easeML$ease=easeML$ease+reML$ProblemID[easeML$ProblemID,1]

probPartDat$easeML=easeML$ease[match(probPartDat$probPart,ease$probPart)]+model.matrix(formula(raschML,fixed=TRUE),data=probPartDat)%*%fixef(raschML)

probPartDat%>%group_by(probPart)%>%summarize(across(starts_with('ease'),mean))%>%ggplot(aes(ease,easeML))+geom_point()+geom_smooth()

easeStud=probPartDat%>%group_by(StuID,condition)%>%summarize(ease=mean(ease),easeML=mean(easeML))
easeStud%>%group_by(condition)%>%summarize(ease=mean(ease),easeML=mean(easeML))

studDatT$easeML=easeStud$easeML[match(studDatT$StuID,easeStud$StuID)]

studDatT%>%pivot_longer(c(etaHat,correctness),names_to='measure',values_to='value')%>%ggplot(aes(easeML,value))+geom_point()+geom_smooth()+facet_wrap(~measure,scales="free_y")



### try flps




## psMod
psMod1=glmer(firstTry~as.factor(ProblemSet)+(1|StuID)+(1|ProblemID/probPart)+
               MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,
            family=binomial,data=flpsDat,
            subset=Z==1)
save(psMod1,file='fittedModels/psMod1.RData')

sss1=sim(psMod1,500)
mm=model.matrix(~MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,data=subset(studDat,StuID%in%rownames(re2$StuID)))[,-1]
rownames(mm)=subset(studDat,StuID%in%rownames(re2$StuID))$StuID
ppp=mm%*%t(sss1@fixef[,colnames(mm)])
studEff1=ranef(sss)$StuID[,,1]+t(ppp[colnames(ranef(sss)$StuID[,,1]),])

ps=predict(psMod1,type='response')
arm::binnedplot(ps,psMod1@frame$firstTry-ps)




psMod2=glmer(firstTry~(1|StuID)+(1|probPart)+
               +ProblemOrder+(ProblemOrder|ProblemSet)+
               MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,
            family=binomial,data=flpsDat,
            subset=Z==1)

psMod3=glmer(firstTry~(1|StuID)+#(1|probPart)+
               +ProblemOrder+(ProblemOrder|ProblemSet)+
               MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,
            family=binomial,data=flpsDat,
            subset=Z==1)


psMod4=glmer(firstTry~(1|StuID)+#(1|probPart)+
               +ProblemOrder+(1|ProblemSet)+
               MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,
            family=binomial,data=flpsDat,
            subset=Z==1)


anova(psMod2,psMod3,psMod4)

save(psMod1,psMod2,psMod3,psMod4,data='fittedModels/psMods.RData')

### first try flps
studDat1=studDat%>%
  filter(StuID%in%flpsDat$StuID,!is.na(Scale.Score7))%>%
  arrange(StuID)%>%
  mutate(
    stud=as.numeric(as.factor(StuID))
  )

### flps dat only IDs that are in studDat1 & new stud id
flpsDat1=right_join(flpsDat,select(studDat1,StuID,stud))

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
sdat$X[,-1]=scale(sdat$X[,-1])
sdat$Z=ifelse(studDat1$rdm_condition=='ASSISTments',1,0)
sdat$Y=studDat1$Scale.Score7

sdat$nprobWorked=nrow(flpsDat1)
sdat$ncov=ncol(sdat$X)
sdat$nstud=nrow(studDat1)
sdat$nprob=max(sdat$prob)

flpsRasch1=stan('R/rasch1lev.stan',data=sdat,chains=1,iter=100)
