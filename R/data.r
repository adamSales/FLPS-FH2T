library(tidyverse)
library(lubridate)
library(lme4)
library(missForest)


actDat=read_csv('data/logs_for_adam.csv')

studDat=read_csv('data/DATA20220202_3591.csv',na=c('','NA','#NULL!'))%>%
  filter(rdm_condition%in%c('ASSISTments','BAU'))

probMeta=read_csv('data/ASSISTments-Table 1.csv')
probMeta=select(probMeta,!starts_with('...'))
names(probMeta)=gsub(' ','',names(probMeta))
probMeta$Hints[probMeta$Note=='3']=3
actDat$condition=studDat$rdm_condition[match(actDat$StuID,studDat$StuID)]

actDat=
  probMeta%>%
  filter(Hints!='n/a',CorrectAnswer!='n/a')%>%
  mutate(Hints=as.numeric(Hints),
         Subproblem=ifelse(Subproblem%in%c('n/a','N/A'),'A',Subproblem))%>%
  select(ProblemID,ProblemSet,ProblemNumber,ProblemOrder,Hints,Subproblem)%>%
  ## group_by(ProblemID)%>%
  ## summarize(ProblemSet=ProblemSet[1],ProblemOrder=ProblemOrder[1],
  ##           nparts=n(),Hints=sum(Hints))%>%
  inner_join(mutate(actDat,Subproblem=LETTERS[problem_part]),
             by=c("ProblemID"="problem_id","Subproblem"))%>%
  select(-'...1')

plid=actDat%>%filter(!is.na(StuID))%>%group_by(StuID,ProblemID)%>%summarize(n=n_distinct(problem_log_id))
table(plid$n)

probPartDat=
  actDat%>%
  filter(!is.na(StuID),!is.na(condition))%>%
  group_by(StuID,condition,ProblemID,Subproblem,ProblemSet,ProblemOrder,Hints)%>%
  summarize(
    nact=n(),
    drop=all(action%in%c('start','resume')),
    nerr=sum(action=='incorrect response'),
    everCorrect=any(action=='correct response'),
    bottomOut=any(action=='answer_hint'),
    nhint=sum(action%in%c('hint','answer_hint','help_stuck')),
    firstTry=everCorrect&nerr==0&nhint==0,
    n=n())%>%
  filter(!drop)

### an order variable incorporating problem part

probPartDat=probPartDat%>%
  group_by(ProblemSet)%>%
  mutate(ProblemOrder=as.numeric(as.factor(ProblemOrder)),
         probPartNum=match(Subproblem,LETTERS),
         partOrder=ProblemOrder+probPartNum/10,
         PartOrder=as.numeric(as.factor(partOrder))
         )%>%
  ungroup()

probPartDat$probPart=paste0(probPartDat$ProblemID,'-',probPartDat$Subproblem)

probPartDat <- probPartDat%>%
  mutate(feedback=nerr+nhint,feedbackOrd=ifelse(feedback==0,1,ifelse(bottomOut,3,2)))

### what about when students start problems and do nothing else?
probPartDat%>%filter(nact<4)%>%group_by(condition,nact)%>%summarize(mean(everCorrect))

xtabs(~nact+condition,data=filter(probPartDat,nact<4))

probPartDat <- filter(probPartDat,nact>2)

save(probPartDat,file='data/probPartDat.RData')

studDat=
  studDat%>%
  filter(is.finite(pre.total_math_score))%>%
  mutate(
    race=raceEthnicityFed%>%
               factor()%>%
               fct_lump_min(100)%>%
               fct_recode(`Hispanic/Latino`="1",Asian="3",White="6")%>%
      fct_relevel('White'),
    accelerated=startsWith(courseName,'Acc'),
    teach=ifelse(is.na(TeaIDPre),TeaIDEnd,TeaIDPre),
    class=ifelse(is.na(ClaIDPre),ClaIDEnd,ClaIDPre))%>%
  select(
           StuID,rdm_condition,MALE,
           race,teach,class,
           SchIDPre,virtual,
           accelerated,Scale.Score5,Scale.Score7,
           EIP,ESOL,GIFTED,IEP,AbsentDays5,AbsentDays6,
           pre.total_math_score,pre.avg_time_on_tasks,
           pre_MA_total_score,pre_MSE_total_score,
           pre_PS_tasks_total_score,
        mid.total_math_score,post.total_math_score)

covariates=studDat%>%select(-rdm_condition,-Scale.Score7,-mid.total_math_score,-post.total_math_score,-teach,-class,-StuID)%>%mutate(across(where(is.logical),as.factor))%>%as.data.frame()

#imp=missForest(covariates)
#save(imp,file='data/covariatesImp.RData')
load('data/covariatesImp.RData')

for(nn in names(imp$ximp)){
  if(is.factor(imp$ximp[[nn]])&is.logical(studDat[[nn]])) imp$ximp[[nn]]<-imp$ximp[[nn]]=='TRUE'
  studDat[[nn]]=imp$ximp[[nn]]
}

save(studDat,file='data/studDat.RData')

flpsDat=
  inner_join(
    select(probPartDat,
           StuID,firstTry,ProblemID,probPart,ProblemOrder,ProblemSet,PartOrder,feedbackOrd),
    studDat
  )%>%
  mutate(Z=ifelse(rdm_condition=='ASSISTments',1,0),
         firstTry=ifelse(Z==1,firstTry,0),)

save(flpsDat,file='data/flpsDat.RData')
