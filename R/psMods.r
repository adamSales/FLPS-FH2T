library(tidyverse)
library(lme4)


load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

psMod1=glmer(firstTry~as.factor(ProblemSet)+(1|StuID)+(1|ProblemID/probPart)+
               MALE+race+as.factor(teach)+accelerated+
               Scale.Score5+pre.total_math_score+EIP+ESOL+GIFTED+
               IEP+AbsentDays6+pre.avg_time_on_tasks+
               pre_MA_total_score+pre_MSE_total_score+
               pre_PS_tasks_total_score,
            family=binomial,data=flpsDat,
            subset=Z==1)
save(psMod1,file='fittedModels/psMod1.RData')


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
save(psMod1,psMod2,psMod3,psMod4,data='fittedModels/psMods.RData')

################################################################
