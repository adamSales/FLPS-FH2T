library(dplyr)
#library(rstan)
library(splines)
library(huxtable)
library(flextable)
library(purrr)
library(table1)
library(tidyr)
library(ggplot2)

source('R/tableFuncs.r')
#options(mc.cores = 8)
#rstan_options(auto_write = TRUE)

select <- dplyr::select

load('data/probPartDat.RData')
load('data/studDat.RData')
load('data/flpsDat.RData')

probPartDat1 <- filter(probPartDat,StuID%in%flpsDat$StuID,condition=='ASSISTments')

probPartDat1%>%summarize(across(c(ProblemSet,ProblemID,probPart),n_distinct))



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

coefNames=c(
  acceleratedTRUE="Accelerated",
  MALE="Male",
  Scale.Score5="5th Grd State Test",
  pre.total_math_score="Pretest",
  GIFTED="Gifted",
  `log(pre.avg_time_on_tasks)`="Log(Pretest Avg. Time on Tasks)",
  `sqrt(AbsentDays6)`="sqrt(6th Grd Days Absent)",
  pre_MA_total_score="Math Anxiety",
  pre_PS_tasks_total_score="Perceptual Sensitivity",
  pre_MSE_total_score="Math Self-Efficacy")

for(i in 1:ncol(sdat$X))
  if(colnames(sdat$X)[i]%in%names(coefNames)){
    colnames(sdat$X)[i] <- 
    colnames(sdatObs$Xt)[i] <- 
    colnames(sdatObs$Xc)[i] <- 
      coefNames[colnames(sdat$X)[i]]
  }

colnames(sdat$X)=gsub("race","",colnames(sdat$X))
colnames(sdatObs$Xt)=gsub("race","",colnames(sdatObs$Xt))
colnames(sdatObs$Xc)=gsub("race","",colnames(sdatObs$Xc))


## fix "lambda_adj" for gpcm model
cent <- colMeans(sdat$X)
scl <- apply(sdat$X,2,function(x) if(length(unique(x))==2) max(x)-min(x) else sd(x)*2)

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

load('data/studDatAnalysisSample.RData')

#### huh there are 2 extra students in "analysis sample"
## better figure that out. In the meantime...
studDat1=studDat1%>%group_by(rdm_condition)%>%slice(-1)%>%ungroup()

studDat=studDat1%>%
  mutate(
    Sex=ifelse(MALE>0,"Male","Female"),
    across(where(~all(.x%in%c(0,1))), ~.x==1),
    Treatment=factor(ifelse(rdm_condition=='ASSISTments','Immediate','Delayed')),
    time=pre.avg_time_on_tasks/60,
    firstTry=ifelse(Treatment=='Immediate',
      map_dbl(which(sdat$Z==1),~mean(sdat$firstTry[sdat$studentM==.])*100),NA),
    feedback=ifelse(Treatment=='Immediate',
      map_dbl(which(sdat$Z==1),~mean(sdat$feedbackOrd[sdat$studentM==.]==2)*100),NA),
    bottom=ifelse(Treatment=='Immediate',
      map_dbl(which(sdat$Z==1),~mean(sdat$feedbackOrd[sdat$studentM==.]==3)*100),NA),
    Posttest=sdat$Y
    )%>%
    rename(
      Accelerated=accelerated,
      `5th Grd State Test`=Scale.Score5,
      Pretest=pre.total_math_score,
      Gifted=GIFTED)


studDat%>%
filter(Treatment=='Immediate')%>%
select(firstTry,feedback,bottom)%>%
pivot_longer(everything(),names_to="feedback",values_to="Percent")%>%
mutate(feedback=factor(ifelse(feedback=='firstTry','No Feedback',ifelse(feedback=='feedback','Partial Feedback','Bottom Out')),levels=c('No Feedback','Partial Feedback','Bottom Out')))%>%
ggplot(aes(x=Percent))+
geom_histogram(aes(y = after_stat(density)),colour = 1, fill = "white",binwidth=5,boundary=0) +
  geom_density()+#scale_x_continuous(expand=c(0,0))+
  facet_wrap(~feedback,scales="free")+xlab("Percent of Problem Parts, Calculated by Student")
ggsave("plots/eedbackHistograms.pdf",height=3,width=6)


label(studDat$race)="Race/Ethnicity"
label(studDat$time)="Avg. Time on Tasks (Pretest)"
label(studDat$AbsentDays6)="Days Absent (6th Grd)"
label(studDat$pre_MA_total_score)="Math Anxiety"
label(studDat$pre_PS_tasks_total_score)="Perceptual Sensitivity"
label(studDat$pre_MSE_total_score)="Math Self-Efficacy"
label(studDat$firstTry)="% Correct w/o Feedback"
label(studDat$feedback)="% Partial Feedback"
label(studDat$bottom)="% Bottom Out"


rndr <- function(x,name, ...) {
    if(is.numeric(x)){
      if(name%in%c('pre.avg_time_on_tasks','AbsentDays6')){
         y <- parse.abbrev.render.code(c("", "Median [IQR]"))(x)
      } else y <- parse.abbrev.render.code(c("", "Mean (SD)"))(x)
    } else y <- render.default(x, ...)
    if (is.logical(x)) y[2] else y
}



t1<-table1(~Pretest+`5th Grd State Test`+Sex+race+Accelerated+EIP+Gifted+IEP+AbsentDays6+time+pre_MA_total_score+pre_MSE_total_score+pre_PS_tasks_total_score+firstTry+feedback+bottom+Posttest|Treatment,data=studDat,render=rndr)


%>%
t1flex("flextable")%>%
save_as_docx(path="tables/table1.docx")






