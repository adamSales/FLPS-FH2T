library(dplyr)
#library(rstan)
library(splines)
library(huxtable)
library(flextable)
library(purrr)

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

coefNames <- c(
  "beta0"="a0",
  "omega"="a1",
  "tau0"="b0",
  "tau1"="b1",
  Male="MALE"
  )

hist(table(sdat$studentM))
summary(as.vector(table(sdat$studentM)))
sd(as.vector(table(sdat$studentM)))

summary(probPartDat1$nerr)
summary(probPartDat1$nhint)

mean(probPartDat1$nerr==0)
mean(probPartDat1$firstTry)




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
         y <- parse.abbrev.render.code(c("", "Median [Min, Max]"))(x)
      } else y <- parse.abbrev.render.code(c("", "Mean (SD)"))(x)
    } else y <- render.default(x, ...)
    if (is.logical(x)) y[2] else y
}



table1(~Pretest+`5th Grd State Test`+Sex+race+Accelerated+EIP+Gifted+IEP+AbsentDays6+time+pre_MA_total_score+pre_MSE_total_score+pre_PS_tasks_total_score+firstTry+feedback+bottom+Posttest|Treatment,data=studDat,render=rndr)%>%
t1flex("flextable")%>%
save_as_docx(path="tables/table1.docx")






