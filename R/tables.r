library(dplyr)

library(splines)
library(huxtable)
library(flextable)
library(purrr)
#library(table1)
library(tidyr)
library(ggplot2)
library(xtable)
library(table1)
library(rstan)

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

#print(load('fittedModels/classicPSlogit.RData'))
#drawsObs <- rstan::extract(psObs)

print(load('fittedModels/flpsRasch1.RData'))
raschDraws <- rstan::extract(flpsRasch1)

print(load('fittedModels/flps2plStan2.RData'))
tplDraws <- rstan::extract(flps2pl)

load('data/sdatSimp.RData')

modelOrd=c('Classic','Rasch','2PL','GRM')


coefNames=c(
  acceleratedTRUE="Accelerated",
  MALE="Male",
  Scale.Score5="5th Grd State Test",
  pre.total_math_score="Pretest",
  GIFTED="Gifted",
  `log(pre.avg_time_on_tasks)`="Log(Pretest Avg. Time)",
  `sqrt(AbsentDays6)`="$\\sqrt{6^{th}\\mbox{ Grd Days Absent}}$",
  pre_MA_total_score="Math Anxiety",
  pre_PS_tasks_total_score="Perceptual Sensitivity",
  pre_MSE_total_score="Math Self-Efficacy")

for(i in 1:ncol(sdat$X))
  if(colnames(sdat$X)[i]%in%names(coefNames)){
    colnames(sdat$X)[i] <- 
#    colnames(sdatObs$Xt)[i] <- 
#    colnames(sdatObs$Xc)[i] <- 
      coefNames[colnames(sdat$X)[i]]
  }

colnames(sdat$X)=gsub("race","",colnames(sdat$X))
#colnames(sdatObs$Xt)=gsub("race","",colnames(sdatObs$Xt))
#colnames(sdatObs$Xc)=gsub("race","",colnames(sdatObs$Xc))


## fix "lambda_adj" for gpcm model
cent <- colMeans(sdat$X)
scl <- apply(sdat$X,2,function(x) if(length(unique(x))==2) max(x)-min(x) else sd(x)*2)

ur=huxreg(
  #Classical=do.call("coefSumm",getStuff(drawsObs,sdatObs,"U")),
  Rasch=do.call("coefSumm",getStuff(raschDraws,sdat,"U")),
  `2pl`=do.call("coefSumm",getStuff(tplDraws,sdat,"U")),
  #do.call("coefSumm",getStuff(grmDraws,sdat,"Y")),
  GRM=do.call("coefSumm",getStuff(grmDraws,sdat,"U")) ,
  error_pos = "right",
  statistics=NULL)#%>%
#  quick_latex(file='tables/usageReg.tex')
  #quick_docx(file='tables/usageReg.docx')
  #,coefs=coefNames)

rows=c(1:(which(ur$names=='nobs')-1),which(ur$names%in%c('sdResid','R2all')),nrow(ur))
urd=as.data.frame(ur[rows,])
for(i in 2:(nrow(urd)-1)) for(j in 2:ncol(urd)){
  num=round(as.numeric(gsub("[\\*\\)\\(]","",urd[i,j])),2)
  urd[i,j]=gsub("(\\(?)([0-9\\.]+)([\\) \\*]*)",paste0("\\1",num,"\\3"),urd[i,j])
}
urd[urd[,1]%in%c('sdResid','R2all'),1]=c("$\\sigma_U$","$R^2$")
#urd[nrow(urd),]=c(paste("\\multicolumn{5}{l}{",urd[nrow(urd),1],"}"),
urd[nrow(urd),]=c(paste0("\\multicolumn{",ncol(urd),"}{l}{* Central 95\\% credible interval excludes 0.}"),#urd[nrow(otr),1],"}"),
  rep("",ncol(urd)-1))  
urd[1,c(3,5,7)]=""
urd[1,urd[1,]=='2pl'] <- "2PL"

print(xtable(urd),floating=F,hline.after=c(-1,1,nrow(urd)-3,nrow(urd)-1),include.rownames=F,include.colnames=F,sanitize.text.function=function(x) x,
file='tables/usageReg.tex')



ot=huxreg(
  #Classical=do.call("coefSumm",getStuff(drawsObs,sdatObs,"Y")),
  Rasch=do.call("coefSumm",getStuff(raschDraws,sdat,"Y")),
  `2pl`=do.call("coefSumm",getStuff(tplDraws,sdat,"Y")),
  #do.call("coefSumm",getStuff(grmDraws,sdat,"Y")),
  GRM=do.call("coefSumm",getStuff(grmDraws,sdat,"Y")) ,
  error_pos = "right",
  statistics=NULL)


rows=c(1:(which(ot$names=='nobs')-1),which(ot$names%in%c('sdResid','R2all')),nrow(ot))

otr=as.data.frame(ot[rows,])
for(i in 2:(nrow(otr)-1)) for(j in 2:ncol(otr)){
  num=round(as.numeric(gsub("[\\*\\)\\(]","",otr[i,j])),2)
  otr[i,j]=gsub("(\\(?)([0-9\\.]+)([\\) \\*]*)",paste0("\\1",num,"\\3"),otr[i,j])
}


#otr=as.data.frame(ot[c(1,4:9,47,nrow(ot)),])
#for(i in 2:(nrow(otr)-1)) for(j in 2:ncol(otr)){
#  num=round(as.numeric(gsub("[\\*\\)\\(]","",otr[i,j])),3)
#  otr[i,j]=gsub("(\\(?)([0-9\\.]+)([\\) \\*]*)",paste0("\\1",num,"\\3"),otr[i,j])
#}

otr[,1]=gsub('a0','(Intercept)',otr[,1])
otr[,1]=gsub('a1','$\\\\omega$',otr[,1])
otr[,1]=gsub('b0','$\\\\tau_0$',otr[,1])
otr[,1]=gsub('b1','$\\\\tau_1$',otr[,1])
otr[,1]=gsub('R2all','$R^2$',otr[,1])
otr[,1]=gsub('sdResid','$\\\\sigma_Y$',otr[,1])
otr[1,4]="2PL"
otr[1,c(3,5,7)]=""
otr[nrow(otr),]=c(paste0("\\multicolumn{",ncol(otr),"}{l}{* Central 95\\% credible interval excludes 0.}"),#otr[nrow(otr),1],"}"),
rep("",ncol(otr)-1))


print(
  xtable(otr),floating=F,hline.after=c(-1,1,nrow(otr)-3,nrow(otr)-1),include.rownames=F,include.colnames=F,sanitize.text.function=function(x) x,file='tables/outcomeReg.tex')

otrSmall= otr[
  c(1,which(otr[,1]=='$\\omega$'):(which(otr[,1]=="Male")-1)),]#,
#  nrow(otr)-(1:2)),]
print(
  xtable(otrSmall),floating=F,hline.after=c(-1,1,nrow(otrSmall)),#,nrow(otrSmall)-1),
  include.rownames=F,include.colnames=F,sanitize.text.function=function(x) x,
file='tables/outcomeRegSmall.tex')



CIs=map(list(#Classical=drawsObs,
  Rasch=raschDraws,`2pl`=tplDraws,GRM=grmDraws),
  function(x)  map_dfr(c('b0','b1'),function(y) data.frame(
    parm=y,
    normL=mean(x[[y]])-2*sd(x[[y]]),
    normH=mean(x[[y]])+2*sd(x[[y]]),
    qL=quantile(x[[y]],0.025),
    qH=quantile(x[[y]],0.975))))
    

### Table 1

print(load('data/studDatAnalysisSample.RData'))

#### huh there are 2 extra students in "analysis sample"
## better figote that out. In the meantime...
#studDat1=studDat1%>%group_by(rdm_condition)%>%slice(-1)%>%ungroup()

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
ggsave("plots/feedbackHistograms.pdf",height=3,width=6)


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
      if(name%in%c('time','AbsentDays6')){
         y <- parse.abbrev.render.code(c("", "Median [IQR]"))(x)
      } else y <- parse.abbrev.render.code(c("", "Mean (SD)"))(x)
    } else y <- render.default(x, ...)
    if (is.logical(x)) y[2] else y
}



t1<-table1(~Pretest+`5th Grd State Test`+Sex+race+Accelerated+EIP+Gifted+IEP+ESOL+AbsentDays6+time+pre_MA_total_score+pre_MSE_total_score+pre_PS_tasks_total_score+firstTry+feedback+bottom+Posttest|Treatment,data=studDat,render=rndr)

sink('tables/table1.tex')
t1kable(t1,format='latex')
sink()

t1flex(t1,"flextable")%>%
save_as_docx(path="tables/table1.docx")






### r^2 from measurement model

r2=function(draws,Xmat){
  
  if("betaU"%in%names(draws)){
    betaU=draws$betaU
    sigU=draws$sigU
  } else{
    betaU=draws$bu
    sigU=1
  }

  varXBeta=apply(betaU%*%t(Xmat),1,var)
  R2=varXBeta/(varXBeta+sigU^2)
  c(mean(R2),median(R2),sd(R2))
}

