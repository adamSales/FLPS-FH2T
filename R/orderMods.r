library(lme4)
library(splines)

load('data/flpsDat.RData')

flpsDat$pOrd=flpsDat$PartOrder-1

flpsDat<-split(flpsDat,flpsDat$ProblemSet)|>
	lapply(function(x) {x$pOrd=x$pOrd/max(x$pOrd); x})
flpsDat<-do.call("rbind",flpsDat)


for(df in 2:10){
  print(df)
  assign(paste0('ordMod',df),
         glmer(firstTry~ns(pOrd,df)+(ns(pOrd,df)|ProblemSet)+(1|StuID),family=binomial,data=flpsDat,subset=Z==1)
         )
  save(list=paste0('ordMod',df),file=paste0('fittedModels/ordMod',df,'.RData'))
}

pd=flpsDat%>%distinct(ProblemSet,pOrd)%>%mutate(StuID=0)%>%arrange(pOrd)
for(df in 2:10){
    mmm=paste0('ordMod',df)
    if(exists(mmm)){
        mod=get(mmm )
        pd[[paste0('pred',df)]]<-predict(mod,pd,allow.new.levels=TRUE)
    }}

ggplot(pd,aes(pOrd,pred4))+geom_line()+facet_wrap(~ProblemSet)
