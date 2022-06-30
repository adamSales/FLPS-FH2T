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
         glmer(firstTry~ns(PartOrder,df)+(ns(PartOrder,df)|ProblemSet)+(1|StuID),family=binomial,data=flpsDat,subset=Z==1)
         )
  save(list=paste0('ordMod',df),file=paste0('fittedModels/ordMod',df,'.RData'))
}
