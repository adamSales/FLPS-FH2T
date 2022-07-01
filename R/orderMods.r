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


library(tidyverse)
pd=flpsDat%>%distinct(ProblemSet,pOrd)%>%mutate(StuID=0)%>%arrange(pOrd)

pdf('splinePlots.pdf')
for(df in 2:10){
    mmm=paste0('ordMod',df)
    if(!exists(mmm)) print(load(paste0('fittedModels/',mmm,'.RData')))
    mod=get(mmm )
    pd$pred <- pd[[paste0('pred',df)]]<-predict(mod,pd,allow.new.levels=TRUE)
    plt <- ggplot(pd,aes(pOrd,pred))+geom_line()+facet_wrap(~ProblemSet)+ggtitle(paste('df=',df))
    ggsave(paste0('plots/splineMod',df,'df.jpg'),plt)
    print(plt)
}
dev.off()

### anova
eval(parse(text=paste0('anova(',paste0('ordMod',2:10,collapse=','),')')))

genv=environment()
etas=sapply(2:10,function(x) ranef(get(paste0('ordMod',x),envir=genv))$StuID[[1]])
cor(etas)

ordMod0=glmer(firstTry~as.factor(ProblemSet)+(1|StuID),family=binomial,data=flpsDat,subset=Z==1)

cor(etas[,9],ranef(ordMod0)$StuID[[1]])
cor(etas[,1],ranef(ordMod0)$StuID[[1]])

print(load('fittedModels/raschMod.RData'))
print(load('fittedModels/raschMLMod.RData'))

cor(etas[,9],ranef(rasch)$StuID[rownames(ranef(ordMod2)$StuID),1])
cor(etas[,9],ranef(raschML)$StuID[rownames(ranef(ordMod2)$StuID),1])
