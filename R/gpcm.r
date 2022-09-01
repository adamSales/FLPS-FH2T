library(tidyverse)
load('data/sdatSimp.RData')

setwd('../FLPS')
devtools::load_all()
setwd('../FLPS-FH2T')

inpDat <- with(sdat,data.frame(Y,Z,X))
ddd <- with(sdat,data.frame(feedbackOrd,studentM,prob))
usage <- ddd%>%pivot_wider(id_cols=studentM,names_from="prob",values_from="feedbackOrd",names_prefix='i')
usage <- as.matrix(as.data.frame(usage))
usage2 <- matrix(nrow=sdat$nstud,ncol=sdat$nprob)
for(i in 1:nrow(usage)) usage2[usage[i,1],] <- usage[i,-1]

colnames(usage2) <- colnames(usage)[-1]

inpDat <- cbind(inpDat,usage2)

sss <- makeFLPSdata(inpDat,'Y','Z',names(data.frame(sdat$X)),lv_model=paste0("F=~",paste0("i",1:sdat$nprob,collapse='+')),lv_type='GPCM',stan_options=list(iter=10,chains=1))

fit <- runFLPS(inpDat,outcome='Y',group='Z',covariate=names(data.frame(sdat$X)),lv_model=paste0("F=~",paste0("i",1:sdat$nprob,collapse='+')),lv_type='GPCM',stan_options=list(iter=5000,chain=8,warmup=4000,thin=2))

save(fit,file='fittedModels/gpcm.RData')
