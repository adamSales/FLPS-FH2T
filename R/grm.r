library(rstan)
options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatSimp.RData')

### actually want feedbackOrd opposite
if(!all(sdat$firstTry[sdat$feedbackOrd==3]==1))
    sdat <- within(sdat,feedbackOrd <- max(feedbackOrd)-feedbackOrd+1)

stopifnot(all(sdat$firstTry[sdat$feedbackOrd==3]==1))

fit <- stan('R/stanGRM.stan',model_name='grm',data=sdat,chains=8,iter=4000,warmup=3000)#,thin=2)

save(fit,file='fittedModels/grm2.RData')


  
