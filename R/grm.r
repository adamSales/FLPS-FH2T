library(rstan)
options(mc.cores = 8)
rstan_options(auto_write = TRUE)

library(tidyr)#verse)
load('data/sdatSimp.RData')



fit <- stan('R/stanGRM.stan',model_name='grm',data=sdat,chains=8,iter=4000,warmup=3000)#,thin=2)

save(fit,file='fittedModels/grm2.RData')


  
