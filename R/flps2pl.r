library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatScl.RData')

flps2pl=stan('R/stan2plFLPS.stan',data=sdat,chains=8,iter=4000,warmup=3000)#,thin=2)

tt=try(save.image(file='fittedModels/flps2plStan.RData'))

if(inherits(tt,'try-error'))
 save.image(file='flps2plStan.RData')

