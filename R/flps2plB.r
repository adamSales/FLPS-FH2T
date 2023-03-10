library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatScl.RData')

flps2pl=stan('R/twoplML.stan',data=sdat,chains=8,iter=4000,warmup=3000)#,thin=2)

tt=try(save.image(file='fittedModels/flps2plML.RData'))

if(inherits(tt,'try-error'))
 save.image(file='flps2plML.RData')


load('fittedModels/flps2plML.RData')
