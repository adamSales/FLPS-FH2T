library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatSimp.RData')

flpsRasch1=stan('R/rasch1lev.stan',data=sdat,chains=8,iter=5000,warmup=4000,thin=2)
save(flpsRasch1,sdat,Xsds,Ypsd,file='fittedModels/flpsRasch1.RData')
