library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

fake=commandArgs(TRUE)

if(!length(fake))  {
  load('data/sdatSimp.RData')
  datname="sdat"
} else{
    print(fake)
    datname=load(fake)
    print(datname)
}


flps2pl=stan('R/stan2plFLPS.stan',data=get(datname),chains=8,iter=4000,warmup=3000)#,thin=2)

tt=try(save.image(file=paste0('fittedModels/flps2plStan2',datname,'.RData')))
if(inherits(tt,'try-error'))
 save.image(file='flps2plStan.RData')

