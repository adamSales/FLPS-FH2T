library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatSimp.RData')

flpsRasch1=stan('R/rasch1lev.stan',data=sdat,chains=8,iter=5000,warmup=4000,thin=2)
save(flpsRasch1,sdat,Xsds,Ypsd,file='fittedModels/flpsRasch1.RData')


## observed version


sdatObs <- with(sdat,
                list(
                  ncov=ncov,
                  nstudT=sum(Z),
                  nstudC=nstud-sum(Z),
                  propT=map_dbl(1:nstud,~mean(firstTry[studentM==.]))[Z==1],
                  Xt=X[Z==1,],
                  Xc=X[Z==0,],
                  Yt=Y[Z==1],
                  Yc=Y[Z==0]))

psObs <- stan('R/classicPS.stan',data=sdatObs,chains=8)
save(sdatObs,psObs,file='fittedMods/classicPS.RData')