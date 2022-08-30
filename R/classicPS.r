library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatSimp.RData')

sdatObs <- with(sdat,
                list(
                  ncov=ncov,
                  nstudT=sum(Z),
                  nstudC=nstud-sum(Z),
                  propT=vapply(1:nstud,function(x) mean(firstTry[studentM==x]),1.2)[Z==1],
                  Xt=X[Z==1,],
                  Xc=X[Z==0,],
                  Yt=Y[Z==1],
                  Yc=Y[Z==0]))

psObs <- stan('R/classicPS.stan',data=sdatObs,chains=8)
save(sdatObs,psObs,file='fittedModels/classicPS.RData')
