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
                  propT=qlogis(vapply(1:nstud,function(i) mean(firstTry[studentM==i]),.3)[Z==1]),
                  Xt=X[Z==1,],
                  Xc=X[Z==0,],
                  Yt=Y[Z==1],
                  Yc=Y[Z==0]))

sdatObs$propT[sdatObs$propT==Inf]=max(sdatObs$propT[is.finite(sdatObs$propT)])

psObs <- stan('R/classicPSlogit.stan',data=sdatObs,chains=8,iter=3000,warmup=1000)
save(sdatObs,psObs,file='fittedModels/classicPSlogit.RData')
