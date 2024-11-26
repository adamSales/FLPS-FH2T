library(rstan)
library(splines)

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

load('data/sdatSimp.RData')

flpsRasch1=stan('R/rasch1lev.stan',data=sdat,chains=8,iter=5000,warmup=4000,thin=2)
tt=try(save.image(file='fittedModels/flpsRasch1.RData'))
if(inherits(tt,'try-error'))
 save.image(file='flpsRasch1.RData')


### model check
trtNum <- seq(sdat$nstud)[sdat$Z==1]
nt <- sum(sdat$Z==1)
newNum <- seq(nt)
names(newNum) <- trtNum

sdatFake <- within(sdat,{
  studentM <- newNum[as.character(studentM)]
  X <- X[rep(which(Z==1),2),]
  Z <- c(rep(1,nt),rep(0,nt))
  Y <- Y[rep(which(Z==1),2)]
  nstud <- nt*2
})

flpsRaschCheck0 = stan('R/rasch1lev.stan',data=sdatFake,chains=8,iter=5000,warmup=4000,thin=2)
tt=try(save.image(file='fittedModels/flpsRasch1.RData'))
if(inherits(tt,'try-error'))
 save.image(file='flpsRasch1.RData')

