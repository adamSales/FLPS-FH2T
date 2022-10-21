library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(tidyr)#verse)
load('data/sdatSimp.RData')


## sdat$nfac <- 1
## sdat$min_k <- min(sdat$feedbackOrd)
## sdat$max_k <- max(sdat$feedbackOrd)
## sdat$section <- sdat$prob
## names(sdat) <- gsub('prob','sec',names(sdat))
## sdat$lambda_prior <- sdat$factoridx <- matrix(1,nrow=sdat$nsec,ncol=1)
## sdat$firstitem <- c(1,rep(0,sdat$nsec-1))
## sdat$grad <- sdat$feedbackOrd

sdat$X <- cbind(intercept=1,sdat$X)
sdat$ncov <- sdat$ncov+1

fit <- stan('R/gpcm.stan',model_name='gpcm',data=sdat,chains=8,iter=5000,warmup=4000,thin=2)

save(fit,file='fittedModels/gpcm.RData')


  
