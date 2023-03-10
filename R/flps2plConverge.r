library(rstan)
library(ltm)

print(load('fittedModels/flps2pl.RData'))

sss <- summary(flps2pl)

summ <- sapply(setNames(flps2pl@model_pars,flps2pl@model_pars),
               function(pp)
                   if(length(flps2pl@par_dims[[pp]])) sss$summary[startsWith(rownames(sss$summary),paste0(pp,'[')),] else sss$summary[pp,],simplify=FALSE)


### fit 2pl w mle

mleDat <- matrix(nrow=sdat$nstud,ncol=sdat$nprob)
for(i in 1:sdat$nprobWorked) mleDat[sdat$studentM[i],sdat$prob[i]] <- sdat$firstTry[i]

mleDat <- mleDat[sdat$Z==1,]

mle <- tpm(data=mleDat,constraint=cbind(seq(1,ncol(mleDat)),1,0))

rhos <- apply(mleDat,2,function(x) cor(x,RM,use='pair'))
rhos[is.na(rhos)] <- 0


mle2 <- tpm(data=mleDat[,-which(rhos<0.2)],constraint=cbind(seq(1,ncol(mleDat)-sum(rhos<0.2)),1,0))


save(mle,mle2,sdat,file='fittedModels/mle2pl.RData')


#### try bayesian 2pl

### take out bad probs
goodProbs <- which(rhos>=0.2)
sdat1 <- within(sdat,{
    studentM <- studentM[prob%in%goodProbs]
    firstTry <- firstTry[prob%in%goodProbs]
    prob <- as.numeric(as.factor(prob[prob%in%goodProbs]))
    nprobWorked <- length(studentM)
    nprob <- max(prob)
})

options(mc.cores = 8)
rstan_options(auto_write = TRUE)

stan2pl <- stan('R/stan2pl2.stan',data=sdat1)

save(stan2pl,file='fittedModels/stan2pl.RData')

sss <- summary(stan2pl)

summ <- sapply(setNames(stan2pl@model_pars,stan2pl@model_pars),
               function(pp)
                   if(length(stan2pl@par_dims[[pp]])) sss$summary[startsWith(rownames(sss$summary),paste0(pp,'[')),] else sss$summary[pp,],simplify=FALSE)


stan2plCovs <- stan('R/stan2plCovs.stan',data=sdat1)
save(stan2plCovs,file='fittedModels/stan2plCovs.RData')

sssCovs <- summary(stan2plCovs)
summ <- sapply(setNames(stan2pl@model_pars,stan2pl@model_pars),
               function(pp)
                   if(length(stan2pl@par_dims[[pp]])) sss$summary[startsWith(rownames(sss$summary),paste0(pp,'[')),] else sss$summary[pp,],simplify=FALSE)



### do estimates agree?
#pdf('plots.pdf')
plot(coef(mle2)[,2],summ$beta[,'mean']+summ$mu_beta['mean'])#,col=ifelse(summ$probEff[,'Rhat']<1.1,'blue',ifelse(summ$probEff[,'Rhat']<2,'orange','red')))
abline(0,1)

plot(coef(mle2)[,3],summ$gamma[,'mean'])#,col=ifelse(summ$probEff[,'Rhat']<1.1,'blue',ifelse(summ$probEff[,'Rhat']<2,'orange','red')))
abline(0,1)

ab=factor.scores(mle,resp.patterns=mleDat,method="EAP")

plot(ab$score.dat$z1,summ$alpha[sdat$Z==1,'mean'])


quantile(coef(mle)[,1],seq(0,.1,.01))
quantile(coef(mle)[,1],seq(0.9,1,.01))

plot(coef(mle)[,1],summ$probEff[,'mean'],col=ifelse(summ$probEff[,'Rhat']<1.1,'blue',ifelse(summ$probEff[,'Rhat']<2,'orange','red')),xlim=c(-5,4),ylim=c(-5,4))
abline(0,1)

plot(coef(mle)[,2],summ$disc[,'mean'],col=ifelse(summ$disc[,'Rhat']<1.1,'blue',ifelse(summ$disc[,'Rhat']<2,'orange','red')))

tail(sort(summ$disc[,'mean']))

tail(sort(coef(mle)[,2]))

plot(coef(mle)[,2],summ$disc[,'mean'],col=ifelse(summ$disc[,'Rhat']<1.1,'blue',ifelse(summ$disc[,'Rhat']<2,'orange','red')),xlim=c(0,5),ylim=c(0,5))
abline(0,1)

dev.off()


print(load('fittedModels/flps2plScl.RData'))

sss <- summary(flps2pl)

summ <- sapply(setNames(flps2pl@model_pars,flps2pl@model_pars),
               function(pp)
                   if(length(flps2pl@par_dims[[pp]])) sss$summary[startsWith(rownames(sss$summary),paste0(pp,'[')),] else sss$summary[pp,],simplify=FALSE)

mleDat <- matrix(nrow=sdat$nstud,ncol=sdat$nprob)
for(i in 1:sdat$nprobWorked) mleDat[sdat$studentM[i],sdat$prob[i]] <- sdat$firstTry[i]

mleDat <- mleDat[sdat$Z==1,]

mle <- ltm(mleDat~z1)


plot(coef(mle)[,1],summ$probEff[,'mean'],col=ifelse(summ$probEff[,'Rhat']<1.1,'blue',ifelse(summ$probEff[,'Rhat']<2,'orange','red')))

quantile(coef(mle)[,1],seq(0,.1,.01))
quantile(coef(mle)[,1],seq(0.9,1,.01))

plot(coef(mle)[,1],summ$probEff[,'mean'],col=ifelse(summ$probEff[,'Rhat']<1.1,'blue',ifelse(summ$probEff[,'Rhat']<2,'orange','red')),xlim=c(-5,4),ylim=c(-5,4),pch=ifelse(summ$probEff[,'Rhat']<1.1,1,16))
abline(0,1)
abline(MASS::rlm(summ$probEff[,'mean']~coef(mle)[,1]))

plot(coef(mle)[,2],summ$disc[,'mean'],col=ifelse(summ$disc[,'Rhat']<1.1,'blue',ifelse(summ$disc[,'Rhat']<2,'orange','red')))
abline(0,1)
abline(MASS::rlm(summ$disc[,'mean']~coef(mle)[,2]))



plot(coef(mle)[,2],summ$disc[,'mean'],col=ifelse(summ$disc[,'Rhat']<1.1,'blue',ifelse(summ$disc[,'Rhat']<2,'orange','red')),xlim=c(0,5),ylim=c(0,5))
abline(0,1)
