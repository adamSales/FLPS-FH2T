
set.seed(613)
library(bayesplot)

tplDraws=rstan::extract(flps2pl)
newY=with(tplDraws,{
  eff=as.vector(b0)+as.vector(b1)*alpha
  Yhat=as.vector(a0) + 
        by%*%t(sdat$X)+
        as.vector(a1)*alpha+sweep(eff,2,sdat$Z,"*")
  error=t(vapply(tplDraws$sigma_Y,function(s) rnorm(sdat$nstud,0,s),
              numeric(sdat$nstud)))
  Ynew=Yhat+error
  list(eff=eff,Yhat=Yhat,Ynew=Ynew)
})

ate=apply(newY$eff,1,mean)
plot(tplDraws$b0,ate)


ppc_dens_overlay(sdat$Y,newY$Ynew[1:100,] )

ppc_dens_overlay_grouped(sdat$Y,newY$Ynew[1:100,],sdat$Z )


#### fake data


studDatFake=
  bind_rows(
    ## real treatment group data
    studDat1%>%
    filter(rdm_condition=='ASSISTments')%>%
    mutate(stud=seq(n())),
    ## fake control gorup data
    studDat1%>%
    filter(rdm_condition=='ASSISTments')%>%
    mutate(
      rdm_condition="BAU",
      stud=seq(n())+n()
    )    
  )

sdatNoEff <- makeSdat(
  flpsDat1=mutate(flpsDat1,stud=as.numeric(as.factor(stud))),
  studDat1=studDatFake
)

save(sdatNoEff,file='data/sdatNoEff.RData')

load('fittedModels/flps2plStan2sdatNoEff.RData')
noEff=flps2pl
print(noEff,pars=c('a1','b0','b1'),digits=3)

set.seed(613)
sdatConstEff=within(
  sdatNoEff,
  Y[Z==1] <- Y[Z==1]+rnorm(sum(Z==1),0.2,0.1)
)

save(sdatConstEff,file='data/sdatConstEff.RData')

print(load('fittedModels/flps2plStan2sdatConstEff.RData'))
constEff=flps2pl

print(constEff,pars=c('a1','b0','b1'),digits=3)

print(load('fittedModels/flps2plStan2sdat.RData'))
real=flps2pl
print(real,pars=c('a1','b0','b1'))

realSamp=rstan::extract(real,pars=c('a1','b0','b1','alpha'))
effs=with(realSamp, as.vector(b0)+as.vector(b1)*alpha)

### check that alpha is in the right order
library(purrr)
plot(colMeans(realSamp$alpha[,sdat$Z==1]),
    with(sdatConstEff,map_dbl(1:sum(Z), ~mean(firstTry[studentM==.]))))

sdatModelEff=within(sdatNoEff,
  Y[Z==1] <- Y[Z==1]+colMeans(effs[,sdat$Z==1]))

save(sdatModelEff,file='data/sdatModelEff.RData')

bb1=sqrt(var(colMeans(effs[,sdat$Z==1]))/var(colMeans(realSamp$alpha[,sdat$Z==1])^2))

bb0=mean(colMeans(effs[,sdat$Z==1]))-bb1*mean(colMeans(realSamp$alpha[,sdat$Z==1])^2)

sdatQuadEff=within(sdatNoEff,
  Y[Z==1] <- Y[Z==1]+bb0+bb1*colMeans(realSamp$alpha[,sdat$Z==1])^2)

save(sdatQuadEff,file="data/sdatQuadEff.RData")

print(load('fittedModels/flps2plStan2sdatModelEff.RData'))
modEff=flps2pl

print(load('fittedModels/flps2plStan2sdatQuadEff.RData'))
quadEff=flps2pl

print(modEff,pars=c('b0','b1'),digits=3)
print(quadEff,pars=c('b0','b1'),digits=3)



