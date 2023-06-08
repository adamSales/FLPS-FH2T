
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


