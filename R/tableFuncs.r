tidy.stanSumm <- function(x,...){
  xx=x[,c('mean','sd','2.5%','97.5%','p')]
  xx=cbind(xx,T=xx[,'mean']/xx[,'sd'])
  xx%>%tibble::as_tibble(rownames="term")%>%
    dplyr::rename(estimate = mean,
                  std.error = sd,
                  statistic = T,
                  p.value = p,
                  `2.5 %`=`2.5%`,
                  `97.5 %`=`97.5%`)
}

glance.stanSumm <- function(x,...)
  rbind(unlist(attr(x,'varComps')))

nobs.stanSumm <- function(x,...){
  out <- cbind(attr(x,'nobs'))
  class(out) <- rep("numeric",nrow(out))
  out
}

nobs.stanSumm <- function(x,...)
  attr(x,'nobs')


coefSumm <- function(coefdraws,sigdraws,X,out,ab){
  colnames(coefdraws) <- colnames(X)
  coefs <- coefdraws[,!grepl('as.factor(teach)',colnames(coefdraws),fixed=TRUE)]
  if(!missing(ab)) coefs <- cbind(ab,coefs)
  mean <- colMeans(coefs)
  sd <- apply(coefs,2,sd)
  p <- apply(coefs,2,function(x) 2*min(mean(x<0),mean(x>0)))
  beta <- cbind(mean,sd,t(apply(coefs,2,quantile,probs=c(0.025,0.975),na.rm=TRUE)),p)
  class(beta) <- 'stanSumm'
  attr(beta,"varComps") <- varComps(coefdraws,sigdraws,X,out)
  attr(beta,"nobs") <- nrow(X)
  beta
}

varComps <- function(coefdraws,sigdraws,X,out){
  varTot <- if(is.matrix(out)) mean(apply(out,1,var)) else var(out)
  residVar <- if(is.null(sigdraws)) 1 else mean(sigdraws^2)
  coefSigV <- mean(apply(coefdraws%*%t(X),1,var))
  teachCols <- grep('as.factor(teach)',colnames(X),fixed=TRUE)
  teachV <- mean(apply(coefdraws[,teachCols]%*%t(X[,teachCols]),1,var))
  R2all <- coefSigV/varTot
  R2teach <- teachV/varTot
  list(sdTot=sqrt(varTot),
       sdResid=sqrt(residVar),
       sdTeach=sqrt(teachV),
       sdCoef=sqrt(coefSigV),
       R2all=R2all,
       R2teach=R2teach)}


abStand <- function(draws,sdat){
  names(draws) <- gsub('studEff|theta','eta',names(draws))
  names(draws) <- gsub('b00','a0',names(draws))
  if(!missing(sdat)&"propT"%in%names(sdat)){
    sig <- sd(sdat$propT)
    mu <- mean(sdat$propT)
  } else{
    sig <- sd(colMeans(draws$eta))
    mu <- mean(draws$eta)
  }
  a0 <- if("a0"%in%names(draws)) draws$a0+draws$a1*mu
  with(draws,
       cbind(a0=a0+a1*mu,a1=a1*sig,b0=b0+b1*mu,b1=b1*sig))
}

getStuff <- function(draws,sdat,YU){
  names(draws) <- gsub('studEff|theta|alpha','eta',names(draws))
  names(draws) <- gsub('lambda|bu','betaU',names(draws))
  names(draws) <- gsub('by','betaY',names(draws))
  names(draws) <- gsub('sigma_Y','sigY',names(draws))



  if(!length(draws$a0)){
    draws$a0 <- draws$betaY[,1]
    draws$betaY <- draws$betaY[,-1]
    #draws$betaU <- draws$betaU[,-1]
  }

  stuff <- list(
    coefdraws=draws[[paste0('beta',YU)]],
    sigdraws=draws[[paste0('sig',YU)]],
    X=with(sdat,
           if(YU=='Y'){
             if('Xt'%in%names(sdat)) rbind(Xt,Xc) else X
           } else if('Xt'%in%names(sdat)) Xt else X[Z==1,]),
    out=if(YU=='Y'){
          if('Yt'%in%names(sdat)) c(sdat$Yt,sdat$Yc) else sdat$Y
        } else if('Yt'%in%names(sdat)) sdat$propT else draws$eta[,sdat$Z==1]
  )
  ## if(length(sdat$nprob)|length(sdat$nsec)){
  ##   stuff$nprob=max(sdat$nprob,sdat$nsec)
  ##   stuff$nstud=sdat$nstud
  ##   stuff$nprobWorked=max(sdat$nprobWorked,sdat$nsecWorked)
  ##}
  if(YU=='Y')
    stuff$ab <- abStand(draws,sdat)
  stuff
}
