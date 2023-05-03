


pd%>%
filter(par=="\\eta_T")%>%
group_by(model)%>%
mutate(id=1:n())%>%
pivot_wider( id_cols="id",names_from="model",values_from="est")%>%
bind_cols(prop.correct=qlogis(pStud))%>%
select(-id)%>%
filter(is.finite(prop.correct))%>%
ggpairs()
ggsave('plots/studEffs.png')

#ggsave('plots/measurementPars.png', width=6,height=3)


#######################################################
# change parameterization
# for 2pl prob. intercept= -diff*disc

pd2=pd
pd2$est[pd2$par=='diff.'&pd2$model=='2PL']=
  with(rstan::extract(flps2pl),
       colMeans(gamma*(sweep(beta,1,mu_beta,"+"))))

pd2$est[pd2$par=='d_1'&pd2$model=='GRM']=
  with(rstan::extract(fit),
       colMeans(gamma*beta[,,1]))

pd2$est[pd2$par=='d_2'&pd2$model=='GRM']=
  with(rstan::extract(fit),
       colMeans(gamma*beta[,,2]))

pd2$par[pd2$par=='diff.'] = 'd'
pd2$par[pd2$par=='disc.'] = 'a'

ggplot(pd2,aes(par,est))+geom_violin()+geom_jitter(alpha=0.2)+geom_boxplot(width=0.1,outlier.shape=NA)+
    facet_wrap(~model,scales='free_x')+
    scale_x_discrete(labels=c("\\eta_T"=expression(eta),"d_1"=expression(d[1]),"d_2"=expression(d[2])))+
    theme(text = element_text(size = 30))+labs(x=NULL,y="Point Estimates")  
ggsave('plots/measurementParsSlopeInt.pdf')






betaY=
bind_rows(
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(summObs[grep('betaY',rownames(summObs)),]),
  model='obs'
  ),
    tibble(terms=c(colnames(sdat$X)),
  as.data.frame(raschsumm[grep('betaY',rownames(raschsumm)),]),
  model='rasch'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(tplsumm[grep('by',rownames(tplsumm)),]),
  model='2pl'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(grmsumm[grep('by',rownames(grmsumm)),]),
  model='grm'
  )
)%>%mutate(lhs='Y')

betaU=
bind_rows(
  tibble(terms=c('Int',colnames(sdat$X)),
  as.data.frame(summObs[grep('betaU',rownames(summObs)),]),
  model='obs'
  ),
    tibble(terms=c(colnames(sdat$X)),
  as.data.frame(raschsumm[grep('betaU',rownames(raschsumm)),]),
  model='rasch'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(tplsumm[grep('bu',rownames(tplsumm)),]),
  model='2pl'
  ),
  tibble(terms=c(colnames(sdat$X)),
  as.data.frame(grmsumm[grep('bu',rownames(grmsumm)),]),
  model='grm'
  )
)%>%mutate(lhs='U')


bind_rows(betaU,betaY)%>%
filter(!startsWith(terms,'as.factor(teach)'))%>%
ggplot(aes(terms,mean,ymin=mean-2*sd,ymax=mean+2*sd,color=model))+
geom_point(position=position_dodge(width=0.5))+
geom_errorbar(position=position_dodge(width=0.5),width=0)+
geom_hline(yintercept=0)+
facet_wrap(~lhs,scale="free_x")+
coord_flip()+ theme(text = element_text(size = 30))  
ggsave("plots/coef.pdf")





###############3
### summaries from main model
#################

##############################
#### Potential Outcomes Plot
###########################

a0 <- rnorm(length(tplDraws$a1),mean(sdat$Y[sdat$Z==0]),sd(sdat$Y[sdat$Z==0])/sqrt(sum(sdat$Z==0)))
a1 <- tplDraws$a1
b0 <- tplDraws$b0
b1 <- tplDraws$b1

studEff95 <- quantile(Usamp,c(0.025,0.975))
xx <- seq(studEff95[1],studEff95[2],length=100)
Yc <- outer(a1,xx)
Yc <- sweep(Yc,1,a0,'+')

YcUp <- apply(Yc,2,function(x) quantile(x,0.975))
YcDown <- apply(Yc,2,function(x) quantile(x,0.025))


Yt <- outer(a1+b1,xx)
Yt <- sweep(Yt,1,a0+b0,'+')
YtUp <- apply(Yt,2,function(x) quantile(x,0.975))
YtDown <- apply(Yt,2,function(x) quantile(x,0.025))

png('plots/potentialOutcomes.png')
curve(mean(a0)+mean(a1)*x,from=min(xx), to=max(xx),lwd=2,col='red',xlab=expression(eta),ylab=expression(paste('E[',Y[Z],'|',eta,']',sep='')),ylim=range(c(YtDown,YcDown,YtUp,YcUp)),cex.lab=1.25)

curve(mean(a0)+mean(b0)+(mean(b1)+mean(a1))*x,add=TRUE,lwd=2,col='blue')
polygon(c(xx,rev(xx)),c(YcUp,rev(YcDown)),col=adjustcolor('red',0.1))
polygon(c(xx,rev(xx)),c(YtUp,rev(YtDown)),col=adjustcolor('blue',0.1))

legend('topleft',legend=c(expression(Y[C]),expression(Y[T])),col=c('red','blue'),lwd=2)
dev.off()

############################
######## main effect plot
#########################


pdMod <- function(mod,row=1,column=1,func){
    tplDraws <- rstan::extract(mod)
    if('alpha'%in%names(tplDraws)) tplDraws$studEff=tplDraws$alpha
    samp <- seq(1,length(tplDraws$b1),length=1000)
    Usamp <- tplDraws$studEff[samp,]
    iqr <- apply(Usamp,1,IQR)
    studEff95 <- quantile(Usamp,c(0.025,0.975))
    Usamp[Usamp<studEff95[1] | Usamp>studEff95[2]] <- NA
    trtEff <- sweep(sweep(Usamp,1,tplDraws$b1[samp],'*'),1,tplDraws$b0[samp],'+')



    if(missing(func)){
        func <- function(x) mean(tplDraws$b0)+mean(tplDraws$b1)*x
        knownTruth <- FALSE
    } else knownTruth <- TRUE
    truth <- curve(func,from=studEff95[1],to=studEff95[2],n=length(samp)/3)
    avg <- curve(mean(tplDraws$b0)+x*mean(tplDraws$b1),
                 from=studEff95[1],to=studEff95[2],n=length(samp)/3)
    postDraw <- curve(mean(tplDraws$b0)+x*mean(tplDraws$b1),
                      from=studEff95[1],to=studEff95[2],n=length(samp)-length(truth$x)-length(avg$x))
    x <- c(postDraw$x,truth$x,avg$x)
    y <- c(postDraw$y,truth$y,avg$y)
    if(knownTruth) truthOrAvg <- c(rep('Posterior\ntplDraws',length(postDraw$x)),rep('True\nEffect',length(truth$x)),rep('Posterior\nAverage',length(avg$x))) else
     truthOrAvg <- c(rep('Posterior\ntplDraws',length(postDraw$x)),rep('Posterior\nAverage',length(avg$x)+length(truth$x)))

#    if(knownTruth) title <- paste('True Effe

    pd <- data.frame(b0=tplDraws$b0[samp],b1=tplDraws$b1[samp],id=1:length(samp),row=row,column=column,xmin=studEff95[1],xmax=studEff95[2],ymin=min(trtEff,na.rm=T),ymax=max(trtEff,na.rm=T),x=x,y=y,
                     truthOrAvg=truthOrAvg,
                     iqr=iqr)
    pd
}

effectDat=bind_rows(
  #pdMod(psObs)%>%mutate(model='Classic')#,
  pdMod(flpsRasch1)%>%mutate(model='Rasch'),  
  pdMod(flps2pl)%>%mutate(model='2PL'),
  pdMod(fit)%>%mutate(model='GRM')
)%>%mutate(model=factor(model,levels=modelOrd))

pdRasch <- 
## effectDat <- within(effectDat,
## {
##     b0 <- b0/pooledSD
##     b1 <- b1/pooledSD*iqr
##     xmin <- xmin/mean(iqr)
##     xmax <- xmax/mean(iqr)
##     ymin <- ymin/pooledSD
##     ymax <- ymax/pooledSD
## }
## )
tikz('plots/mainEffects.tex', standAlone=T,
     width=6,height=5)
#print(

  ggplot(effectDat)+
    geom_abline(aes(intercept=b0,slope=b1,group=id),color='red')+
    coord_cartesian(xlim=c(min(effectDat$xmin),max(effectDat$xmax)),
                    ylim=c(min(effectDat$ymin),max(effectDat$ymax)),expand=FALSE)+
    geom_line(aes(x=x,y=y,group=truthOrAvg,linetype=truthOrAvg,color=truthOrAvg,alpha=truthOrAvg),size=1.5)+
              geom_hline(yintercept=0)+
    xlab('$\\eta_T$')+ylab('$\\hat{\\tau}(\\eta_T)$')+
    labs(group=NULL,color=NULL,linetype=NULL)+
    scale_color_manual(values=c('black','red','black'))+scale_linetype_manual(values=c('solid','solid','dotted'))+
    scale_alpha_manual(values=c(1,0,1),guide=FALSE)+theme(legend.position='top')+
    theme(text=element_text(size=15),legend.key.width=unit(.5,'in'))+facet_wrap(~model)



dev.off()
setwd('figure'); tools::texi2dvi('mainEffects.tex', pdf = T, clean = T); setwd('..')


