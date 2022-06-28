data{
//Sample sizes
 int<lower=1> nprobWorked;
 int<lower=1> ncov;
 int<lower=1> nstud;
 int<lower=1> nprob;

// indices
 int<lower=1,upper=nstud> studentM[nprobWorked];
 int<lower=1,upper=nprob> prob[nprobWorked];

// data data
 int<lower=0,upper=1> firstTry[nprobWorked];
 matrix[nstud,ncov] X;
 int<lower=0,upper=1> Z[nstud];
 real Y[nstud];

}
parameters{

 vector[nstud] studEff;

 vector[ncov] betaU;
 vector[ncov] betaY;

 real a1;
 real b0;
 real b1;

 real probEff[nprob];

 real<lower=0> sigY[2];
 real<lower=0> sigU;
 real<lower=0> sigProb;
}

model{
 real linPred[nprobWorked];
 vector[nstud] muY;
 vector[nstud] muU;
 real useEff[nstud];
 real trtEff[nstud];
 real sigYI[nstud];


// firstTry model
 for(i in 1:nprobWorked)
  linPred[i]= probEff[prob[i]]+studEff[studentM[i]];

 for(i in 1:nstud){
  useEff[i]=a1*studEff[i];
  trtEff[i]=b0+b1*studEff[i];
  muY[i]=useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=sigY[Z[i]+1];
 }

 //priors
 betaY~normal(0,2);
 betaU~normal(0,2);

 a1~normal(0,1);
 b0~normal(0,1);
 b1~normal(0,1);

 probEff~normal(0,sigProb);

 firstTry~bernoulli_logit(linPred);

 studEff~normal(X*betaU,sigU);
 Y~normal(muY+X*betaY,sigYI);
}
