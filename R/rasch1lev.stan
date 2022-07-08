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
 //int<lower=0,upper=1> Z[nstud];
 vector[nstud] Z;
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
// real useEff[nstud];
// real trtEff[nstud];
 real sigYI[nstud];


// firstTry model
 for(i in 1:nprobWorked)
  linPred[i]= probEff[prob[i]]+studEff[studentM[i]];

 for(i in 1:nstud){
  //useEff[i]=a1*studEff[i];
  //trtEff[i]=b0+b1*studEff[i];
  //muY[i]=useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=Z[i]>0 ? sigY[2]:sigY[1];//sigY[Z[i]+1];
 }

 //priors
 betaY~std_normal();
 betaU~std_normal();

// a0~std_normal();
 a1~std_normal();
 b0~std_normal();
 b1~std_normal();

 probEff~normal(0,sigProb);

 firstTry~bernoulli_logit(linPred);

 studEff~normal(X*betaU,sigU);
 Y~normal(X*betaY+a1*studEff+(Z .*(b0+b1*studEff)),sigYI);
}
