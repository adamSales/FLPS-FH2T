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
 int<lower=1,upper=3> feedbackOrd[nprobWorked];
 matrix[nstud,ncov] X;
 //int<lower=0,upper=1> Z[nstud];
 vector[nstud] Z;
 real Y[nstud];

}
parameters{

 vector[nstud] studEff;

 vector[ncov] betaU;
 vector[ncov] betaY;

 real a0;
 real a1;
 real b0;
 real b1;

 real<lower=0> disc[nprob];
 ordered[2] probEff[nprob];

 real<lower=0> sigY;//[2];
 real<lower=0> sigU;
 //real<lower=0> sigProb;
}

model{
 real linPred[nprobWorked];
 vector[nstud] muY;


 

 //priors
 betaY~std_normal();
 betaU~std_normal();

 a0~std_normal();
 a1~std_normal();
 b0~std_normal();
 b1~std_normal();

// probEff~normal(0,1);//sigProb);
 disc~lognormal(0,2);

// feedbackOrd model
 for(i in 1:nprobWorked){
   feedbackOrd[i]~ordered_logistic(disc[prob[i]]*studEff[studentM[i]],probEff[prob[i]]);
 }

 studEff~normal(X*betaU,sigU);
 Y~normal(a0+X*betaY+a1*studEff+(Z .*(b0+b1*studEff)),sigY);
}
