data{
//Sample sizes
 int<lower=1> ncov;
 int<lower=1> nstudT;
 int<lower=1> nstudC;


// data data
 vector<lower=0,upper=1>[nstudT] propT;
 matrix[nstudT,ncov] Xt;
 matrix[nstudC,ncov] Xc;
 real Yt[nstudT];
 real Yc[nstudC];

}
parameters{

 vector[ncov] betaU;
 vector[ncov] betaY;

 real a0;
 real a1;
 real b0;
 real b1;

 vector<lower=0,upper=1>[nstudC] propC;

 real<lower=0> sigY;//[2];
 real<lower=0> sigU;
}

model{

 //priors
 betaY~std_normal();
 betaU~std_normal();

 a0~std_normal();
 a1~std_normal();
 b0~std_normal();
 b1~std_normal();

 propT~normal(Xt*betaU,sigU);
 propC~normal(Xc*betaU,sigU);
 Yt~normal(a0+b0+Xt*betaY+(a1+b1)*propT,sigY);
 Yc~normal(a0+Xc*betaY+a1*propC,sigY);
}
