data {
  int<lower=1> nstud;                     // number of students
  int<lower=1> nprob;                     // number of questions
  int<lower=1> nprobWorked;                     // number of observations
  int<lower=1> ncov;

  int<lower=1, upper=nstud> studentM[nprobWorked];  // student for observation n
  int<lower=1, upper=nprob> prob[nprobWorked];  // question for observation n
  int<lower=1, upper=3> feedbackOrd[nprobWorked];   // correctness for observation n

  matrix[nstud,ncov] X;
  vector[nstud] Z;
  real Y[nstud];


}
parameters {
  real mu_beta[2];                // mean question difficulty
  vector[nstud] epsilon;             // ability for j - mean
  real beta1[nprob];              // difficulty for k
  real<lower=0> betaDiff[nprob]; 
  vector<lower=0>[nprob] gamma;    // discrimination of k
  real<lower=0> sigma_beta1;    // scale of difficulties
  real<lower=0> sigma_betaDiff; 
  real<lower=0> sigma_gamma;   // scale of log discrimination
  vector[ncov] bu;

  vector[ncov] by;
  real a0;
  real a1;
  real b0;
  real b1;
  real<lower=0> sigma_Y;
  
}
transformed parameters{
  vector[nstud] alpha=X*bu+epsilon;
  ordered[2] beta[nprob];

  for(i in 1:nprob){
    beta[i][1] = beta1[i];
    beta[i][2] = beta1[i]+betaDiff[i];
  }
  
}

model {
  epsilon ~ std_normal();
  beta1 ~ normal(mu_beta[1], sigma_beta1);
  betaDiff ~ normal(mu_beta[2], sigma_betaDiff);
  gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta1 ~ cauchy(0, 5);
    sigma_betaDiff ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  feedbackOrd ~ ordered_logistic(gamma[prob] .* alpha[studentM],beta[prob]);

  Y~normal(a0+X*by+a1*alpha+(Z .*(b0+b1*alpha)),sigma_Y);

}

