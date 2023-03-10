data {
  int<lower=1> nstud;                     // number of students
  int<lower=1> nprob;                     // number of questions
  int<lower=1> nprobWorked;                     // number of observations
  int<lower=1, upper=nstud> studentM[nprobWorked];  // student for observation n
  int<lower=1, upper=nprob> prob[nprobWorked];  // question for observation n
  int<lower=0, upper=1> firstTry[nprobWorked];   // correctness for observation n
}
parameters {
  real mu_beta;                // mean question difficulty
  vector[nstud] alpha;             // ability for j - mean
  vector[nprob] beta;              // difficulty for k
  vector<lower=0>[nprob] gamma;    // discrimination of k
  real<lower=0> sigma_beta;    // scale of difficulties
  real<lower=0> sigma_gamma;   // scale of log discrimination
}
model {
  alpha ~ std_normal();
  beta ~ normal(0, sigma_beta);
  gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  firstTry ~ bernoulli_logit(gamma[prob] .* (alpha[studentM] - (beta[prob] + mu_beta)));
}

