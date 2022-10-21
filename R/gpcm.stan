// from https://mc-stan.org/users/documentation/case-studies/pcm_and_gpcm.html


functions {
  real pcm(int y, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }
  matrix obtain_adjustments(matrix W) {
    real min_w;
    real max_w;
    int minmax_count;
    matrix[2, cols(W)] adj;
    adj[1, 1] = 0;
    adj[2, 1] = 1;
    if(cols(W) > 1) {
      for(k in 2:cols(W)) {                       // remaining columns
        min_w = min(W[1:rows(W), k]);
        max_w = max(W[1:rows(W), k]);
        minmax_count = 0;
        for(j in 1:rows(W))
          minmax_count = minmax_count + W[j,k] == min_w || W[j,k] == max_w;
        if(minmax_count == rows(W)) {       // if column takes only 2 values
          adj[1, k] = mean(W[1:rows(W), k]);
          adj[2, k] = (max_w - min_w);
        } else {                            // if column takes > 2 values
          adj[1, k] = mean(W[1:rows(W), k]);
          adj[2, k] = sd(W[1:rows(W), k]) * 2;
        }
      }
    }
    return adj;
  }
}
data {
  int<lower=1> nprob;                // # items
  int<lower=1> nstud;                // # persons
  int<lower=1> nprobWorked;                // # responses
  int<lower=1,upper=nprob> prob[nprobWorked];    // i for n
  int<lower=1,upper=nstud> studentM[nprobWorked];    // j for n
  int<lower=0> feedbackOrd[nprobWorked];             // response for n; y = 0, 1 ... m_i
  int<lower=1> ncov;                // # person covariates
  matrix[nstud,ncov] X;                 // person covariate matrix
  vector[nstud] Z;
  real Y[nstud];

}
transformed data {
  int m[nprob];                      // # parameters per item
  int pos[nprob];                    // first position in beta vector for item
  matrix[2,ncov] adj;               // values for centering and scaling covariates
  matrix[nstud,ncov] X_adj;             // centered and scaled covariates
  m = rep_array(0, nprob);
  for(n in 1:nprobWorked)
    if(feedbackOrd[n] > m[prob[n]]) m[prob[n]] = feedbackOrd[n];
  pos[1] = 1;
  for(i in 2:(nprob))
    pos[i] = m[i-1] + pos[i-1];
  adj = obtain_adjustments(X);
  for(k in 1:ncov) for(j in 1:nstud)
      X_adj[j,k] = (X[j,k] - adj[1,k]) / adj[2,k];
}
parameters {
  vector<lower=0>[nprob] alpha;
  vector[sum(m)-1] beta_free;
  vector[nstud] theta;
  vector[ncov] lambda_adj;

  real a1;
  real b0;
  real b1;

  real<lower=0> sigY;//[2];

 vector[ncov] betaY;
}
transformed parameters {
  vector[sum(m)] beta;
  beta[1:(sum(m)-1)] = beta_free;
  beta[sum(m)] = -1*sum(beta_free);
}
model {
  alpha ~ lognormal(1, 1);
  target += normal_lpdf(beta | 0, 3);
  theta ~ normal(X_adj*lambda_adj, 1);
  lambda_adj ~ student_t(3, 0, 1);
  for (n in 1:nprobWorked)
    target += pcm(feedbackOrd[n], theta[studentM[n]].*alpha[prob[n]],
                  segment(beta, pos[prob[n]], m[prob[n]]));

 a1~std_normal();
 b0~std_normal();
 b1~std_normal();
 betaY~std_normal();

 Y~normal(X*betaY+a1*theta+(Z .*(b0+b1*theta)),sigY);



}