stan_logistic_mod_no_lag <-'
data {
int<lower=0> T_train; // length of training time-series
int<lower=0> T_test; // length of testing time-series
int<lower=0> P; // # of predictors
matrix[T_train, P] X_train; // the training predictors
matrix[T_test, P] X_test; // the testing predictors
int<lower=0,upper=1> y_train[T_train];
}
parameters {
real alpha;
vector[P] beta;
}
model {
y_train ~ bernoulli_logit(alpha + X_train * beta);


// priors
alpha ~ normal(0, 25); // priors
beta ~ normal(0, 5); //
}
generated quantities{

// define some new variables
vector[T_train] y_train_rep;
vector[T_train] y_train_sim;
vector[T_test] y_test_sim;

// check the training set fit
for(tt in 1:T_train){
y_train_rep[tt] = bernoulli_logit_rng(alpha + X_train[tt,] * beta);
}

// simulate the training set
for(tt in 1:T_train){
y_train_sim[tt] = bernoulli_logit_rng(alpha + X_train[tt,] * beta);
}

// simulate the testing set
for(tt in 1:T_test){
y_test_sim[tt] = bernoulli_logit_rng(alpha + X_test[tt,] * beta);
}
}'
