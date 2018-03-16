source("R/GetSeasonDate.R")

load("Processed_Data/Shifted_reanalysis/REP_EWD_M_O_all.RData")

# try the stan model
source("R/stan_logistic_mod_no_lag.R")
dat_include = REP_EWD_M_O_all %>% dplyr::filter(model == "obs") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(west_index_anom = west_index_anom,
                east_index_anom = east_index_anom,
                M_index_anom = M_index_anom,
                REP_lag1 = REP_lag1,
                NPH_index_anom = NPH_index_anom) %>%
  dplyr::select(date, REP, M_index_anom, O_index_anom, west_index_anom, east_index_anom, NPH_index_anom, REP_lag1) %>%
  dplyr::filter(!is.na(west_index_anom) &
                !is.na(east_index_anom) &
                !is.na(M_index_anom) &
                !is.na(NPH_index_anom) &
                !is.na(REP_lag1))


train_test_ratio = 3/4

training_data = dat_include %>% dplyr::filter(lubridate::year(date) < (1950+ceiling(56*train_test_ratio)))
testing_data = dat_include %>% dplyr::filter(lubridate::year(date) > (1950+ceiling(56*train_test_ratio) - 1))

dat_list = list(T_train = nrow(training_data),
                T_test = nrow(testing_data),
                P = 5,
                X_train = as.matrix(training_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                X_test = as.matrix(testing_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                y_train = training_data$REP)

stan_fit = 
  stan(model_code = stan_logistic_mod_no_lag,
       data = dat_list,
       verbose = FALSE,
       seed = 22,
       chains = 4,
       iter = 1000,
       thin = 2)

print(stan_fit, pars = c("alpha","beta"))
plot(stan_fit, pars = c("alpha","beta"))
traceplot(stan_fit, pars = c("alpha","beta"))


# look at probability of simulating a extreme event given that the observed value was an extreme precipitation event
# first the calibration period
REP_sim = data.frame(date = training_data$date,
                     REP = training_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable) %>%
                 dplyr::mutate(outcome = ifelse(REP == 1 & value == 1, "true_positive", 
                                                ifelse(REP == 1 & value == 0, "false_negative",
                                                       ifelse(REP == 0 & value == 0, "true_negative","false_positive"))))

true_false_pos_neg_table = table(REP_sim_daily$outcome)

hit_rate = true_false_pos_neg_table[4] / (true_false_pos_neg_table[4] + true_false_pos_neg_table[1])
false_alarm_rate = true_false_pos_neg_table[2] / (true_false_pos_neg_table[2] + true_false_pos_neg_table[4])

hit_rate
# 0.1224286 

false_alarm_rate
# 0.877312 



# now apply the +/- window of one day -- so if:
# REP == 1 & any of REP_sim_t-1, REP_sim_t, REP_sim_t+1 is = 1, then true +
# REP == 0 & any of REP_sim_t-1, REP_sim_t, REP_sim_t+1 is = 0, then true -

# testing
REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable) %>%
  dplyr::mutate(outcome = ifelse(REP == 1 & (lag(value,1) == 1 | value == 1 | lead(value,1) == 1), "true_positive", 
                                 ifelse(REP == 1 & (lag(value,1) == 0 & value == 0 & lead(value,1) == 0), "false_negative",
                                               ifelse(REP == 0 & (lag(value,1) == 0 | value == 0 | lead(value,1) == 0),"true_negative","false_positive"))))



true_false_pos_neg_table = table(REP_sim_daily$outcome)

hit_rate = true_false_pos_neg_table[4] / (true_false_pos_neg_table[4] + true_false_pos_neg_table[1])
false_alarm_rate = true_false_pos_neg_table[2] / (true_false_pos_neg_table[2] + true_false_pos_neg_table[4])

hit_rate
# 0.2227714 

false_alarm_rate
# 0.001920123 

REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable, REP) %>%
  dplyr::summarise(num_1 = sum(value),
                   num_0 = sum(abs(value - 1))) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(global_prob_1 = sum(num_1)/sum(num_1 + num_0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prob_1_0 = ifelse(REP == 0, num_0/(num_1 + num_0)/(1-global_prob_1), num_1/(num_1 + num_0)/(global_prob_1)))
                   
daily_check_cal = 
  ggplot(REP_sim_daily) +
  geom_boxplot(aes(factor(REP), prob_1_0)) +
  theme_bw() +
  scale_y_log10(limits = c(0.5, 200)) +
  labs(title = "Calibration period",
       y = expression(paste('P(REP'[sim],' = REP'[obs], ')/P(REP'[obs],' = x)')),
       x = "x") 


# now the testing period
REP_sim = data.frame(date = testing_data$date,
                     REP = testing_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))


REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable, REP) %>%
  dplyr::mutate(outcome = ifelse(REP == 1 & value == 1, "true_positive", 
                                 ifelse(REP == 1 & value == 0, "false_negative",
                                        ifelse(REP == 0 & value == 0, "true_negative","false_positive"))))

true_false_pos_neg_table = table(REP_sim_daily$outcome)

hit_rate = true_false_pos_neg_table[4] / (true_false_pos_neg_table[4] + true_false_pos_neg_table[1])
false_alarm_rate = true_false_pos_neg_table[2] / (true_false_pos_neg_table[2] + true_false_pos_neg_table[4])

hit_rate
# 0.1124545  

false_alarm_rate
# 0.8818303 


# testing
REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable) %>%
  dplyr::mutate(outcome = ifelse(REP == 1 & (lag(value,1) == 1 | value == 1 | lead(value,1) == 1), "true_positive", 
                                 ifelse(REP == 1 & (lag(value,1) == 0 & value == 0 & lead(value,1) == 0), "false_negative",
                                        #ifelse(REP == 0 & value == 1,"false_positive",
                                        ifelse(REP == 0 & (lag(value,1) == 0 | value == 0 | lead(value,1) == 0),"true_negative","false_positive"))))



true_false_pos_neg_table = table(REP_sim_daily$outcome)

hit_rate = true_false_pos_neg_table[4] / (true_false_pos_neg_table[4] + true_false_pos_neg_table[1])
false_alarm_rate = true_false_pos_neg_table[2] / (true_false_pos_neg_table[2] + true_false_pos_neg_table[4])

hit_rate
# 0.1383636 

false_alarm_rate
# 0.002621232 


#### MAKE A 4 PANEL FIGURE FOR MODEL VERIFICATION ####
# now compare the decadal trends in the raw data and the model simulations
# start with the calibration set
REP_sim = data.frame(date = training_data$date,
                     REP = training_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

REP_sim_yearly_train = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(REPs = sum(value),
                   REP = sum(REP)) %>%
  dplyr::group_by(REP) %>%
  dplyr::mutate(REPs_mean = mean(REPs)) %>%
  dplyr::mutate(sample = "Calibration")

# now the testing set
REP_sim = data.frame(date = testing_data$date,
                     REP = testing_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

REP_sim_yearly_test = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(REPs = sum(value),
                   REP = sum(REP)) %>%
  dplyr::group_by(REP) %>%
  dplyr::mutate(REPs_mean = mean(REPs)) %>%
  dplyr::mutate(sample = "Testing")


REP_sim_yearly_both = rbind(REP_sim_yearly_train, REP_sim_yearly_test)

# now make them into proportions
REP_sim_yearly_both = REP_sim_yearly_both %>% dplyr::group_by(sample, REP) %>%
                                              dplyr::mutate(REPs_by_REP = length(REPs))

REP_sim_yearly_both_binned = REP_sim_yearly_both %>% dplyr::group_by(sample, REP, REPs) %>%
                                                     dplyr::summarise(REPs_prop = length(REPs)/mean(REPs_by_REP))

yearly_train_test = 
  ggplot(REP_sim_yearly_both_binned, aes(REP, factor(REPs))) +
  geom_tile(aes(fill = REPs_prop*100)) +
  scale_fill_gradient(name = "%", low = "white", high = "black") +
  geom_abline(aes(slope = 1, intercept = 1, col = sample), linetype = "dashed") +
  theme_bw() +
  labs(x = "# REPs observed in year",
       y = "# REPs predicted in year") +
  
  facet_wrap(~sample,
             scales = "free_x") +
  scale_color_manual(values = c("blue", "red"),
                     guide = FALSE) +
  theme(legend.position = "bottom")
  


# now check the cdf
REP_sim_yearly_both = REP_sim_yearly_both %>% ungroup() %>%
                                                      dplyr::mutate(REPs_jitter = REPs + rnorm(nrow(REP_sim_yearly_both),0,0.001)) %>%
                                                      dplyr::group_by(year) %>%
                                                      dplyr::mutate(sim_cdf_jitter = ecdf(REPs_jitter)(REP),
                                                                    sim_cdf = ecdf(REPs)(REP),
                                                                    sim_cdf_less_than = ecdf(REPs)(REP-0.1))

REP_sim_yearly_both_unique = unique(REP_sim_yearly_both[,c("year", "sample", "sim_cdf_jitter", "sim_cdf", "sim_cdf_less_than")])

REP_sim_yearly_both_unique_long = melt(REP_sim_yearly_both_unique, id.vars = c("year", "sample")) %>% dplyr::filter(variable == "sim_cdf_jitter")

cdf_year_plot = 
  ggplot() +
  geom_line(data = REP_sim_yearly_both_unique_long, aes(x = year, y = value, col = sample)) +
  geom_point(data = REP_sim_yearly_both_unique_long, aes(x = year, y = value, col = sample)) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed") + 
  geom_hline(yintercept = c(0, 1)) + 
  labs(y = expression(paste(P,"(X*"[forecast] <= x[observation],")"))) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c("blue", "red"),
                     guide = FALSE)

cdf_year_plot


# Manu's idea on prob
REP_sim = data.frame(date = training_data$date,
                     REP = training_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

REP_sim_yearly_training = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(REPs = sum(value),
                   REP = sum(REP))

REP_sim_probs = REP_sim_yearly_training %>% dplyr::select(year, variable, REPs, REP) %>%
                                            dplyr::group_by(year, REPs) %>%
                                            dplyr::summarise(REP = mean(REP),
                                                             REP_sim_prob = length(variable)/1000) %>%
                                            dplyr::mutate(sample = "Calibration")

REP_obs_probs = REP_sim_yearly_training %>% dplyr::select(year, variable, REPs, REP) %>%
  dplyr::group_by(REP) %>%
  dplyr::summarise(REP_obs_prob = length(variable)/nrow(REP_sim_yearly_training))

REP_obs_sim_probs_training = merge(REP_sim_probs, REP_obs_probs, by = "REP") %>%
                    dplyr::filter(REP == REPs) %>%
                    dplyr::mutate(prob_ratio = REP_sim_prob/REP_obs_prob) %>%
                    ungroup() %>%
                    dplyr::mutate(prob_ratio_mean = median(prob_ratio))


REP_sim = data.frame(date = testing_data$date,
                     REP = testing_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

REP_sim_yearly_testing = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(REPs = sum(value),
                   REP = sum(REP))

REP_sim_probs = REP_sim_yearly_testing %>% dplyr::select(year, variable, REPs, REP) %>%
  dplyr::group_by(year, REPs) %>%
  dplyr::summarise(REP = mean(REP),
                   REP_sim_prob = length(variable)/1000) %>%
  dplyr::mutate(prob_ratio_mean = median(REP_sim_prob)) %>%
  dplyr::mutate(sample = "Testing")

REP_obs_probs = REP_sim_yearly_testing %>% dplyr::select(year, variable, REPs, REP) %>%
  dplyr::group_by(REP) %>%
  dplyr::summarise(REP_obs_prob = length(variable)/nrow(REP_sim_yearly_testing))

REP_obs_sim_probs_testing = merge(REP_sim_probs, REP_obs_probs, by = "REP") %>%
  dplyr::filter(REP == REPs) %>%
  dplyr::mutate(prob_ratio = REP_sim_prob/REP_obs_prob) %>%
  ungroup() %>%
  dplyr::mutate(prob_ratio_mean = median(prob_ratio))

REP_obs_sim_probs = rbind(REP_obs_sim_probs_training,REP_obs_sim_probs_testing)


ratio_year_plot = 
  ggplot(REP_obs_sim_probs) +
  geom_line(aes(x = year, y = prob_ratio, col = sample)) +
  geom_line(aes(x = year, y = prob_ratio_mean, col = sample), linetype = "dashed", size = 1.2) +
  geom_point(aes(x = year, y = prob_ratio, col = sample)) +
  geom_hline(yintercept = c(1), linetype = "dashed") + 
  theme_bw() +
  labs(y = expression(paste(P[forecast],"(k = ", k,"*",")"/P[marginal],"(k = ", k,"*",")"))) +
  scale_y_continuous(limits = c(0,2.5)) +
  scale_color_manual(values = c("blue", "red"),
                     guide = FALSE)



REP_sim_yearly_both = REP_sim_yearly_both %>% dplyr::group_by(year) %>%
  dplyr::mutate(REPs_mean = mean(REPs),
                REPs_median = median(REPs),
                REPs_25 = quantile(REPs, probs = 0.25),
                REPs_75 = quantile(REPs, probs = 0.75),
                REPs_05 = quantile(REPs, probs = 0.55),
                REPs_95 = quantile(REPs, probs = 0.95))
REP_sim_yearly_both_unique = unique(REP_sim_yearly_both[,c("year", "sample", "REP", "REPs_mean", "REPs_median", "REPs_25", "REPs_75", "REPs_05", "REPs_95")])

pred_band_train_test = 
  ggplot() +
  geom_line(data = REP_sim_yearly_both_unique, aes(x = year, y = REPs_mean, col = sample)) +
  geom_point(data = REP_sim_yearly_both_unique, aes(x = year, y = REPs_mean, col = sample)) +
  geom_pointrange(data = REP_sim_yearly_both_unique, aes(x = year, y = REPs_mean, ymin = REPs_25, ymax = REPs_75, fill = sample, col = sample), 
                  fatten = 1, size = 1, alpha = 0.45) +
  geom_point(data = REP_sim_yearly_both_unique, aes(x = year, y = REP)) +
  geom_line(data = REP_sim_yearly_both_unique, aes(x = year, y = REP)) +
  theme_bw() +
  labs(y = "# REPs") +
  scale_y_continuous(limits = c(0,3.25)) +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "top")


pdf("Final figures/Figure_S8.pdf", width = 10, height = 11)
  grid.arrange(pred_band_train_test,
                   ratio_year_plot,
                   cdf_year_plot,
                   yearly_train_test,
                   layout_matrix = rbind(c(1),c(1),c(1),c(2),c(2),c(3),c(3),c(4),c(4),c(4)),
                   nrow = 4)
dev.off()


# now check the REP lag 1 correlation by simulation during the training (calibration) period
REP_sim = data.frame(date = training_data$date,
                     REP = training_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable) %>%
                                                             dplyr::mutate(value_lag1 = lag(value, 1),
                                                                           REP_lag1 = lag(REP, 1))

# create a three way contingency table for the simulations
REP_sim_daily_table = as.data.frame(ftable(REP_sim_daily %>% dplyr::select(value, value_lag1, variable)))

for(ii in 1:1000){
  REP_mod_persist0 = data.frame(variable = REP_sim_daily_table$variable[ii*4],
                                set = "sims",
                                lag1_persist_prob = REP_sim_daily_table$Freq[ii*4] / (REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4]),
                                lag1_persist_prob_norm = (REP_sim_daily_table$Freq[ii*4] / (REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4]))/
                                  ((REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4])/sum(REP_sim_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){REP_mod_persist = REP_mod_persist0}
  if(ii > 1){REP_mod_persist = rbind(REP_mod_persist,REP_mod_persist0)}
  
}


# create a three way contingency table for the observations -- only keep the top four lines since the rest are copies
REP_obs_daily_table = as.data.frame(ftable(REP_sim_daily %>% dplyr::select(REP, REP_lag1, variable)))[1:4,]

REP_obs_persist = data.frame(variable = "obs",
                             set = "obs",
                             lag1_persist_prob = REP_obs_daily_table$Freq[4] / (REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4]),
                             lag1_persist_prob_norm = (REP_obs_daily_table$Freq[4] / (REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4]))/
                               ((REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4])/sum(REP_obs_daily_table$Freq)))


REP_persist = rbind(REP_mod_persist, REP_obs_persist)

presist_prob_cal = 
ggplot(REP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = set)) +
  theme_bw() +
  labs(title = "Calibration period",
       y = expression(P(REP[t] ~"|" ~ REP[t-1])/P(REP))) +
  scale_y_continuous(limits = c(0,25))

# now check the REP lag 1 correlation by simulation during the testing period
REP_sim = data.frame(date = testing_data$date,
                     REP = testing_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

REP_sim_daily = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(variable) %>%
  dplyr::mutate(value_lag1 = lag(value, 1),
                REP_lag1 = lag(REP, 1))

# create a three way contingency table for the simulations
REP_sim_daily_table = as.data.frame(ftable(REP_sim_daily %>% dplyr::select(value, value_lag1, variable)))

for(ii in 1:1000){
  REP_mod_persist0 = data.frame(variable = REP_sim_daily_table$variable[ii*4],
                                set = "sims",
                                lag1_persist_prob = REP_sim_daily_table$Freq[ii*4] / (REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4]),
                                lag1_persist_prob_norm = (REP_sim_daily_table$Freq[ii*4] / (REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4]))/
                                  ((REP_sim_daily_table$Freq[(ii-1)*4+2] + REP_sim_daily_table$Freq[ii*4])/sum(REP_sim_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){REP_mod_persist = REP_mod_persist0}
  if(ii > 1){REP_mod_persist = rbind(REP_mod_persist,REP_mod_persist0)}
  
}

# create a three way contingency table for the observations -- only keep th top four lines since the rest are copies
REP_obs_daily_table = as.data.frame(ftable(REP_sim_daily %>% dplyr::select(REP, REP_lag1, variable)))[1:4,]

REP_obs_persist = data.frame(variable = "obs",
                             set = "obs",
                             lag1_persist_prob = REP_obs_daily_table$Freq[4] / (REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4]),
                             lag1_persist_prob_norm = (REP_obs_daily_table$Freq[4] / (REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4]))/
                               ((REP_obs_daily_table$Freq[2] + REP_obs_daily_table$Freq[4])/sum(REP_obs_daily_table$Freq)))


REP_persist = rbind(REP_mod_persist, REP_obs_persist)

presist_prob_test = 
  ggplot(REP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = set)) +
  theme_bw() +
  labs(title = "Testing period",
       y = expression(P(REP[t] ~"|" ~ REP[t-1])/P(REP))) +
  scale_y_continuous(limits = c(0,25))

pdf('Final figures/Figure_S9.pdf', width = 6, height = 3)
grid.arrange(presist_prob_cal,
             presist_prob_test,
             nrow = 1)
dev.off()

rm(list = ls())
