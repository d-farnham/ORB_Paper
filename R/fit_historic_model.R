rm(list = ls())
source("R/GetSeasonDate.R")

load("Processed_Data/Shifted_reanalysis/RIP_EWD_M_O_all.RData")

# try the stan model
source("R/stan_logistic_mod_no_lag.R")
dat_include = RIP_EWD_M_O_all %>% dplyr::filter(model == "obs") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(west_index_anom = lead(west_index_anom,1),
                east_index_anom = east_index_anom,
                M_index_anom = M_index_anom,
                RIP_lag1 = RIP_lag1,
                NPH_index_anom = NPH_index_anom) %>%
  dplyr::select(date, RIP, M_index_anom, O_index_anom, west_index_anom, east_index_anom, NPH_index_anom, RIP_lag1) %>%
  dplyr::filter(!is.na(west_index_anom) &
                !is.na(east_index_anom) &
                !is.na(M_index_anom) &
                !is.na(NPH_index_anom) &
                !is.na(RIP_lag1))


# look at the correlations and partial correlations
cor(dat_include %>% dplyr::ungroup() %>% dplyr::select(-date), use = "complete.obs")
library(ppcor)
pcor(dat_include %>% dplyr::ungroup() %>% dplyr::select(-date))

# can we get a sense of where the dominant signals are in the data?
library(WaveletComp)
dat_include
dat_include_yearly = dat_include %>% dplyr::group_by(year = lubridate::year(date)) %>%
  dplyr::summarise(RIP = sum(RIP))

wave_out = analyze.wavelet(dat_include_yearly, my.series = "RIP")
wt.image(wave_out)
wt.avg(wave_out)

# what is sd(RIP) = 0.9743
# n = 56
# so sd * n ^(-1/5)
sd(dat_include_yearly$RIP) * (nrow(dat_include_yearly)^(-0.2))


train_test_ratio = 3/4

training_data = dat_include %>% dplyr::filter(lubridate::year(date) < (1950+ceiling(56*train_test_ratio)))
testing_data = dat_include %>% dplyr::filter(lubridate::year(date) > (1950+ceiling(56*train_test_ratio) - 1))

dat_list = list(T_train = nrow(training_data),
                T_test = nrow(testing_data),
                P = 5,
                X_train = as.matrix(training_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                X_test = as.matrix(testing_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                y_train = training_data$RIP)

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


# look at probability of simulating a extreme event given htat the observed value was an extreme precipitation event
# first the calibration period
RIP_sim = data.frame(date = training_data$date,
                     RIP = training_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

RIP_sim_daily = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(variable, RIP) %>%
  dplyr::summarise(num_1 = sum(value),
                   num_0 = sum(abs(value - 1))) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(global_prob_1 = sum(num_1)/sum(num_1 + num_0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prob_1_0 = ifelse(RIP == 0, num_0/(num_1 + num_0)/(1-global_prob_1), num_1/(num_1 + num_0)/(global_prob_1)))
                   
daily_check_cal = 
  ggplot(RIP_sim_daily) +
  geom_boxplot(aes(factor(RIP), prob_1_0)) +
  theme_bw() +
  scale_y_log10(limits = c(0.5, 200)) +
  labs(title = "Calibration period",
       y = expression(paste('P(REP'[sim],' = REP'[obs], ')/P(REP'[obs],' = x)')),
       x = "x") 


# now the testing period
RIP_sim = data.frame(date = testing_data$date,
                     RIP = testing_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

RIP_sim_daily = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(variable, RIP) %>%
  dplyr::summarise(num_1 = sum(value),
                   num_0 = sum(abs(value - 1))) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(global_prob_1 = sum(num_1)/sum(num_1 + num_0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prob_1_0 = ifelse(RIP == 0, num_0/(num_1 + num_0)/(1-global_prob_1), num_1/(num_1 + num_0)/(global_prob_1)))

daily_check_test = 
  ggplot(RIP_sim_daily) +
  geom_boxplot(aes(factor(RIP), prob_1_0)) +
  theme_bw() +
  scale_y_log10(limits = c(0.5, 200)) +
  labs(title = "Testing period",
       y = expression(paste('P(REP'[sim],' = REP'[obs], ')/P(REP'[obs],' = x)')),
       x = "x") 

pdf('Final figures/REVISED/model_checking_1.pdf', width = 6, height = 3)
grid.arrange(daily_check_cal,
             daily_check_test,
             nrow = 1)
dev.off()





# now compare the decadal trends in the raw data and the model simulations
# start with the calibration set
RIP_sim = data.frame(date = training_data$date,
                     RIP = training_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

RIP_sim_yearly0 = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(RIPs = sum(value),
                   RIP = sum(RIP)) %>%
  dplyr::group_by(RIP) %>%
  dplyr::mutate(RIPs_mean = mean(RIPs))

yearly_train = 
  ggplot(RIP_sim_yearly0) +
  geom_boxplot(aes(factor(RIP), RIPs)) +
  geom_point(aes(factor(RIP), RIPs_mean), shape = 3, col = "red", size = 3) +
  theme_bw() +
  labs(x = "# REPs observed in year",
       y = "# REPs predicted in year",
       title = "Calibration period")

RIP_sim_yearly_non_smoothed = RIP_sim_yearly0 %>% dplyr::group_by(year) %>%
  dplyr::summarise(RIP_obs = mean(RIP),
                   RIP_sim_mean = mean(RIPs),
                   RIP_sim_median = median(RIPs))

RIP_sim = data.frame(date = testing_data$date,
                     RIP = testing_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

RIP_sim_yearly0 = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(RIPs = sum(value),
                   RIP = sum(RIP)) %>%
  dplyr::group_by(RIP) %>%
  dplyr::mutate(RIPs_mean = mean(RIPs))

yearly_test = 
ggplot(RIP_sim_yearly0) +
  geom_boxplot(aes(factor(RIP), RIPs)) +
  geom_point(aes(factor(RIP), RIPs_mean), shape = 3, col = "red", size = 3) +
  theme_bw() +
  labs(x = "# REPs observed in year",
       y = "# REPs predicted in year",
       title = "Testing period")


# now do the same thing for a ten-year bandwidth
RIP_sim = data.frame(date = training_data$date,
                     RIP = training_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

RIP_sim_yearly0 = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(RIPs = sum(value),
                   RIP = sum(RIP))



smoothing_window = 10

RIP_sim_yearly = RIP_sim_yearly0

RIP_sim_yearly$RIP_smooth = RIP_sim_yearly$RIP_sim_smooth = NA
for(vvar in unique(RIP_sim_yearly$variable)){
  RIP_sim_yearly$RIP_smooth[RIP_sim_yearly$variable == vvar] = ksmooth(RIP_sim_yearly$year[RIP_sim_yearly$variable == vvar], 
                                                                       RIP_sim_yearly$RIP[RIP_sim_yearly$variable == vvar], 
                                                                       kernel = "normal", 
                                                                       bandwidth = smoothing_window, 
                                                                       n.points = length(RIP_sim_yearly$RIP[RIP_sim_yearly$variable == vvar]))$y
  
  RIP_sim_yearly$RIP_sim_smooth[RIP_sim_yearly$variable == vvar] = ksmooth(RIP_sim_yearly$year[RIP_sim_yearly$variable == vvar], 
                                                                           RIP_sim_yearly$RIPs[RIP_sim_yearly$variable == vvar], 
                                                                           kernel = "normal", 
                                                                           bandwidth = smoothing_window, 
                                                                           n.points = length(RIP_sim_yearly$RIPs[RIP_sim_yearly$variable == vvar]))$y
  print(vvar)
}


RIP_sim_yearly_training = RIP_sim_yearly %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(RIPs_smooth_mean = mean(RIP_sim_smooth, na.rm = TRUE),
                   RIPs_smooth_25 = quantile(RIP_sim_smooth, probs = 0.25, na.rm = TRUE),
                   RIPs_smooth_75 = quantile(RIP_sim_smooth, probs = 0.75, na.rm = TRUE),
                   RIPs_smooth_2.5 = quantile(RIP_sim_smooth, probs = 0.025, na.rm = TRUE),
                   RIPs_smooth_97.5 = quantile(RIP_sim_smooth, probs = 0.975, na.rm = TRUE),
                   RIP_smooth = median(RIP_smooth, na.rm = TRUE))


# now move onto the testing period
RIP_sim = data.frame(date = testing_data$date,
                     RIP = testing_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

RIP_sim_yearly0 = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(RIPs = sum(value),
                   RIP = sum(RIP))

RIP_sim_yearly = RIP_sim_yearly0
RIP_sim_yearly$RIP_smooth = RIP_sim_yearly$RIP_sim_smooth = NA
for(vvar in unique(RIP_sim_yearly$variable)){
  RIP_sim_yearly$RIP_smooth[RIP_sim_yearly$variable == vvar] = ksmooth(RIP_sim_yearly$year[RIP_sim_yearly$variable == vvar], 
                                                                       RIP_sim_yearly$RIP[RIP_sim_yearly$variable == vvar], 
                                                                       kernel = "normal", 
                                                                       bandwidth = smoothing_window, 
                                                                       n.points = length(RIP_sim_yearly$RIP[RIP_sim_yearly$variable == vvar]))$y
  
  RIP_sim_yearly$RIP_sim_smooth[RIP_sim_yearly$variable == vvar] = ksmooth(RIP_sim_yearly$year[RIP_sim_yearly$variable == vvar], 
                                                                           RIP_sim_yearly$RIPs[RIP_sim_yearly$variable == vvar], 
                                                                           kernel = "normal", 
                                                                           bandwidth = smoothing_window, 
                                                                           n.points = length(RIP_sim_yearly$RIPs[RIP_sim_yearly$variable == vvar]))$y
  print(vvar)
}


RIP_sim_yearly_testing = RIP_sim_yearly %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(RIPs_smooth_mean = mean(RIP_sim_smooth, na.rm = TRUE),
                   RIPs_smooth_25 = quantile(RIP_sim_smooth, probs = 0.25, na.rm = TRUE),
                   RIPs_smooth_75 = quantile(RIP_sim_smooth, probs = 0.75, na.rm = TRUE),
                   RIPs_smooth_2.5 = quantile(RIP_sim_smooth, probs = 0.025, na.rm = TRUE),
                   RIPs_smooth_97.5 = quantile(RIP_sim_smooth, probs = 0.975, na.rm = TRUE),
                   RIP_smooth = median(RIP_smooth, na.rm = TRUE))


# exclude the edges from both the calibration and testing sets

RIP_sim_yearly_training_plot =  RIP_sim_yearly_training %>% dplyr::filter(year > (min(year) + (smoothing_window/2 - 1)) &
                                                                            year < (max(year) - (smoothing_window/2 - 1)))
RIP_sim_yearly_testing_plot =  RIP_sim_yearly_testing %>% dplyr::filter(year > (min(year) + (smoothing_window/2 - 1)) &
                                                                          year < (max(year) - (smoothing_window/2 - 1)))

# now plot these together

mod_check_2_10_year_band = 
  ggplot() +
  geom_ribbon(data = RIP_sim_yearly_training_plot, aes(x = year, ymin = RIPs_smooth_25, ymax = RIPs_smooth_75), alpha = 0.25, fill = "blue") +
  geom_ribbon(data = RIP_sim_yearly_training_plot, aes(x = year, ymin = RIPs_smooth_2.5, ymax = RIPs_smooth_97.5), alpha = 0.25, fill = "blue") +
  geom_line(data = RIP_sim_yearly_training_plot,aes(year, RIPs_smooth_mean), col = "blue") +
  geom_line(data = RIP_sim_yearly_training_plot,aes(year, RIP_smooth), col = "black") +
  geom_ribbon(data = RIP_sim_yearly_testing_plot, aes(x = year, ymin = RIPs_smooth_25, ymax = RIPs_smooth_75), alpha = 0.25, fill = "red") +
  geom_ribbon(data = RIP_sim_yearly_testing_plot, aes(x = year, ymin = RIPs_smooth_2.5, ymax = RIPs_smooth_97.5), alpha = 0.25, fill = "red") +
  geom_line(data = RIP_sim_yearly_testing_plot,aes(year, RIPs_smooth_mean), col = "red") +
  geom_line(data = RIP_sim_yearly_testing_plot,aes(year, RIP_smooth), col = "black") +
  theme_bw() +
  labs(y = "kernel smoothed avg. # of yearly REPs \n (bandwidth = 10 years)") +
  scale_y_continuous(limits = c(0,2.5))



pdf("Final figures/REVISED/model_checking_2.pdf", width = 7, height = 7)
grid.arrange(yearly_train,
             yearly_test,
             mod_check_2_10_year_band,
             layout_matrix = cbind(c(1,3),c(2,3)))
dev.off()


# now check the RIP lag 1 correlation by simulation during the training (calibration) period
RIP_sim = data.frame(date = training_data$date,
                     RIP = training_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

RIP_sim_daily = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(variable) %>%
                                                             dplyr::mutate(value_lag1 = lag(value, 1),
                                                                           RIP_lag1 = lag(RIP, 1))

# create a three way contingency table for the simulations
RIP_sim_daily_table = as.data.frame(ftable(RIP_sim_daily %>% dplyr::select(value, value_lag1, variable)))

for(ii in 1:1000){
  RIP_mod_persist0 = data.frame(variable = RIP_sim_daily_table$variable[ii*4],
                                set = "sims",
                                lag1_persist_prob = RIP_sim_daily_table$Freq[ii*4] / (RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4]),
                                lag1_persist_prob_norm = (RIP_sim_daily_table$Freq[ii*4] / (RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4]))/
                                  ((RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4])/sum(RIP_sim_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){RIP_mod_persist = RIP_mod_persist0}
  if(ii > 1){RIP_mod_persist = rbind(RIP_mod_persist,RIP_mod_persist0)}
  
}


# create a three way contingency table for the observations -- only keep the top four lines since the rest are copies
RIP_obs_daily_table = as.data.frame(ftable(RIP_sim_daily %>% dplyr::select(RIP, RIP_lag1, variable)))[1:4,]

RIP_obs_persist = data.frame(variable = "obs",
                             set = "obs",
                             lag1_persist_prob = RIP_obs_daily_table$Freq[4] / (RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4]),
                             lag1_persist_prob_norm = (RIP_obs_daily_table$Freq[4] / (RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4]))/
                               ((RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4])/sum(RIP_obs_daily_table$Freq)))


RIP_persist = rbind(RIP_mod_persist, RIP_obs_persist)

presist_prob_cal = 
ggplot(RIP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = set)) +
  theme_bw() +
  labs(title = "Calibration period",
       y = expression(P(REP[t] ~"|" ~ REP[t-1])/P(REP))) +
  scale_y_continuous(limits = c(0,25))

# now check the RIP lag 1 correlation by simulation during the testing period
RIP_sim = data.frame(date = testing_data$date,
                     RIP = testing_data$RIP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_test_sim"))))

RIP_sim_daily = melt(RIP_sim, id.vars = c("date","RIP")) %>% dplyr::group_by(variable) %>%
  dplyr::mutate(value_lag1 = lag(value, 1),
                RIP_lag1 = lag(RIP, 1))

# create a three way contingency table for the simulations
RIP_sim_daily_table = as.data.frame(ftable(RIP_sim_daily %>% dplyr::select(value, value_lag1, variable)))

for(ii in 1:1000){
  RIP_mod_persist0 = data.frame(variable = RIP_sim_daily_table$variable[ii*4],
                                set = "sims",
                                lag1_persist_prob = RIP_sim_daily_table$Freq[ii*4] / (RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4]),
                                lag1_persist_prob_norm = (RIP_sim_daily_table$Freq[ii*4] / (RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4]))/
                                  ((RIP_sim_daily_table$Freq[(ii-1)*4+2] + RIP_sim_daily_table$Freq[ii*4])/sum(RIP_sim_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){RIP_mod_persist = RIP_mod_persist0}
  if(ii > 1){RIP_mod_persist = rbind(RIP_mod_persist,RIP_mod_persist0)}
  
}

# create a three way contingency table for the observations -- only keep th top four lines since the rest are copies
RIP_obs_daily_table = as.data.frame(ftable(RIP_sim_daily %>% dplyr::select(RIP, RIP_lag1, variable)))[1:4,]

RIP_obs_persist = data.frame(variable = "obs",
                             set = "obs",
                             lag1_persist_prob = RIP_obs_daily_table$Freq[4] / (RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4]),
                             lag1_persist_prob_norm = (RIP_obs_daily_table$Freq[4] / (RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4]))/
                               ((RIP_obs_daily_table$Freq[2] + RIP_obs_daily_table$Freq[4])/sum(RIP_obs_daily_table$Freq)))


RIP_persist = rbind(RIP_mod_persist, RIP_obs_persist)

presist_prob_test = 
  ggplot(RIP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = set)) +
  theme_bw() +
  labs(title = "Testing period",
       y = expression(P(REP[t] ~"|" ~ REP[t-1])/P(REP))) +
  scale_y_continuous(limits = c(0,25))

pdf('Final figures/REVISED/model_checking_3.pdf', width = 6, height = 3)
grid.arrange(presist_prob_cal,
             presist_prob_test,
             nrow = 1)
dev.off()
