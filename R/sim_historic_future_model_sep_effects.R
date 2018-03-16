########################################
############### Figure 10 ##############
########################################
source("R/GetSeasonDate.R")

load("Processed_Data/Shifted_reanalysis/REP_EWD_M_O_all_future.RData")

# try the stan model
source("R/stan_logistic_mod_no_lag.R")
dat_include = REP_EWD_M_O_all %>% dplyr::group_by(model) %>%
  dplyr::mutate(west_index_anom = west_index_anom,
                east_index_anom = east_index_anom,
                M_index_anom = M_index_anom,
                REP_lag1 = REP_lag1,
                NPH_index_anom = NPH_index_anom) %>%
  dplyr::select(date, model, REP, M_index_anom, O_index_anom, west_index_anom, east_index_anom, NPH_index_anom, REP_lag1) %>%
  dplyr::filter(!is.na(west_index_anom) &
                  !is.na(east_index_anom) &
                  !is.na(M_index_anom) &
                  !is.na(NPH_index_anom) &
                  !is.na(REP_lag1))


load("Processed_Data/Shifted_reanalysis/REP_EWD_M_O_all.RData")

# try the stan model
source("R/stan_logistic_mod_no_lag.R")
dat_include_historic = REP_EWD_M_O_all %>% dplyr::group_by(model) %>%
  dplyr::mutate(west_index_anom = west_index_anom,
                east_index_anom = east_index_anom,
                M_index_anom = M_index_anom,
                REP_lag1 = REP_lag1,
                NPH_index_anom = NPH_index_anom) %>%
  dplyr::select(date, model, REP, M_index_anom, O_index_anom, west_index_anom, east_index_anom, NPH_index_anom, REP_lag1) %>%
  dplyr::filter(!is.na(west_index_anom) &
                  !is.na(east_index_anom) &
                  !is.na(M_index_anom) &
                  !is.na(NPH_index_anom) &
                  !is.na(REP_lag1))

# take the mean of each index between 1970 and 1999 (all non-zero O for NPH and M)
historic_means = dat_include_historic %>% dplyr::filter(model %in% c(1,4) &
                                                        lubridate::year(date) > 1969 &
                                                        lubridate::year(date) < 2000) %>%
  dplyr::ungroup() %>%
                                          dplyr::summarise(M_index_anom = mean(M_index_anom),
                                                           O_index_anom = mean(O_index_anom),
                                                           west_index_anom = mean(west_index_anom),
                                                           east_index_anom = mean(east_index_anom),
                                                           NPH_index_anom = mean(NPH_index_anom))

future_means = dat_include %>% dplyr::filter(model %in% c(1,4) &
                                               lubridate::year(date) > 2069 &
                                               lubridate::year(date) < 2100) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(M_index_anom = mean(M_index_anom),
                   O_index_anom = mean(O_index_anom),
                   west_index_anom = mean(west_index_anom),
                   east_index_anom = mean(east_index_anom),
                   NPH_index_anom = mean(NPH_index_anom))


index_diff_1970_2070 = future_means - historic_means

training_data = dat_include %>% dplyr::filter(model == "obs") %>% dplyr::ungroup()

testing_data_1970_1999 = dat_include_historic %>% dplyr::filter(model %in% c(1,4) &
                                                         lubridate::year(date) > 1969 &
                                                         lubridate::year(date) < 2000) %>% 
                                                  dplyr::ungroup()

testing_data_2070_2099 = dat_include %>% dplyr::filter(model == 1 &
                                                lubridate::year(date) > 2069 &
                                                lubridate::year(date) < 2100) %>% 
                                dplyr::ungroup()

testing_data_2070_2099_M_trend_removed = dat_include %>% dplyr::filter(model == 1 &
                                                                lubridate::year(date) > 2069 &
                                                                lubridate::year(date) < 2100) %>% 
                                                dplyr::ungroup() %>%
                                                dplyr::mutate(M_index_anom = ifelse(O_index_anom < 0, 
                                                                                    M_index_anom - index_diff_1970_2070$M_index_anom,
                                                                                    0))

# set parameters for the MCMC
iter = 1000
thin = 2
chains = 4

dat_list_1970_1999 = list(T_train = nrow(training_data),
                  T_test = nrow(testing_data_1970_1999),
                  P = 5,
                  X_train = as.matrix(training_data %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                  X_test = as.matrix(testing_data_1970_1999 %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                  y_train = training_data$REP)
    
stan_fit_1970_1999 =
      stan(model_code = stan_logistic_mod_no_lag,
           data = dat_list_1970_1999,
           verbose = FALSE,
           seed = 22,
           chains = chains,
           iter = iter,
           thin = thin)
    
print(stan_fit_1970_1999, pars = c("alpha","beta"))
plot(stan_fit_1970_1999, pars = c("alpha","beta"))
traceplot(stan_fit_1970_1999, pars = c("alpha","beta"))


dat_list_2070_2099 = list(T_train = nrow(training_data),
                          T_test = nrow(testing_data_2070_2099),
                          P = 5,
                          X_train = as.matrix(training_data %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                          X_test = as.matrix(testing_data_2070_2099 %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                          y_train = training_data$REP)

stan_fit_2070_2099 =
  stan(model_code = stan_logistic_mod_no_lag,
       data = dat_list_2070_2099,
       verbose = FALSE,
       seed = 22,
       chains = chains,
       iter = iter,
       thin = thin)

print(stan_fit_2070_2099, pars = c("alpha","beta"))
plot(stan_fit_2070_2099, pars = c("alpha","beta"))
traceplot(stan_fit_2070_2099, pars = c("alpha","beta"))


dat_list_2070_2099_M_trend_removed = list(T_train = nrow(training_data),
                          T_test = nrow(testing_data_2070_2099_M_trend_removed),
                          P = 5,
                          X_train = as.matrix(training_data %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                          X_test = as.matrix(testing_data_2070_2099_M_trend_removed %>% dplyr::select(M_index_anom, west_index_anom, east_index_anom, NPH_index_anom,O_index_anom)),
                          y_train = training_data$REP)

stan_fit_2070_2099_M_trend_removed =
  stan(model_code = stan_logistic_mod_no_lag,
       data = dat_list_2070_2099_M_trend_removed,
       verbose = FALSE,
       seed = 22,
       chains = chains,
       iter = iter,
       thin = thin)

print(stan_fit_2070_2099_M_trend_removed, pars = c("alpha","beta"))
plot(stan_fit_2070_2099_M_trend_removed, pars = c("alpha","beta"))
traceplot(stan_fit_2070_2099_M_trend_removed, pars = c("alpha","beta"))

# testing data

# look at the testing sim
REP_test_sim_1970_1999 = data.frame(date = testing_data_1970_1999$date,
                                    model = testing_data_1970_1999$model,
                                    t(data.frame(rstan::extract(stan_fit_1970_1999, pars = "y_test_sim"))))


REP_test_sim_yearly_mean_1970_1999 = melt(REP_test_sim_1970_1999, id.vars = c("date","model")) %>% dplyr::group_by(year = lubridate::year(date), variable, model) %>%
  dplyr::summarise(REP_sim = sum(value)) %>%
  dplyr::group_by(variable, model) %>%
  dplyr::summarise(REP_sim_mean = mean(REP_sim)) %>% 
  dplyr::mutate(experiment = "1970-1999")


REP_test_sim_2070_2099 = data.frame(date = testing_data_2070_2099$date,
                                                    model = testing_data_2070_2099$model,
                                                    t(data.frame(rstan::extract(stan_fit_2070_2099, pars = "y_test_sim"))))


REP_test_sim_yearly_mean_2070_2099 = melt(REP_test_sim_2070_2099, id.vars = c("date","model")) %>% dplyr::group_by(year = lubridate::year(date), variable, model) %>%
  dplyr::summarise(REP_sim = sum(value)) %>%
  dplyr::group_by(variable, model) %>%
  dplyr::summarise(REP_sim_mean = mean(REP_sim)) %>% 
  dplyr::mutate(experiment = "2070-2099")

REP_test_sim_2070_2099_M_trend_removed = data.frame(date = testing_data_2070_2099_M_trend_removed$date,
                          model = testing_data_2070_2099_M_trend_removed$model,
                          t(data.frame(rstan::extract(stan_fit_2070_2099_M_trend_removed, pars = "y_test_sim"))))


REP_test_sim_yearly_mean_2070_2099_M_trend_removed = melt(REP_test_sim_2070_2099_M_trend_removed, id.vars = c("date","model")) %>% dplyr::group_by(year = lubridate::year(date), variable, model) %>%
  dplyr::summarise(REP_sim = sum(value)) %>%
  dplyr::group_by(variable, model) %>%
  dplyr::summarise(REP_sim_mean = mean(REP_sim)) %>% 
  dplyr::mutate(experiment = "2070-2099 \n (moisture trend removed)")

REP_test_sim_yearly_mean = rbind(REP_test_sim_yearly_mean_2070_2099_M_trend_removed,
                                 REP_test_sim_yearly_mean_2070_2099,
                                 REP_test_sim_yearly_mean_1970_1999)

density_bw_setting = 0.25

# medians of the experiments
REP_test_sim_yearly_mean %>% dplyr::group_by(experiment) %>%
                             dplyr::summarise(REP_median = median(REP_sim_mean))
#                                 experiment REP_median
# <chr>      <dbl>
#   1                             1970-1999  0.9333333
# 2                               2070-2099  2.8000000
# 3 2070-2099 \n (moisture trend removed)    1.4666667

2.8000000/0.9333333
1.4666667/0.9333333

future_density_plots = 
ggplot(REP_test_sim_yearly_mean) +
  stat_density(aes(x = REP_sim_mean, col = experiment, linetype = experiment), 
               bw = density_bw_setting, geom="line",position="identity", size = 0.75) +
  scale_x_continuous(limits= c(0,5)) +
  labs(x = 'Mean # of REPs per year',
       y = 'density') +
  theme_bw()

pdf('Final figures/Figure_10.pdf', width = 8, height = 4)
plot(future_density_plots)
dev.off()

rm(list = ls())
