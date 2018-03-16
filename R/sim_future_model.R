########################################
############### Figure 9 ###############
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

training_data = dat_include %>% dplyr::filter(model == "obs") %>% dplyr::ungroup()

testing_data1 = dat_include %>% dplyr::filter(model == 1) %>% dplyr::ungroup()

# set parameters for the MCMC
iter = 1000
thin = 2
chains = 4

dat_list_1 = list(T_train = nrow(training_data),
                  T_test = nrow(testing_data1),
                  P = 5,
                  X_train = as.matrix(training_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                  X_test = as.matrix(testing_data1 %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                  y_train = training_data$REP)
    
stan_fit_1 =
      stan(model_code = stan_logistic_mod_no_lag,
           data = dat_list_1,
           verbose = FALSE,
           seed = 22,
           chains = chains,
           iter = iter,
           thin = thin)
    
print(stan_fit_1, pars = c("alpha","beta"))
plot(stan_fit_1, pars = c("alpha","beta"))
traceplot(stan_fit_1, pars = c("alpha","beta"))

# look at the testing sim
REP_test_sim = data.frame(date = testing_data1$date,
                          REP = testing_data1$REP,
                          model = testing_data1$model,
                          t(data.frame(rstan::extract(stan_fit_1, pars = "y_test_sim"))))


REP_test_sim_yearly0 = melt(REP_test_sim, id.vars = c("date","model","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable, model) %>%
  dplyr::summarise(REP_sim = sum(value),
                   REP = sum(REP))

REP_test_sim_yearly0 = melt(REP_test_sim, id.vars = c("date","model","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable, model) %>%
  dplyr::summarise(REP_sim = sum(value),
                   REP = sum(REP))



smoothing_window = 10

REP_sim_yearly = REP_test_sim_yearly0

REP_sim_yearly$REP_smooth = REP_sim_yearly$REP_sim_smooth = NA
for(mmodel in c(1)){
  for(vvar in unique(REP_sim_yearly$variable)){
    REP_sim_yearly$REP_smooth[REP_sim_yearly$variable == vvar &
                                REP_sim_yearly$model == mmodel] = ksmooth(REP_sim_yearly$year[REP_sim_yearly$variable == vvar &
                                                                                                REP_sim_yearly$model == mmodel], 
                                                                          REP_sim_yearly$REP[REP_sim_yearly$variable == vvar &
                                                                                               REP_sim_yearly$model == mmodel], 
                                                                          kernel = "normal", 
                                                                          bandwidth = smoothing_window, 
                                                                          n.points = length(REP_sim_yearly$REP[REP_sim_yearly$variable == vvar &
                                                                                                                 REP_sim_yearly$model == mmodel]))$y
    
    REP_sim_yearly$REP_sim_smooth[REP_sim_yearly$variable == vvar &
                                    REP_sim_yearly$model == mmodel] = ksmooth(REP_sim_yearly$year[REP_sim_yearly$variable == vvar &
                                                                                                    REP_sim_yearly$model == mmodel], 
                                                                              REP_sim_yearly$REP_sim[REP_sim_yearly$variable == vvar &
                                                                                                       REP_sim_yearly$model == mmodel], 
                                                                              kernel = "normal", 
                                                                              bandwidth = smoothing_window, 
                                                                              n.points = length(REP_sim_yearly$REP_sim[REP_sim_yearly$variable == vvar &
                                                                                                                         REP_sim_yearly$model == mmodel]))$y
    print(vvar)
  }
}

REP_sim_yearly_training = REP_sim_yearly %>%
  dplyr::group_by(year,model) %>%
  dplyr::summarise(REPs_smooth_mean = mean(REP_sim_smooth, na.rm = TRUE),
                   REPs_smooth_25 = quantile(REP_sim_smooth, probs = 0.25, na.rm = TRUE),
                   REPs_smooth_75 = quantile(REP_sim_smooth, probs = 0.75, na.rm = TRUE),
                   REPs_smooth_2.5 = quantile(REP_sim_smooth, probs = 0.025, na.rm = TRUE),
                   REPs_smooth_97.5 = quantile(REP_sim_smooth, probs = 0.975, na.rm = TRUE),
                   REP_smooth = median(REP_smooth, na.rm = TRUE))


REP_sim_yearly_training_plot =  REP_sim_yearly_training %>% dplyr::filter(year > (2006 + (smoothing_window/2 - 1)) &
                                                                            year < (2100 - (smoothing_window/2 - 1)))

line.width = 1
REP_mod_obs_compare_1 =
  ggplot(REP_sim_yearly_training_plot) +
  geom_ribbon(aes(x = year, ymin = REPs_smooth_25, ymax = REPs_smooth_75, group = model), alpha = 0.5) +
  geom_ribbon(aes(x = year, ymin = REPs_smooth_2.5, ymax = REPs_smooth_97.5, group = model), alpha = 0.5) +
  geom_line(aes(year, REPs_smooth_mean, group = model), linetype = "dashed", size = line.width) +
  geom_line(aes(year, REP_smooth, group = model), size = line.width) +
  theme_bw() +
  geom_line(aes(year, REP_smooth/2.206522, group = model), linetype = "twodash", size = line.width, col = "blue") +
  theme_bw() +
  labs(y = "# REP") +
  annotate("text", label = "a)", x = 2010, y = 5.8, size = 6, colour = "black") +
  scale_y_continuous(limits = c(0,6))


line.width = 1
alpha.line = 1
col.line = "grey80"
bin_breaks = seq(-0.5,12.5,1)
bin_labels = as.character(bin_breaks-0.5)
hist_lims = data.frame(xlim = c(0.5,11.6),
                       ylim = c(0,50))
REP_mod_obs_compare_2 =
  ggplot() + 
  stat_bin(data = REP_test_sim_yearly0[REP_test_sim_yearly0$model == 1,],
           aes(x = REP_sim, y =..count../(1000), group = model),
           geom = "step", linetype = "dashed", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_test_sim_yearly0,
           aes(x = REP, y =..count../(1000), group = model),
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_test_sim_yearly0,
           aes(x = REP/2.206522, y =..count../(1000), group = model),
           geom = "step", linetype = "twodash", size = line.width,
           breaks = bin_breaks, col = "blue") +
  annotate("text", label = "b)", x = 0.5, y = 48, size = 6, colour = "black") +
  labs(x = "# REP",
       y = "count") +
  coord_cartesian(xlim = hist_lims$xlim, 
                  ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()

pdf('Final figures/Figure_9.pdf', width = 11, height = 2.75)
grid.arrange(REP_mod_obs_compare_1,
             REP_mod_obs_compare_2,
             nrow = 1)
dev.off()

rm(list = ls())
