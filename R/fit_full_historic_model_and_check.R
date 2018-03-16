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


training_data = dat_include
dat_list = list(T_train = nrow(training_data),
                T_test = nrow(training_data),
                P = 5,
                X_train = as.matrix(training_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
                X_test = as.matrix(training_data %>% dplyr::select(west_index_anom, east_index_anom, NPH_index_anom, M_index_anom, O_index_anom)),
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

#          mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# alpha   -7.20    0.01 0.44 -8.11 -7.50 -7.19 -6.88 -6.44   930    1
# beta[1] -0.72    0.01 0.18 -1.07 -0.84 -0.72 -0.61 -0.37   961    1
# beta[2]  0.65    0.01 0.30  0.07  0.45  0.64  0.84  1.26   785    1
# beta[3]  0.41    0.01 0.18  0.07  0.30  0.41  0.54  0.77   960    1
# beta[4]  0.90    0.01 0.25  0.42  0.74  0.91  1.07  1.39   899    1
# beta[5] -1.11    0.01 0.21 -1.53 -1.25 -1.11 -0.97 -0.71   935    1

# check the percent of deviance explained by a non-Bayesian model

mod_fit_full_data = glm(REP ~ west_index_anom + east_index_anom + NPH_index_anom + M_index_anom + O_index_anom, 
                        data = training_data, family = binomial(link = "logit"))

summary(mod_fit_full_data)
1 - (mod_fit_full_data$deviance/mod_fit_full_data$null.deviance)


# wavelet of observed vs. mean simulated yearly records
REP_sim = data.frame(date = training_data$date,
                     REP = training_data$REP,
                     t(data.frame(rstan::extract(stan_fit, pars = "y_train_sim"))))

REP_sim_yearly = melt(REP_sim, id.vars = c("date","REP")) %>% dplyr::group_by(year = lubridate::year(date), variable) %>%
  dplyr::summarise(REPs = sum(value),
                   REP = sum(REP))

REP_sim_yearly_non_smoothed = REP_sim_yearly %>% dplyr::group_by(year) %>%
                              dplyr::summarise(REP_obs = mean(REP),
                                               REP_sim_mean = mean(REPs),
                                               REP_sim_median = median(REPs))


# Mann-Whitney test
wilcox.test(x = REP_sim_yearly_non_smoothed$REP_obs,
            y = REP_sim_yearly_non_smoothed$REP_sim_mean)

# Wilcoxon rank sum test with continuity correction
# 
# data:  REP_sim_yearly_non_smoothed$REP_obs and REP_sim_yearly_non_smoothed$REP_sim_mean
# W = 1306, p-value = 0.1246
# alternative hypothesis: true location shift is not equal to 0



library(WaveletComp)


wave_out_obs = analyze.wavelet(REP_sim_yearly_non_smoothed,  my.series = "REP_obs", method = "shuffle")
wave_out_mean_pred = analyze.wavelet(REP_sim_yearly_non_smoothed, my.series = "REP_sim_mean", method = "shuffle")

pdf("Final figures/Figure_S10.pdf", width = 8, height = 9)
par(mfrow=c(2,1))
wt.image(wave_out_obs, main = "Observed spectrum", timelab = "years past 1950", periodlab = "period (years)", plot.ridge = FALSE, graphics.reset = FALSE, color.key = "q")
wt.image(wave_out_mean_pred, main = "mean predicted spectrum", timelab = "years past 1950", periodlab = "period (years)", plot.ridge = FALSE, graphics.reset = FALSE, color.key = "q")
dev.off()

rm(list = ls())
