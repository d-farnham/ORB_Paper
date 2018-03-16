########################################
############### Figure 3 ###############
########################################
# now plot the time-series and histograms of REPs
# only include ensemble members 1 and 4
source("R/GetSeasonDate.R")

# compare the 99th percentiles in the model and the CPC
load(file = 'Processed_Data/CPC_mod_cell_pr.RData')
load(file = 'Processed_Data/pr_mod.RData')

ninety_ninth_p_mod = pr_mod %>% dplyr::filter(model %in% c(1,4)) %>%
  dplyr::group_by(lat,lon,model) %>%
  dplyr::summarise(cut = quantile(value*24*60*60, probs = 0.99)) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(cut_min = min(cut),
                   cut_max = max(cut),
                   cut_med = median(cut))

ninety_ninth_p_CPC = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
  dplyr::summarise(cut = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
  dplyr::group_by() %>%
  dplyr::summarise(cut_min = min(cut),
                   cut_max = max(cut),
                   cut_med = median(cut))


# load the REP records
load(file = 'Processed_Data/mod_REP.RData')
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')
load(file = 'Processed_Data/mod_CPC_thresh_REP.RData')

# compute the counts of REP by year for MAM for the observed record (CPC)
REP_CPC_by_season_year = CPC_mod_cell_REP %>% dplyr::filter(season == "MAM") %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::summarise(REP = sum(REP, na.rm = TRUE))

REP_CPC_by_season_year %>% dplyr::summarise(tot_REPs = sum(REP))
# 46

# compute the counts of REP by year for MAM for the model record
REP_mod_by_season_year = mod_REP %>% dplyr::filter(season == "MAM" &
                                                     model %in% c(1,4)) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year, model) %>%
  dplyr::summarise(REP = sum(REP, na.rm = TRUE)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::mutate(REP_ensemble_mean = mean(REP, na.rm = TRUE))

REP_mod_by_season_year %>% dplyr::group_by(model) %>% dplyr::summarise(tot_REPs = sum(REP))

# 103
# 115

# compute the counts of REP by year for MAM for the model record with observed thresholds
REP_mod_CPC_thresh_by_season_year = mod_CPC_thresh_REP %>% dplyr::filter(season == "MAM" &
                                                                           model %in% c(1,4)) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year, model) %>%
  dplyr::summarise(REP = sum(REP, na.rm = TRUE)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::mutate(REP_ensemble_mean = mean(REP, na.rm = TRUE))

# now compute the 10-year running mean for each of these three records
source('R/ma.R')
smoothing_window = 10

REP_CPC_by_season_year = REP_CPC_by_season_year %>% dplyr::mutate(REP_smooth = ma(REP, smoothing_window))
REP_mod_by_season_year = REP_mod_by_season_year %>% dplyr::group_by(model,season) %>%
  dplyr::mutate(REP_smooth = ma(REP, smoothing_window),
                REP_ensemble_mean_smooth = ma(REP_ensemble_mean, smoothing_window))

REP_mod_CPC_thresh_by_season_year = REP_mod_CPC_thresh_by_season_year %>% dplyr::group_by(model,season) %>%
  dplyr::mutate(REP_smooth = ma(REP, smoothing_window),
                REP_ensemble_mean_smooth = ma(REP_ensemble_mean, smoothing_window))

# what is the mean bias between the GCM ensemble members and the observed
REP_mod_by_season_year %>% dplyr::filter(season == "MAM") %>% dplyr::group_by(model) %>% dplyr::summarise(REP = sum(REP)) # 88 and 115 REPs
REP_mod_CPC_thresh_by_season_year %>% dplyr::filter(season == "MAM") %>% dplyr::group_by(model) %>% dplyr::summarise(REP = sum(REP)) # 213 and 209 REPs
REP_CPC_by_season_year %>% dplyr::filter(season == "MAM") %>% dplyr::summarise(REP = sum(REP)) # 46 REPs

# (103 + 115) / 2 / 46
# 2.37 -- the GCM has 2.37 more REPs than the observed


# Mann-Whitney test
wilcox.test(x = REP_mod_by_season_year$REP[REP_mod_by_season_year$model == 1],
            y = REP_CPC_by_season_year$REP)

# Wilcoxon rank sum test with continuity correction
# 
# data:  REP_mod_by_season_year$REP[REP_mod_by_season_year$model == 1] and REP_CPC_by_season_year$REP
# W = 2188, p-value = 0.0001812
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(x = REP_mod_by_season_year$REP[REP_mod_by_season_year$model == 4],
            y = REP_CPC_by_season_year$REP)

# Wilcoxon rank sum test with continuity correction
# 
# data:  REP_mod_by_season_year$REP[REP_mod_by_season_year$model == 4] and REP_CPC_by_season_year$REP
# W = 2462, p-value = 8.127e-08
# alternative hypothesis: true location shift is not equal to 0

line.width = 1
alpha.line = 1


bin_breaks = seq(-0.5,12.5,1)
bin_labels = as.character(bin_breaks-0.5)
hist_lims = data.frame(xlim = c(0.5,11.6),
                       ylim = c(0,40))
REP_mod_obs_compare_1 =
  ggplot() + 
  stat_bin(data = REP_mod_by_season_year[REP_mod_by_season_year$model == 1,],
           aes(x = REP, y =..count..), alpha = alpha.line,
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_mod_by_season_year[REP_mod_by_season_year$model == 4,],
           aes(x = REP, y =..count..), alpha = alpha.line,
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_CPC_by_season_year,
           aes(x = REP, y=..count..), 
           geom = "step", size = line.width,
           breaks = bin_breaks, col = "red") +
  annotate("text", label = c("a)"), x = 0.75, y = 38.75, size = 6, colour = "black") +
  xlab("# REP days") +
  coord_cartesian(xlim = hist_lims$xlim, ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()

REP_mod_obs_compare_2 =
  ggplot() + 
  stat_bin(data = REP_mod_CPC_thresh_by_season_year[REP_mod_CPC_thresh_by_season_year$model == 1,],
           aes(x = REP, y =..count..), alpha = alpha.line,
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_mod_CPC_thresh_by_season_year[REP_mod_CPC_thresh_by_season_year$model == 4,],
           aes(x = REP, y =..count..), alpha = alpha.line,
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = REP_CPC_by_season_year,
           aes(x = REP, y=..count..), 
           geom = "step", size = line.width,
           breaks = bin_breaks, col = "red") +
  annotate("text", label = c("c)"), x = 0.75, y = 38.75, size = 6, colour = "black") +
  xlab("# REP days") +
  coord_cartesian(xlim = hist_lims$xlim, ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()



# now check the REP lag 1 correlation by simulation -- start with CPC and GCM thresholds
CPC_mod_cell_REP_MAM = CPC_mod_cell_REP %>% dplyr::filter(season == "MAM")
mod_REP_MAM = mod_REP %>% dplyr::filter(season == "MAM")
REP_obs_mod = data.frame(date = c(CPC_mod_cell_REP_MAM$date,mod_REP_MAM$date[mod_REP_MAM$model %in% c(1,4)]),
                         REP = c(CPC_mod_cell_REP_MAM$REP,mod_REP_MAM$REP[mod_REP_MAM$model %in% c(1,4)]),
                         mod = c(rep('obs', length(CPC_mod_cell_REP_MAM$REP)),mod_REP_MAM$model[mod_REP_MAM$model %in% c(1,4)]))


REP_obs_mod_daily = REP_obs_mod %>% dplyr::group_by(mod) %>%
  dplyr::mutate(REP_lag1 = lag(REP, 1))

# create a three way contingency table for the simulations
REP_obs_mod_daily_table = as.data.frame(ftable(REP_obs_mod_daily %>% dplyr::select(REP, REP_lag1, mod)))

for(ii in 1:3){
  REP_persist0 = data.frame(set = REP_obs_mod_daily_table$mod[ii*4],
                            obs = ifelse(REP_obs_mod_daily_table$mod[ii*4] == "obs", "obs", "GCM"),
                            lag1_persist_prob = REP_obs_mod_daily_table$Freq[ii*4] / (REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4]),
                            lag1_persist_prob_norm = (REP_obs_mod_daily_table$Freq[ii*4] / (REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4]))/
                                                     ((REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4])/sum(REP_obs_mod_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){REP_persist = REP_persist0}
  if(ii > 1){REP_persist = rbind(REP_persist,REP_persist0)}
  
}
presist_prob_GCM_vs_obs = 
  ggplot(REP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = obs)) +
  theme_bw() +
  labs(y = expression(P(REP[t] ~"|" ~ REP[t-lag])/P(REP)),
       x = "") + 
  annotate("text", label = c("b)"), x = 0.75, y = 24, size = 6, colour = "black") +
  scale_y_continuous(limits = c(0,25))


# now check the REP lag 1 correlation by simulation -- now use the CPC thresholds
CPC_mod_cell_REP_MAM = CPC_mod_cell_REP %>% dplyr::filter(season == "MAM")
mod_REP_MAM = mod_CPC_thresh_REP %>% dplyr::filter(season == "MAM")
REP_obs_mod = data.frame(date = c(CPC_mod_cell_REP_MAM$date,mod_REP_MAM$date[mod_REP_MAM$model %in% c(1,4)]),
                         REP = c(CPC_mod_cell_REP_MAM$REP,mod_REP_MAM$REP[mod_REP_MAM$model %in% c(1,4)]),
                         mod = c(rep('obs', length(CPC_mod_cell_REP_MAM$REP)),mod_REP_MAM$model[mod_REP_MAM$model %in% c(1,4)]))


REP_obs_mod_daily = REP_obs_mod %>% dplyr::group_by(mod) %>%
  dplyr::mutate(REP_lag1 = lag(REP, 1))

# create a three way contingency table for the simulations
REP_obs_mod_daily_table = as.data.frame(ftable(REP_obs_mod_daily %>% dplyr::select(REP, REP_lag1, mod)))

for(ii in 1:3){
  REP_persist0 = data.frame(set = REP_obs_mod_daily_table$mod[ii*4],
                            obs = ifelse(REP_obs_mod_daily_table$mod[ii*4] == "obs", "obs", "GCM"),
                            lag1_persist_prob = REP_obs_mod_daily_table$Freq[ii*4] / (REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4]),
                            lag1_persist_prob_norm = (REP_obs_mod_daily_table$Freq[ii*4] / (REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4]))/
                              ((REP_obs_mod_daily_table$Freq[(ii-1)*4+2] + REP_obs_mod_daily_table$Freq[ii*4])/sum(REP_obs_mod_daily_table$Freq[((ii-1)*4 + 1):((ii-1)*4 + 4)])))
  if(ii == 1){REP_persist = REP_persist0}
  if(ii > 1){REP_persist = rbind(REP_persist,REP_persist0)}
  
}
presist_prob_GCM_vs_obs_CPC_thresh = 
  ggplot(REP_persist) +
  geom_boxplot(aes(y = lag1_persist_prob_norm, x = obs)) +
  theme_bw() +
  labs(y = expression(P(REP[t] ~"|" ~ REP[t-lag])/P(REP)),
       x = "") +
  annotate("text", label = c("d)"), x = 0.75, y = 24, size = 6, colour = "black") +
  scale_y_continuous(limits = c(0,25))

pdf('Final figures/Figure_3.pdf', width = 8.5, height = 5.5)
grid.arrange(REP_mod_obs_compare_1,
             REP_mod_obs_compare_2,
             presist_prob_GCM_vs_obs,
             presist_prob_GCM_vs_obs_CPC_thresh,
             nrow = 2,
             layout_matrix = rbind(c(1,1,3),c(2,2,4)))
dev.off()

rm(list = ls())