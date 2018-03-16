########################################
############### Figure 7 ##############
########################################
source("R/GetSeasonDate.R")

load(file = 'Processed_Data/CPC_mod_cell_REP.RData')
load(file = 'Processed_Data/Shifted_reanalysis/EWD_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/M_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/O_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/NPH_index.RData')

# combine the reanalysis record and add model = "obs"
REP_EWD_M_O = merge(merge(merge(merge(CPC_mod_cell_REP,EWD_index, by = c("date")), 
                                M_index %>% dplyr::filter(level == 700) %>% dplyr::select(-level), by = c("date")), 
                                O_index %>% dplyr::filter(level == 700) %>% dplyr::select(-level), by = c("date")),
                                NPH_index %>% dplyr::select(-Z_700), by = c("date")) %>%
  dplyr::mutate(model = "obs")


# do the same for the GCM runs
load(file = 'Processed_Data/mod_REP.RData')
load(file = 'Processed_Data/Shifted_reanalysis/EWD_index_mod.RData')
load(file = 'Processed_Data/Shifted_reanalysis/M_index_mod.RData')
load(file = 'Processed_Data/Shifted_reanalysis/O_index_mod.RData')
load(file = 'Processed_Data/Shifted_reanalysis/NPH_index_mod.RData')

# combine the reanalysis record and add model = "obs"
REP_EWD_M_O_mod = merge(merge(merge(merge(mod_REP,EWD_index_mod, by = c("date","model")), 
                                M_index_mod, by = c("date","model")), 
                                O_index_mod, by = c("date","model")),
                                NPH_index_mod %>% dplyr::select(-Z_700), by = c("date","model"))


REP_EWD_M_O_all = rbind(REP_EWD_M_O_mod, REP_EWD_M_O) %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5)) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(M_index_delta = M_index - lead(M_index, 1),
                O_index_anom = (O_index - mean(O_index, na.rm = TRUE))/sd(O_index, na.rm = TRUE),
                M_index_anom = (M_index - mean(M_index, na.rm = TRUE))/sd(M_index, na.rm = TRUE),
                EWD_index_anom = (EWD_index - mean(EWD_index, na.rm = TRUE))/sd(EWD_index, na.rm = TRUE),
                east_index_anom = (east_Z - mean(east_Z, na.rm = TRUE))/sd(east_Z, na.rm = TRUE),
                west_index_anom = (west_Z - mean(west_Z, na.rm = TRUE))/sd(west_Z, na.rm = TRUE),
                NPH_index_anom = (NPH_index - mean(NPH_index, na.rm = TRUE))/sd(NPH_index, na.rm = TRUE)) %>%
  dplyr::mutate(EWD.positive = ifelse(EWD_index > 0, 1, 0),
                O.negative = ifelse(O_index < 0, 1, 0),
                REP_lag1 = lag(REP, 1)) %>%
  dplyr::select(date, model, REP, EWD_index_anom, east_index_anom, west_index_anom, O_index_anom, M_index_anom, NPH_index_anom, REP_lag1, EWD.positive, O.negative)

# let's save this combined index file
save(REP_EWD_M_O_all, file = 'Processed_Data/Shifted_reanalysis/REP_EWD_M_O_all.RData')


# compute and store the mean and sd of the historic indices
REP_EWD_M_O_clims = rbind(REP_EWD_M_O_mod, REP_EWD_M_O) %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5)) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(O_index_mean = mean(O_index, na.rm = TRUE), O_index_sd = sd(O_index, na.rm = TRUE),
                   M_index_mean = mean(M_index, na.rm = TRUE), M_index_sd = sd(M_index, na.rm = TRUE),
                   EWD_index_mean = mean(EWD_index, na.rm = TRUE), EWD_index_sd = sd(EWD_index, na.rm = TRUE),
                   east_index_mean = mean(east_Z, na.rm = TRUE), east_index_sd = sd(east_Z, na.rm = TRUE),
                   west_index_mean = mean(west_Z, na.rm = TRUE), west_index_sd = sd(west_Z, na.rm = TRUE),
                   NPH_index_mean = mean(NPH_index, na.rm = TRUE), NPH_index_sd = sd(NPH_index, na.rm = TRUE))

# let's save this combined index file
save(REP_EWD_M_O_clims, file = 'Processed_Data/Shifted_reanalysis/REP_EWD_M_O_clims.RData')

# now let's plot the CDFs -- Figure 7


REP_EWD_M_O_all_long = melt(REP_EWD_M_O_all %>% dplyr::select(-date, -REP, -EWD_index_anom, 
                                                              -REP_lag1, -O.negative, -EWD.positive), id.vars = c("model")) %>%
  # let's define OMG and Z[L] by their negative so that positive is associated with REPs
                                                dplyr::mutate(value == ifelse(variable %in% c('west_index_anom','O_index_anom'), -value, value)) %>%
                                                dplyr::mutate(var.lab = ifelse(variable == 'east_index_anom', 'Z[H]',
                                                                               ifelse(variable == 'west_index_anom', '-Z[L]',
                                                                                      ifelse(variable == 'O_index_anom', '-OMG',
                                                                                             ifelse(variable == 'M_index_anom', 'HUM', 'Z[P]')))))

# M-W Test to compare the HUM index
wilcox.test(x = REP_EWD_M_O_all$M_index_anom[REP_EWD_M_O_all$model == 1],
        y = REP_EWD_M_O_all$M_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13237000, p-value = 0.8171
wilcox.test(x = REP_EWD_M_O_all$M_index_anom[REP_EWD_M_O_all$model == 4],
        y = REP_EWD_M_O_all$M_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13248000, p-value = 0.8766

# M-W Test to compare the OMG index
wilcox.test(x = REP_EWD_M_O_all$O_index_anom[REP_EWD_M_O_all$model == 1],
        y = REP_EWD_M_O_all$O_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13518000, p-value = 0.1027
wilcox.test(x = REP_EWD_M_O_all$O_index_anom[REP_EWD_M_O_all$model == 4],
        y = REP_EWD_M_O_all$O_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13523000, p-value = 0.09644

# M-W Test to compare the Z_P index
wilcox.test(x = REP_EWD_M_O_all$NPH_index_anom[REP_EWD_M_O_all$model == 1],
        y = REP_EWD_M_O_all$NPH_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13416000, p-value = 0.3125
wilcox.test(x = REP_EWD_M_O_all$NPH_index_anom[REP_EWD_M_O_all$model == 4],
        y = REP_EWD_M_O_all$NPH_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13374000, p-value = 0.464

# M-W Test to compare the Z_L index
wilcox.test(x = REP_EWD_M_O_all$west_index_anom[REP_EWD_M_O_all$model == 1],
        y = REP_EWD_M_O_all$west_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13289000, p-value = 0.9085
wilcox.test(x = REP_EWD_M_O_all$west_index_anom[REP_EWD_M_O_all$model == 4],
        y = REP_EWD_M_O_all$west_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13246000, p-value = 0.8653

# M-W Test to compare the Z_H index
wilcox.test(x = REP_EWD_M_O_all$east_index_anom[REP_EWD_M_O_all$model == 1],
        y = REP_EWD_M_O_all$east_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13235000, p-value = 0.8086
wilcox.test(x = REP_EWD_M_O_all$east_index_anom[REP_EWD_M_O_all$model == 4],
        y = REP_EWD_M_O_all$east_index_anom[REP_EWD_M_O_all$model == "obs"])
# W = 13195000, p-value = 0.6111

cdfs_plot = 
ggplot() + 
         stat_ecdf(data = REP_EWD_M_O_all_long[REP_EWD_M_O_all_long$model != "obs",], 
                   aes(x = value, group = model), linetype = "dashed") +
         stat_ecdf(data = REP_EWD_M_O_all_long[REP_EWD_M_O_all_long$model == "obs",], 
                   aes(x = value)) +
         theme(legend.position="none") +
         labs(x = "Index",
              y = "F(Index)") +
         coord_cartesian(xlim = c(-3,3)) +
         theme_bw() +
  facet_wrap(~var.lab,
             labeller = label_parsed,
             nrow = 1)







# now need to compute the indices persistence -- just look at acfs
num.preds = 5
num.lags = 7
pred_acfs = data.frame(mod = rep(c(1,4,"obs"), each = num.lags * num.preds),
                       pred = rep(c("east_index_anom", "west_index_anom", "O_index_anom", "M_index_anom", "NPH_index_anom"),num.lags * 3),
                       pred_lab = rep(c('Z[H]', '-Z[L]', '-OMG', 'HUM', 'Z[P]'),num.lags * 3),
                       lag = rep(1:num.lags, each = num.preds),
                       cor = NA)
REP_EWD_M_O_all_long = melt(REP_EWD_M_O_all, id.vars = c('date', 'model','REP','REP_lag1'))
REP_EWD_M_O_all_long_neg_OW = REP_EWD_M_O_all_long %>% dplyr::mutate(value = ifelse(variable %in% c("west_index_anom","O_index_anom"), -value, value))

for(ii in 1:nrow(pred_acfs)){
  subset = REP_EWD_M_O_all_long_neg_OW %>% dplyr::ungroup() %>% 
    dplyr::filter(model == pred_acfs$mod[ii] &
                    variable == as.character(pred_acfs$pred[ii])) %>% 
    dplyr::select(value)
    
  pred_acfs$cor[ii] =  as.numeric(acf(subset, na.action = na.pass, plot = FALSE)[pred_acfs$lag[ii]])
  
}

acf_plot = 
ggplot() +
  geom_line(data = pred_acfs %>% dplyr::filter(mod == "obs"), aes(x = lag, y = cor, group = mod)) +
  geom_line(data = pred_acfs %>% dplyr::filter(mod != "obs"), aes(x = lag, y = cor, group = mod), linetype = "dashed") +
  facet_wrap(~pred_lab,
             labeller = label_parsed,
             nrow = 1) +
  theme_bw() +
  scale_x_continuous(breaks = c(1:7)) +
  labs(y = expression(Cor(Index[t] ~ ',' ~ Index[t-lag]))) +
  geom_hline(yintercept = 0, linetype = "twodash")


# now make the tail dependence plots

# load function to calculate tail dependence coefficient (tdc)
source('R/tdc.R')
lag.max = 7

# initialize lists to store the tdc for each lag
pred_tail_dep = data.frame(mod = rep(c(1,4,"obs"), each = num.lags * num.preds),
                           pred = rep(c("east_index_anom", "west_index_anom", "O_index_anom", "M_index_anom", "NPH_index_anom"),num.lags * 3),
                           pred_lab = rep(c('Z[H]', '-Z[L]', '-OMG', 'HUM', 'Z[P]'),num.lags * 3),
                           lag = rep(1:num.lags, each = num.preds),
                           tail_dep = NA)

for(ii in 1:nrow(pred_tail_dep)){
  subset = REP_EWD_M_O_all_long_neg_OW %>% dplyr::ungroup() %>% 
                                    dplyr::filter(model == pred_tail_dep$mod[ii] &
                                                  variable == as.character(pred_tail_dep$pred[ii])) %>% 
                                    dplyr::select(value) %>%
                                    dplyr::mutate(value_lag = dplyr::lag(value, (pred_tail_dep$lag[ii])))
    
  # get rid of the NAs
  subset = subset %>% dplyr::filter(!is.na(value) &
                                      !is.na(value_lag))
  
  pred_tail_dep$tail_dep[ii] =  tdc(subset$value,
                                    subset$value_lag,
                                    p = 0.9,
                                    upper = TRUE)
}

tail_dep_plot = 
  ggplot() +
  geom_line(data = pred_tail_dep %>% dplyr::filter(mod == "obs"), aes(x = lag, y = tail_dep, group = mod)) +
  geom_line(data = pred_tail_dep %>% dplyr::filter(mod != "obs"), aes(x = lag, y = tail_dep, group = mod), linetype = "dashed") +
  facet_wrap(~pred_lab,
             labeller = label_parsed,
             nrow = 1) +
  theme_bw() +
  scale_x_continuous(breaks = c(1:7)) +
  labs(y = expression(Pr(F(Index[t])~ ">" ~ 0.9 ~ "|" ~ F(Index[t-lag]) ~ ">" ~ 0.9))) +
  geom_hline(yintercept = 0, linetype = "twodash")


pdf('Final figures/Figure_7.pdf', width = 12, height = 8.5)
grid.arrange(cdfs_plot,
             acf_plot,
             tail_dep_plot,
             nrow = 3)
dev.off()

rm(list = ls())
