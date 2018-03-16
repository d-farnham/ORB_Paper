########################################
############### Figure 4 ###############
########################################
source('R/GetSeasonDate.R')
# now let's calculate anomalies and plot them
load('Processed_Data/Shifted_reanalysis/SHUM.RData')
load('Processed_Data/Shifted_reanalysis/Z_700.RData')


# compute and save the monthly means
monthly_mean_Z = Z_700 %>% dplyr::mutate(month = month(date)) %>%
  dplyr::group_by(lon,lat,month) %>%
  dplyr::summarise(Z_700_clim = mean(Z_700))

save(monthly_mean_Z, file = "Processed_Data/Shifted_reanalysis/monthly_mean_Z.RData")

monthly_mean_SHUM = SHUM %>% dplyr::mutate(month = month(date)) %>%
  dplyr::group_by(lon,lat,level,month) %>%
  dplyr::summarise(SHUM_clim = mean(SHUM))

save(monthly_mean_SHUM, file = "Processed_Data/Shifted_reanalysis/monthly_mean_SHUM.RData")

source('R/GetSeasonDate.R')

# load xtrs from the CPC calcs -- only inlcude the MAM events
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')

REP_dates = CPC_mod_cell_REP %>% data.frame() %>% dplyr::filter(REP == 1 &
                                                                season == "MAM" &
                                                                date > "1949-12-31" &
                                                                date < "2006-01-01") %>%
                                 dplyr::select(date) 

subset.params = list(dates = REP_dates$date,
                     lags = c(-4,-3,-2,-1,0,1)) # how many days prior to the event do you want?


# calculate the mean value of Z relevant days 
REP_event_Z = Z_700 %>% dplyr::filter(date %in% c(subset.params$dates + days(subset.params$lags[1]))) %>%
  data.table() %>%
  dplyr::mutate(lag = subset.params$lags[1])


for(llags in 2:length(subset.params$lags)){
  REP_event_Z0 = Z_700 %>% dplyr::filter(date %in% c(REP_dates$date + days(subset.params$lags[llags]))) %>%
    data.table() %>%
    dplyr::mutate(lag = subset.params$lags[llags])
  REP_event_Z = rbind(REP_event_Z,REP_event_Z0)
  print(llags)
}

sig.level = 0.8

REP_event_Z = REP_event_Z %>% dplyr::mutate(month = month(date))

REP_event_Z_anom = merge(REP_event_Z, monthly_mean_Z, by = c("lat", "lon", "month")) %>%
  dplyr::mutate(Z_700_anom = Z_700 - Z_700_clim,
                Z_700_positive = ifelse(Z_700_anom > 0, 1, 0)) %>% 
  dplyr::group_by(lon,lat,lag) %>%
  dplyr::summarise(Z_700_anom = mean(Z_700_anom),
                   prop.pos = mean(Z_700_positive)) %>% 
  dplyr::mutate(Z_700_sig = ifelse(Z_700_anom > 0 & prop.pos > sig.level,1,
                            ifelse(Z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))



# calculate the mean value of SHUM relevant days 
REP_event_SHUM = SHUM %>% dplyr::filter(date %in% c(subset.params$dates + days(subset.params$lags[1]))) %>%
  data.table() %>%
  dplyr::mutate(lag = subset.params$lags[1])


for(llags in 2:length(subset.params$lags)){
  REP_event_SHUM0 = SHUM %>% dplyr::filter(date %in% c(REP_dates$date + days(subset.params$lags[llags]))) %>%
    data.table() %>%
    dplyr::mutate(lag = subset.params$lags[llags])
  REP_event_SHUM = rbind(REP_event_SHUM,REP_event_SHUM0)
  print(llags)
}

REP_event_SHUM = REP_event_SHUM %>% dplyr::mutate(month = month(date))

REP_event_SHUM_anom = merge(REP_event_SHUM, monthly_mean_SHUM, by = c("lat", "lon", "level", "month")) %>%
  dplyr::mutate(SHUM_anom = SHUM - SHUM_clim) %>% 
  dplyr::group_by(lon,lat,level,lag) %>%
  dplyr::summarise(SHUM_anom = mean(SHUM_anom))


state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")


Z_700.lim = max(-floor(min(REP_event_Z_anom$Z_700_anom)/2)*2,ceiling(max(REP_event_Z_anom$Z_700_anom)/2)*2)


pdf(paste0("Final figures/Figure_4.pdf"), width=10, height=4)
plot(
ggplot(data = REP_event_Z_anom) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(Z_700_anom))) + 
  facet_wrap(~lag, ncol = 3) +
  scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                       limits = c(-Z_700.lim,Z_700.lim), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_point(data = REP_event_Z_anom[REP_event_Z_anom$Z_700_sig == 1, ], 
             aes(x = (lon-360),y = lat),shape = "X", size = 1) +
  stat_contour(data = REP_event_SHUM_anom %>% dplyr::filter(level == 700),
               aes(x = (lon-360),y = lat, z = SHUM_anom),
               breaks = seq(4,20,4)/10000, size=0.35, col = "black") +
  stat_contour(data = REP_event_SHUM_anom %>% dplyr::filter(level == 700),
               aes(x = (lon-360),y = lat, z = SHUM_anom),
               breaks = seq(-4,-20,-4)/10000, size=0.35, col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y), size = 0.25) + 
  scale_y_continuous(limits = c(10,60)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-170,-45), breaks = c(-140, -100, -60)) +
  theme_bw() +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points)
)
dev.off()

rm(list = ls())