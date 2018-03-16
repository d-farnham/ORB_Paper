########################################
############### Figure 5 ###############
########################################

# now we compare the GCM Z_700 to the OBS Z_700 associated with REP days
source('R/GetSeasonDate.R')

# load xtrs from the CPC calcs
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')

REP_dates = CPC_mod_cell_REP %>% dplyr::filter(REP == 1) %>%
  dplyr::select(date) %>%
  data.frame()

load('Processed_Data/Shifted_reanalysis/Z_700.RData')
load('Processed_Data/Shifted_reanalysis/SHUM.RData')

load('Processed_Data/Shifted_reanalysis/monthly_mean_Z.RData')
load('Processed_Data/Shifted_reanalysis/monthly_mean_SHUM.RData')

SHUM = SHUM %>% dplyr::filter(level == 700) %>%
                dplyr::ungroup() %>%
                dplyr::select(-level)

monthly_mean_SHUM = monthly_mean_SHUM %>% dplyr::filter(level == 700) %>%
                                          dplyr::ungroup() %>%
                                          dplyr::select(-level)

# calculate the mean value of relevant days 

Z_REP_event = Z_700 %>% dplyr::filter(date %in% c(REP_dates$date)) %>%
  data.table() 

Z_REP_event = Z_REP_event %>% dplyr::mutate(month = month(date),
                                            season = GetSeasonDate(date))

# mark locations where over 80% of the observations are positive or negative
sig.level = 0.8
Z_REP_event_anom = merge(Z_REP_event, monthly_mean_Z, by = c("lat", "lon", "month")) %>%
  dplyr::mutate(Z_700_anom = Z_700 - Z_700_clim,
                Z_700_positive = ifelse(Z_700_anom > 0, 1, 0)) %>% 
  dplyr::group_by(lon,lat,season) %>%
  dplyr::summarise(Z_700_anom = mean(Z_700_anom),
                   Z_700 = mean(Z_700),
                   prop.pos = mean(Z_700_positive))

Z_REP_event_anom_obs = Z_REP_event_anom %>% dplyr::mutate(Z_700_sig = ifelse(Z_700_anom > 0 & prop.pos > sig.level,1,
                                                                         ifelse(Z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))

Z_REP_event_anom_obs$model = "OBS & NCEP/NCAR"


Q_REP_event = SHUM %>% dplyr::filter(date %in% c(REP_dates$date)) %>%
  data.table() 

Q_REP_event = Q_REP_event %>% dplyr::mutate(month = month(date),
                                            season = GetSeasonDate(date))


Q_REP_event_anom = merge(Q_REP_event, monthly_mean_SHUM, by = c("lat", "lon", "month")) %>%
  dplyr::mutate(Q_700_anom = SHUM - SHUM_clim) %>% 
  dplyr::group_by(lon,lat,season) %>%
  dplyr::summarise(Q_700_anom = mean(Q_700_anom),
                   Q_700 = mean(SHUM))

Q_REP_event_anom_obs = Q_REP_event_anom
Q_REP_event_anom_obs$model = "OBS & NCEP/NCAR"




# load xtrs from the mod runs
load(file = 'Processed_Data/mod_REP.RData')

# only include models 1 & 4
REP_dates_mod = mod_REP %>% dplyr::filter(REP == 1 &
                                            model %in% c(1,4)) %>%
  dplyr::select(date, season, model) %>%
  data.frame()

# now let's calculate anomalies and plot them for the Z_700
load(file = 'Processed_Data/Z_700_mod_field.RData')

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(season = GetSeasonDate(date))
colnames(Z_700_mod_field)[5] = "Z_700"

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(month = month(date),
                                                    model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))

monthly_mean_Z_mod = Z_700_mod_field %>% dplyr::group_by(lon,lat,month,model) %>%
  dplyr::summarise(Z_700_clim = mean(Z_700),
                   Z_700_sd = sd(Z_700))

save(monthly_mean_Z_mod, file = "Processed_Data/monthly_mean_Z_700_mod.RData")

Z_REP_event_mod = Z_700_mod_field %>% dplyr::filter(date %in% c(REP_dates_mod$date)) %>%
  data.table()

# now exclude the models that did not have REPs on days when another model had a REP
Z_REP_dates_mod = REP_dates_mod %>% dplyr::mutate(model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))
Z_REP_dates_mod$include = 1

Z_REP_event_mod_subset = merge(Z_REP_event_mod,Z_REP_dates_mod, by = c("date","model","season"), all.x = TRUE)

Z_REP_event_mod_anom = merge(Z_REP_event_mod_subset[!is.na(Z_REP_event_mod_subset$include),], monthly_mean_Z_mod, by = c("lat", "lon", "month","model")) %>%
  dplyr::mutate(Z_700_anom = Z_700 - Z_700_clim,
                Z_700_positive = ifelse(Z_700_anom > 0, 1, 0)) %>% 
  dplyr::group_by(lon,lat,season,model) %>%
  dplyr::summarise(Z_700_anom = mean(Z_700_anom),
                   Z_700 = mean(Z_700),
                   prop.pos = mean(Z_700_positive))

sig.level = 0.8
Z_REP_event_mod_anom = Z_REP_event_mod_anom %>% dplyr::mutate(Z_700_sig = ifelse(Z_700_anom > 0 & prop.pos > sig.level,1,
                                                                             ifelse(Z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))


# now let's calculate anomalies and plot them for the Q_700
load(file = 'Processed_Data/SHUM_700_mod_field.RData')

Q_700_mod_field = SHUM_700_mod_field %>% dplyr::mutate(season = GetSeasonDate(date))
colnames(Q_700_mod_field)[5] = "Q_700"

Q_700_mod_field = Q_700_mod_field %>% dplyr::mutate(month = month(date),
                                                    model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))

monthly_mean_Q_mod = Q_700_mod_field %>% dplyr::group_by(lon,lat,month,model) %>%
  dplyr::summarise(Q_700_clim = mean(Q_700),
                   Q_700_sd = sd(Q_700))

save(monthly_mean_Q_mod, file = "Processed_Data/monthly_mean_Q_700_mod.RData")

Q_REP_event_mod = Q_700_mod_field %>% dplyr::filter(date %in% c(REP_dates_mod$date)) %>%
  data.table()

# now exclude the models that did not have REPs on days when another model had a REP
Q_REP_dates_mod = REP_dates_mod %>% dplyr::mutate(model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))
Q_REP_dates_mod$include = 1

Q_REP_event_mod_subset = merge(Q_REP_event_mod,Q_REP_dates_mod, by = c("date","model","season"), all.x = TRUE)

Q_REP_event_mod_anom = merge(Q_REP_event_mod_subset[!is.na(Q_REP_event_mod_subset$include),], monthly_mean_Q_mod, by = c("lat", "lon", "month","model")) %>%
  dplyr::mutate(Q_700_anom = Q_700 - Q_700_clim) %>% 
  dplyr::group_by(lon,lat,season,model) %>%
  dplyr::summarise(Q_700_anom = mean(Q_700_anom),
                   Q_700 = mean(Q_700))


# combine the GCM and obs into single data frames
Z_REP_event_anom_all = rbind(Z_REP_event_mod_anom, Z_REP_event_anom_obs)
Q_REP_event_anom_all = rbind(Q_REP_event_mod_anom, Q_REP_event_anom_obs)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

# define a blank list of plots
plots = list()

for(mmodel in 1:length(unique(Z_REP_event_anom_all$model))){
  Z_subset0 = Z_REP_event_anom_all[Z_REP_event_anom_all$model == unique(Z_REP_event_anom_all$model)[mmodel],]
  Z_subset = Z_subset0[Z_subset0$season == "MAM",]
  
  Q_subset0 = Q_REP_event_anom_all[Q_REP_event_anom_all$model == unique(Q_REP_event_anom_all$model)[mmodel],]
  Q_subset = Q_subset0[Q_subset0$season == "MAM",]
  
  plots[[mmodel]] =
    ggplot() + 
    geom_tile(data = Z_subset, aes(x = (lon-360),y = lat,fill=(Z_700_anom))) + 
    scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                         limits = c(-75,75),
                         midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey",
                         guide = FALSE) +
    geom_point(data = Z_subset[Z_subset$Z_700_sig == 1, ], 
               aes(x = (lon-360),y = lat),shape = "X", size = 1.5, alpha = 0.6) +
    stat_contour(data = Q_subset, aes(x = (lon-360),y = lat, z=Q_700_anom), 
                 breaks = seq(4,20,4)/10000, size=0.35, col = "black") +
    stat_contour(data = Q_subset, aes(x = (lon-360),y = lat, z=Q_700_anom),
                 breaks = seq(-4,-20,-4)/10000, size=0.35, col = "black", linetype = "dashed") +
    geom_path(data=world, aes(x,y), size = 0.25) + 
    scale_y_continuous(limits = c(10,60)) +
    labs(x = "Longitude", y = "Latitude") +
    scale_x_continuous(limits = c(-170,-45), breaks = c(-140, -100, -60)) +
    theme_bw() +
    geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
    ggtitle(unique(Z_REP_event_anom_all$model)[mmodel])

}


# Function to extract legend (from: http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram)
source('R/g_legend.R')

plot_legend <- g_legend(ggplot() + 
                          geom_tile(data = Z_subset, aes(x = (lon-360),y = lat,fill=(Z_700_anom))) + 
                          scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                                               limits = c(-75,75),
                                               midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey"))

# define the layout of the plots
lay <- rbind(c(1,1,2,2,3,3,4))

pdf("Final figures/Figure_5.pdf", width=12.5, height=2.5)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plot_legend,layout_matrix = lay)
dev.off()

rm(list = ls())