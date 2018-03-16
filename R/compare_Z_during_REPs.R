########################################
############### Figure S5 ###############
########################################

# now we compare the GCM Z_700 to the OBS Z_700 associated with REP days
source('R/GetSeasonDate.R')

# load xtrs from the CPC calcs
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')

REP_dates = CPC_mod_cell_REP %>% dplyr::filter(REP == 1) %>%
  dplyr::select(date) %>%
  data.frame()

load('Processed_Data/Shifted_reanalysis/Z_700.RData')

load('Processed_Data/Shifted_reanalysis/monthly_mean_Z.RData')
# calculate the mean value of relevant days 
REP_event = Z_700 %>% dplyr::filter(date %in% c(REP_dates$date)) %>%
  data.table() 

REP_event = REP_event %>% dplyr::mutate(month = month(date),
                                        season = GetSeasonDate(date))

# mark locations where over 80% of the observations are positive or negative
sig.level = 0.8
REP_event_anom = merge(REP_event, monthly_mean_Z, by = c("lat", "lon", "month")) %>%
  dplyr::mutate(Z_700_anom = Z_700 - Z_700_clim,
                Z_700_positive = ifelse(Z_700_anom > 0, 1, 0)) %>% 
  dplyr::group_by(lon,lat,season) %>%
  dplyr::summarise(Z_700_anom = mean(Z_700_anom),
                   Z_700 = mean(Z_700),
                   prop.pos = mean(Z_700_positive))

REP_event_anom_obs = REP_event_anom %>% dplyr::mutate(Z_700_sig = ifelse(Z_700_anom > 0 & prop.pos > sig.level,1,
                                                                         ifelse(Z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))

REP_event_anom_obs$model = "OBS & NCEP/NCAR"


# load xtrs from the mod runs
load(file = 'Processed_Data/mod_REP.RData')

# only include models 1 & 4
REP_dates_mod = mod_REP %>% dplyr::filter(REP == 1 &
                                          model %in% c(1,4)) %>%
                            dplyr::select(date, season, model) %>%
                            data.frame()

# now let's calculate anomalies and plot them for the ta_700
load(file = 'Processed_Data/Z_700_mod_field.RData')

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(season = GetSeasonDate(date))
colnames(Z_700_mod_field)[5] = "Z_700"

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(month = month(date),
                                                    model = ifelse(model == 4, "GCM ens 2" ,paste0("GCM ens ",model)))

monthly_mean_Z_mod = Z_700_mod_field %>% dplyr::group_by(lon,lat,month,model) %>%
  dplyr::summarise(Z_700_clim = mean(Z_700),
                   Z_700_sd = sd(Z_700))

save(monthly_mean_Z_mod, file = "Processed_Data/monthly_mean_Z_700_mod.RData")

REP_event_mod = Z_700_mod_field %>% dplyr::filter(date %in% c(REP_dates_mod$date)) %>%
  data.table()

# now exclude the models that did not have REPs on days when another model had a REP
REP_dates_mod$include = 1
REP_dates_mod = REP_dates_mod %>% dplyr::mutate(model = ifelse(model == 4, "GCM ens 2" ,paste0("GCM ens ",model)))

REP_event_mod_subset = merge(REP_event_mod,REP_dates_mod, by = c("date","model","season"), all.x = TRUE)

REP_event_mod_anom = merge(REP_event_mod_subset[!is.na(REP_event_mod_subset$include),], monthly_mean_Z_mod, by = c("lat", "lon", "month","model")) %>%
  dplyr::mutate(Z_700_anom = Z_700 - Z_700_clim,
                Z_700_positive = ifelse(Z_700_anom > 0, 1, 0)) %>% 
  dplyr::group_by(lon,lat,season,model) %>%
  dplyr::summarise(Z_700_anom = mean(Z_700_anom),
                   Z_700 = mean(Z_700),
                   prop.pos = mean(Z_700_positive))

sig.level = 0.8
REP_event_mod_anom = REP_event_mod_anom %>% dplyr::mutate(Z_700_sig = ifelse(Z_700_anom > 0 & prop.pos > sig.level,1,
                                                                             ifelse(Z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))
REP_event_anom_all = rbind(REP_event_mod_anom, REP_event_anom_obs)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

# define a blank list of plots
plots = list()

for(mmodel in 1:length(unique(REP_event_anom_all$model))){
  subset0 = REP_event_anom_all[REP_event_anom_all$model == unique(REP_event_anom_all$model)[mmodel],]
  subset = subset0[subset0$season == "MAM",]
  
  plots[[mmodel]] =
    ggplot(data = subset) + 
    geom_tile(aes(x = (lon-360),y = lat,fill=(Z_700_anom))) + 
    scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                         limits = c(-85,85),
                         midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey",
                         guide = FALSE) +
    geom_point(data = subset[subset$Z_700_sig == 1, ], 
               aes(x = (lon-360),y = lat),shape = "X", size = 1.5, alpha = 0.6) +
    stat_contour(aes(x = (lon-360),y = lat, z=Z_700), 
                 breaks = seq(2700,3300,50), size=0.35, col = "black") +
    stat_contour(aes(x = (lon-360),y = lat, z=Z_700),
                 breaks = 3000, size=0.85, col = "black") +
    geom_path(data=world, aes(x,y), size = 0.25) + 
    scale_y_continuous(limits = c(10,60)) +
    labs(x = "Longitude", y = "Latitude") +
    scale_x_continuous(limits = c(-170,-45), breaks = c(-140, -100, -60)) +
    theme_bw() +
    geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
    ggtitle(unique(REP_event_anom_all$model)[mmodel])
  
}


# Function to extract legend (from: http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram)
source('R/g_legend.R')

plot_legend <- g_legend(ggplot(data = subset) + 
                          geom_tile(aes(x = (lon-360),y = lat,fill=(Z_700_anom))) + 
                          scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                                               limits = c(-85,85),
                                               midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey"))

# define the layout of the plots
lay <- rbind(c(1,1,2,2,3,3,4))

pdf("Final figures/Figure_S5.pdf", width=12.5, height=2.5)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plot_legend,layout_matrix = lay)
dev.off()

rm(list = ls())