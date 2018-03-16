########################################
############### Figure S2 ##############
########################################
source("R/GetSeasonDate.R")

# load the REP records
load(file = 'Processed_Data/mod_REP.RData')
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')
load(file = 'Processed_Data/mod_CPC_thresh_REP.RData')

# compute the number of REPs by month for the OBS and MOD records
MOD_REP = mod_REP %>% dplyr::filter(model %in% c(1,4)) %>% dplyr::mutate(model = ifelse(model == 4, "GCM ens 2",paste0("GCM ens ",model))) %>% data.frame()
OBS_REP = CPC_mod_cell_REP %>% dplyr::mutate(model = "OBS") %>% data.frame()

# now merge these records, count the number of REPs during each month 
# and compute the % of REPs that occur within each month for each model
REP_MOD_OBS_counts = rbind(MOD_REP, OBS_REP) %>% dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::group_by(model, month) %>%
  dplyr::summarise(REPs = sum(REP, na.rm = TRUE)) %>% dplyr::group_by(model) %>%
  dplyr::mutate(percent_REPs = REPs/sum(REPs))

pdf('Final figures/Figure_S2.pdf', height = 2, width = 8)
plot(
ggplot(REP_MOD_OBS_counts) +
  geom_bar(aes(month, percent_REPs*100), stat = "identity") +
  facet_wrap(~model) +
  labs(x = "month", y = "% of REPs") +
  scale_x_continuous(breaks = 1:12) +
  theme_bw())
dev.off()


########################################
############### Figure S1 ##############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")

# load the records of cell-based intense precipitation for the CPC and GCM
load(file = 'Processed_Data/CPC_mod_cell_EP.RData')
load(file = 'Processed_Data/mod_EP.RData')

# now only consider Dec 1950 through Nov 2005
mod_EP = mod_EP %>% dplyr::filter(model %in% c(1,4)) %>% 
  data.frame() %>%
  dplyr::filter(date > '1950-11-30' &
                date < '2005-12-01') %>%
  dplyr::mutate(model_rename = ifelse(model == 4, "GCM ens 2",paste0("GCM ens ",model)))

CPC_mod_cell_EP = CPC_mod_cell_EP %>% data.frame() %>%
  dplyr::filter(date > '1950-11-30' &
                  date < '2005-12-01') %>%
  dplyr::mutate(model_rename = "OBS")
# now merge the OBS and MOD EPS
CPC_GCM_EP = rbind(CPC_mod_cell_EP, mod_EP[,c("date", "EP", "season", "model_rename")]) %>%
  dplyr::mutate(REP = ifelse(EP > 3,"REP","no REP")) %>%
  dplyr::filter(!is.na(EP) & EP > 0)

# calculate the number of EPs by season and model_rename 
num_CPC_GCM_EP_season = CPC_GCM_EP %>% dplyr::group_by(season, model_rename, REP) %>%
  dplyr::summarise(num_EP = length(EP)) %>%
  dplyr::mutate(prop_EP = round(num_EP/sum(num_EP),3),
                lab = paste0(REP, ": ",num_EP," (",prop_EP*100,"%*)")) %>%
  dplyr::group_by(season, model_rename) %>%
  dplyr::mutate(num_EP_all = sum(num_EP)) %>%
  dplyr::group_by(model_rename) %>%
  dplyr::mutate(prop_EP_all = round(num_EP_all/(sum(num_EP_all)/2),3), # need to divide by 2 b/c there are two rows ('REP' & 'no REP')
                lab_all = paste0("all: ",num_EP_all," (",prop_EP_all*100,"%**)")) # that each contain a 'num_EP_all'


# calculate the mean for EP for each model_rename and season
mean_CPC_GCM_EP_season = CPC_GCM_EP %>% dplyr::group_by(season, model_rename) %>%
  dplyr::summarise(mean_EP = mean(EP)) 

# below is a figure that is only for our reference
pdf('Final figures/Figure_R1.pdf', height = 5, width = 10)
plot(
ggplot(CPC_GCM_EP) +
  geom_bar(aes(EP, fill = factor(REP))) +
  scale_fill_manual(values = c("black","red"), name = "") +
  geom_vline(data = mean_CPC_GCM_EP_season, aes(xintercept = mean_EP), 
             linetype = "dashed") +
  geom_text(data = num_CPC_GCM_EP_season[num_CPC_GCM_EP_season$REP == "no REP",],
            aes(x = 12, y = 225, label = lab), hjust = "right") +
  geom_text(data = num_CPC_GCM_EP_season[num_CPC_GCM_EP_season$REP == "REP",],
            aes(x = 12, y = 175, label = lab), col = "red", hjust = "right") +
  geom_text(data = num_CPC_GCM_EP_season[num_CPC_GCM_EP_season$REP == "REP",],
            aes(x = 12, y = 125, label = lab_all), col = "black", fontface = "bold", hjust = "right") +
  facet_grid(model_rename ~ season) +
  labs(x = "# of cell-based intense precipitation events on a given day",
       y = "count",
       subtitle = "* = the percent of days in each season that were/were not REP days given that at least one cell was > it's 99th percentile during that day \n ** = the percent of days when at least one cell was > it's 99th percentile that occurred during that season") +
  scale_x_continuous(breaks = seq(1,12,by = 2)))
dev.off()


OBS_EP_counts = CPC_GCM_EP %>% dplyr::mutate(EP_factor = as.factor(EP)) %>%
  dplyr::filter(model_rename == "OBS") %>%
  dplyr::group_by(season,EP_factor,REP) %>%
  dplyr::summarise(OBS_count = length(date))

MOD_EP_counts = CPC_GCM_EP %>% dplyr::mutate(EP_factor = as.factor(EP)) %>%
  dplyr::filter(model_rename != "OBS") %>%
  dplyr::group_by(season,EP_factor,REP,model_rename) %>%
  dplyr::summarise(ENS_MEAN_count = length(date))


EP_counts = merge(OBS_EP_counts, MOD_EP_counts, by = c("season", "EP_factor", "REP")) %>%
  dplyr::mutate(`OBS - ENSEMBLE MEAN` = OBS_count - ENS_MEAN_count)


pdf('Final figures/Figure_S1.pdf', height = 3, width = 11)
plot(
ggplot(EP_counts) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 3.5, linetype = "dashed", alpha = 0.5) +
  geom_point(aes(x = as.numeric(EP_factor), y = `OBS - ENSEMBLE MEAN`, col = factor(REP)), stat = "identity") +
  scale_color_manual(values = c("black","red"), name = "") +
  facet_wrap(~season, nrow = 1) +
  labs(x = "# of local intense precipitation events on a given day",
       y = "OBS count - GCM count") +
  scale_x_continuous(breaks = seq(1,12,by = 2)) +
  theme_bw())
dev.off()


########################################
############### Figure S3 ##############
########################################
rm(list = ls())
load(file = 'Processed_Data/pr_CPC_EP_US.RData')
load(file = 'Processed_Data/pr_mod_EP_US.RData')

pr_mod_EP = pr_mod_EP %>% dplyr::filter(model %in% c("GCM 1", "GCM 4")) %>%
                          dplyr::mutate(model = ifelse(model == "GCM 4", "GCM ens 2", "GCM ens 1"))

pr_mod_EP$lat = as.numeric(as.character(pr_mod_EP$lat))
pr_mod_EP$lon = as.numeric(as.character(pr_mod_EP$lon))
pr_CPC_EP$lat = as.numeric(as.character(pr_CPC_EP$lat))
pr_CPC_EP$lon = as.numeric(as.character(pr_CPC_EP$lon))

pr_mod_obs_EP = merge(rbind(pr_mod_EP, pr_CPC_EP), pr_CPC_EP[,c('season','lat', 'lon', 'mean_pr_percentile')], by = c('season','lat', 'lon')) %>%
  dplyr::mutate(delta_pr_percentile = ifelse(model != "OBS", mean_pr_percentile.x - mean_pr_percentile.y,mean_pr_percentile.y),
                model_lab = ifelse(model != "OBS", paste0(model, "- OBS"), "OBS"))

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

load(file = 'Processed_Data/pr_box.RData')

pr_mod_obs_EP_box = pr_mod_obs_EP %>% dplyr::filter(lat >= pr_box$lat.min,
                                                    lat <= pr_box$lat.max,
                                                    lon >= pr_box$lon.min,
                                                    lon <= pr_box$lon.max)

subset = pr_mod_obs_EP[pr_mod_obs_EP$season == "MAM",]

pdf('Final figures/Figure_S3.pdf', height = 3.5, width = 12)
plot(
ggplot(data = subset) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(mean_pr_percentile.x))) + 
  scale_fill_gradient(name=expression(paste(P, " percentile")),
                      limits = c(0.75,0.9), 
                      low="white", high = "black", na.value = "white") +
  geom_rect(data = pr_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max), 
            col = "black", fill = "transparent", alpha=0, size = 1) +
  geom_path(data=world, aes(x,y), size = 0.25) + 
  geom_path(data=state, aes(x,y), size = 0.25) + 
  scale_y_continuous(limits = c(25,50)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-100,-65)) +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -80, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  facet_wrap(~model) +
  theme(legend.position = "right"))
dev.off()

########################################
############### Figure S4 ##############
########################################
rm(list = ls())
load(file = 'Processed_Data/pr_CPC_REP_same_day.RData')
load(file = 'Processed_Data/pr_mod_REP_same_day.RData')

pr_mod_REP_same_day = pr_mod_REP_same_day %>% dplyr::filter(model %in% c("GCM 1", "GCM 4")) %>%
                                              dplyr::mutate(model = ifelse(model == "GCM 4", "GCM ens 2", "GCM ens 1"))

pr_mod_REP_same_day$lat = as.numeric(as.character(pr_mod_REP_same_day$lat))
pr_mod_REP_same_day$lon = as.numeric(as.character(pr_mod_REP_same_day$lon))
pr_CPC_REP_same_day$lat = as.numeric(as.character(pr_CPC_REP_same_day$lat))
pr_CPC_REP_same_day$lon = as.numeric(as.character(pr_CPC_REP_same_day$lon))

pr_CPC_mod_REP_same_day = merge(rbind(pr_mod_REP_same_day, pr_CPC_REP_same_day), pr_CPC_REP_same_day[,c('season','lat', 'lon', 'mean_pr_percentile')], by = c('season','lat', 'lon')) %>%
  dplyr::mutate(delta_pr_percentile = ifelse(model != "OBS", mean_pr_percentile.x - mean_pr_percentile.y,mean_pr_percentile.y),
                model_lab = ifelse(model != "OBS", paste0(model, " - OBS"), "OBS"))

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

load(file = 'Processed_Data/pr_box.RData')

subset = pr_CPC_mod_REP_same_day[pr_CPC_mod_REP_same_day$season == "MAM",]

pdf('Final figures/Figure_S4.pdf', height = 3.5, width = 9)
plot(
ggplot(data = subset[subset$model != "OBS", ]) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(delta_pr_percentile))) + 
  scale_fill_gradient2(name=expression(atop(P[GCM] ~ " percentile -",P[OBS] ~ " percentile")),							 
                       limits = c(-0.27,0.27), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_rect(data = pr_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max), 
            col = "black", fill = "transparent", alpha=0, size = 1) +
  geom_path(data=world, aes(x,y), size = 0.25) + 
  geom_path(data=state, aes(x,y), size = 0.25) + 
  scale_y_continuous(limits = c(25,50)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-100,-65)) +
  theme_bw() +
  stat_contour(aes(x = (lon-360),y = lat, z=delta_pr_percentile, colour = ..level..), 
               breaks = seq(0.25,1,0.25), size=1, col = "black") +
  stat_contour(aes(x = (lon-360),y = lat, z=delta_pr_percentile, colour = ..level..), 
               breaks = seq(-0.25,-1,-0.25), size=1, col = "black", linetype = "dashed") +
  coord_map("ortho", orientation=c(40, -80, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
  facet_wrap(~model_lab, nrow = 1) +
  theme(legend.position = "right"))
dev.off()


########################################
############### Figure S6 ##############
########################################
rm(list = ls())
source('R/GetSeasonDate.R')
load(file = 'Processed_Data/Z_700_mod_field.RData')

Z_700_mod_field = Z_700_mod_field %>% dplyr::filter(model %in% c(1,4)) %>%
  dplyr::mutate(season = GetSeasonDate(date)) %>%
  dplyr::filter(season == 'MAM')

colnames(Z_700_mod_field)[5] = "z_700"

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))

season_mean_sd_mod = Z_700_mod_field %>% dplyr::group_by(lon,lat,model) %>%
  dplyr::summarise(z_700_clim = mean(z_700),
                   z_700_sd = sd(z_700))

season_mean_sd_mod_long = melt(season_mean_sd_mod, id.vars = c('lon', 'lat', 'model'))

# now load the reanalysis data
load('Processed_Data/Shifted_reanalysis/Z_700.RData')

Z_700 = Z_700 %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
  dplyr::filter(season == 'MAM')
colnames(Z_700)[4] = "z_700"


season_mean_sd_obs = Z_700 %>% dplyr::group_by(lon,lat) %>%
  dplyr::summarise(z_700_clim = mean(z_700),
                   z_700_sd = sd(z_700))

season_mean_sd_obs$model = "NCEP/NCAR"

season_mean_sd_all = rbind(season_mean_sd_mod, season_mean_sd_obs)

# interpolate onto a coarser grid that is common
# for both the GCM ensemble members and the observed
new_lat = seq(16, 60, by = 4)
new_lon = seq(208, 312, by = 4)
season_mean_sd_all_interp = list()
for(mm in 1:length(unique(season_mean_sd_all$model))){
  mmodel = 	unique(season_mean_sd_all$model)[mm]
  
  season_mean_sd_all_interp[[mm]] = data.frame(model = mmodel,
                                               lat = rep(new_lat, each = length(new_lon)),
                                               lon = rep(new_lon, length(new_lat)),
                                               z_700_sd = c(interp(x = season_mean_sd_all[season_mean_sd_all$model == mmodel,]$lon,
                                                                   y = season_mean_sd_all[season_mean_sd_all$model == mmodel,]$lat,
                                                                   z = season_mean_sd_all[season_mean_sd_all$model == mmodel,]$z_700_sd,
                                                                   xo = new_lon,
                                                                   yo = new_lat)$z))
}

season_mean_sd_mod_interp = do.call("rbind", season_mean_sd_all_interp[unique(season_mean_sd_all$model) != "NCEP/NCAR"])
season_mean_sd_obs_interp = do.call("rbind", season_mean_sd_all_interp[unique(season_mean_sd_all$model) == "NCEP/NCAR"])

plot_data = merge(season_mean_sd_obs_interp,season_mean_sd_mod_interp, by = c("lon", "lat")) %>%
  dplyr::mutate(z_700_sd = z_700_sd.y - z_700_sd.x,
                model = paste0(model.y," - ", model.x))



world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

pdf("Final figures/Figure_S6.pdf", width=8, height=2.5) # NOTE: the original submission included the state boundaries
plot(
ggplot(data = plot_data) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(z_700_sd))) + 
  facet_wrap(~model, ncol = 3) +
  scale_fill_gradient2(name=expression(paste(Delta, sigma[Z[700]], " (m)")),
                       limits = c(-22,22),
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_path(data=world, aes(x,y), size = 0.2, alpha = 0.6) + 
  scale_y_continuous(limits = c(15,60)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-145,-45), breaks = c(-140, -100, -60))  +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -95, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points))
dev.off()


########################################
############### Figure S7 ##############
########################################
# now let's plot the average jet in the reanalysis and the models after making the 
rm(list = ls())
source('R/GetSeasonDate.R')

load(file = 'Processed_Data/U_200_mod_MAM.RData')
load(file = 'Processed_Data/U_200_MAM.RData')

U_200_mod_MAM = U_200_mod_MAM %>% dplyr::filter(model %in% c(1,4)) %>% 
                                  dplyr::mutate(model = ifelse(model == 4, "GCM ens 2", paste0("GCM ens ",model)))
U_200_MAM = U_200_MAM %>% dplyr::mutate(model = "NCEP/NCAR")

# merge the reanalysis and the GCM fields, and calculate the MAM climatology
U_200_MAM_all = rbind(U_200_mod_MAM, U_200_MAM)  %>%
  dplyr::group_by(lon, lat, model) %>%
  dplyr::summarise(u_200_clim = mean(u_200))

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

# define a blank list of plots
plots = list()

for(mmodel in 1:length(unique(U_200_MAM_all$model))){
  subset = U_200_MAM_all[U_200_MAM_all$model == unique(U_200_MAM_all$model)[mmodel],]
  
  plots[[mmodel]] = 
    ggplot(data = subset) + 
    geom_tile(aes(x = (lon-360),y = lat,fill=(u_200_clim))) +
    facet_wrap(~model, ncol = 3) +
    scale_fill_gradient2(name=expression(paste(U[200], " (m/s)")),
                         limits = c(0,40),
                         midpoint = 20,low="blue", mid = "green",  high = "red",na.value = "grey",
                         guide = FALSE) +
    stat_contour(aes(x = (lon-360),y = lat, z=u_200_clim),
                 breaks = seq(15,45,10), size=0.35, col = "black") +
    geom_path(data=world, aes(x,y), size = 0.2, alpha = 0.6) + 
    scale_y_continuous(limits = c(10,60)) +
    labs(x = "Longitude", y = "Latitude") +
    scale_x_continuous(limits = c(-160,-30)) +
    theme_bw() +
    coord_map("ortho", orientation=c(40, -90, 0)) +
    geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points)
}


# Function to extract legend (from: http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram)
source('R/g_legend.R')

plot_legend <- g_legend(	ggplot(data = subset) + 
                           geom_tile(aes(x = (lon-360),y = lat,fill=(u_200_clim))) +
                           facet_wrap(~model, ncol = 3) +
                           scale_fill_gradient2(name=expression(paste(U[200], " (m/s)")),
                                                limits = c(0,40),
                                                midpoint = 20,low="blue", mid = "green",  high = "red",na.value = "grey") +
                           stat_contour(aes(x = (lon-360),y = lat, z=u_200_clim),
                                        breaks = seq(15,45,10), size=0.35, col = "black") +
                           geom_path(data=world, aes(x,y), size = 0.2, alpha = 0.6) + 
                           scale_y_continuous(limits = c(10,60)) +
                           labs(x = "Longitude", y = "Latitude") +
                           scale_x_continuous(limits = c(-160,-30)) +
                           theme_bw() +
                           coord_map("ortho", orientation=c(40, -90, 0)) +
                           geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
                           theme(legend.position = "right"))



lay <- rbind(c(1,1,2,2,3,3,7))

pdf("Final figures/Figure_S7.pdf", width=10, height=2.5)
plot(
grid.arrange(plots[[2]],plots[[3]],plots[[1]],
             plot_legend, layout_matrix = lay))
dev.off()

rm(list = ls())

