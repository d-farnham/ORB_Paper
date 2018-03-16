source('R/GetSeasonDate.R')

# define the EWD index boxes
EWD_boxes = data.frame(box = c('east', 'west'),
                       lat.min = c(30, 30),
                       lat.max = c(45, 45),
                       lon.min = c(-77.5, -102.5),
                       lon.max = c(-65, -90))

save(EWD_boxes, file = 'Processed_Data/Shifted_reanalysis/EWD_boxes.RData')

# define the N.Pac high index boxes
NPH_box = data.frame(lat.min = 30,
                     lat.max = 55,
                     lon.min = -155,
                     lon.max = -130)

save(NPH_box, file = 'Processed_Data/Shifted_reanalysis/NPH_box.RData')

# load the boxes that define the pr
load(file = 'Processed_Data/pr_box.RData')


# now let's define the indices
load('Processed_Data/Shifted_reanalysis/SHUM.RData')
load('Processed_Data/Shifted_reanalysis/Z_700.RData')
load('Processed_Data/Shifted_reanalysis/OMG.RData')


# make the OMG index
O_index = OMG %>% dplyr::filter(lat >= pr_box$lat.min & 
                                lat <= pr_box$lat.max &
                                lon >= pr_box$lon.min &
                                lon <= pr_box$lon.max) %>%
                  dplyr::group_by(date, level) %>%
                  dplyr::summarise(O_index = mean(OMG))
save(O_index, file = 'Processed_Data/Shifted_reanalysis/O_index.RData')

# make the MOIST index
M_index = SHUM %>% dplyr::filter(lat >= pr_box$lat.min & 
                                 lat <= pr_box$lat.max &
                                 lon >= pr_box$lon.min &
                                 lon <= pr_box$lon.max) %>%
                   dplyr::group_by(date, level) %>%
                   dplyr::summarise(M_index = mean(SHUM))
# %>%
#                    dplyr::group_by(level) %>%
#                    dplyr::mutate(M_index = SHUM - lead(SHUM, 1))
  
save(M_index, file = 'Processed_Data/Shifted_reanalysis/M_index.RData')

# make the NPH index
NPH_index = Z_700 %>% dplyr::filter(lat >= NPH_box$lat.min & 
                                    lat <= NPH_box$lat.max &
                                    lon >= NPH_box$lon.min + 360 &
                                    lon <= NPH_box$lon.max + 360) %>%
                      dplyr::group_by(date) %>%
                      dplyr::summarise(Z_700 = mean(Z_700)) %>%
                      dplyr::mutate(NPH_index = (lag(Z_700,1) + lag(Z_700,2) + lag(Z_700,3))/3)
save(NPH_index, file = 'Processed_Data/Shifted_reanalysis/NPH_index.RData')

# make the EWD index
east.box = Z_700 %>% dplyr::filter(lat >= EWD_boxes$lat.min[EWD_boxes$box == "east"] & 
                                    lat <= EWD_boxes$lat.max[EWD_boxes$box == "east"] &
                                    lon >= EWD_boxes$lon.min[EWD_boxes$box == "east"] + 360 &
                                    lon <= EWD_boxes$lon.max[EWD_boxes$box == "east"] + 360) %>%
                     dplyr::group_by(date) %>%
                     dplyr::summarise(east_Z = mean(Z_700))

west.box = Z_700 %>% dplyr::filter(lat >= EWD_boxes$lat.min[EWD_boxes$box == "west"] & 
                                     lat <= EWD_boxes$lat.max[EWD_boxes$box == "west"] &
                                     lon >= EWD_boxes$lon.min[EWD_boxes$box == "west"] + 360 &
                                     lon <= EWD_boxes$lon.max[EWD_boxes$box == "west"] + 360) %>%
                     dplyr::group_by(date) %>%
                     dplyr::summarise(west_Z = mean(Z_700))

EWD_index = merge(east.box, west.box, by = c("date")) %>% dplyr::mutate(EWD_index = east_Z - west_Z)
save(EWD_index, file = 'Processed_Data/Shifted_reanalysis/EWD_index.RData')


# now plot the indices before during and after the REP events
########################################
############### Figure 6 ###############
########################################
# plot dipole locations and the lead up to a REP
rm(list = ls())
load(file = 'Processed_Data/CPC_mod_cell_REP.RData')
load(file = 'Processed_Data/Shifted_reanalysis/EWD_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/M_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/O_index.RData')
load(file = 'Processed_Data/Shifted_reanalysis/NPH_index.RData')

# embed the laging and leading values of EWD and QA_700_anom
REP_EWD_M_O = merge(merge(merge(merge(CPC_mod_cell_REP,EWD_index, by = c("date")), 
                                      M_index %>% dplyr::filter(level == 700) %>% dplyr::select(-level), by = c("date")), 
                                      O_index %>% dplyr::filter(level == 700) %>% dplyr::select(-level), by = c("date")),
                                      NPH_index, by = c("date")) %>%
  # dplyr::select(-east_Z, -west_Z) %>%
  dplyr::mutate(EWD.positive = ifelse(EWD_index > 0, 1, 0),
                O.negative = ifelse(O_index < 0, 1, 0)) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(O_index_anom = (O_index - mean(O_index, na.rm = TRUE))/sd(O_index, na.rm = TRUE),
                M_index_anom = (M_index - mean(M_index, na.rm = TRUE))/sd(M_index, na.rm = TRUE),
                EWD_index_anom = (EWD_index - mean(EWD_index, na.rm = TRUE))/sd(EWD_index, na.rm = TRUE),
                east_index_anom = (east_Z - mean(east_Z, na.rm = TRUE))/sd(east_Z, na.rm = TRUE),
                west_index_anom = (west_Z - mean(west_Z, na.rm = TRUE))/sd(west_Z, na.rm = TRUE),
                NPH_index_anom = (NPH_index - mean(NPH_index, na.rm = TRUE))/sd(NPH_index, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`EWD_index_anom_-1` = lag(EWD_index_anom, 1), 
                `O_index_anom_-1` = lag(O_index_anom, 1), 
                `M_index_anom_-1` = lag(M_index_anom, 1), 
                `NPH_index_anom_-1` = lag(NPH_index_anom, 1),
                `east_index_anom_-1` = lag(east_index_anom, 1), 
                `west_index_anom_-1` = lag(west_index_anom, 1),
                `EWD_index_anom_-2` = lag(EWD_index_anom, 2), 
                `O_index_anom_-2` = lag(O_index_anom, 2), 
                `M_index_anom_-2` = lag(M_index_anom, 2), 
                `NPH_index_anom_-2` = lag(NPH_index_anom, 2),
                `east_index_anom_-2` = lag(east_index_anom, 2), 
                `west_index_anom_-2` = lag(west_index_anom, 2),
                `EWD_index_anom_-3` = lag(EWD_index_anom, 3), 
                `O_index_anom_-3` = lag(O_index_anom, 3), 
                `M_index_anom_-3` = lag(M_index_anom, 3), 
                `NPH_index_anom_-3` = lag(NPH_index_anom, 3),
                `east_index_anom_-3` = lag(east_index_anom, 3), 
                `west_index_anom_-3` = lag(west_index_anom, 3),
                `EWD_index_anom_-4` = lag(EWD_index_anom, 4), 
                `O_index_anom_-4` = lag(O_index_anom, 4), 
                `M_index_anom_-4` = lag(M_index_anom, 4), 
                `NPH_index_anom_-4` = lag(NPH_index_anom, 4),
                `east_index_anom_-4` = lag(east_index_anom, 4), 
                `west_index_anom_-4` = lag(west_index_anom, 4),
                `EWD_index_anom_-5` = lag(EWD_index_anom, 5), 
                `O_index_anom_-5` = lag(O_index_anom, 5), 
                `M_index_anom_-5` = lag(M_index_anom, 5), 
                `NPH_index_anom_-5` = lag(NPH_index_anom, 5),
                `east_index_anom_-5` = lag(east_index_anom, 5), 
                `west_index_anom_-5` = lag(west_index_anom, 5),
                `EWD_index_anom_00` = EWD_index_anom,         
                `O_index_anom_00` = O_index_anom,         
                `M_index_anom_00` = M_index_anom,          
                `NPH_index_anom_00` = NPH_index_anom,
                `east_index_anom_00` = east_index_anom, 
                `west_index_anom_00` = west_index_anom,
                `EWD_index_anom_01` = lead(EWD_index_anom, 1), 
                `O_index_anom_01` = lead(O_index_anom, 1),
                `M_index_anom_01` = lead(M_index_anom, 1), 
                `NPH_index_anom_01` = lead(NPH_index_anom, 1),
                `east_index_anom_01` = lead(east_index_anom, 1), 
                `west_index_anom_01` = lead(west_index_anom, 1),
                `EWD_index_anom_02` = lead(EWD_index_anom, 2), 
                `O_index_anom_02` = lead(O_index_anom, 2),
                `M_index_anom_02` = lead(M_index_anom, 2), 
                `NPH_index_anom_02` = lead(NPH_index_anom, 2),
                `east_index_anom_01` = lead(east_index_anom, 2), 
                `west_index_anom_01` = lead(west_index_anom, 2),
                `EWD_index_anom_03` = lead(EWD_index_anom, 3), 
                `O_index_anom_03` = lead(O_index_anom, 3),
                `M_index_anom_03` = lead(M_index_anom, 3), 
                `NPH_index_anom_03` = lead(NPH_index_anom, 3),
                `east_index_anom_03` = lead(east_index_anom, 3), 
                `west_index_anom_03` = lead(west_index_anom, 3)) %>%
  dplyr::mutate(O_index_anom_mean = mean(O_index_anom, na.rm = TRUE),
                M_index_anom_mean = mean(M_index_anom, na.rm = TRUE),
                EWD_index_anom_mean = mean(EWD_index_anom, na.rm = TRUE),
                NPH_index_anom_mean = mean(NPH_index_anom, na.rm = TRUE),
                east_index_anom_mean = mean(east_index_anom, na.rm = TRUE),
                west_index_anom_mean = mean(west_index_anom, na.rm = TRUE))

# only select the REP days
REP_EWD_M_O_long = melt(REP_EWD_M_O %>% dplyr::select(-EWD_index, -O_index, -M_index, -NPH_index, -EWD.positive, -east_Z, - west_Z, -O.negative, 
                                                      -EWD_index_anom, -M_index_anom, -O_index_anom, -NPH_index_anom, -east_index_anom, -west_index_anom), 
                       id.vars = c("season", "date", "REP", "EWD_index_anom_mean", "M_index_anom_mean", 
                                   "O_index_anom_mean", "NPH_index_anom_mean", "east_index_anom_mean", "west_index_anom_mean")) %>%
  dplyr::filter(REP == 1)

# compute the percentiles of interest
REP_EWD_M_O_time = REP_EWD_M_O_long %>% dplyr::group_by(season, variable) %>%
  dplyr::summarise(median = median(value, na.rm = TRUE),
                   `5th` = quantile(value, probs = 0.05, na.rm = TRUE),
                   `95th` = quantile(value, probs = 0.95, na.rm = TRUE),
                   `25th` = quantile(value, probs = 0.25, na.rm = TRUE),
                   `75th` = quantile(value, probs = 0.75, na.rm = TRUE),
                   EWD_index_anom_mean = mean(EWD_index_anom_mean),
                   M_index_anom_mean = mean(M_index_anom_mean),
                   O_index_anom_mean = mean(O_index_anom_mean),
                   NPH_index_anom_mean = mean(NPH_index_anom_mean),
                   east_index_anom_mean = mean(east_index_anom_mean),
                   west_index_anom_mean = mean(west_index_anom_mean))

# separate 'variable' into 'EWD' or 'MHC' and the lag number
# load function to efficiently do this
source("R/SubstrRightLeft.R")

REP_EWD_M_O_time = REP_EWD_M_O_time %>% dplyr::mutate(lag = as.numeric(substrRight(as.character(variable),2)),
                                                      var = substrLeft(as.character(variable),3))

EWD_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                          REP_EWD_M_O_time$var == "EWD_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = EWD_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab("EWD Index") +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
  theme_bw()

O_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                          REP_EWD_M_O_time$var == "O_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = O_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab("OMG Index") +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
  theme_bw()

M_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                          REP_EWD_M_O_time$var == "M_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = M_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab("HUM Index") +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
  theme_bw()

NPH_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                            REP_EWD_M_O_time$var == "NPH_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = NPH_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab(expression(Z[P] ~ "Index")) +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
#  scale_y_continuous(limits = c(-3.75, 3.75), breaks = c(seq(-3,3,by=1))) +
  theme_bw()

EAST_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                          REP_EWD_M_O_time$var == "east_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = east_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab(expression(Z[H] ~ "Index")) +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
#  scale_y_continuous(limits = c(-3.75, 3.75), breaks = c(seq(-3,3,by=1))) +
  theme_bw()

WEST_time =
  ggplot(REP_EWD_M_O_time[REP_EWD_M_O_time$season == "MAM" &
                            REP_EWD_M_O_time$var == "west_index_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = west_index_anom_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab(expression(Z[L] ~ "Index")) +
  xlab("Days after REP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
#  scale_y_continuous(limits = c(-3.75, 3.75), breaks = c(seq(-3,3,by=1))) +
  theme_bw()

load(file = 'Processed_Data/Shifted_reanalysis/EWD_boxes.RData')
load(file = 'Processed_Data/Shifted_reanalysis/NPH_box.RData')
load(file = 'Processed_Data/pr_box.RData')

# plot the locations of the dipole index
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
state <- data.frame(map("state", plot=FALSE)[c("x","y")])

source("R/load_basin_boundary.R")

O_M_Z_location =
  ggplot() + 
  geom_rect(data = EWD_boxes,
            aes(xmin=lon.min, xmax=lon.max, ymin=lat.min, ymax=lat.max),
            alpha = 0, col = "black", linetype = c("dashed"), size = 0.75) +
  geom_rect(data = pr_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max),
            alpha = 0, col = "black", linetype = c("solid"), size = 0.75) +
  geom_rect(data = NPH_box,
            aes(xmin=lon.min, xmax=lon.max, ymin=lat.min, ymax=lat.max),
            alpha=0, col = "black", linetype = "dashed", size = 0.75) +
  geom_path(data=state, aes(x,y), size = 0.25) + 
  geom_path(data=world, aes(x,y), size = 0.25) + 
  scale_y_continuous(limits = c(24,57)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_x_continuous(limits = c(-170,-45)) +
  theme_bw() +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  theme(legend.position="none") +
  labs(x = "Longitude", y = "Latitude") +
  geom_text(aes(x = c(-152.5, -100, -86, -86, -75),
                y = c(32, 32, 39.75, 37.5, 32),
                label = c(paste("Z","[P]",sep =""),
                          paste("Z","[L]",sep =""),
                          "HUM",
                          "OMG",
                          paste("Z","[H]",sep =""))), 
            parse = TRUE,
            size = 3,
            col = "red")

layout_matrix = cbind(c(NA,2,NA),c(1,2,5),c(1,3,5),c(1,3,6), c(1,4,6), c(NA,4,NA))
# Figure 6
pdf('Final figures/Figure_6.pdf', width = 10, height = 7)
grid.arrange(O_M_Z_location,
             NPH_time,
             WEST_time,
             M_time,
             O_time,
             EAST_time,
             layout_matrix = layout_matrix)
dev.off()

rm(list = ls())
