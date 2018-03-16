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
load('Processed_Data/SHUM_700_mod_field_future.RData')
load('Processed_Data/Z_700_mod_field_future.RData')
load('Processed_Data/OMG_700_mod_field_future.RData')


# make the OMG index
O_index_mod = OMG_700_mod_field %>% dplyr::filter(lat >= pr_box$lat.min & 
                                lat <= pr_box$lat.max &
                                lon >= pr_box$lon.min &
                                lon <= pr_box$lon.max) %>%
                  dplyr::group_by(date, model) %>%
                  dplyr::summarise(O_index = mean(value))
save(O_index_mod, file = 'Processed_Data/Shifted_reanalysis/O_index_mod_future.RData')

# make the MOIST index
M_index_mod = SHUM_700_mod_field %>% dplyr::filter(lat >= pr_box$lat.min & 
                                 lat <= pr_box$lat.max &
                                 lon >= pr_box$lon.min &
                                 lon <= pr_box$lon.max) %>%
                   dplyr::group_by(date, model) %>%
                   dplyr::summarise(M_index = mean(value)) 
# %>%
#                    dplyr::group_by(model) %>%
#                    dplyr::mutate(M_index = SHUM - lead(SHUM, 1))
  
save(M_index_mod, file = 'Processed_Data/Shifted_reanalysis/M_index_mod_future.RData')

# make the NPH index
NPH_index_mod = Z_700_mod_field %>% dplyr::filter(lat >= NPH_box$lat.min & 
                                    lat <= NPH_box$lat.max &
                                    lon >= NPH_box$lon.min + 360 &
                                    lon <= NPH_box$lon.max + 360) %>%
                      dplyr::group_by(date, model) %>%
                      dplyr::summarise(Z_700 = mean(value)) %>%
                      dplyr::group_by(model) %>%
                      dplyr::mutate(NPH_index = (lag(Z_700,1) + lag(Z_700,2) + lag(Z_700,3))/3)
save(NPH_index_mod, file = 'Processed_Data/Shifted_reanalysis/NPH_index_mod_future.RData')

# make the EWD index
east.box_mod = Z_700_mod_field %>% dplyr::filter(lat >= EWD_boxes$lat.min[EWD_boxes$box == "east"] & 
                                    lat <= EWD_boxes$lat.max[EWD_boxes$box == "east"] &
                                    lon >= EWD_boxes$lon.min[EWD_boxes$box == "east"] + 360 &
                                    lon <= EWD_boxes$lon.max[EWD_boxes$box == "east"] + 360) %>%
                     dplyr::group_by(date, model) %>%
                     dplyr::summarise(east_Z = mean(value))

west.box_mod = Z_700_mod_field %>% dplyr::filter(lat >= EWD_boxes$lat.min[EWD_boxes$box == "west"] & 
                                     lat <= EWD_boxes$lat.max[EWD_boxes$box == "west"] &
                                     lon >= EWD_boxes$lon.min[EWD_boxes$box == "west"] + 360 &
                                     lon <= EWD_boxes$lon.max[EWD_boxes$box == "west"] + 360) %>%
                     dplyr::group_by(date, model) %>%
                     dplyr::summarise(west_Z = mean(value))

EWD_index_mod = merge(east.box_mod, west.box_mod, by = c("date", "model")) %>% dplyr::mutate(EWD_index = east_Z - west_Z)
save(EWD_index_mod, file = 'Processed_Data/Shifted_reanalysis/EWD_index_mod_future.RData')

rm(list = ls())

