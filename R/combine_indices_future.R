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
load(file = 'Processed_Data/mod_REP_future.RData')
load(file = 'Processed_Data/Shifted_reanalysis/EWD_index_mod_future.RData')
load(file = 'Processed_Data/Shifted_reanalysis/M_index_mod_future.RData')
load(file = 'Processed_Data/Shifted_reanalysis/O_index_mod_future.RData')
load(file = 'Processed_Data/Shifted_reanalysis/NPH_index_mod_future.RData')



# combine the reanalysis record and add model = "obs"
REP_EWD_M_O_mod = merge(merge(merge(merge(mod_REP_future,EWD_index_mod, by = c("date","model")), 
                                    M_index_mod, by = c("date","model")), 
                              O_index_mod, by = c("date","model")),
                        NPH_index_mod %>% dplyr::select(-Z_700), by = c("date","model"))

load(file = 'Processed_Data/Shifted_reanalysis/REP_EWD_M_O_clims.RData')


REP_EWD_M_O_all = merge(rbind(REP_EWD_M_O_mod, REP_EWD_M_O) %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5)),
                        REP_EWD_M_O_clims, by = "model", all.x = TRUE) %>%
  dplyr::mutate(O_index_anom = (O_index - O_index_mean)/O_index_sd,
                M_index_anom = (M_index - M_index_mean)/M_index_sd,
                west_index_anom = (west_Z - west_index_mean)/west_index_sd,
                east_index_anom = (east_Z - east_index_mean)/east_index_sd,
                EWD_index_anom = (EWD_index - EWD_index_mean)/EWD_index_sd,
                NPH_index_anom = (NPH_index - NPH_index_mean)/NPH_index_sd) %>%
  dplyr::mutate(EWD.positive = ifelse(EWD_index > 0, 1, 0),
                O.negative = ifelse(O_index < 0, 1, 0),
                REP_lag1 = lag(REP, 1)) %>%
  dplyr::select(date, model, REP, EWD_index_anom, east_index_anom, west_index_anom, O_index_anom, M_index_anom, NPH_index_anom, REP_lag1, EWD.positive, O.negative)

# let's save this combined index file
save(REP_EWD_M_O_all, file = 'Processed_Data/Shifted_reanalysis/REP_EWD_M_O_all_future.RData')

rm(list = ls())
