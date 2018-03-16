source("R/GetSeasonDate.R")

# load the reanalysis data -- start with the Z
list_Z_files = list.files(path = '/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/Z/')

Z_700 = data.frame()

for(file_num in 1:length(list_Z_files)){
    Z_ncdf = nc_open(paste0('/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/Z/',list_Z_files[file_num]))
    
    # extract time and lat/lon from the file
    time = ncvar_get(Z_ncdf, varid = "time", start = c(1), count = c(-1))
    # convert time to dates and times
    date_time = as.POSIXct(time*60*60, origin = "1800-01-01 00:00:00", tz = "UTC")
    
    # now need to make a new date column that adds 12 hours to each time so that each day is now from the priors day 12z to the current days 12z
    date_time_adjust = date_time + 12 * 60 * 60
    
    lat = ncvar_get(Z_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(Z_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    # hgt[lon,lat,level,time] -- only level included is 700
    tmp0 = melt(ncvar_get(Z_ncdf, 
                          varid = "hgt", 
                          start = c(1,1,1,1), 
                          count = c(-1,-1,1,-1)))
    
    # insert actual lat, lon, date values and average by day
    tmp0 = tmp0 %>% dplyr::mutate(lon = lon[Var1],
                                  lat = lat[Var2],
                                  date = as.Date(date_time_adjust[Var3]), tz = "UTC") %>%
                    dplyr::group_by(lat, lon, date) %>%
                    dplyr::summarise(Z_700 = mean(value)) %>%
                    dplyr::select(lon, lat, date, Z_700)
    
    # only retain Feb 20 through May 31st
    # also only retain between 1950 and 2005
    tmp0 = tmp0 %>% dplyr::filter(lubridate::month(date) %in% c(2,3,4,5) &
                                  !(lubridate::month(date) == 2 & lubridate::day(date) < 20) &
                                  lubridate::year(date) > 1949 &
                                  lubridate::year(date) < 2006)
    
  
    
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }

Z_700 = tmp
save(Z_700, file = 'Processed_Data/Shifted_reanalysis/Z_700.RData')



# load the reanalysis data -- now load the SHUM
rm(list = ls())
list_SHUM_files = list.files(path = '/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/SHUM/')

SHUM = data.frame()

for(file_num in 1:length(list_SHUM_files)){
  SHUM_ncdf = nc_open(paste0('/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/SHUM/',list_SHUM_files[file_num]))
  
  # extract time and lat/lon from the file
  time = ncvar_get(SHUM_ncdf, varid = "time", start = c(1), count = c(-1))
  # convert time to dates and times
  date_time = as.POSIXct(time*60*60, origin = "1800-01-01 00:00:00", tz = "UTC")
  
  # now need to make a new date column that adds 12 hours to each time so that each day is now from the priors day 12z to the current days 12z
  date_time_adjust = date_time + 12 * 60 * 60
  
  lat = ncvar_get(SHUM_ncdf, varid = "lat", start = c(1), count = c(-1))
  lon = ncvar_get(SHUM_ncdf, varid = "lon", start = c(1), count = c(-1))
  level = ncvar_get(SHUM_ncdf, varid = "level", start = c(1), count = c(-1))
  
  
  # shum[lon,lat,level,time] -- store all levels
  tmp0 = melt(ncvar_get(SHUM_ncdf, 
                        varid = "shum", 
                        start = c(1,1,1,1), 
                        count = c(-1,-1,-1,-1)))
  
  # insert actual lat, lon, date values and average by day
  tmp0 = tmp0 %>% dplyr::mutate(lon = lon[Var1],
                                lat = lat[Var2],
                                level = level[Var3],
                                date = as.Date(date_time_adjust[Var4]), tz = "UTC") %>%
    dplyr::group_by(lat, lon, level, date) %>%
    dplyr::summarise(SHUM = mean(value)) %>%
    dplyr::select(lon, lat, level, date, SHUM)
  
  # only retain Feb 20 through May 31st
  # also only retain between 1950 and 2005
  tmp0 = tmp0 %>% dplyr::filter(lubridate::month(date) %in% c(2,3,4,5) &
                                  !(lubridate::month(date) == 2 & lubridate::day(date) < 20) &
                                  lubridate::year(date) > 1949 &
                                  lubridate::year(date) < 2006)
  
  
  
  if(file_num == 1){tmp = tmp0}
  
  if(file_num > 1){tmp = rbind(tmp, tmp0)}
  print(file_num)
}

SHUM = tmp
save(SHUM, file = 'Processed_Data/Shifted_reanalysis/SHUM.RData')


# load the reanalysis data -- now load the OMG
rm(list = ls())
list_OMG_files = list.files(path = '/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/OMEGA/')

OMG = data.frame()

for(file_num in 1:length(list_OMG_files)){
  OMG_ncdf = nc_open(paste0('/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/six_hourly/OMEGA/',list_OMG_files[file_num]))
  
  # extract time and lat/lon from the file
  time = ncvar_get(OMG_ncdf, varid = "time", start = c(1), count = c(-1))
  # convert time to dates and times
  date_time = as.POSIXct(time*60*60, origin = "1800-01-01 00:00:00", tz = "UTC")
  
  # now need to make a new date column that adds 12 hours to each time so that each day is now from the priors day 12z to the current days 12z
  date_time_adjust = date_time + 12 * 60 * 60
  
  lat = ncvar_get(OMG_ncdf, varid = "lat", start = c(1), count = c(-1))
  lon = ncvar_get(OMG_ncdf, varid = "lon", start = c(1), count = c(-1))
  level = ncvar_get(OMG_ncdf, varid = "level", start = c(1), count = c(-1))
  
  
  # omega[lon,lat,level,time] -- store all levels
  tmp0 = melt(ncvar_get(OMG_ncdf, 
                        varid = "omega", 
                        start = c(1,1,1,1), 
                        count = c(-1,-1,-1,-1)))
  
  # insert actual lat, lon, date values and average by day
  tmp0 = tmp0 %>% dplyr::mutate(lon = lon[Var1],
                                lat = lat[Var2],
                                level = level[Var3],
                                date = as.Date(date_time_adjust[Var4]), tz = "UTC") %>%
    dplyr::group_by(lat, lon, level, date) %>%
    dplyr::summarise(OMG = mean(value)) %>%
    dplyr::select(lon, lat, level, date, OMG)
  
  # only retain Feb 20 through May 31st
  # also only retain between 1950 and 2005
  tmp0 = tmp0 %>% dplyr::filter(lubridate::month(date) %in% c(2,3,4,5) &
                                  !(lubridate::month(date) == 2 & lubridate::day(date) < 20) &
                                  lubridate::year(date) > 1949 &
                                  lubridate::year(date) < 2006)
  
  
  
  if(file_num == 1){tmp = tmp0}
  
  if(file_num > 1){tmp = rbind(tmp, tmp0)}
  print(file_num)
}

OMG = tmp
save(OMG, file = 'Processed_Data/Shifted_reanalysis/OMG.RData')

# now load and save the U_200 monthly reanalysis fields
rm(list = ls())
source('R/GetSeasonDate.R')

# load and save the U_200 field -- monthly data
reanal_output = nc_open('/Users/davidfarnham/Google Drive/ORB_Paper/Raw_Data/REANALYSIS_data/u_200_REANALYSIS.nc')

time = ncvar_get(reanal_output, varid = "T", start = c(1), count = c(-1))
# data spans from jan 1949 to mar 2017
dates = data.frame(year = c(rep(1949:2016, each = 12),rep(2017,3)),
                   month = c(rep(1:12,(2016-1949+1)),1:3),
                   day = 1)
date = as.Date(paste(dates$year,
                     dates$month,
                     dates$day, sep = "-"), 
               origin = "1948-01-01 00:00:00")

lat = ncvar_get(reanal_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(reanal_output, varid = "X", start = c(1), count = c(-1))

start_date = which(date > "1949-12-31" &
                     date < "2005-01-01")[1]

count_date = sum(date > "1949-12-31" &
                   date < "2005-01-01")

U_200 = melt(ncvar_get(reanal_output, 
                       varid = "u", 
                       start = c(1,1,1,start_date), 
                       count = c(length(lon),length(lat),1,count_date)))

U_200 = U_200 %>% dplyr::mutate(lon = lon[Var1],
                                lat = lat[Var2],
                                date = date[which(date > "1949-12-31" &
                                                    date < "2005-01-01")][Var3],
                                season = GetSeasonDate(date),
                                u_200 = value) %>%
  dplyr::select(c(date, season, lon, lat, u_200))

U_200_MAM = U_200 %>% dplyr::filter(season %in% c("MAM"))
save(U_200_MAM, file = 'Processed_Data/U_200_MAM.RData')

rm(list = ls())
