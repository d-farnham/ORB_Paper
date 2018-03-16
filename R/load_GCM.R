source('R/GetSeasonDate.R')

# now load the zg_700 field from the GCM to compare the composites from the GCM and the observations
load(file = 'Processed_Data/data.file.path')
list_zg_files = list.files(path = paste0(data.file.path,'GCM_data/historic/zg/'))

Z_700_mod_field = data.frame()

# define the extent of Z_700 field to retain
min.lat = 14
max.lat = 60
min.lon = 180
max.lon = 320
for(m in c(1,4)){ # only need to load ensemble member 1 and 4 since moisture is only stored from those two
  mmod = c("r1i","r2i","r3i","r4i", "r5i")
  mmod_files = list_zg_files[grep(list_zg_files, pattern = mmod[m])]
  
  for(file_num in 1:length(mmod_files)){
    GCM_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/zg/',mmod_files[file_num]))
    
    # extract time and lat/on from the file
    time = ncvar_get(GCM_ncdf, varid = "time", start = c(1), count = c(-1))
    lat = ncvar_get(GCM_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(GCM_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    start_lat = which(lat > min.lat & lat < max.lat)[1]
    count_lat = sum(lat > min.lat & lat < max.lat)
    
    start_lon = which(lon > min.lon & lon < max.lon)[1]
    count_lon = sum(lon > min.lon & lon < max.lon)
    
    plev = ncvar_get(GCM_ncdf, varid = "plev", start = c(1), count = c(-1))
    
    
    tmp0 = melt(ncvar_get(GCM_ncdf, 
                          varid = "zg", 
                          start = c(start_lon,start_lat,which(plev == 70000),1), 
                          count = c(count_lon,count_lat,1,length(time))))
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }
  tmp$model = m
  
  if(m == 1){Z_700_mod_field = tmp}
  if(m > 1){Z_700_mod_field = rbind(Z_700_mod_field,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
                         day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(Z_700_mod_field[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
Z_700_mod_field$date = rep(rep(date,each = num.cells),2)

# can get rid of non MAM months
Z_700_mod_field = Z_700_mod_field %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5))

# can get rid of lats > max.lat and lat < min.lat and lon < min.lon and lon > max.lon for space
Z_700_mod_field$lat = lat[which(lat > min.lat & lat < max.lat)][Z_700_mod_field$Var2]
Z_700_mod_field$lon = lon[which(lon > min.lon & lon < max.lon)][Z_700_mod_field$Var1]


Z_700_mod_field = Z_700_mod_field %>% dplyr::select(date,lon,lat,model,value)

save(Z_700_mod_field, file = 'Processed_Data/Z_700_mod_field.RData')

# now laod the SHUM data
# load and process the SHUM data
load(file = 'Processed_Data/data.file.path')
list_SHUM_files = list.files(path = paste0(data.file.path,'GCM_data/historic/hus/'))

SHUM_700_mod_field = data.frame()

# define the extent of SHUM_700 field to retain
min.lat = 14
max.lat = 60
min.lon = 180
max.lon = 320
for(m in c(1,4)){ # only need to load ensemble member 1 and 4 since moisture is only stored from those two
  mmod = c("r1i","r2i","r3i","r4i", "r5i")
  mmod_files = list_SHUM_files[grep(list_SHUM_files, pattern = mmod[m])]
  
  for(file_num in 1:length(mmod_files)){
    GCM_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/hus/',mmod_files[file_num]))
    
    # extract time and lat/on from the file
    time = ncvar_get(GCM_ncdf, varid = "time", start = c(1), count = c(-1))
    lat = ncvar_get(GCM_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(GCM_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    start_lat = which(lat > min.lat & lat < max.lat)[1]
    count_lat = sum(lat > min.lat & lat < max.lat)
    
    start_lon = which(lon > min.lon & lon < max.lon)[1]
    count_lon = sum(lon > min.lon & lon < max.lon)
    
    plev = ncvar_get(GCM_ncdf, varid = "plev", start = c(1), count = c(-1))
    
    
    tmp0 = melt(ncvar_get(GCM_ncdf, 
                          varid = "hus", 
                          start = c(start_lon,start_lat,which(plev == 70000),1), 
                          count = c(count_lon,count_lat,1,length(time))))
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }
  tmp$model = m
  
  if(m == 1){SHUM_700_mod_field = tmp}
  if(m > 1){SHUM_700_mod_field = rbind(SHUM_700_mod_field,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
                         day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(SHUM_700_mod_field[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
SHUM_700_mod_field$date = rep(rep(date,each = num.cells),2)

# can get rid of non MAM months
SHUM_700_mod_field = SHUM_700_mod_field %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5))

# can get rid of lats > max.lat and lat < min.lat and lon < min.lon and lon > max.lon for space
SHUM_700_mod_field$lat = lat[which(lat > min.lat & lat < max.lat)][SHUM_700_mod_field$Var2]
SHUM_700_mod_field$lon = lon[which(lon > min.lon & lon < max.lon)][SHUM_700_mod_field$Var1]


SHUM_700_mod_field = SHUM_700_mod_field %>% dplyr::select(date,lon,lat,model,value)

save(SHUM_700_mod_field, file = 'Processed_Data/SHUM_700_mod_field.RData')

# now do the same thing for omega
load(file = 'Processed_Data/data.file.path')
list_OMG_files = list.files(path = paste0(data.file.path,'GCM_data/historic/wap/'))

OMG_700_mod_field = data.frame()

# define the extent of OMG_700 field to retain
min.lat = 30
max.lat = 50
min.lon = 260
max.lon = 310
for(m in c(1,4)){ # only need to load ensemble member 1 and 4 since moisture is only stored from those two
  mmod = c("r1i","r2i","r3i","r4i", "r5i")
  mmod_files = list_OMG_files[grep(list_OMG_files, pattern = mmod[m])]
  
  for(file_num in 1:length(mmod_files)){
    GCM_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/wap/',mmod_files[file_num]))
    
    # extract time and lat/on from the file
    time = ncvar_get(GCM_ncdf, varid = "time", start = c(1), count = c(-1))
    lat = ncvar_get(GCM_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(GCM_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    start_lat = which(lat > min.lat & lat < max.lat)[1]
    count_lat = sum(lat > min.lat & lat < max.lat)
    
    start_lon = which(lon > min.lon & lon < max.lon)[1]
    count_lon = sum(lon > min.lon & lon < max.lon)
    
    plev = ncvar_get(GCM_ncdf, varid = "plev", start = c(1), count = c(-1))
    
    
    tmp0 = melt(ncvar_get(GCM_ncdf, 
                          varid = "wap", 
                          start = c(start_lon,start_lat,which(plev == 70000),1), 
                          count = c(count_lon,count_lat,1,length(time))))
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }
  tmp$model = m
  
  if(m == 1){OMG_700_mod_field = tmp}
  if(m > 1){OMG_700_mod_field = rbind(OMG_700_mod_field,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
                         day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(OMG_700_mod_field[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
OMG_700_mod_field$date = rep(rep(date,each = num.cells),2)

# can get rid of non MAM months
OMG_700_mod_field = OMG_700_mod_field %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5))

# can get rid of lats > max.lat and lat < min.lat and lon < min.lon and lon > max.lon for space
OMG_700_mod_field$lat = lat[which(lat > min.lat & lat < max.lat)][OMG_700_mod_field$Var2]
OMG_700_mod_field$lon = lon[which(lon > min.lon & lon < max.lon)][OMG_700_mod_field$Var1]


OMG_700_mod_field = OMG_700_mod_field %>% dplyr::select(date,lon,lat,model,value)

save(OMG_700_mod_field, file = 'Processed_Data/OMG_700_mod_field.RData')

# now load the U_200 GCM monthly data
rm(list = ls())
source('R/GetSeasonDate.R')
load(file = 'Processed_Data/data.file.path')
list_ua_files = list.files(path = paste0(data.file.path,'GCM_data/historic/ua_monthly/'))

U_200_mod_field = data.frame()

for(m in c(1,4)){
  mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
  mmod_files = list_ua_files[grep(list_ua_files, pattern = mmod[m])]
  
  for(file_num in 1:length(mmod_files)){
    ua_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/ua_monthly/',mmod_files[file_num]))
    
    # extract time and lat/on from the file
    time = ncvar_get(ua_ncdf, varid = "time", start = c(1), count = c(-1))
    lat = ncvar_get(ua_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(ua_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    plev = ncvar_get(ua_ncdf, varid = "plev", start = c(1), count = c(-1))
    
    
    tmp0 = melt(ncvar_get(ua_ncdf, 
                          varid = "ua", 
                          start = c(1,1,which(plev == 20000),1), 
                          count = c(length(lon),length(lat),1,length(time))))
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }
  tmp$model = m
  
  if(m == 1){U_200_mod_field = tmp}
  if(m > 1){U_200_mod_field = rbind(U_200_mod_field,tmp)}	
}

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 12),"-",rep(1:12,(2005-1950+1)),"-","01")))

# how many GCM cells exist in the study region
num.cells = nrow(unique(U_200_mod_field[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
U_200_mod_field$date = rep(rep(date,each = num.cells),2)
U_200_mod_field$lon = lon[U_200_mod_field$Var1]
U_200_mod_field$lat = lat[U_200_mod_field$Var2]

U_200_mod_field = U_200_mod_field %>% dplyr::mutate(season = GetSeasonDate(date),
                                                    u_200 = value) %>%
  dplyr::select(c(date, season, lon, lat, model, u_200))


U_200_mod_MAM = U_200_mod_field %>% dplyr::filter(season %in% c("MAM"))
save(U_200_mod_MAM, file = 'Processed_Data/U_200_mod_MAM.RData')

rm(list = ls())
