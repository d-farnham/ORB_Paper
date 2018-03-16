source("R/GetSeasonDate.R")

# define the spatial domain for the REP events
pr_box = data.frame(lat.min = 36,
										lat.max = 42,
										lon.min = 270,
										lon.max = 282.5)

save(pr_box, file = 'Processed_Data/pr_box.RData')

# save the path to all of the raw data
data.file.path = 'Raw_Data/'
save(data.file.path, file = 'Processed_Data/data.file.path')

# next let's load the GCM precipitation data
list_pr_files = list.files(path = paste0(data.file.path,'GCM_data/historic/pr/'))

pr_mod = data.frame()

for(m in 1:5){
	mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
	mmod_files = list_pr_files[grep(list_pr_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		pr_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/pr/',mmod_files[file_num]))
		
		# extract time and lat/lon from the file
		time = ncvar_get(pr_ncdf, varid = "time", start = c(1), count = c(-1))
		lat = ncvar_get(pr_ncdf, varid = "lat", start = c(1), count = c(-1))
		lon = ncvar_get(pr_ncdf, varid = "lon", start = c(1), count = c(-1))
		
		start_lat = which(lat > pr_box$lat.min & lat < pr_box$lat.max)[1]
		count_lat = sum(lat > pr_box$lat.min & lat < pr_box$lat.max)
		
		start_lon = which(lon > pr_box$lon.min & lon < pr_box$lon.max)[1]
		count_lon = sum(lon > pr_box$lon.min & lon < pr_box$lon.max)
		
		
		tmp0 = melt(ncvar_get(pr_ncdf, 
													varid = "pr", 
													start = c(start_lon,start_lat,1), 
													count = c(count_lon,count_lat,length(time))))
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){pr_mod = tmp}
	if(m > 1){pr_mod = rbind(pr_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
													day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic runs
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(pr_mod[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
pr_mod$date = rep(rep(date,each = num.cells),length(mmod))
pr_mod$lon = lon[which(lon > pr_box$lon.min & lon < pr_box$lon.max)][pr_mod$Var1]
pr_mod$lat = lat[which(lat > pr_box$lat.min & lat < pr_box$lat.max)][pr_mod$Var2]

pr_mod = pr_mod %>% dplyr::select(date,lon,lat,model,value)
save(pr_mod, file = 'Processed_Data/pr_mod.RData')

# save the GCM grid so that we can regrid the CPC data to match it
mod_ORB_grid = data.frame(expand.grid(lon = unique(pr_mod$lon), 
																			lat = unique(pr_mod$lat)),
													            cell = 1:(length(unique(pr_mod$lat))*length(unique(pr_mod$lon))))
save(mod_ORB_grid, file = 'Processed_Data/mod_ORB_grid.RData')


# now load and save the CPC precipitation data
rm(list = ls())
source("R/GetSeasonDate.R")
load(file = 'Processed_Data/pr_box.RData')
load(file = 'Processed_Data/data.file.path')

# load the CPC reanalysis precipitation
pr_CPC_output = nc_open(paste0(data.file.path,'REANALYSIS_data/pr_CPC_daily.nc'))


time = ncvar_get(pr_CPC_output, varid = "time", start = c(1), count = c(-1))
lat = ncvar_get(pr_CPC_output, varid = "lat", start = c(1), count = c(-1))
lon = ncvar_get(pr_CPC_output, varid = "lon", start = c(1), count = c(-1))


# formatting of pr is float prate[X,Y,T] 
start_time = which(as.Date(unique(time), origin = "1948-01-01") > as.Date("1949-12-31") &
									 as.Date(unique(time), origin = "1948-01-01") < as.Date("2006-01-01"))[1]
count_time = sum(as.Date(unique(time), origin = "1948-01-01") > as.Date("1949-12-31") &
								 as.Date(unique(time), origin = "1948-01-01") < as.Date("2006-01-01"))

start_lat = which(lat > pr_box$lat.min & lat < pr_box$lat.max)[1]
count_lat = sum(lat > pr_box$lat.min & lat < pr_box$lat.max)

start_lon = which(lon > pr_box$lon.min & lon < pr_box$lon.max)[1]
count_lon = sum(lon > pr_box$lon.min & lon < pr_box$lon.max)

pr_CPC = ncvar_get(pr_CPC_output, 
									 varid = "prcp", 
									 start = c(start_lon,start_lat,start_time), 
									 count = c(count_lon,count_lat,count_time))

pr_CPC_long = melt(pr_CPC)


# now we need to upscale the CPC data to match the grid of the GCM
lats_CPC = lat[start_lat:(start_lat + count_lat - 1)]
lons_CPC = lon[start_lon:(start_lon + count_lon - 1)]
dates_CPC = as.Date(time[start_time:(start_time + count_time - 1)], origin = "1948-01-01")

pr_CPC = pr_CPC_long %>% dplyr::mutate(lon = lons_CPC[Var1],
																			 lat = lats_CPC[Var2],
																			 date = dates_CPC[Var3]) %>%
												 dplyr::select(lon, lat, date, value) 

# save the CPC grid
CPC_grid = unique(pr_CPC[,c("lat", "lon")])
save(CPC_grid, file = 'Processed_Data/CPC_grid.RData')

# assign each CPC cell to a model cell
load(file = 'Processed_Data/mod_ORB_grid.RData')

source("R/GetcellnumberFromlatlon.R")

CPC_mod_cell = data.frame(pr_CPC[1:(count_lon*count_lat) , c("lat", "lon")])
CPC_mod_cell$cell = NA
for(ii in 1:(nrow(CPC_mod_cell))){
	CPC_mod_cell$cell[ii] = GetcellnumberFromlatlon(lat = CPC_mod_cell$lat[ii], 
																									lon = CPC_mod_cell$lon[ii], 
																									mod_lat_lon_cell = mod_ORB_grid)
}

pr_CPC$cell = rep(CPC_mod_cell$cell, length(dates_CPC))

# regrid the CPC onto the GDM grid
CPC_mod_cell_pr = pr_CPC %>% dplyr::group_by(cell, date) %>%
														 dplyr::summarise(value = mean(value, na.rm = TRUE))
save(CPC_mod_cell_pr, file = 'Processed_Data/CPC_mod_cell_pr.RData')


# Now calculate the REPs
rm(list = ls())
source("R/GetSeasonDate.R")


# compare the 99th percentiles in the model and the CPC
load(file = 'Processed_Data/CPC_mod_cell_pr.RData')
load(file = 'Processed_Data/pr_mod.RData')

ninety_ninth_p_mod = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::summarise(cut = quantile(value*24*60*60, probs = 0.99)) %>%
	dplyr::group_by(model) %>%
	dplyr::summarise(cut_min = min(cut),
									 cut_max = max(cut),
									 cut_med = median(cut))

ninety_ninth_p_CPC = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::summarise(cut = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::group_by() %>%
	dplyr::summarise(cut_min = min(cut),
									 cut_max = max(cut),
									 cut_med = median(cut))

# choose the proportion of cells that must be extreme for a REP to occur
REP_threshold = 3/15
save(REP_threshold, file = "Processed_Data/REP_threshold")

# now calculate the REPs for the CPC
load(file = 'Processed_Data/CPC_mod_cell_pr.RData')

# Calculate and save the REPs based on the REP_threshold 
CPC_mod_cell_REP = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date) %>%
	dplyr::summarise(REP = ifelse((mean(xtr, na.rm = TRUE)) > REP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(CPC_mod_cell_REP, file = 'Processed_Data/CPC_mod_cell_REP.RData')

# save the CPC thresholds
CPC_xtr_thresh = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::summarise(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::select(cell,xtr_thresh) %>%
	data.frame()

save(CPC_xtr_thresh, file = "Processed_Data/CPC_xtr_thresh.RData")

# Calculate and save the number of cell-based extreme precip days per day
CPC_mod_cell_EP = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date) %>%
	dplyr::summarise(EP = sum(xtr)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(CPC_mod_cell_EP, file = 'Processed_Data/CPC_mod_cell_EP.RData')


# now calculate the REPs for the GCM
load(file = "Processed_Data/pr_mod.RData")

mod_REP = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(REP = ifelse((sum(xtr)/length(xtr)) > REP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(mod_REP, file = 'Processed_Data/mod_REP.RData')

# Calculate and save the number of cell-based intense precip days per day
mod_EP = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(EP = sum(xtr)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(mod_EP, file = 'Processed_Data/mod_EP.RData')

# now re-calculate the REPs for the GCM but using the thresholds calculated from the CPC precip
load(file = 'Processed_Data/CPC_xtr_thresh.RData')
load(file = 'Processed_Data/mod_ORB_grid.RData')

mod_CPC_thresh_pr = merge(merge(pr_mod, mod_ORB_grid, by = c("lon","lat"), all = TRUE),
													CPC_xtr_thresh, by = "cell", all = TRUE)

					
mod_CPC_thresh_REP = mod_CPC_thresh_pr %>% dplyr::group_by(cell,model) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh/60/60/24, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(REP = ifelse((sum(xtr)/length(xtr)) > REP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))


save(mod_CPC_thresh_REP, file = 'Processed_Data/mod_CPC_thresh_REP.RData')


# check the precipitation on the REP and EP days
rm(list = ls())
source("R/GetSeasonDate.R")

# only retain a protion of the precipitation data
# define the spatial domain here
pr_extent = data.frame(lat.min = c(25),
											 lat.max = c(50),
											 lon.min = c(260),
											 lon.max = c(300))

save(pr_extent, file = "Processed_Data/pr_extent.RData")

# load the record of when any REPs happened inside the ORB 
load('Processed_Data/CPC_mod_cell_REP.RData')

REP_same_day_dates_obs = CPC_mod_cell_REP %>% dplyr::filter(REP == 1) %>%
	data.frame() %>%
	dplyr::mutate(date = date) 

EP_min = 0

# also load record of when any xtrs happened inside the ORB 
load(file = 'Processed_Data/CPC_mod_cell_EP.RData')

EP_dates_obs = CPC_mod_cell_EP %>% dplyr::filter(EP > EP_min) %>%
	data.frame()

# load the CPC reanalysis precipitation and re-grid to the GCM resolution
load(file = 'Processed_Data/data.file.path')
pr_CPC_output = nc_open(paste0(data.file.path,'REANALYSIS_data/pr_CPC_daily.nc'))

time = ncvar_get(pr_CPC_output, varid = "time", start = c(1), count = c(-1))
lat = ncvar_get(pr_CPC_output, varid = "lat", start = c(1), count = c(-1))
lon = ncvar_get(pr_CPC_output, varid = "lon", start = c(1), count = c(-1))

start_time = which(as.Date(unique(time), origin = "1948-01-01") > as.Date("1949-12-31") &
									 	as.Date(unique(time), origin = "1948-01-01") < as.Date("2006-01-01"))[1]
count_time = sum(as.Date(unique(time), origin = "1948-01-01") > as.Date("1949-12-31") &
								 	as.Date(unique(time), origin = "1948-01-01") < as.Date("2006-01-01"))

start_lat = which(lat > pr_extent$lat.min & lat < pr_extent$lat.max)[1]
count_lat = sum(lat > pr_extent$lat.min & lat < pr_extent$lat.max)

start_lon = which(lon > pr_extent$lon.min & lon < pr_extent$lon.max)[1]
count_lon = sum(lon > pr_extent$lon.min & lon < pr_extent$lon.max)

pr_CPC = ncvar_get(pr_CPC_output, 
									 varid = "prcp", 
									 start = c(start_lon,start_lat,start_time), 
									 count = c(count_lon,count_lat,count_time))

pr_CPC_long = melt(pr_CPC)
rm(pr_CPC)

lats_CPC = lat[start_lat:(start_lat + count_lat - 1)]
lons_CPC = lon[start_lon:(start_lon + count_lon - 1)]
dates_CPC = as.Date(time[start_time:(start_time + count_time - 1)], origin = "1948-01-01")

pr_CPC = pr_CPC_long %>% dplyr::mutate(lon = lons_CPC[Var1],
																			 lat = lats_CPC[Var2],
																			 date = dates_CPC[Var3]) %>%
	dplyr::select(lon, lat, date, value) 
rm(pr_CPC_long)


# now upscale to match the spatial scale of the GCM
load(file = "Processed_Data/pr_extent.RData")

pr_CPC_coarse = pr_CPC %>% dplyr::mutate(lat = 2*round((lat+1)/2) - 1,
																				 lon =  2.5*round((lon+1.25)/2.5) - 1.25) %>%
	dplyr::group_by(lat, lon, date) %>%
	dplyr::summarise(value = mean(value, na.rm = TRUE))

# now compute the percentile of precip for each cell for each day
source('R/prank.R')
pr_CPC_coarse_percentile = pr_CPC_coarse %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::group_by(lat, lon) %>%
	dplyr::mutate(pr_percentile = prank(value))

save(pr_CPC_coarse_percentile, file = 'Processed_Data/pr_CPC_coarse_percentile.RData')

load(file = 'Processed_Data/pr_CPC_coarse_percentile.RData')

# now compute the mean of precipitation precentile for all REP days
pr_CPC_REP_same_day = pr_CPC_coarse_percentile %>% dplyr::filter(date %in%  c(REP_same_day_dates_obs$date)) %>%
	dplyr::group_by(lat,lon,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))
pr_CPC_REP_same_day$model = "OBS"

pr_CPC_REP_same_day = pr_CPC_REP_same_day %>% dplyr::filter(lat < pr_extent$lat.max &
																															lon < pr_extent$lon.max &
																															lon > pr_extent$lon.min)

pr_CPC_REP_same_day = pr_CPC_REP_same_day %>% data.frame()

save(pr_CPC_REP_same_day, file = 'Processed_Data/pr_CPC_REP_same_day.RData')


# now compute the mean of precipitation precentile for all EP days
pr_CPC_EP = pr_CPC_coarse_percentile %>% dplyr::filter(date %in%  c(EP_dates_obs$date)) %>%
	dplyr::group_by(lat,lon,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))
pr_CPC_EP$model = "OBS"

pr_CPC_EP = pr_CPC_EP %>% dplyr::filter(lat < pr_extent$lat.max &
																				lon < pr_extent$lon.max &
																				lon > pr_extent$lon.min)

pr_CPC_EP = pr_CPC_EP %>% data.frame()

save(pr_CPC_EP, file = 'Processed_Data/pr_CPC_EP_US.RData')


# load the GCM precipitation
load(file = 'Processed_Data/data.file.path')
list_pr_files = list.files(path = paste0(data.file.path,'GCM_data/historic/pr/'))

pr_mod = data.frame()

for(m in 1:5){
	mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
	mmod_files = list_pr_files[grep(list_pr_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		pr_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/pr/',mmod_files[file_num]))
		
		# extract time and lat/on from the file
		time = ncvar_get(pr_ncdf, varid = "time", start = c(1), count = c(-1))
		lat = ncvar_get(pr_ncdf, varid = "lat", start = c(1), count = c(-1))
		lon = ncvar_get(pr_ncdf, varid = "lon", start = c(1), count = c(-1))
		
		start_lat = which(lat > pr_extent$lat.min & lat < pr_extent$lat.max)[1]
		count_lat = sum(lat > pr_extent$lat.min & lat < pr_extent$lat.max)
		
		start_lon = which(lon > pr_extent$lon.min & lon < pr_extent$lon.max)[1]
		count_lon = sum(lon > pr_extent$lon.min & lon < pr_extent$lon.max)
		
		
		tmp0 = melt(ncvar_get(pr_ncdf,
													varid = "pr",
													start = c(start_lon,start_lat,1),
													count = c(count_lon,count_lat,length(time))))
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){pr_mod = tmp}
	if(m > 1){pr_mod = rbind(pr_mod,tmp)}
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
												 day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(pr_mod[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
pr_mod$date = rep(rep(date,each = num.cells),length(mmod))
pr_mod$lon = lon[which(lon > pr_extent$lon.min & lon < pr_extent$lon.max)][pr_mod$Var1]
pr_mod$lat = lat[which(lat > pr_extent$lat.min & lat < pr_extent$lat.max)][pr_mod$Var2]

pr_mod = pr_mod %>% dplyr::select(date,lon,lat,model,value)

# now compute the mean of precipitation precentile for all REP and EP days for the GCM
load(file = 'Processed_Data/mod_REP.RData')
REP_same_day_dates_mod = mod_REP  %>% dplyr::filter(REP == 1) %>%
	data.frame() %>%
	dplyr::mutate(date = date) 

EP_min = 0
load(file = 'Processed_Data/mod_EP.RData')
EP_dates_mod = mod_EP %>% dplyr::filter(EP > EP_min) %>%
	data.frame()

source('R/prank.R')

pr_mod_percentile = pr_mod  %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::group_by(lat, lon, model) %>%
	dplyr::mutate(pr_percentile = prank(value)) %>%
  dplyr::filter(season == "MAM")

save(pr_mod_percentile, file = 'Processed_Data/pr_mod_percentile.RData')


# REP same day
pr_mod_REP_same_day = merge(pr_mod_percentile %>% dplyr::select(-c(value)), REP_same_day_dates_mod, by = c("date", "model", "season")) 

pr_mod_REP_same_day = pr_mod_REP_same_day %>% dplyr::group_by(lat,lon,model,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))

pr_mod_REP_same_day = pr_mod_REP_same_day %>% data.frame() %>% 
	dplyr::filter(lat < pr_extent$lat.max &
									lon < pr_extent$lon.max &
									lon > pr_extent$lon.min)

pr_mod_REP_same_day = pr_mod_REP_same_day %>% dplyr::mutate(model = paste0("GCM ", model))

save(pr_mod_REP_same_day, file = 'Processed_Data/pr_mod_REP_same_day.RData')


# now for the EP
pr_mod_EP = merge(pr_mod_percentile, EP_dates_mod, by = c("date", "model", "season")) 

pr_mod_EP = pr_mod_EP %>% dplyr::group_by(lat,lon,model,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))

pr_mod_EP = pr_mod_EP %>% data.frame() %>% 
	dplyr::filter(lat < pr_extent$lat.max &
									lon < pr_extent$lon.max &
									lon > pr_extent$lon.min)

pr_mod_EP = pr_mod_EP %>% dplyr::mutate(model = paste0("GCM ", model))

save(pr_mod_EP, file = 'Processed_Data/pr_mod_EP_US.RData')

# load the the RCP 8.5 data
# start with the precipitation
rm(list = ls())
source("R/GetSeasonDate.R")

# load the precipitation box
load(file = 'Processed_Data/pr_box.RData')


# next let's load and evaluate the GCM precipitation data
# 8.5
load(file = 'Processed_Data/data.file.path')
list_pr_files = list.files(path = paste0(data.file.path,'GCM_data/future GFDL CM3 RCP 8.5/pr/'))

pr_mod = data.frame()

for(m in 1){
  mmod = c("r1i")
  mmod_files = list_pr_files[grep(list_pr_files, pattern = mmod[m])]
  
  for(file_num in 1:length(mmod_files)){
    pr_ncdf = nc_open(paste0(data.file.path,'GCM_data/future GFDL CM3 RCP 8.5/pr/',mmod_files[file_num]))
    
    # extract time and lat/on from the file
    time = ncvar_get(pr_ncdf, varid = "time", start = c(1), count = c(-1))
    lat = ncvar_get(pr_ncdf, varid = "lat", start = c(1), count = c(-1))
    lon = ncvar_get(pr_ncdf, varid = "lon", start = c(1), count = c(-1))
    
    start_lat = which(lat > pr_box$lat.min & lat < pr_box$lat.max)[1]
    count_lat = sum(lat > pr_box$lat.min & lat < pr_box$lat.max)
    
    start_lon = which(lon > pr_box$lon.min & lon < pr_box$lon.max)[1]
    count_lon = sum(lon > pr_box$lon.min & lon < pr_box$lon.max)
    
    
    tmp0 = melt(ncvar_get(pr_ncdf, 
                          varid = "pr", 
                          start = c(start_lon,start_lat,1), 
                          count = c(count_lon,count_lat,length(time))))
    if(file_num == 1){tmp = tmp0}
    
    if(file_num > 1){tmp = rbind(tmp, tmp0)}
    print(file_num)
  }
  tmp$model = m
  
  if(m == 1){pr_mod = tmp}
  if(m > 1){pr_mod = rbind(pr_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
                         day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(2006:2100, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# how many GCM cells exist in the study region
num.cells = nrow(unique(pr_mod[,c("Var1", "Var2")]))

# replace the date, lon, and lat indices with actual values
pr_mod$date = rep(rep(date,each = num.cells),length(mmod))
pr_mod$lon = lon[which(lon > pr_box$lon.min & lon < pr_box$lon.max)][pr_mod$Var1]
pr_mod$lat = lat[which(lat > pr_box$lat.min & lat < pr_box$lat.max)][pr_mod$Var2]

pr_mod_future = pr_mod %>% dplyr::select(date,lon,lat,model,value)
save(pr_mod_future, file = 'Processed_Data/pr_mod_future.RData')



# compute future REPs -- using the historic ensemble mean of the 99th %-ile
rm(list = ls())
source("R/GetSeasonDate.R")

# get the historic 99th percentile threshold
load(file = 'Processed_Data/pr_mod.RData')

historic_thresh = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
  dplyr::summarise(cut = quantile(value, probs = 0.99)) %>%
  dplyr::group_by(lat,lon) %>%
  dplyr::summarise(xtr_thresh = mean(cut))

load(file = "Processed_Data/REP_threshold")

load(file = 'Processed_Data/pr_mod_future.RData')
mod_REP_future = merge(pr_mod_future, historic_thresh, by = c('lat', 'lon')) %>% 
  dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
  dplyr::group_by(date,model) %>%
  dplyr::summarise(REP = ifelse((sum(xtr)/length(xtr)) > REP_threshold, 1, 0)) %>%
  dplyr::mutate(season = GetSeasonDate(date))

save(mod_REP_future, file = 'Processed_Data/mod_REP_future.RData')

rm(list = ls())

