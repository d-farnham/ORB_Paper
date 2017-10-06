# all codes run in RStudio version 1.0.136 and R version R version 3.3.3
# Older and newer versions of RStudio and R have not been tested
rm(list = ls())
package.list <- list("akima","dataRetrieval","data.table","dplyr","ggmap","ggplot2","gridExtra", 
                     "Kendall","locfit", "lubridate","maps","ncdf4","readr","reshape2","tidyr")
source('R/load_packages.R') # clear workspace, clear console, load packages
source("R/GetSeasonDate.R")

# define the spatial domain for the RIP events
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

GetcellnumberFromlatlon = function(lat, lon, mod_lat_lon_cell){
	tmp_lat_lon = data.frame(lat = lat,
													 lon = lon)
	colnames(tmp_lat_lon) = c("lat", "lon")
	tmp_dist = as.matrix(dist(rbind(tmp_lat_lon,mod_lat_lon_cell[,c("lat","lon")])))[-1,1]
	
	return(mod_lat_lon_cell$cell[which.min(tmp_dist)])
}

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


# now load the streamflow data
rm(list = ls())
source("R/GetSeasonDate.R")

# set the minimum drainage area in sq. miles (NOTE: 1 sq. mile = 2.59 sq. km; so 5792 sq. mile is about 15000 sq. km)
min.da = 5792

# set the maximum number of missing days in the streamflow record
max.miss.day = 25

load(file = 'Processed_Data/data.file.path')

# NOTE: total area of ORB ~ 189,423 mi²
# load the streamflow stations with drainage areas larger than 'min.da'
Ohio <- read.csv(paste0(data.file.path,'STREAMFLOW_data/HUC5_HCDN_gages.csv'),skip=15)
pCode <- "00060"
start.date <- "1950-01-01"
end.date <- "2005-12-31"
siteNo <- paste("0",Ohio[Ohio[,"D.A."] > min.da,"Number"],sep="")
OhioHCDN <- lapply(siteNo, function(x) setNames(readNWISdv(siteNumber=x,parameterCd=pCode,startDate=start.date,endDate=end.date)[,c("Date","X_00060_00003")],c("dates","streamflow")))
HCDN <- c(OhioHCDN)
shortname <- c(as.vector(Ohio[Ohio[,"D.A."] > min.da,"Short.name"]))
names(HCDN) <- shortname


# no gages with more than 'max.miss.day' days of missing data
HCDN <- HCDN[!(as.vector(unlist(lapply(HCDN, function(x) nrow(x)))) < (length(seq(as.Date(start.date),as.Date(end.date),by="day")) - max.miss.day))]
HCDNgageinfo <- rbind(Ohio)[c(as.vector(Ohio[,"Short.name"])) %in% names(HCDN),]

# convert to decimal lat/lon and add drainage area in sq km
HCDNgagecoords <- HCDNgageinfo[,c("Latitude","Longitude")]
HCDNgagecoords <- apply(HCDNgagecoords,MAR=2,function(x) as.character(x))
HCDNgagecoords <- cbind(Latitude=sapply(HCDNgagecoords[,"Latitude"],function(x) as.numeric(substr(x, 1,2)) + as.numeric(substr(x, 4,5))/60 + as.numeric(substr(x, 7,8))/3600, USE.NAMES=F),
												Longitude=sapply(HCDNgagecoords[,"Longitude"],function(x) -1*(as.numeric(substr(x, 2,3)) + as.numeric(substr(x, 5,6))/60 + as.numeric(substr(x, 8,9))/3600), USE.NAMES=F))
rownames(HCDNgagecoords) <- as.vector(HCDNgageinfo[,"Short.name"])
HCDNgagearea <- HCDNgageinfo[,c("Short.name","D.A.","Datum")]
rownames(HCDNgagearea) <- as.vector(HCDNgageinfo[,"Short.name"])
gage_info = merge(HCDNgagearea, HCDNgagecoords, by = "row.names")

# transform sq mi to sq km
gage_info = gage_info %>% dplyr::mutate(da_sq_km = D.A. * 2.58999)

save(HCDN, file = 'Processed_Data/HCDN.RData')
save(gage_info, file = 'Processed_Data/gage_info.RData')


# Now calculate the RIPs
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

# choose the proportion of cells that must be extreme for a RIP to occur
RIP_threshold = 3/15
save(RIP_threshold, file = "Processed_Data/RIP_threshold")

# now calculate the RIPs for the CPC
load(file = 'Processed_Data/CPC_mod_cell_pr.RData')

# Calculate and save the RIPs based on the RIP_threshold 
CPC_mod_cell_RIP = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date) %>%
	dplyr::summarise(RIP = ifelse((mean(xtr, na.rm = TRUE)) > RIP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(CPC_mod_cell_RIP, file = 'Processed_Data/CPC_mod_cell_RIP.RData')

# save the CPC thresholds
CPC_xtr_thresh = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::summarise(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::select(cell,xtr_thresh) %>%
	data.frame()

save(CPC_xtr_thresh, file = "Processed_Data/CPC_xtr_thresh.RData")

# Calculate and save the number of cell-based intense precip days per day
CPC_mod_cell_IP = CPC_mod_cell_pr %>% dplyr::group_by(cell) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99, na.rm = TRUE)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date) %>%
	dplyr::summarise(IP = sum(xtr)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(CPC_mod_cell_IP, file = 'Processed_Data/CPC_mod_cell_IP.RData')


# now calculate the RIPs for the GCM
load(file = "Processed_Data/pr_mod.RData")

mod_RIP = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(RIP = ifelse((sum(xtr)/length(xtr)) > RIP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(mod_RIP, file = 'Processed_Data/mod_RIP.RData')

# Calculate and save the number of cell-based intense precip days per day
mod_IP = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::mutate(xtr_thresh = quantile(value, probs = 0.99)) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(IP = sum(xtr)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(mod_IP, file = 'Processed_Data/mod_IP.RData')

# now re-calculate the RIPs for the GCM but using the thresholds calculated from the CPC precip
load(file = 'Processed_Data/CPC_xtr_thresh.RData')
load(file = 'Processed_Data/mod_ORB_grid.RData')

mod_CPC_thresh_pr = merge(merge(pr_mod, mod_ORB_grid, by = c("lon","lat"), all = TRUE),
													CPC_xtr_thresh, by = "cell", all = TRUE)

					
mod_CPC_thresh_RIP = mod_CPC_thresh_pr %>% dplyr::group_by(cell,model) %>%
	dplyr::mutate(xtr = ifelse(value > xtr_thresh/60/60/24, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(RIP = ifelse((sum(xtr)/length(xtr)) > RIP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))


save(mod_CPC_thresh_RIP, file = 'Processed_Data/mod_CPC_thresh_RIP.RData')


# check the precipitation on the RIP and IP days
rm(list = ls())
source("R/GetSeasonDate.R")

# only retain a protion of the precipitation data
# define the spatial domain here
pr_extent = data.frame(lat.min = c(25),
											 lat.max = c(50),
											 lon.min = c(260),
											 lon.max = c(300))

save(pr_extent, file = "Processed_Data/pr_extent.RData")

# load the record of when any RIPs happened inside the ORB 
load('Processed_Data/CPC_mod_cell_RIP.RData')

RIP_same_day_dates_obs = CPC_mod_cell_RIP %>% dplyr::filter(RIP == 1) %>%
	data.frame() %>%
	dplyr::mutate(date = date) 

IP_min = 0

# also load record of when any xtrs happened inside the ORB 
load(file = 'Processed_Data/CPC_mod_cell_IP.RData')

IP_dates_obs = CPC_mod_cell_IP %>% dplyr::filter(IP > IP_min) %>%
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

# now compute the mean of precipitation precentile for all RIP days
pr_CPC_RIP_same_day = pr_CPC_coarse_percentile %>% dplyr::filter(date %in%  c(RIP_same_day_dates_obs$date)) %>%
	dplyr::group_by(lat,lon,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))
pr_CPC_RIP_same_day$model = "OBS"

pr_CPC_RIP_same_day = pr_CPC_RIP_same_day %>% dplyr::filter(lat < pr_extent$lat.max &
																															lon < pr_extent$lon.max &
																															lon > pr_extent$lon.min)

pr_CPC_RIP_same_day = pr_CPC_RIP_same_day %>% data.frame()

save(pr_CPC_RIP_same_day, file = 'Processed_Data/pr_CPC_RIP_same_day.RData')


# now compute the mean of precipitation precentile for all IP days
pr_CPC_IP = pr_CPC_coarse_percentile %>% dplyr::filter(date %in%  c(IP_dates_obs$date)) %>%
	dplyr::group_by(lat,lon,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))
pr_CPC_IP$model = "OBS"

pr_CPC_IP = pr_CPC_IP %>% dplyr::filter(lat < pr_extent$lat.max &
																				lon < pr_extent$lon.max &
																				lon > pr_extent$lon.min)

pr_CPC_IP = pr_CPC_IP %>% data.frame()

save(pr_CPC_IP, file = 'Processed_Data/pr_CPC_IP_US.RData')


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

# now compute the mean of precipitation precentile for all RIP and IP days for the GCM
load(file = 'Processed_Data/mod_RIP.RData')
RIP_same_day_dates_mod = mod_RIP  %>% dplyr::filter(RIP == 1) %>%
	data.frame() %>%
	dplyr::mutate(date = date) 

IP_min = 0
load(file = 'Processed_Data/mod_IP.RData')
IP_dates_mod = mod_IP %>% dplyr::filter(IP > IP_min) %>%
	data.frame()

source('R/prank.R')

pr_mod_percentile = pr_mod  %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::group_by(lat, lon, model) %>%
	dplyr::mutate(pr_percentile = prank(value)) %>%
  dplyr::filter(season == "MAM")

save(pr_mod_percentile, file = 'Processed_Data/pr_mod_percentile.RData')


# RIP same day
pr_mod_RIP_same_day = merge(pr_mod_percentile %>% dplyr::select(-c(value)), RIP_same_day_dates_mod, by = c("date", "model", "season")) 

pr_mod_RIP_same_day = pr_mod_RIP_same_day %>% dplyr::group_by(lat,lon,model,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))

pr_mod_RIP_same_day = pr_mod_RIP_same_day %>% data.frame() %>% 
	dplyr::filter(lat < pr_extent$lat.max &
									lon < pr_extent$lon.max &
									lon > pr_extent$lon.min)

pr_mod_RIP_same_day = pr_mod_RIP_same_day %>% dplyr::mutate(model = paste0("GCM ", model))

save(pr_mod_RIP_same_day, file = 'Processed_Data/pr_mod_RIP_same_day.RData')


# now for the IP
pr_mod_IP = merge(pr_mod_percentile, IP_dates_mod, by = c("date", "model", "season")) 

pr_mod_IP = pr_mod_IP %>% dplyr::group_by(lat,lon,model,season) %>%
	dplyr::summarise(mean_pr_percentile = mean(pr_percentile, na.rm = TRUE))

pr_mod_IP = pr_mod_IP %>% data.frame() %>% 
	dplyr::filter(lat < pr_extent$lat.max &
									lon < pr_extent$lon.max &
									lon > pr_extent$lon.min)

pr_mod_IP = pr_mod_IP %>% dplyr::mutate(model = paste0("GCM ", model))

save(pr_mod_IP, file = 'Processed_Data/pr_mod_IP_US.RData')


# now load and save teh Z_700, PR_WAT, and T_700 fields
rm(list = ls())
source('R/GetSeasonDate.R')

# load and save Z_700 and PR_WAT and T_700 fields
# first Z_700
load(file = 'Processed_Data/data.file.path')
reanal_output = nc_open(paste0(data.file.path,'REANALYSIS_data/zg_700mb_REANALYSIS.nc'))

time = ncvar_get(reanal_output, varid = "T", start = c(1), count = c(-1))
date = as.Date(time, origin = "1948-01-01 00:00:00")

lat = ncvar_get(reanal_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(reanal_output, varid = "X", start = c(1), count = c(-1))

start_date = which(date > "1949-12-31" &
									 	date < "2006-01-01")[1]

count_date = sum(date > "1949-12-31" &
								 	date < "2006-01-01")

Z_700 = melt(ncvar_get(reanal_output, 
											 varid = "phi", 
											 start = c(1,1,1,start_date), 
											 count = c(length(lon),length(lat),1,count_date)))

Z_700 = Z_700 %>% dplyr::mutate(lon = lon[Var1],
																lat = lat[Var2],
																date = date[which(date > "1949-12-31" &
																										date < "2006-01-01")][Var3],
																season = GetSeasonDate(date),
																z_700 = value) %>%
	dplyr::select(c(date, season, lon, lat, z_700))

# retain for DJF and MAM b/c events in early Mar will require Feb data for lag composites
Z_700 = Z_700 %>% dplyr::filter(season %in% c("DJF", "MAM"))
save(Z_700, file = 'Processed_Data/Z_700.RData')

# clear workspace
rm(list = ls())
source('R/GetSeasonDate.R')
# now load the PR_WAT
load(file = 'Processed_Data/data.file.path')
reanal_output = nc_open(paste0(data.file.path,'REANALYSIS_data/pr_wat_col_REANALYSIS.nc'))

time = ncvar_get(reanal_output, varid = "T", start = c(1), count = c(-1))
date = as.Date(time, origin = "1948-01-01 00:00:00")

lat = ncvar_get(reanal_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(reanal_output, varid = "X", start = c(1), count = c(-1))

start_date = which(date > "1949-12-31" &
									 	date < "2006-01-01")[1]

count_date = sum(date > "1949-12-31" &
								 	date < "2006-01-01")

PR_WAT = melt(ncvar_get(reanal_output, 
												varid = "PWAT", 
												start = c(1,1,start_date), 
												count = c(length(lon),length(lat),count_date)))

PR_WAT = PR_WAT %>% dplyr::mutate(lon = lon[Var1],
																	lat = lat[Var2],
																	date = date[which(date > "1949-12-31" &
																										date < "2006-01-01")][Var3],
																	season = GetSeasonDate(date),
																	pr_wat = value) %>%
	dplyr::select(c(date, season, lon, lat, pr_wat))

# set the values < 0 to 0
PR_WAT = PR_WAT %>% dplyr::mutate(pr_wat = ifelse(pr_wat < 0, 0, pr_wat))

PR_WAT = PR_WAT %>% dplyr::filter(season %in% c("DJF", "MAM"))
save(PR_WAT, file = 'Processed_Data/PR_WAT.RData')


# clear workspace
rm(list = ls())
source('R/GetSeasonDate.R')
# now load the QA_700
load(file = 'Processed_Data/data.file.path')
reanal_output = nc_open(paste0(data.file.path,'REANALYSIS_data/qa_700mb_REANALYSIS.nc'))

time = ncvar_get(reanal_output, varid = "T", start = c(1), count = c(-1))
date = as.Date(time, origin = "1948-01-01 00:00:00")

lat = ncvar_get(reanal_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(reanal_output, varid = "X", start = c(1), count = c(-1))

start_date = which(date > "1949-12-31" &
									 date < "2006-01-01")[1]

count_date = sum(date > "1949-12-31" &
								 date < "2006-01-01")

QA_700 = melt(ncvar_get(reanal_output, 
											 varid = "qa", 
											 start = c(1,1,1,start_date), 
											 count = c(length(lon),length(lat),1,count_date)))

QA_700 = QA_700 %>% dplyr::mutate(lon = lon[Var1],
																lat = lat[Var2],
																date = date[which(date > "1949-12-31" &
																										date < "2006-01-01")][Var3],
																season = GetSeasonDate(date),
																qa_700 = value) %>%
	dplyr::select(c(date, season, lon, lat, qa_700))

QA_700 = QA_700 %>% dplyr::filter(season %in% c("DJF", "MAM"))
save(QA_700, file = 'Processed_Data/QA_700.RData')


# now load the model Z_700 field
rm(list = ls())
source('R/GetSeasonDate.R')
# now load the zg_700 field from the GCM to compare the composites from the GCM and the observations
load(file = 'Processed_Data/data.file.path')
list_zg_files = list.files(path = paste0(data.file.path,'GCM_data/historic/zg/'))

Z_700_mod_field = data.frame()

# define the retent of Z_700 field to retain
min.lat = 14
max.lat = 60
min.lon = 205
max.lon = 315
for(m in 1:5){
	mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
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
Z_700_mod_field$date = rep(rep(date,each = num.cells),length(mmod))

# can get rid of non MAM months
Z_700_mod_field = Z_700_mod_field %>% dplyr::filter(lubridate::month(date) %in% c(3,4,5))

# can get rid of lats > max.lat and lat < min.lat and lon < min.lon and lon > max.lon for space
Z_700_mod_field$lat = lat[which(lat > min.lat & lat < max.lat)][Z_700_mod_field$Var2]
Z_700_mod_field$lon = lon[which(lon > min.lon & lon < max.lon)][Z_700_mod_field$Var1]


Z_700_mod_field = Z_700_mod_field %>% dplyr::select(date,lon,lat,model,value)

save(Z_700_mod_field, file = 'Processed_Data/Z_700_mod_field.RData')


# now load and save the U_200 monthly fields
rm(list = ls())
source('R/GetSeasonDate.R')

# load and save the U_200 field -- monthly data
load(file = 'Processed_Data/data.file.path')
reanal_output = nc_open(paste0(data.file.path,'REANALYSIS_data/u_200_REANALYSIS.nc'))

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


# now load the U_200 GCM monthly data
rm(list = ls())
source('R/GetSeasonDate.R')
load(file = 'Processed_Data/data.file.path')
list_ua_files = list.files(path = paste0(data.file.path,'GCM_data/historic/ua_monthly/'))

U_200_mod_field = data.frame()

for(m in 1:5){
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
U_200_mod_field$date = rep(rep(date,each = num.cells),length(mmod))
U_200_mod_field$lon = lon[U_200_mod_field$Var1]
U_200_mod_field$lat = lat[U_200_mod_field$Var2]

U_200_mod_field = U_200_mod_field %>% dplyr::mutate(season = GetSeasonDate(date),
																										u_200 = value) %>%
	dplyr::select(c(date, season, lon, lat, model, u_200))


U_200_mod_MAM = U_200_mod_field %>% dplyr::filter(season %in% c("MAM"))
save(U_200_mod_MAM, file = 'Processed_Data/U_200_mod_MAM.RData')


# now compute and save the EWD and QA indices
rm(list = ls())
source("R/GetSeasonDate.R")

# define the EWD index boxes
EWD_boxes = data.frame(box = c('east', 'west'),
											 lat.min = c(30, 30),
											 lat.max = c(45, 45),
											 lon.min = c(-77.5, -102.5),
											 lon.max = c(-62.5, -87.5))

save(EWD_boxes, file = 'Processed_Data/EWD_boxes.RData')

# load and process the Z_700 data
load(file = 'Processed_Data/data.file.path')
list_zg_files = list.files(path = paste0(data.file.path,'/GCM_data/historic/zg/'))

EWD_mod = data.frame()

for(m in 1:5){
	mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
	mmod_files = list_zg_files[grep(list_zg_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		zg_ncdf = nc_open(paste0(data.file.path,'GCM_data/historic/zg/',mmod_files[file_num]))
		
		# extract time and lat/on from the file
		time = ncvar_get(zg_ncdf, varid = "time", start = c(1), count = c(-1))
		lat = ncvar_get(zg_ncdf, varid = "lat", start = c(1), count = c(-1))
		lon = ncvar_get(zg_ncdf, varid = "lon", start = c(1), count = c(-1))
		lon[lon > 180] = lon[lon > 180] - 360
		plev = ncvar_get(zg_ncdf, varid = "plev", start = c(1), count = c(-1))
		
		
		start_lat_east = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
													 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]
		count_lat_east = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
												 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]
		
		start_lat_west = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
													 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]
		count_lat_west = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
												 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]
		
		start_lon_east = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
													 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]
		count_lon_east = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
												 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]
		
		start_lon_west = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
													 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]
		count_lon_west = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
												 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]
		
		
		# zg[lon,lat,plev,time]
		EWD_east_box = melt(ncvar_get(zg_ncdf, 
																	varid = "zg", 
																	start = c(start_lon_east,start_lat_east,which(plev == 70000),1), 
																	count = c(count_lon_east,count_lat_east,1,length(time))))
		
		EWD_east_box = EWD_east_box %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(east.box = mean(value))
		
		EWD_west_box = melt(ncvar_get(zg_ncdf, 
																	varid = "zg", 
																	start = c(start_lon_west,start_lat_west,which(plev == 70000),1), 
																	count = c(count_lon_west,count_lat_west,1,length(time))))
		
		EWD_west_box = EWD_west_box %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(west.box = mean(value))
		
		tmp0 = cbind(merge(EWD_east_box, EWD_west_box),time) %>% dplyr::select(east.box, west.box, time)
		
		
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){EWD_mod = tmp}
	if(m > 1){EWD_mod = rbind(EWD_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
												 day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# replace the date, lon, and lat indices with actual values
EWD_mod$date = rep(date,length(mmod))
EWD_mod = EWD_mod %>% dplyr::mutate(EWD = east.box - west.box)

EWD_mod = EWD_mod %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::select(date, model, EWD, season)

save(EWD_mod, file = 'Processed_Data/EWD_mod.RData')

# calculate the EWD index from the reananlysis
rm(list = ls())
source("R/GetSeasonDate.R")

load(file = 'Processed_Data/EWD_boxes.RData')

load(file = 'Processed_Data/data.file.path')
obs_output = nc_open(paste0(data.file.path,'REANALYSIS_data/NCAR_zg_700_REANALYSIS.nc'))

time = ncvar_get(obs_output, varid = "T", start = c(1), count = c(-1))
# NOTE: there are no leaps years in this data #

lat = ncvar_get(obs_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(obs_output, varid = "X", start = c(1), count = c(-1))


# formatting of Z is float zg[lon,lat,plev,time] 

start_lat_east = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
											 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]
count_lat_east = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
										 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]

start_lat_west = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
											 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]
count_lat_west = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
										 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]

start_lon_east = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
											 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]
count_lon_east = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
										 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]

start_lon_west = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
											 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]
count_lon_west = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
										 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]

# both boxes are 5 by 5 cells

EWD_east_box = melt(ncvar_get(obs_output, 
															varid = "phi", 
															start = c(start_lon_east,start_lat_east,1,1), 
															count = c(count_lon_east,count_lat_east,1,length(time))))

EWD_east_box = EWD_east_box %>% dplyr::group_by(Var3) %>%
	dplyr::summarise(east.box = mean(value))

EWD_west_box = melt(ncvar_get(obs_output, 
															varid = "phi", 
															start = c(start_lon_west,start_lat_west,1,1), 
															count = c(count_lon_west,count_lat_west,1,length(time))))

EWD_west_box = EWD_west_box %>% dplyr::group_by(Var3) %>%
	dplyr::summarise(west.box = mean(value))


EWD = cbind(merge(EWD_east_box, EWD_west_box, by = "Var3"),time)

EWD = EWD %>% dplyr::mutate(date = as.Date(time, origin = "1948-01-01 00:00:00"),
														EWD = east.box - west.box,
														season = GetSeasonDate(date)) %>%
	dplyr::select(date, EWD, season)


EWD = EWD %>% dplyr::filter(date > "1949-12-31" &
															date < "2006-01-01"	)

save(EWD, file = 'Processed_Data/EWD.RData')


# now compute the QA index
rm(list = ls())
source("R/GetSeasonDate.R")

# Define temp region for QA
QA_box = data.frame(lat.min = c(36),
										 lat.max = c(42),
										 lon.min = c(270),
										 lon.max = c(282.5))


save(QA_box, file = 'Processed_Data/QA_box.RData')

# load and process the qa data
load(file = 'Processed_Data/data.file.path')
list_qa_files = list.files(path = paste0(data.file.path,'GCM_data/historic/hus/'))

QA_mod = data.frame()
# we only have outputs from 2 ensemble members: 1 & 4
for(m in c(1,4)){
	mmod = c("r1i", "r2i", "r3i", "r4i", "r5i")
	mmod_files = list_qa_files[grep(list_qa_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		qa_ncdf = nc_open(paste0(data.file.path,'/GCM_data/historic/hus/',mmod_files[file_num]))
		
		# extract time and lat/on from the file
		lon = ncvar_get(qa_ncdf, varid = "lon", start = c(1), count = c(-1))
		lat = ncvar_get(qa_ncdf, varid = "lat", start = c(1), count = c(-1))
		plev = ncvar_get(qa_ncdf, varid = "plev", start = c(1), count = c(-1))
		time = ncvar_get(qa_ncdf, varid = "time", start = c(1), count = c(-1))
		
		lats_keep = lat[which(lat >= QA_box$lat.min & 
														lat <= QA_box$lat.max)]
		
		start_lat = which(lat >= QA_box$lat.min & 
												lat <= QA_box$lat.max)[1]
		count_lat = sum(lat >= QA_box$lat.min & 
											lat <= QA_box$lat.max)[1]
		
		start_lon = which(lon >= QA_box$lon.min & 
												lon <= QA_box$lon.max)[1]
		count_lon = sum(lon >= QA_box$lon.min & 
											lon <= QA_box$lon.max)[1]
		
		# qa[lon,lat,plev,time]
		QA_700_mod = melt(ncvar_get(qa_ncdf, 
															 varid = "hus", 
															 start = c(start_lon,start_lat,which(plev == 70000),1), 
															 count = c(count_lon,count_lat,1,length(time))))
		

		QA_700_mod = QA_700_mod %>% dplyr::mutate(lat = lats_keep[Var2])
		
		QA_700_mod = QA_700_mod %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(QA_700_mod = mean(value, na.rm = TRUE))
		
		tmp0 = cbind(QA_700_mod,time) 
		
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){QA_mod = tmp}
	if(m > 1){QA_mod = rbind(QA_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
												 day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(1950:2005, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

QA_mod$date = rep(date,2) # only 2 ensemble members for QA

QA_mod = QA_mod %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::select(date, model, QA_700_mod, season)

# define the MHC
QA_mod = QA_mod %>% dplyr::mutate(QA_700 = QA_700_mod) %>%
  dplyr::group_by(season) %>%
	dplyr::mutate(QA_700_clim = mean(QA_700, na.rm = TRUE),
	              QA_700_anom = QA_700 - QA_700_clim)

save(QA_mod, file = 'Processed_Data/QA_mod.RData')

# now compute the QA for the reanalysis
rm(list = ls())
source("R/GetSeasonDate.R")

load(file = 'Processed_Data/QA_box.RData')

# calculate QA from the reananlysis
load(file = 'Processed_Data/data.file.path')
reanal_output = nc_open(paste0(data.file.path,'REANALYSIS_data/qa_700mb_REANALYSIS.nc'))

time = ncvar_get(reanal_output, varid = "T", start = c(1), count = c(-1))
lat = ncvar_get(reanal_output, varid = "Y", start = c(1), count = c(-1))
lon = ncvar_get(reanal_output, varid = "X", start = c(1), count = c(-1))
plev = ncvar_get(reanal_output, varid = "P", start = c(1), count = c(-1))

start_lat = which(lat >= QA_box$lat.min & 
										lat <= QA_box$lat.max)[1]
count_lat = sum(lat >= QA_box$lat.min & 
									lat <= QA_box$lat.max)[1]

lats_keep = lat[which(lat >= QA_box$lat.min & 
												lat <= QA_box$lat.max)]

start_lon = which(lon >= QA_box$lon.min & 
										lon <= QA_box$lon.max)[1]
count_lon = sum(lon >= QA_box$lon.min & 
									lon <= QA_box$lon.max)[1]

QA_700 = melt(ncvar_get(reanal_output, 
											 varid = "qa", 
											 start = c(start_lon,start_lat,1,1), 
											 count = c(count_lon,count_lat,1,length(time))))

QA_700 = QA_700 %>% dplyr::mutate(lat = lats_keep[Var2])

QA_700 = QA_700 %>% dplyr::group_by(Var3) %>%
	dplyr::summarise(QA_700 = mean(value))

QA_700 = cbind(QA_700,time)

QA = QA_700 %>% dplyr::mutate(date = as.Date(time, origin = "1948-01-01 00:00:00"),
															season = GetSeasonDate(date)) %>%
	dplyr::select(date, QA_700, season)

# define the MHC
QA = QA %>% dplyr::group_by(season) %>%
	dplyr::mutate(QA_700_clim = mean(QA_700, na.rm = TRUE),
	              QA_700_anom = QA_700 - QA_700_clim)


save(QA, file = 'Processed_Data/QA.RData')



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



# compute future RIPs -- using the historic ensemble mean of the 99th %-ile
rm(list = ls())
source("R/GetSeasonDate.R")

# get the historic 99th percentile threshold
load(file = 'Processed_Data/pr_mod.RData')

historic_thresh = pr_mod %>% dplyr::group_by(lat,lon,model) %>%
	dplyr::summarise(cut = quantile(value, probs = 0.99)) %>%
	dplyr::group_by(lat,lon) %>%
	dplyr::summarise(xtr_thresh = mean(cut))

load(file = "Processed_Data/RIP_threshold")

load(file = 'Processed_Data/pr_mod_future.RData')
mod_RIP_future = merge(pr_mod_future, historic_thresh, by = c('lat', 'lon')) %>% 
	dplyr::mutate(xtr = ifelse(value > xtr_thresh, 1, 0)) %>%
	dplyr::group_by(date,model) %>%
	dplyr::summarise(RIP = ifelse((sum(xtr)/length(xtr)) > RIP_threshold, 1, 0)) %>%
	dplyr::mutate(season = GetSeasonDate(date))

save(mod_RIP_future, file = 'Processed_Data/mod_RIP_future.RData')

# now compute the future EWD and MHC indices
rm(list = ls())
source("R/GetSeasonDate.R")

load(file = 'Processed_Data/EWD_boxes.RData')

# load and process the Z_700 data
load(file = 'Processed_Data/data.file.path')
list_zg_files = list.files(path = paste0(data.file.path,'GCM_data/future GFDL CM3 RCP 8.5/zg/'))

EWD_mod = data.frame()

for(m in 1:1){
	mmod = c("r1i")
	mmod_files = list_zg_files[grep(list_zg_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		zg_ncdf = nc_open(paste0(data.file.path,'GCM_data/future GFDL CM3 RCP 8.5/zg/',mmod_files[file_num]))
		
		# extract time and lat/on from the file
		time = ncvar_get(zg_ncdf, varid = "time", start = c(1), count = c(-1))
		lat = ncvar_get(zg_ncdf, varid = "lat", start = c(1), count = c(-1))
		lon = ncvar_get(zg_ncdf, varid = "lon", start = c(1), count = c(-1))
		lon[lon > 180] = lon[lon > 180] - 360
		plev = ncvar_get(zg_ncdf, varid = "plev", start = c(1), count = c(-1))
		
		
		start_lat_east = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
													 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]
		count_lat_east = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'east'] & 
												 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'east'])[1]
		
		start_lat_west = which(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
													 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]
		count_lat_west = sum(lat >= EWD_boxes$lat.min[EWD_boxes$box == 'west'] & 
												 	lat <= EWD_boxes$lat.max[EWD_boxes$box == 'west'])[1]
		
		start_lon_east = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
													 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]
		count_lon_east = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'east'] & 
												 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'east'])[1]
		
		start_lon_west = which(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
													 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]
		count_lon_west = sum(lon >= EWD_boxes$lon.min[EWD_boxes$box == 'west'] & 
												 	lon <= EWD_boxes$lon.max[EWD_boxes$box == 'west'])[1]
		
		
		# zg[lon,lat,plev,time]
		EWD_east_box = melt(ncvar_get(zg_ncdf, 
																	varid = "zg", 
																	start = c(start_lon_east,start_lat_east,which(plev == 70000),1), 
																	count = c(count_lon_east,count_lat_east,1,length(time))))
		
		EWD_east_box = EWD_east_box %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(east.box = mean(value))
		
		EWD_west_box = melt(ncvar_get(zg_ncdf, 
																	varid = "zg", 
																	start = c(start_lon_west,start_lat_west,which(plev == 70000),1), 
																	count = c(count_lon_west,count_lat_west,1,length(time))))
		
		EWD_west_box = EWD_west_box %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(west.box = mean(value))
		
		tmp0 = cbind(merge(EWD_east_box, EWD_west_box),time) %>% dplyr::select(east.box, west.box, time)
		
		
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){EWD_mod = tmp}
	if(m > 1){EWD_mod = rbind(EWD_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
												 day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(2006:2100, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

# replace the date, lon, and lat indices with actual values
EWD_mod$date = rep(date,length(mmod))
EWD_mod = EWD_mod %>% dplyr::mutate(EWD = east.box - west.box)

EWD_mod_future = EWD_mod %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::select(date, model, EWD, season)

save(EWD_mod_future, file = 'Processed_Data/EWD_mod_future.RData')


# now load the atmospheric temperatures
rm(list = ls())
source("R/GetSeasonDate.R")

# Define temp region for MHC
load(file = 'Processed_Data/MHC_box.RData')

# load and process the ta data
load(file = 'Processed_Data/data.file.path')
list_ta_files = list.files(path = paste0(data.file.path, 'GCM_data/future GFDL CM3 RCP 8.5/ta/'))

MHC_mod = data.frame()

for(m in 1:1){
	mmod = c("r1i")
	mmod_files = list_ta_files[grep(list_ta_files, pattern = mmod[m])]
	
	for(file_num in 1:length(mmod_files)){
		ta_ncdf = nc_open(paste0(data.file.path, 'GCM_data/future GFDL CM3 RCP 8.5/ta/',mmod_files[file_num]))
		
		# extract time and lat/on from the file
		lon = ncvar_get(ta_ncdf, varid = "lon", start = c(1), count = c(-1))
		lat = ncvar_get(ta_ncdf, varid = "lat", start = c(1), count = c(-1))
		plev = ncvar_get(ta_ncdf, varid = "plev", start = c(1), count = c(-1))
		time = ncvar_get(ta_ncdf, varid = "time", start = c(1), count = c(-1))
		
		lats_keep = lat[which(lat >= MHC_box$lat.min & 
														lat <= MHC_box$lat.max)]
		
		start_lat = which(lat >= MHC_box$lat.min & 
												lat <= MHC_box$lat.max)[1]
		count_lat = sum(lat >= MHC_box$lat.min & 
											lat <= MHC_box$lat.max)[1]
		
		start_lon = which(lon >= MHC_box$lon.min & 
												lon <= MHC_box$lon.max)[1]
		count_lon = sum(lon >= MHC_box$lon.min & 
											lon <= MHC_box$lon.max)[1]
		
		# ta[lon,lat,plev,time]
		T_700_mod = melt(ncvar_get(ta_ncdf, 
															 varid = "ta", 
															 start = c(start_lon,start_lat,which(plev == 70000),1), 
															 count = c(count_lon,count_lat,1,length(time))))
		
		T_700_mod = T_700_mod %>% dplyr::mutate(lat = lats_keep[Var2])
		
		T_700_mod = T_700_mod %>% dplyr::group_by(Var3) %>%
			dplyr::summarise(T_700_mod = mean(value, na.rm = TRUE))
		
		tmp0 = cbind(T_700_mod,time) 
		
		if(file_num == 1){tmp = tmp0}
		
		if(file_num > 1){tmp = rbind(tmp, tmp0)}
		print(file_num)
	}
	tmp$model = m
	
	if(m == 1){MHC_mod = tmp}
	if(m > 1){MHC_mod = rbind(MHC_mod,tmp)}	
}

# all years of the GCM data have 365 days (no leap years)
# below we create the month and day of each of these no-leap year days
doy_no_leap = data.frame(month = month(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")),
												 day = day(seq.Date(from = as.Date("1999-01-01"), to = as.Date("1999-12-31"), by = "1 day")))

# create a vector of dates for the GCM historic run
date = c(as.Date(paste0(rep(2006:2100, each = 365),"-",doy_no_leap$month,"-",doy_no_leap$day)))

MHC_mod$date = rep(date,length(mmod))

MHC_mod = MHC_mod %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
	dplyr::select(date, model, T_700_mod, season)

# get rid of very high and low values -- they are present during JJA, so it doesn't matter for MAM
MHC_mod$T_700_mod[MHC_mod$T_700_mod > 1000 | MHC_mod$T_700_mod < 10] = NA

# define the MHC
MHC_mod_future = MHC_mod %>% dplyr::mutate(MHC = 6.1 * exp((17.625 * (T_700_mod - 273.15)) / ((T_700_mod - 273.15) + 243.04)))

# need to load the historic climatology to calculate the anomaly
load(file = "Processed_Data/MHC_mod.RData")

MHC_clim = MHC_mod %>% dplyr::group_by(season) %>%
	dplyr::summarise(MHC_clim = unique(MHC_clim))

MHC_mod_future = merge(MHC_mod_future, MHC_clim, by = "season") %>%
	dplyr::mutate(MHC_anom = MHC - MHC_clim)

save(MHC_mod_future, file = 'Processed_Data/MHC_mod_future.RData')

