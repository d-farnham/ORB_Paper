# all codes run in RStudio version 1.0.136 and R version R version 3.3.3
# Older and newer versions of RStudio and R have not been tested

########################################
############### Figure 1 ###############
########################################
rm(list = ls())
package.list <- list("akima","dataRetrieval","data.table","dplyr","epitools","ggmap","ggplot2","gridExtra", 
                     "Kendall","locfit", "lubridate","maps","ncdf4","readr","reshape2","tidyr")
source('R/load_packages.R') # clear workspace, clear console, load packages
source("R/GetSeasonDate.R")

# compare the grids from the CPC and the GCM
load(file = 'Processed_Data/CPC_grid.RData')
load(file = 'Processed_Data/mod_ORB_grid.RData')


world <- data.frame(map("world", plot=FALSE)[c("x","y")])
state <- data.frame(map("state", plot=FALSE)[c("x","y")])

source("R/load_basin_boundary.R")
pdf('Final figures/Figure_1.pdf', width = 6, height = 4.25)
ggplot() + 
  geom_tile(data = CPC_grid,
            aes(x = (lon-360),y = lat), fill = "transparent", col = "red", size = 0.2, alpha = 0.5) +
  geom_tile(data = mod_ORB_grid,
            aes(x = (lon-360),y = lat), fill = "transparent", col = "blue", size = 0.6) + 
  geom_path(data=state, aes(x,y,z=NULL), alpha = 0.75) +
  scale_y_continuous(limits = c(33,45)) +
  scale_x_continuous(limits = c(-93,-75)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw()
dev.off()


########################################
############### Figure 2 ###############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")
load(file = 'Processed_Data/HCDN.RData')
load(file = 'Processed_Data/gage_info.RData')


# plot the streamflow gauges

# load 1) the US state boundaries, 
#      2) the ORB basin shapefile, 
#      3) the the domain that we consider for the RIPs
state <- data.frame(map("state", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")
load('Processed_Data/pr_box.RData')

gage_info = gage_info %>% dplyr::mutate(new_names = paste0(Short.name," (",substr(Short.name,1,1),")"))

# panel 1 of Figure 2
stream_gauge_locs_plot =
  ggplot() +
  geom_point(data = as.data.frame(gage_info), aes(x = Longitude,y = Latitude, size=da_sq_km/10000)) +
  scale_size_continuous(name = "Drainage Area (x 10000 sq. km)", limits = c(0,25), range = c(0,4)) +
  geom_text(data = as.data.frame(gage_info), aes(x = Longitude+c(0,0.35,-0.35,0,0,0),
                                                 y = Latitude+c(rep(-0.5,4),0.5,-0.5), 
                                                 label = new_names),
            size = 3,
            col = "black") +
  geom_rect(data = data.frame(ymin = c(pr_box$lat.min), ymax = c(pr_box$lat.max), xmin = c(pr_box$lon.max - 360), xmax = c(pr_box$lon.min - 360)),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), col = "red",
            alpha=0.15, inherit.aes = FALSE, size = 0.5) +
  geom_path(data=state, aes(x,y,z=NULL), alpha = 0.25) +
  scale_y_continuous(limits = c(35.5,42.5)) +
  scale_x_continuous(limits = c(-90,-77.5)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  ggtitle("Streamflow gauges") +
  theme_bw() +
  theme(legend.position = "bottom")


# reshape the streamflow data and only use data from 1950 to 2005
streamflows0 = melt(HCDN,id.vars = c("dates","streamflow")) %>%
  dplyr::filter(dates > "1949-12-31" &
                  dates < "2006-01-01")

# rename column names
colnames(streamflows0) = c("date", "streamflow", "site")


# calculate the 1 in 365 (or about the 99.7th percentiles) from each site
# and mark each day that exceeds that threshold for each site
xtr_threshold = (1 - 1/365)
streamflows = streamflows0 %>% dplyr::mutate(day = day(date),
                                             month = month(date),
                                             year = year(date),
                                             streamflow = ifelse(streamflow < 0, 0 , streamflow)) %>%
  dplyr::group_by(site) %>%
  dplyr::mutate(xtr = quantile(streamflow, probs = xtr_threshold),
                season = GetSeasonDate(date),
                str_xtr = ifelse(streamflow >= xtr, 1, 0))
# now save the processed streamflows
save(streamflows, file = 'Processed_Data/streamflows.RData')


flow_seasons = streamflows %>% dplyr::group_by(season,site) %>%
  dplyr::summarise(prob_str_xtr = sum(str_xtr)/length(str_xtr)) %>%
  dplyr::ungroup()

# make 'season' a factor, with order c("DJF", "MAM", "JJA", "SON")
flow_seasons = flow_seasons %>% dplyr::mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")))

# make 'site' a factor, ordered according to drainage basin size
flow_seasons = flow_seasons %>% dplyr::mutate(site = factor(site, levels = gage_info$Short.name[order(gage_info$D.A, decreasing = TRUE)]))

# panel 2 for Figure 2
prob_str_S_by_season_plot =
  ggplot(data = flow_seasons) +
  geom_line(aes(x = season, y = prob_str_xtr, group = as.factor(site), col = site), size = 1.25) +
  geom_point(aes(x = season, y = prob_str_xtr, group = as.factor(site), col = site), size = 2.5) +
  theme(legend.position = "none") +
  ylab(expression(Pr(S^{s} > S[364/365]^{s}))) +
  ylim(c(0,0.01)) +
  theme_bw() +
  theme(legend.position = "bottom")

# find the days when a RIP occured:
load('Processed_Data/CPC_mod_cell_RIP.RData')

# compute the 15 day running sum for the RIPs
RIP_CPC = CPC_mod_cell_RIP %>% dplyr::select(date, RIP) %>%
  dplyr::mutate(RIP_sum_15 = stats::filter(RIP, rep(1, 15), method = "convolution", sides = 1))

# now merge the streamflow record and the RIP record
precip_streamflow = merge(streamflows,RIP_CPC, by = "date", all.y = TRUE)

# next we compute the odds ratio for RIP_sum_15 > 0 vs. RIP_sum_15 = 0 & 
# str_xtr = 1 vs. str_xtr = 0 (i.e. whether the streamflow is extreme)
odds_ratio = data.frame(expand.grid(site = unique(precip_streamflow$site),
                                    season = c("MAM"),
                                    RIP_sum_15 = c(0)))

odds_ratio$odds_est = odds_ratio$odds_lower = odds_ratio$odds_upper = NA
odds_ratio_tmp = precip_streamflow %>% dplyr::mutate(RIP_over = ifelse(RIP_sum_15 > 0, 1, 0))

# exclude the NAs
odds_ratio_tmp = odds_ratio_tmp %>% dplyr::filter(!is.na(RIP_over) &
                                                    !is.na(str_xtr))
for(rrow in 1:nrow(odds_ratio)){
  table_tmp = table(odds_ratio_tmp[odds_ratio_tmp$season == odds_ratio$season[rrow] & 
                                     odds_ratio_tmp$site == odds_ratio$site[rrow],]$RIP_over, 
                    odds_ratio_tmp[odds_ratio_tmp$season == odds_ratio$season[rrow] &
                                     odds_ratio_tmp$site == odds_ratio$site[rrow],]$str_xtr)
  tryCatch({ # if there is an error, we want the loop to continue running	
    odds_ratio$odds_est[rrow] = oddsratio.wald(table_tmp)$measure[2,1]
    odds_ratio$odds_lower[rrow] = oddsratio.wald(table_tmp)$measure[2,2]
    odds_ratio$odds_upper[rrow] = oddsratio.wald(table_tmp)$measure[2,3]
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(rrow)
}

# make the # RIP events label for plot
odds_ratio = odds_ratio %>% dplyr::mutate(RIP_sum_15_lab = "1+")

# order the sites according to drainage basin size
odds_ratio = odds_ratio %>% dplyr::mutate(site = factor(site, levels = gage_info$Short.name[order(gage_info$D.A, decreasing = TRUE)]))

# take the natural log of the odds ratio
odds_ratio = odds_ratio %>% dplyr::mutate(ln_odds_est = log(odds_est),
                                          ln_odds_lower = log(odds_lower),
                                          ln_odds_upper = log(odds_upper))

ann_text_pet <- data.frame(x = c(1.08, 0.8), y = c(1),lab = "Inf",
                           site = factor(c("Petersburg","MtCarmel")),
                           season = "MAM")

# panel 3 for Figure 2
odds_ratio_str_S_by_season_plot =
  ggplot(data = odds_ratio[odds_ratio$season == "MAM",]) +
  geom_pointrange(aes(x = factor(RIP_sum_15_lab), y = ln_odds_est, ymin = ln_odds_lower, ymax = ln_odds_upper, col = site), 
                  position = position_dodge(width = 0.75), fatten = 1.75, size = 1) +
  facet_grid( ~ season) +
  scale_y_continuous(name = expression((LOR^s ~ '|' ~ RIP)), limits = c(-1,6.25)) +
  xlab("") +
  geom_text(data = ann_text_pet, aes(x,y,label = lab), col = c("#00BFC4", "#B79F00")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none")


############################

pdf(paste0("Final figures/Figure_2.pdf"), height = 5, width = 10)
grid.arrange(stream_gauge_locs_plot,
             prob_str_S_by_season_plot,
             odds_ratio_str_S_by_season_plot,
             nrow = 2,
             layout_matrix = rbind(c(1,2),c(1,2),c(1,2),c(1,3),c(1,3)))
dev.off()

########################################
############### Figure S1 ##############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")

# load the RIP records
load(file = 'Processed_Data/mod_RIP.RData')
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')
load(file = 'Processed_Data/mod_CPC_thresh_RIP.RData')

# compute the number of RIPs by month for the OBS and MOD records
MOD_RIP = mod_RIP %>% dplyr::mutate(model = paste0("GCM ",model)) %>% data.frame()
OBS_RIP = CPC_mod_cell_RIP %>% dplyr::mutate(model = "OBS") %>% data.frame()

# now merge these records, count the number of RIPs during each month 
# and compute the % of RIPs that occur within each month for each model
RIP_MOD_OBS_counts = rbind(MOD_RIP, OBS_RIP) %>% dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::group_by(model, month) %>%
  dplyr::summarise(RIPs = sum(RIP, na.rm = TRUE)) %>% dplyr::group_by(model) %>%
  dplyr::mutate(percent_RIPs = RIPs/sum(RIPs))

pdf('Final figures/Figure_S1.pdf', height = 4, width = 8)
ggplot(RIP_MOD_OBS_counts) +
  geom_bar(aes(month, percent_RIPs*100), stat = "identity") +
  facet_wrap(~model) +
  labs(x = "month", y = "% of RIPs") +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()
dev.off()


########################################
############### Figure 3 ###############
########################################
# now plot the time-series and histograms of RIPs

# compute the counts of RIP by year for MAM for the observed record (CPC)
RIP_CPC_by_season_year = CPC_mod_cell_RIP %>% dplyr::filter(season == "MAM") %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::summarise(RIP = sum(RIP, na.rm = TRUE))

# compute the counts of RIP by year for MAM for the model record
RIP_mod_by_season_year = mod_RIP %>% dplyr::filter(season == "MAM") %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year, model) %>%
  dplyr::summarise(RIP = sum(RIP, na.rm = TRUE)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::mutate(RIP_ensemble_mean = mean(RIP, na.rm = TRUE))

# compute the counts of RIP by year for MAM for the model record with observed thresholds
RIP_mod_CPC_thresh_by_season_year = mod_CPC_thresh_RIP %>% dplyr::filter(season == "MAM") %>% 
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(season, year, model) %>%
  dplyr::summarise(RIP = sum(RIP, na.rm = TRUE)) %>%
  dplyr::group_by(season, year) %>%
  dplyr::mutate(RIP_ensemble_mean = mean(RIP, na.rm = TRUE))

# now compute the 10-year running mean for each of these three records
source('R/ma.R')
smoothing_window = 10

RIP_CPC_by_season_year = RIP_CPC_by_season_year %>% dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window))
RIP_mod_by_season_year = RIP_mod_by_season_year %>% dplyr::group_by(model,season) %>%
  dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window),
                RIP_ensemble_mean_smooth = ma(RIP_ensemble_mean, smoothing_window))

RIP_mod_CPC_thresh_by_season_year = RIP_mod_CPC_thresh_by_season_year %>% dplyr::group_by(model,season) %>%
  dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window),
                RIP_ensemble_mean_smooth = ma(RIP_ensemble_mean, smoothing_window))

line.width = 1
alpha.line = 1

RIP_mod_obs_compare_1 =
  ggplot() + 
  geom_line(data = RIP_mod_by_season_year,
            aes(year, y = RIP_smooth, group = factor(model)), 
            linetype = "dashed", size = line.width/2, alpha = alpha.line) +
  geom_line(data = RIP_mod_by_season_year,
            aes(year, y = RIP_ensemble_mean_smooth), col = "red", 
            linetype = "dashed", size = line.width) +
  geom_line(data = RIP_CPC_by_season_year,
            aes(year, y = RIP_smooth), 
            size = line.width) +
  ylab("# RIP") +
  coord_cartesian(xlim = c(1955,2006),
                  ylim = c(0,5.5)) +
  xlab("Year") +
  annotate("text", label = c("a)"), x = 1956, y = 5.3, size = 6, colour = "black") +
  theme_bw()


RIP_mod_obs_compare_2 =
  ggplot() + 
  geom_line(data = RIP_mod_CPC_thresh_by_season_year,
            aes(year, y = RIP_smooth, group = factor(model)), 
            linetype = "dashed", size = line.width/2, alpha = alpha.line) +
  geom_line(data = RIP_mod_CPC_thresh_by_season_year,
            aes(year, y = RIP_ensemble_mean_smooth), col = "red", 
            linetype = "dashed", size = line.width) +
  geom_line(data = RIP_CPC_by_season_year,
            aes(year, y = RIP_smooth), 
            size = line.width) +
  ylab("# RIP") +
  coord_cartesian(xlim = c(1955,2006),
                  ylim = c(0,5.5)) +
  xlab("Year") +
  annotate("text", label = c("c)"), x = 1956, y = 5.3, size = 6, colour = "black") +
  theme_bw()


bin_breaks = seq(-0.5,12.5,1)
bin_labels = as.character(bin_breaks-0.5)
hist_lims = data.frame(xlim = c(0.5,11.6),
                       ylim = c(0,40))
RIP_mod_obs_compare_3 =
  ggplot() + 
  stat_bin(data = RIP_mod_by_season_year[RIP_mod_by_season_year$model == 1,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_by_season_year[RIP_mod_by_season_year$model == 2,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_by_season_year[RIP_mod_by_season_year$model == 3,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_by_season_year[RIP_mod_by_season_year$model == 4,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_by_season_year[RIP_mod_by_season_year$model == 5,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_CPC_by_season_year,
           aes(x = RIP, y=..count..), 
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_by_season_year,
           aes(x = RIP, y =..count../5),
           geom = "step", linetype = "twodash", size = line.width,
           breaks = bin_breaks, col = "red") +
  annotate("text", label = c("b)"), x = 0.75, y = 38.75, size = 6, colour = "black") +
  xlab("# RIP") +
  coord_cartesian(xlim = hist_lims$xlim, ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()

RIP_mod_obs_compare_4 =
  ggplot() + 
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year[RIP_mod_CPC_thresh_by_season_year$model == 1,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year[RIP_mod_CPC_thresh_by_season_year$model == 2,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year[RIP_mod_CPC_thresh_by_season_year$model == 3,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year[RIP_mod_CPC_thresh_by_season_year$model == 4,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year[RIP_mod_CPC_thresh_by_season_year$model == 5,],
           aes(x = RIP, y =..count..), alpha = alpha.line,
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_CPC_by_season_year,
           aes(x = RIP, y=..count..), 
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = RIP_mod_CPC_thresh_by_season_year,
           aes(x = RIP, y =..count../5),
           geom = "step", linetype = "twodash", size = line.width,
           breaks = bin_breaks, col = "red") +
  annotate("text", label = c("d)"), x = 0.75, y = 38.75, size = 6, colour = "black") +
  xlab("# RIP") +
  coord_cartesian(xlim = hist_lims$xlim, ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()

pdf('Final figures/Figure_3.pdf', width = 11, height = 5.5)
grid.arrange(RIP_mod_obs_compare_1,
             RIP_mod_obs_compare_2,
             RIP_mod_obs_compare_3,
             RIP_mod_obs_compare_4,
             nrow = 2,
             layout_matrix = rbind(c(1,3),c(2,4)))
dev.off()

# !!!!!!!! CHECK HERE... DO I NEED THIS? !!!
# calculate the Mann kendall trend for MAM for each of the observations/simulations
library(Kendall)
obs_trend =  RIP_CPC_by_season_year %>% dplyr::group_by(season) %>%
  dplyr::summarise(tau = as.numeric(MannKendall(RIP)["tau"]),
                   p.value = as.numeric(MannKendall(RIP)["sl"]))

mod_trend =  RIP_mod_by_season_year %>% dplyr::group_by(season,model) %>%
  dplyr::summarise(tau = as.numeric(MannKendall(RIP)["tau"]),
                   p.value = as.numeric(MannKendall(RIP)["sl"]))

mod_ens_trend =  RIP_mod_by_season_year %>% dplyr::group_by(season,model) %>%
  dplyr::summarise(tau = as.numeric(MannKendall(RIP_ensemble_mean)["tau"]),
                   p.value = as.numeric(MannKendall(RIP_ensemble_mean)["sl"]))

########################################
############### Figure 4 ###############
########################################

# define new variables of lagged RIPs
RIP_lag_obs = CPC_mod_cell_RIP %>% dplyr::filter(season == "MAM") %>%
  dplyr::mutate(RIP_lag1 = lag(RIP, 1),
                RIP_lag_4_7 = ifelse(lag(RIP, 4) == 1, 1,
                                     ifelse(lag(RIP, 5) == 1, 1,
                                            ifelse(lag(RIP, 6) == 1, 1,
                                                   ifelse(lag(RIP, 7) == 1, 1,0)))))
# need to remove the first 7 days from each year 
# (because we cannot check the lagged days associated with those days in the simulation round)
RIP_lag_obs = RIP_lag_obs %>% dplyr::filter(!(lubridate::month(date) == 3 & lubridate::day(date) %in% 1:7))


RIP_lag_mod = mod_RIP %>% dplyr::filter(season == "MAM") %>%
  dplyr::group_by(model) %>% 
  dplyr::mutate(RIP_lag1 = lag(RIP, 1),
                RIP_lag_4_7 = ifelse(lag(RIP, 4) == 1, 1,
                                     ifelse(lag(RIP, 5) == 1, 1,
                                            ifelse(lag(RIP, 6) == 1, 1,
                                                   ifelse(lag(RIP, 7) == 1, 1,0)))))

RIP_lag_mod = RIP_lag_mod %>% dplyr::filter(!(lubridate::month(date) == 3 & lubridate::day(date) %in% 1:7))

RIP_lag_obs_long = melt(RIP_lag_obs, id.vars = c("date", "season", "RIP"))
RIP_lag_mod_long = melt(RIP_lag_mod, id.vars = c("date", "season", "model", "RIP"))



# initialize a blank data.frame to store the conditional probs for the obs and mod records
RIP_cond_prob_obs = data.frame(lag = unique(RIP_lag_mod_long$variable),
                               OBS = NA)
RIP_cond_prob_mod = data.frame(lag = unique(RIP_lag_mod_long$variable),
                               "GCM1" = NA,
                               "GCM2" = NA,
                               "GCM3" = NA,
                               "GCM4" = NA,
                               "GCM5" = NA)

# now compute the conditional probs
RIP_lag_names = unique(RIP_lag_mod_long$variable)
for(llag in 1:2){
  con_ting_table = table(RIP_lag_obs_long$RIP[RIP_lag_obs_long$variable == RIP_lag_names[llag]], 
                         RIP_lag_obs_long$value[RIP_lag_obs_long$variable == RIP_lag_names[llag]], 
                         deparse.level = 2)
  RIP_cond_prob_obs$obs[llag] = con_ting_table[4]/(con_ting_table[3] + con_ting_table[4])
  
  for(mmod in 1:5){
    con_ting_table = table(RIP_lag_mod_long$RIP[RIP_lag_mod_long$model == mmod &
                                                  RIP_lag_mod_long$variable == RIP_lag_names[llag]], 
                           RIP_lag_mod_long$value[RIP_lag_mod_long$model == mmod &
                                                    RIP_lag_mod_long$variable == RIP_lag_names[llag]], 
                           deparse.level = 2)
    
    RIP_cond_prob_mod[llag,mmod+1] = con_ting_table[4]/(con_ting_table[3] + con_ting_table[4])
  }
}

RIP_cond_prob_mod_long = melt(RIP_cond_prob_mod, id.vars = c("lag"), variable.name = "model") 

# add the observations to this
RIP_cond_prob_obs_long = data.frame(model = "OBS",
                                    lag = RIP_cond_prob_obs$lag,
                                    value = RIP_cond_prob_obs$obs)

RIP_cond_prob_mod_obs_long = rbind(RIP_cond_prob_mod_long,RIP_cond_prob_obs_long)

CPC_marg_probs = 
  RIP_lag_obs_long %>% dplyr::group_by(season) %>%
  dplyr::summarise(value = sum(RIP[!is.na(RIP)], na.rm = TRUE)/length(RIP[!is.na(RIP)])) %>%
  dplyr::mutate(variable = "OBS",
                lag = "marg_prob") %>%
  dplyr::filter(season %in% c("MAM")) %>%
  data.frame()

mod_marg_probs = 
  RIP_lag_mod_long %>% dplyr::group_by(season, model) %>%
  dplyr::summarise(value = sum(RIP[!is.na(RIP)], na.rm = TRUE)/length(RIP[!is.na(RIP)])) %>%
  dplyr::mutate(variable = paste0("GCM",model),
                lag = "marg_prob") %>%
  dplyr::filter(season %in% c("MAM")) %>%
  dplyr::select(-model) %>%
  data.frame()

marg_probs = rbind(mod_marg_probs, CPC_marg_probs)
marg_probs_wide = reshape2::dcast(variable ~ lag, data = marg_probs, value.va	= "value") 

RIP_mod_CPC_cond_marg_prob_long = merge(RIP_cond_prob_mod_obs_long,
                                        marg_probs_wide, by.x = "model", by.y = "variable") %>%
  dplyr::mutate(cond_div_marg = value/ marg_prob)

# rename the lag catagories and the model names for the plot for the plot
RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>% dplyr::mutate(lag_rename = ifelse(lag == "RIP_lag1", "1", "4-7"))
RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>%  dplyr::mutate(model_rename = ifelse(model == "OBS", "OBS", 
                                                                                                           paste0(substring(model,f = 1, l = 3)," ",
                                                                                                                  substring(model,f = 4, l = 4))))
# now mark whether each row is OBS or GCM
RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>%  dplyr::mutate(OBS_OR_GCM = ifelse(model == "OBS", "OBS", "GCM"))

pdf("Final figures/Figure_4.pdf", width = 5, height = 3)
ggplot() +
  geom_boxplot(data = RIP_mod_CPC_cond_marg_prob_long,
               aes(y = cond_div_marg, x = lag_rename, fill = OBS_OR_GCM), width = 0.5, coef = 5) +
  labs(x = "lag (days)", y = expression(P(RIP[t] ~"|" ~ RIP[t-lag])/P(RIP))) +
  scale_fill_discrete(name = "GCM/OBS") +
  scale_color_discrete(name = "GCM/OBS") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,16)) +
  theme_bw()
dev.off()


########################################
############### Figure S2 ##############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")


# load the records of cell-based intense precipitation for the CPC and GCM
load(file = 'Processed_Data/CPC_mod_cell_IP.RData')
load(file = 'Processed_Data/mod_IP.RData')



# now only consider Dec 1950 through Nov 2005
mod_IP = mod_IP %>% data.frame() %>%
  dplyr::filter(date > '1950-11-30' &
                  date < '2005-12-01') %>%
  dplyr::mutate(model_rename = paste0("GCM ", model))

CPC_mod_cell_IP = CPC_mod_cell_IP %>% data.frame() %>%
  dplyr::filter(date > '1950-11-30' &
                  date < '2005-12-01') %>%
  dplyr::mutate(model_rename = "OBS")
# now merge the OBS and MOD IPS
CPC_GCM_IP = rbind(CPC_mod_cell_IP, mod_IP[,c("date", "IP", "season", "model_rename")]) %>%
  dplyr::mutate(RIP = ifelse(IP > 3,"RIP","no RIP")) %>%
  dplyr::filter(!is.na(IP) & IP > 0)

# calculate the number of IPs by season and model_rename 
num_CPC_GCM_IP_season = CPC_GCM_IP %>% dplyr::group_by(season, model_rename, RIP) %>%
  dplyr::summarise(num_IP = length(IP)) %>%
  dplyr::mutate(prop_IP = round(num_IP/sum(num_IP),3),
                lab = paste0(RIP, ": ",num_IP," (",prop_IP*100,"%*)")) %>%
  dplyr::group_by(season, model_rename) %>%
  dplyr::mutate(num_IP_all = sum(num_IP)) %>%
  dplyr::group_by(model_rename) %>%
  dplyr::mutate(prop_IP_all = round(num_IP_all/(sum(num_IP_all)/2),3), # need to divide by 2 b/c there are two rows ('RIP' & 'no RIP')
                lab_all = paste0("all: ",num_IP_all," (",prop_IP_all*100,"%**)")) # that each contain a 'num_IP_all'


# calculate the mean for IP for each model_rename and season
mean_CPC_GCM_IP_season = CPC_GCM_IP %>% dplyr::group_by(season, model_rename) %>%
  dplyr::summarise(mean_IP = mean(IP)) 

# below is a figure that is only for our reference
pdf('Final figures/Figure_R1.pdf', height = 8, width = 10)
ggplot(CPC_GCM_IP) +
  geom_bar(aes(IP, fill = factor(RIP))) +
  scale_fill_manual(values = c("black","red"), name = "") +
  geom_vline(data = mean_CPC_GCM_IP_season, aes(xintercept = mean_IP), 
             linetype = "dashed") +
  geom_text(data = num_CPC_GCM_IP_season[num_CPC_GCM_IP_season$RIP == "no RIP",],
            aes(x = 12, y = 225, label = lab), hjust = "right") +
  geom_text(data = num_CPC_GCM_IP_season[num_CPC_GCM_IP_season$RIP == "RIP",],
            aes(x = 12, y = 175, label = lab), col = "red", hjust = "right") +
  geom_text(data = num_CPC_GCM_IP_season[num_CPC_GCM_IP_season$RIP == "RIP",],
            aes(x = 12, y = 125, label = lab_all), col = "black", fontface = "bold", hjust = "right") +
  facet_grid(model_rename ~ season) +
  labs(x = "# of cell-based intense precipitation events on a given day",
       y = "count",
       subtitle = "* = the percent of days in each season that were/were not RIP days given that at least one cell was > it's 99th percentile during that day \n ** = the percent of days when at least one cell was > it's 99th percentile that occurred during that season") +
  scale_x_continuous(breaks = seq(1,12,by = 2))
dev.off()


OBS_IP_counts = CPC_GCM_IP %>% dplyr::mutate(IP_factor = as.factor(IP)) %>%
  dplyr::filter(model_rename == "OBS") %>%
  dplyr::group_by(season,IP_factor,RIP) %>%
  dplyr::summarise(OBS_count = length(date))

MOD_IP_counts = CPC_GCM_IP %>% dplyr::mutate(IP_factor = as.factor(IP)) %>%
  dplyr::filter(model_rename != "OBS") %>%
  dplyr::group_by(season,IP_factor,RIP,model_rename) %>%
  dplyr::summarise(ENS_MEAN_count = length(date))


IP_counts = merge(OBS_IP_counts, MOD_IP_counts, by = c("season", "IP_factor", "RIP")) %>%
  dplyr::mutate(`OBS - ENSEMBLE MEAN` = OBS_count - ENS_MEAN_count)


pdf('Final figures/Figure_S2.pdf', height = 3, width = 11)
ggplot(IP_counts) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 3.5, linetype = "dashed", alpha = 0.5) +
  geom_point(aes(x = as.numeric(IP_factor), y = `OBS - ENSEMBLE MEAN`, col = factor(RIP)), stat = "identity") +
  scale_color_manual(values = c("black","red"), name = "") +
  facet_wrap(~season, nrow = 1) +
  labs(x = "# of local intense precipitation events on a given day",
       y = "OBS count - GCM count") +
  scale_x_continuous(breaks = seq(1,12,by = 2)) +
  theme_bw()
dev.off()

########################################
############### Figure 5 ###############
########################################

# clear workspace
rm(list = ls())
source('R/GetSeasonDate.R')
# now let's calculate anomalies and plot them
load('Processed_Data/PR_WAT.RData')
load('Processed_Data/Z_700.RData')

# just look at the western hemisphere
PR_WAT = PR_WAT %>% dplyr::filter(lon > 180)
Z_700 = Z_700 %>% dplyr::filter(lon > 180)

dat = cbind(Z_700, PR_WAT$pr_wat)
colnames(dat)[6] = "pr_wat"

rm(Z_700,PR_WAT)

# compute and save the monthly means
monthly_mean_Z_PR_WAT = dat %>% dplyr::mutate(month = month(date)) %>%
  dplyr::group_by(lon,lat,month) %>%
  dplyr::summarise(z_700_clim = mean(z_700),
                   pr_wat_clim = mean(pr_wat))

save(monthly_mean_Z_PR_WAT, file = "Processed_Data/monthly_mean_Z_PR_WAT.RData")

source('R/GetSeasonDate.R')
# load xtrs from the CPC calcs
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')

RIP_dates = CPC_mod_cell_RIP %>% dplyr::filter(RIP == 1) %>%
  dplyr::select(date) %>%
  data.frame() %>%
  dplyr::filter(date > "1949-12-31" &
                  date < "2006-01-01")

subset.params = list(dates = RIP_dates$date,
                     lags = c(-4,-3,-2,-1,0,1), # how many days prior to the event do you want?
                     all_dates = as.Date(as.Date("1950-01-01"):as.Date("2005-12-31"),origin="1970-01-01"))

# calculate the mean value of relevant days 
RIP_event = dat %>% dplyr::filter(date %in% c(subset.params$dates + days(subset.params$lags[1]))) %>%
  data.table() %>%
  dplyr::mutate(lag = subset.params$lags[1])


for(llags in 2:length(subset.params$lags)){
  RIP_event0 = dat %>% dplyr::filter(date %in% c(RIP_dates$date + days(subset.params$lags[llags]))) %>%
    data.table() %>%
    dplyr::mutate(lag = subset.params$lags[llags])
  RIP_event = rbind(RIP_event,RIP_event0)
  print(llags)
}

RIP_event = RIP_event %>% dplyr::mutate(month = month(date))

sig.level = 0.8
RIP_event_anom = merge(RIP_event, monthly_mean_Z_PR_WAT, by = c("lat", "lon", "month")) %>%
  dplyr::mutate(z_700_anom = z_700 - z_700_clim,
                pr_wat_anom = pr_wat - pr_wat_clim,
                z_700_positive = ifelse(z_700_anom > 0, 1, 0))


RIP_event_anom = RIP_event_anom %>% dplyr::group_by(lon,lat,season,lag) %>%
  dplyr::summarise(z_700_anom = mean(z_700_anom),
                   pr_wat_anom = mean(pr_wat_anom),
                   prop.pos = mean(z_700_positive))

RIP_event_anom = RIP_event_anom %>% dplyr::mutate(z_700_sig = ifelse(z_700_anom > 0 & prop.pos > sig.level,1,
                                                                     ifelse(z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))


state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

# just plot the "MAM" composites
subset = RIP_event_anom[RIP_event_anom$season == "MAM",]

z_700.lim = max(-floor(min(subset$z_700_anom)/2)*2,ceiling(max(subset$z_700_anom)/2)*2)


pdf(paste0("Final figures/Figure_5.pdf"), width=10, height=5)
ggplot(data = subset) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(z_700_anom))) + 
  facet_wrap(~lag, ncol = 3) +
  scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                       limits = c(-z_700.lim,z_700.lim), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_point(data = subset[subset$z_700_sig == 1, ], 
             aes(x = (lon-360),y = lat),shape = "X", size = 1) +
  stat_contour(aes(x = (lon-360),y = lat, z=pr_wat_anom), 
               breaks = seq(4,20,4), size=0.25, col = "black") +
  stat_contour(aes(x = (lon-360),y = lat, z=pr_wat_anom), 
               breaks = seq(-4,-20,-4), size=0.25, col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y,z=NULL), size = 0.25) + 
  scale_y_continuous(limits = c(10,60)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-155,-45), breaks = c(-140, -100, -60)) +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -95, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points)
dev.off()

########################################
############### Figure S3 ##############
########################################
rm(list = ls())
load(file = 'Processed_Data/pr_CPC_IP_US.RData')
load(file = 'Processed_Data/pr_mod_IP_US.RData')

pr_mod_IP$lat = as.numeric(as.character(pr_mod_IP$lat))
pr_mod_IP$lon = as.numeric(as.character(pr_mod_IP$lon))
pr_CPC_IP$lat = as.numeric(as.character(pr_CPC_IP$lat))
pr_CPC_IP$lon = as.numeric(as.character(pr_CPC_IP$lon))

pr_mod_obs_IP = merge(rbind(pr_mod_IP, pr_CPC_IP), pr_CPC_IP[,c('season','lat', 'lon', 'mean_pr_percentile')], by = c('season','lat', 'lon')) %>%
  dplyr::mutate(delta_pr_percentile = ifelse(model != "OBS", mean_pr_percentile.x - mean_pr_percentile.y,mean_pr_percentile.y),
                model_lab = ifelse(model != "OBS", paste0(model, "- OBS"), "OBS"))

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

load(file = 'Processed_Data/pr_box.RData')

pr_mod_obs_IP_box = pr_mod_obs_IP %>% dplyr::filter(lat >= pr_box$lat.min,
                                                    lat <= pr_box$lat.max,
                                                    lon >= pr_box$lon.min,
                                                    lon <= pr_box$lon.max)

subset = pr_mod_obs_IP[pr_mod_obs_IP$season == "MAM",]

pdf('Final figures/Figure_S3.pdf', height = 7, width = 12)
ggplot(data = subset) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(mean_pr_percentile.x))) + 
  scale_fill_gradient(name=expression(paste(P, " percentile")),
                      limits = c(0.75,0.9), 
                      low="white", high = "black", na.value = "white") +
  geom_rect(data = pr_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max), 
            col = "black", fill = "transparent", alpha=0, size = 1) +
  geom_path(data=world, aes(x,y,z=NULL), size = 0.25) + 
  geom_path(data=state, aes(x,y,z=NULL), size = 0.25) + 
  scale_y_continuous(limits = c(25,50)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-100,-65)) +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -80, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  facet_wrap(~model) +
  theme(legend.position = "right")
dev.off()

########################################
############### Figure S4 ##############
########################################
rm(list = ls())
load(file = 'Processed_Data/pr_CPC_RIP_same_day.RData')
load(file = 'Processed_Data/pr_mod_RIP_same_day.RData')

pr_mod_RIP_same_day$lat = as.numeric(as.character(pr_mod_RIP_same_day$lat))
pr_mod_RIP_same_day$lon = as.numeric(as.character(pr_mod_RIP_same_day$lon))
pr_CPC_RIP_same_day$lat = as.numeric(as.character(pr_CPC_RIP_same_day$lat))
pr_CPC_RIP_same_day$lon = as.numeric(as.character(pr_CPC_RIP_same_day$lon))

pr_CPC_mod_RIP_same_day = merge(rbind(pr_mod_RIP_same_day, pr_CPC_RIP_same_day), pr_CPC_RIP_same_day[,c('season','lat', 'lon', 'mean_pr_percentile')], by = c('season','lat', 'lon')) %>%
  dplyr::mutate(delta_pr_percentile = ifelse(model != "OBS", mean_pr_percentile.x - mean_pr_percentile.y,mean_pr_percentile.y),
                model_lab = ifelse(model != "OBS", paste0(model, " - OBS"), "OBS"))

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

load(file = 'Processed_Data/pr_box.RData')

subset = pr_CPC_mod_RIP_same_day[pr_CPC_mod_RIP_same_day$season == "MAM",]

pdf('Final figures/Figure_S4.pdf', height = 7, width = 12)
ggplot(data = subset[subset$model != "OBS", ]) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(delta_pr_percentile))) + 
  scale_fill_gradient2(name=expression(atop(P[GCM] ~ " percentile -",P[OBS] ~ " percentile")),							 
                       limits = c(-0.27,0.27), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_rect(data = pr_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max), 
            col = "black", fill = "transparent", alpha=0, size = 1) +
  geom_path(data=world, aes(x,y,z=NULL), size = 0.25) + 
  geom_path(data=state, aes(x,y,z=NULL), size = 0.25) + 
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
  facet_wrap(~model_lab, nrow = 2) +
  theme(legend.position = "right")
dev.off()


########################################
############### Figure S5 ##############
########################################
# look at the example from Mar 1964
rm(list = ls())
# now let's calculate anomalies and plot them
load('Processed_Data/PR_WAT.RData')
load('Processed_Data/Z_700.RData')

# just look at the western hemisphere
PR_WAT = PR_WAT %>% dplyr::filter(lon > 180)
Z_700 = Z_700 %>% dplyr::filter(lon > 180)

dat = cbind(Z_700, PR_WAT$pr_wat)
colnames(dat)[6] = "pr_wat"

rm(Z_700,PR_WAT)

load(file = "Processed_Data/monthly_mean_Z_PR_WAT.RData")

mar_1964 = dat %>% dplyr::filter(lubridate::year(date) == 1964 &
                                 lubridate::month(date) == 3) %>%
                   dplyr::mutate(month = lubridate::month(date))

mar_1964_anom = merge(mar_1964, monthly_mean_Z_PR_WAT, by = c("lat", "lon", "month")) %>%
                      dplyr::mutate(z_700_anom = z_700 - z_700_clim,
                                    pr_wat_anom = pr_wat - pr_wat_clim)

# mark the days when an RIP occurred
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')
RIP_dates = CPC_mod_cell_RIP %>% dplyr::filter(RIP == 1) %>%
                                 dplyr::select(date) %>%
                                 data.frame()

mar_1964_anom = mar_1964_anom %>% dplyr::mutate(date_marked = ifelse(date %in% c(RIP_dates$date),paste0(date,"***"),as.character(date)))

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

subset = mar_1964_anom %>% dplyr::filter(date > "1964-03-02" & 
                                         date < "1964-03-12" &
                                         lat > 10 & lat < 60)

z_700.lim = max(-floor(min(subset$z_700_anom)/2)*2,ceiling(max(subset$z_700_anom)/2)*2)

pdf("Final figures/Figure_S5.pdf", width=10, height=7.5)
ggplot(data = subset) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(z_700_anom))) + 
  facet_wrap(~date_marked, ncol = 3) +
  scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                       limits = c(-z_700.lim,z_700.lim), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  stat_contour(aes(x = (lon-360),y = lat, z=pr_wat_anom), 
               breaks = seq(4,24,4), size=0.25, col = "black") +
  stat_contour(aes(x = (lon-360),y = lat, z=pr_wat_anom), 
               breaks = seq(-4,-24,-4), size=0.25, col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y,z=NULL), size = 0.25) + 
  scale_y_continuous(limits = c(10,60)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-155,-45), breaks = c(-140, -100, -60)) +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -95, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points)
dev.off()


########################################
############### Figure 6 ###############
########################################

# now we compare the GCM Z_700 to the OBS Z_700 associated with RIP days
rm(list = ls())
source('R/GetSeasonDate.R')

# load xtrs from the CPC calcs
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')

RIP_dates = CPC_mod_cell_RIP %>% dplyr::filter(RIP == 1) %>%
                                 dplyr::select(date) %>%
                                 data.frame()

load('Processed_Data/Z_700.RData')

# just look at part of the western hemisphere
Z_700 = Z_700 %>% dplyr::filter(lon > 205 & lon < 315)

monthly_mean_Z = Z_700 %>% dplyr::mutate(month = month(date)) %>%
                           dplyr::group_by(lon,lat,month) %>%
                           dplyr::summarise(z_700_clim = mean(z_700))

save(monthly_mean_Z, file = "Processed_Data/monthly_mean_Z.RData")

# calculate the mean value of relevant days 
RIP_event = Z_700 %>% dplyr::filter(date %in% c(RIP_dates$date)) %>%
                      data.table() 

RIP_event = RIP_event %>% dplyr::mutate(month = month(date))

# mark locations where over 80% of the observations are positive or negative
sig.level = 0.8
RIP_event_anom = merge(RIP_event, monthly_mean_Z, by = c("lat", "lon", "month")) %>%
                        dplyr::mutate(z_700_anom = z_700 - z_700_clim,
                                      z_700_positive = ifelse(z_700_anom > 0, 1, 0)) %>% 
                        dplyr::group_by(lon,lat,season) %>%
                        dplyr::summarise(z_700_anom = mean(z_700_anom),
                                         z_700 = mean(z_700),
                                         prop.pos = mean(z_700_positive))

RIP_event_anom_obs = RIP_event_anom %>% dplyr::mutate(z_700_sig = ifelse(z_700_anom > 0 & prop.pos > sig.level,1,
                                                                   ifelse(z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))

RIP_event_anom_obs$model = "OBS & NCEP/NCAR"


# load xtrs from the mod runs
load(file = 'Processed_Data/mod_RIP.RData')

RIP_dates_mod = mod_RIP %>% dplyr::filter(RIP == 1) %>%
                            dplyr::select(date, season, model) %>%
                            data.frame()


# now let's calculate anomalies and plot them for the ta_700
load(file = 'Processed_Data/Z_700_mod_field.RData')

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(season = GetSeasonDate(date))
colnames(Z_700_mod_field)[5] = "z_700"

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(month = month(date),
                                                    model = paste0("GCM ",model))

monthly_mean_Z_mod = Z_700_mod_field %>% dplyr::group_by(lon,lat,month,model) %>%
                                         dplyr::summarise(z_700_clim = mean(z_700),
                                                          z_700_sd = sd(z_700))

save(monthly_mean_Z_mod, file = "Processed_Data/monthly_mean_ZG_MOD.RData")

RIP_event_mod = Z_700_mod_field %>% dplyr::filter(date %in% c(RIP_dates_mod$date)) %>%
                                    data.table()

# now exclude the models that did not have RIPs on days when another model had a RIP
RIP_dates_mod$include = 1
RIP_dates_mod = RIP_dates_mod %>% dplyr::mutate(model = paste0("GCM ",model))

RIP_event_mod_subset = merge(RIP_event_mod,RIP_dates_mod, by = c("date","model","season"), all.x = TRUE)

RIP_event_mod_anom = merge(RIP_event_mod_subset[!is.na(RIP_event_mod_subset$include),], monthly_mean_Z_mod, by = c("lat", "lon", "month","model")) %>%
                        dplyr::mutate(z_700_anom = z_700 - z_700_clim,
                                      z_700_positive = ifelse(z_700_anom > 0, 1, 0)) %>% 
                        dplyr::group_by(lon,lat,season,model) %>%
                        dplyr::summarise(z_700_anom = mean(z_700_anom),
                                         z_700 = mean(z_700),
                                         prop.pos = mean(z_700_positive))

sig.level = 0.8
RIP_event_mod_anom = RIP_event_mod_anom %>% dplyr::mutate(z_700_sig = ifelse(z_700_anom > 0 & prop.pos > sig.level,1,
                                                                       ifelse(z_700_anom < 0 & prop.pos < (1-sig.level),1,0)))
RIP_event_anom_all = rbind(RIP_event_mod_anom, RIP_event_anom_obs)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
source("R/load_basin_boundary.R")

# define a blank list of plots
plots = list()

for(mmodel in 1:length(unique(RIP_event_anom_all$model))){
  subset0 = RIP_event_anom_all[RIP_event_anom_all$model == unique(RIP_event_anom_all$model)[mmodel],]
  subset = subset0[subset0$season == "MAM",]
  
  plots[[mmodel]] =
    ggplot(data = subset) + 
    geom_tile(aes(x = (lon-360),y = lat,fill=(z_700))) + 
    scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                         limits = c(2800,3200),
                         midpoint = 3000,low="blue", mid = "white",  high = "red",na.value = "grey",
                         guide = FALSE) +
    geom_point(data = subset[subset$z_700_sig == 1, ], 
               aes(x = (lon-360),y = lat),shape = "X", size = 1.5, alpha = 0.6) +
    stat_contour(aes(x = (lon-360),y = lat, z=z_700_anom), 
                 breaks = seq(15,200,15), size=0.35, col = "black") +
    stat_contour(aes(x = (lon-360),y = lat, z=z_700_anom), 
                 breaks = seq(-15,-200,-15), size=0.35, col = "black", linetype = "dashed") +
    geom_path(data=world, aes(x,y,z=NULL), size = 0.2, alpha = 0.6) + 
    scale_y_continuous(limits = c(10,60)) +
    labs(x = "Longitude", y = "Latitude") +
    scale_x_continuous(limits = c(-155,-45), breaks = c(-140, -100, -60))  +
    theme_bw() +
    coord_map("ortho", orientation=c(40, -95, 0)) +
    geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
    ggtitle(unique(RIP_event_anom_all$model)[mmodel])
  
}


# Function to extract legend (from: http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram)
source('R/g_legend.R')

plot_legend <- g_legend(ggplot(data = subset) + 
                          geom_tile(aes(x = (lon-360),y = lat,fill=(z_700))) + 
                          scale_fill_gradient2(name=expression(paste(Z[700], " (m)")),
                                               limits = c(2800,3200),
                                               midpoint = 3000,low="blue", mid = "white",  high = "red",na.value = "grey") +
                          geom_point(data = subset[subset$z_700_sig == 1, ], 
                                     aes(x = (lon-360),y = lat),shape = "X", size = 1.5, alpha = 0.6) +
                          stat_contour(aes(x = (lon-360),y = lat, z=z_700_anom), 
                                       breaks = seq(15,200,15), size=0.25, col = "black") +
                          stat_contour(aes(x = (lon-360),y = lat, z=z_700_anom), 
                                       breaks = seq(-15,-200,-15), size=0.4, col = "black", linetype = "dashed") +
                          geom_path(data=world, aes(x,y,z=NULL), size = 0.2, alpha = 0.6) + 
                          scale_y_continuous(limits = c(15,60)) +
                          labs(x = "Longitude", y = "Latitude") +
                          scale_x_continuous(limits = c(-155,-45)) +
                          theme_bw() +
                          coord_map("ortho", orientation=c(40, -95, 0)) +
                          geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points))

# define the layout of the plots
lay <- rbind(c(1,1,2,2,3,3,7),
             c(4,4,5,5,6,6,7))

pdf("Final figures/Figure_6.pdf", width=10, height=5)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],
             plots[[4]],plots[[5]],plots[[6]],
             plot_legend, layout_matrix = lay)
dev.off()


########################################
############### Figure S6 ##############
########################################
rm(list = ls())
source('R/GetSeasonDate.R')
load(file = 'Processed_Data/Z_700_mod_field.RData')

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
                                      dplyr::filter(season == 'MAM')

colnames(Z_700_mod_field)[5] = "z_700"

Z_700_mod_field = Z_700_mod_field %>% dplyr::mutate(model = paste0("GCM ",model))

season_mean_sd_mod = Z_700_mod_field %>% dplyr::group_by(lon,lat,model) %>%
                                         dplyr::summarise(z_700_clim = mean(z_700),
                                                          z_700_sd = sd(z_700))

season_mean_sd_mod_long = melt(season_mean_sd_mod, id.vars = c('lon', 'lat', 'model'))

# now load the observed data
load('Processed_Data/Z_700.RData')

Z_700 = Z_700 %>% dplyr::mutate(season = GetSeasonDate(date)) %>%
                  dplyr::filter(season == 'MAM')
colnames(Z_700)[5] = "z_700"


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

pdf("Final figures/Figure_S6.pdf", width=10, height=5) # NOTE: the original submission included the state boundaries
ggplot(data = plot_data) + 
  geom_tile(aes(x = (lon-360),y = lat,fill=(z_700_sd))) + 
  facet_wrap(~model, ncol = 3) +
  scale_fill_gradient2(name=expression(paste(Delta, sigma[Z[700]], " (m)")),
                       limits = c(-22,22),
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  geom_path(data=world, aes(x,y,z=NULL), size = 0.2, alpha = 0.6) + 
  scale_y_continuous(limits = c(15,60)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = c(-145,-45), breaks = c(-140, -100, -60))  +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -95, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points)
dev.off()


########################################
############### Figure S7 ##############
########################################
# now let's plot the average jet in the reanalysis and the models after making the 
rm(list = ls())
source('R/GetSeasonDate.R')

load(file = 'Processed_Data/U_200_mod_MAM.RData')
load(file = 'Processed_Data/U_200_MAM.RData')

U_200_mod_MAM = U_200_mod_MAM %>% dplyr::mutate(model = paste0("GCM ",model))
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
    geom_path(data=world, aes(x,y,z=NULL), size = 0.2, alpha = 0.6) + 
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
                           geom_path(data=world, aes(x,y,z=NULL), size = 0.2, alpha = 0.6) + 
                           scale_y_continuous(limits = c(10,60)) +
                           labs(x = "Longitude", y = "Latitude") +
                           scale_x_continuous(limits = c(-160,-30)) +
                           theme_bw() +
                           coord_map("ortho", orientation=c(40, -90, 0)) +
                           geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.5, data = basin_points) +
                           theme(legend.position = "right"))



lay <- rbind(c(1,1,2,2,3,3,7),
             c(4,4,5,5,6,6,7))

pdf("Final figures/Figure_S7.pdf", width=10, height=5)
grid.arrange(plots[[2]],plots[[3]],plots[[4]],
             plots[[5]],plots[[6]],plots[[1]],
             plot_legend, layout_matrix = lay)
dev.off()

########################################
############### Figure 7 ###############
########################################
# plot dipole locations and the lead up to a RIP
rm(list = ls())
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')
load(file = 'Processed_Data/EWD.RData')
load(file = 'Processed_Data/MHC.RData')

# embed the laging and leading values of EWD and MHC_anom
RIP_EWD_MHC = merge(merge(CPC_mod_cell_RIP,EWD, by = c("date","season")),MHC,by = c("date","season")) %>%
                          dplyr::select(c(date, season, RIP, EWD, MHC_anom)) %>%
                          dplyr::mutate(`EWD_-1` = lag(EWD, 1), `MHC_anom_-1` = lag(MHC_anom, 1),
                                        `EWD_-2` = lag(EWD, 2), `MHC_anom_-2` = lag(MHC_anom, 2),
                                        `EWD_-3` = lag(EWD, 3), `MHC_anom_-3` = lag(MHC_anom, 3),
                                        `EWD_-4` = lag(EWD, 4), `MHC_anom_-4` = lag(MHC_anom, 4),
                                        `EWD_-5` = lag(EWD, 5), `MHC_anom_-5` = lag(MHC_anom, 5),
                                        `EWD_00` = EWD,         `MHC_anom_00` = MHC_anom,
                                        `EWD_01` = lead(EWD, 1), `MHC_anom_01` = lead(MHC_anom, 1),
                                        `EWD_02` = lead(EWD, 2), `MHC_anom_02` = lead(MHC_anom, 2),
                                        `EWD_03` = lead(EWD, 3), `MHC_anom_03` = lead(MHC_anom, 3)) %>%
                          dplyr::group_by(season) %>%
                          dplyr::mutate(EWD_mean = mean(EWD, na.rm = TRUE),
                                        MHC_anom_mean = mean(MHC_anom, na.rm = TRUE)) %>%
                          dplyr::select(-EWD, -MHC_anom)

# only select the RIP days
RIP_EWD_MHC_long = melt(RIP_EWD_MHC, id.vars = c("season", "date", "RIP", "EWD_mean", "MHC_anom_mean")) %>%
                        dplyr::filter(RIP == 1)

# compute the percentiles of interest
RIP_EWD_MHC_time = RIP_EWD_MHC_long %>% dplyr::group_by(season, variable) %>%
                                        dplyr::summarise(median = median(value, na.rm = TRUE),
                                                         `5th` = quantile(value, probs = 0.05, na.rm = TRUE),
                                                         `95th` = quantile(value, probs = 0.95, na.rm = TRUE),
                                                         `25th` = quantile(value, probs = 0.25, na.rm = TRUE),
                                                         `75th` = quantile(value, probs = 0.75, na.rm = TRUE),
                                                         EWD_mean = mean(EWD_mean),
                                                         MHC_anom_mean = mean(MHC_anom_mean))

# separate 'variable' into 'EWD' or 'MHC' and the lag number
# load function to efficiently do this
source("R/SubstrRightLeft.R")

RIP_EWD_MHC_time = RIP_EWD_MHC_time %>% dplyr::mutate(lag = as.numeric(substrRight(as.character(variable),2)),
                                                      var = substrLeft(as.character(variable),3))

EWD_time =
  ggplot(RIP_EWD_MHC_time[RIP_EWD_MHC_time$season == "MAM" &
                            RIP_EWD_MHC_time$var == "EWD"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(aes(yintercept = EWD_mean), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab("EWD Index") +
  xlab("Days after RIP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
  theme_bw()

MHC_anom_time =
  ggplot(RIP_EWD_MHC_time[RIP_EWD_MHC_time$season == "MAM" &
                            RIP_EWD_MHC_time$var == "MHC_anom"	, ]) +
  geom_ribbon(aes(x = lag, y = median, ymax = `75th`, ymin = `25th`), alpha = 0.5) +
  geom_ribbon(aes(x = lag, y = median, ymax = `95th`, ymin = `5th`), alpha = 0.5) +
  geom_line(aes(x = lag, y = median)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylab("MHC Index") +
  xlab("Days after RIP event") +
  scale_x_continuous(breaks = c(seq(-5,3,by=1))) +
  theme_bw()


load(file = 'Processed_Data/EWD_boxes.RData')
load(file = 'Processed_Data/MHC_box.RData')

# plot the locations of the dipole index
world <- data.frame(map("world", plot=FALSE)[c("x","y")])
state <- data.frame(map("state", plot=FALSE)[c("x","y")])

source("R/load_basin_boundary.R")

EWD_MHC_location =
  ggplot() + 
  geom_rect(data = EWD_boxes,
            aes(xmin=lon.min, xmax=lon.max, ymin=lat.min, ymax=lat.max, color = factor(box), fill = factor(box)),
            alpha=0.5) + 
  geom_rect(data = MHC_box,
            aes(xmin=lon.min-360, xmax=lon.max-360, ymin=lat.min, ymax=lat.max),
            alpha=0.5) +
  geom_path(data=state, aes(x,y,z=NULL), size = 0.25) + 
  geom_path(data=world, aes(x,y,z=NULL), size = 0.25) + 
  scale_y_continuous(limits = c(20,55)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_x_continuous(limits = c(-122,-58)) +
  theme_bw() +
  coord_map("ortho", orientation=c(40, -90, 0)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  theme(legend.position="none") +
  labs(x = "Longitude", y = "Latitude")

# Figure 7
pdf('Final figures/Figure_7.pdf', width = 12, height = 3)
grid.arrange(EWD_MHC_location,
             EWD_time,
             MHC_anom_time,
             ncol = 3)
dev.off()

########################################
############### Figure 8 ###############
########################################
# plot EWD and MHC distributions
rm(list = ls())
load(file = 'Processed_Data/CPC_mod_cell_RIP.RData')
load(file = 'Processed_Data/EWD.RData')
load(file = 'Processed_Data/MHC.RData')

# embed the laging and leading values of EWD and MHC_anom for the observed
RIP_EWD_MHC_obs = merge(merge(CPC_mod_cell_RIP,EWD, by = c("date","season")),MHC,by = c("date","season")) %>%
                              dplyr::select(c(date, season, RIP, EWD, MHC_anom)) %>%
                              dplyr::mutate(model = "OBS")

load(file = 'Processed_Data/mod_RIP.RData')
load(file = 'Processed_Data/EWD_mod.RData')
load(file = 'Processed_Data/MHC_mod.RData')

# embed the laging and leading values of EWD and MHC_anom for the GCM
RIP_EWD_MHC_mod = merge(merge(mod_RIP,EWD_mod, by = c("date","season","model")),MHC_mod,by = c("date","season","model")) %>%
                              dplyr::select(c(date, season, model, RIP, EWD, MHC_anom))

# combine the observed and GCM
RIP_EWD_MHC_all = rbind(RIP_EWD_MHC_obs, RIP_EWD_MHC_mod)

# mark whether EWD is positive
RIP_EWD_MHC_all = RIP_EWD_MHC_all %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0))

save(RIP_EWD_MHC_all, file = 'Processed_Data/RIP_EWD_MHC_all.RData')

# check the correlations for MHC and EWD
cors = RIP_EWD_MHC_all %>% dplyr::group_by(season,model) %>%
                           dplyr::summarise(pearson = cor(EWD, MHC_anom, use = "complete.obs", method = "pearson"),
                                            spearman = cor(EWD, MHC_anom, use = "complete.obs", method = "spearman"),
                                            pearson_lag = cor(EWD, lag(MHC_anom,1), use = "complete.obs", method = "pearson"),
                                            spearman_lag = cor(EWD, lag(MHC_anom,1), use = "complete.obs", method = "spearman"))

cors[cors$season %in% c("MAM"),]

# OUTPUT:
# Source: local data frame [6 x 6]
# Groups: season [1]
# 
# season model   pearson  spearman pearson_lag spearman_lag
# <chr> <chr>     <dbl>     <dbl>       <dbl>        <dbl>
#  1    MAM     1 0.4117560 0.4274333   0.3795099    0.3910408
#  2    MAM     2 0.3766251 0.3838416   0.3335699    0.3363547
#  3    MAM     3 0.4176295 0.4341362   0.3830847    0.3969970
#  4    MAM     4 0.3714886 0.3948615   0.3248813    0.3459591
#  5    MAM     5 0.4380729 0.4560737   0.4189722    0.4290108
#  6    MAM   OBS 0.3150146 0.3171686   0.3301132    0.3329870


# plot the cdfs for the EWD and MHC
assign("EWD_cdf_plot",
       ggplot() + 
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model != "OBS",], 
                   aes(x = EWD, group = model), linetype = "dashed") +
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model == "OBS",], 
                   aes(x = EWD)) +
         theme(legend.position="none") +
         xlab("EWD Index") +
         ylab("F(EWD Index)") +
         coord_cartesian(xlim = c(-250,250)) +
         theme_bw()
)

assign("EWD_cdf_plot_zoom",
       ggplot() + 
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model != "OBS",], 
                   aes(x = EWD, group = model), linetype = "dashed") +
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model == "OBS",], 
                   aes(x = EWD)) +
         theme(legend.position="none") +
         xlab("EWD Index (upper tail)") +
         ylab("F(EWD Index)") +
         coord_cartesian(xlim = c(75,250), ylim = c(0.9,1)) +
         theme_bw()
)

assign("MHC_cdf_plot",
       ggplot() + 
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model != "OBS",], 
                   aes(x = MHC_anom, group = model), linetype = "dashed") +
         stat_ecdf(data = RIP_EWD_MHC_all[RIP_EWD_MHC_all$season %in% c("MAM") & RIP_EWD_MHC_all$model == "OBS",], 
                   aes(x = MHC_anom)) +
         theme(legend.position="none") +
         theme(legend.position="none") +
         xlab("MHC Index") +
         ylab("F(MHC Index)") +
         coord_cartesian(xlim = c(-5,5.2)) +
         theme_bw()
)

pdf('Final figures/Figure_8.pdf', width = 11, height = 3)
grid.arrange(EWD_cdf_plot,
             EWD_cdf_plot_zoom,
             MHC_cdf_plot,
             nrow = 1)
dev.off()


########################################
############### Figure 9 ###############
########################################

tail_dependence_data = data.frame(date = RIP_EWD_MHC_all$date,
                                  season = RIP_EWD_MHC_all$season,
                                  model = RIP_EWD_MHC_all$model,
                                  EWD = RIP_EWD_MHC_all$EWD,
                                  MHC_anom = RIP_EWD_MHC_all$MHC_anom)


# load function to calculate tail dependence coefficient (tdc)
source('R/tdc.R')
lag.max = 10

# initialize lists to store the tdc for each lag
tdc.EWD = tdc.MHC = list()
for(llag in 1:lag.max){
  tdc.EWD[[llag]] = data.frame(model = unique(tail_dependence_data$model),
                               P_0.9 = NA,
                               P_0.95 = NA,
                               P_0.99 = NA)
  tdc.MHC[[llag]] = data.frame(model = unique(tail_dependence_data$model),
                               P_0.9 = NA,
                               P_0.95 = NA,
                               P_0.99 = NA)
}


for(llag in 1:lag.max){
  tmp = tail_dependence_data %>% dplyr::group_by(model) %>%
                                 dplyr::mutate(EWD_lag = lag(EWD, llag),
                                               MHC_anom_lag = lag(MHC_anom, llag))%>%
                                 dplyr::filter(season == "MAM")
  
  for(mmodel in 1:length(unique(tmp$model))){
    for(pper in 1:(ncol(tdc.EWD[[llag]])-1)){
      tdc.EWD[[llag]][mmodel,pper+1] = tdc(tmp$EWD[tmp$model == unique(tmp$model)[mmodel]], 
                                           tmp$EWD_lag[tmp$model == unique(tmp$model)[mmodel]], 
                                           p = as.numeric(strsplit(names(tdc.EWD[[llag]])[pper+1], "_")[[1]][2]), 
                                           upper = TRUE)
      
      tdc.MHC[[llag]][mmodel,pper+1] = tdc(tmp$MHC_anom[tmp$model == unique(tmp$model)[mmodel]], 
                                           tmp$MHC_anom_lag[tmp$model == unique(tmp$model)[mmodel]], 
                                           p = as.numeric(strsplit(names(tdc.MHC[[llag]])[pper+1], "_")[[1]][2]), 
                                           upper = TRUE)
    }
    
  }
}

tdc.EWD_long = melt(tdc.EWD) %>% setnames("L1","lag") %>%
                                 dplyr::mutate(variable = gsub("_", " = ", as.character(variable)))

EWD_tail_persist = 
  ggplot() +
  geom_line(data = tdc.EWD_long[tdc.EWD_long$model != "OBS",],
            aes(lag, value*100, group = model), linetype = "dashed") +
  geom_line(data = tdc.EWD_long[tdc.EWD_long$model == "OBS",],
            aes(lag, value*100)) +
  facet_wrap(~variable) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1:10)) +
  labs(y = expression(Pr(F(EWD[t])~ ">" ~ P ~ "|" ~ F(EWD[t-lag]) ~ ">" ~ P)))

tdc.MHC_long = melt(tdc.MHC) %>% setnames("L1","lag") %>%
                                 dplyr::mutate(variable = gsub("_", " = ", as.character(variable)))

MHC_tail_persist = 
  ggplot() +
  geom_line(data = tdc.MHC_long[tdc.MHC_long$model != "OBS",],
            aes(lag, value*100, group = model), linetype = "dashed") +
  geom_line(data = tdc.MHC_long[tdc.MHC_long$model == "OBS",],
            aes(lag, value*100)) +
  facet_wrap(~variable) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1:10)) +
  labs(y = expression(Pr(F(MHC[t])~ ">" ~ P ~ "|" ~ F(MHC[t-lag]) ~ ">" ~ P)))


pdf('Final figures/Figure_9.pdf', width = 10, height = 6)
grid.arrange(EWD_tail_persist, 
             MHC_tail_persist,
             nrow = 2)
dev.off()

########################################
############### Figure S8 ############## !! this is still messy !!
########################################
rm(list = ls())
load(file = 'Processed_Data/RIP_EWD_MHC_all.RData')

# rename the model
RIP_EWD_MHC_all = RIP_EWD_MHC_all %>% dplyr::mutate(model = ifelse(model != "OBS", paste0("GCM ", model), model))

alpha = 1
locfit_fits_MAM = list()
for(mm in 1:length(unique(RIP_EWD_MHC_all$model))){
  
  mmodel = unique(RIP_EWD_MHC_all$model)[mm]
  locfit_data = RIP_EWD_MHC_all %>% dplyr::filter(model == mmodel &
                                                  season == "MAM")
  
  locfit_fits_MAM[[mm]] <- locfit(RIP ~ EWD + MHC_anom,
                                  data = locfit_data,
                                  family="binomial",
                                  alpha = alpha)
}
names(locfit_fits_MAM) = unique(RIP_EWD_MHC_all$model)


# now do the same for pooled GCM ensemble members
locfit_data = RIP_EWD_MHC_all %>% dplyr::filter(model != "OBS" &
                                                season == "MAM")

locfit_fits_ens_MAM <- locfit(RIP ~ EWD + MHC_anom,
                              data = locfit_data,
                              family="binomial",
                              alpha = alpha)


# now interpolate onto a regular grid
grid.size.MHC_anom = 0.75
grid.size.EWD = 30
reg_grid_MAM = 
  expand.grid(MHC_anom = seq(-9,9, by = grid.size.MHC_anom), 
              EWD = seq(-300,300, by = grid.size.EWD))

# round to grid.size and find the bins in the regular grid with less than 'min.obs' observations
min.obs = 5

bins_low_count0 = RIP_EWD_MHC_all %>% dplyr::mutate(MHC_anom = grid.size.MHC_anom*round(MHC_anom/(grid.size.MHC_anom)),
                                                    EWD = grid.size.EWD*(round(EWD/grid.size.EWD))) %>%
                                      dplyr::group_by(model, season, MHC_anom, EWD) %>%
                                      dplyr::summarise(count = length(RIP))


all_bins = data.frame(expand.grid(MHC_anom = unique(reg_grid_MAM$MHC_anom), 
                                  EWD = unique(reg_grid_MAM$EWD),
                                  model = unique(RIP_EWD_MHC_all$model),
                                  season = unique(RIP_EWD_MHC_all$season)),
                                  count = 0)


all_bins_count1 = merge(bins_low_count0, all_bins, by = c("model", "MHC_anom",
                                                          "EWD", "season"), all = TRUE) %>%
                        dplyr::mutate(count = ifelse(is.na(count.x), count.y, count.x),
                                      variable = factor(model))

all_bins_count_ens = all_bins_count1 %>% dplyr::group_by(MHC_anom, EWD, season) %>%
                                         dplyr::summarise(count = min(count)) %>%
                                         dplyr::mutate(variable = "ENS - OBS") %>%
                                         dplyr::ungroup()

all_bins_count1 = all_bins_count1 %>% dplyr::select(c(MHC_anom, EWD, season, count, variable))

all_bins_count = rbind(all_bins_count1, all_bins_count_ens)

for(mm in 1:length(unique(RIP_EWD_MHC_all$model))){
  reg_grid_MAM[,mm+2] = predict(locfit_fits_MAM[[mm]], newdata = reg_grid_MAM)
  colnames(reg_grid_MAM)[mm+2] = unique(RIP_EWD_MHC_all$model)[mm]
}
reg_grid_MAM[,9] = predict(locfit_fits_ens_MAM, newdata = reg_grid_MAM)
colnames(reg_grid_MAM)[9] = "ENS"

reg_grid_MAM = reg_grid_MAM %>% dplyr::mutate("ENS - OBS" = ENS - OBS) %>%
  dplyr::select(-c(ENS))

reg_grid_MAM_long = melt(reg_grid_MAM, id.vars = c("MHC_anom","EWD"))

obs_mod_plot = 
  ggplot() +
  geom_tile(data = reg_grid_MAM_long[reg_grid_MAM_long$variable != "ENS - OBS",],
            aes(x = MHC_anom,
                y = EWD,
                fill = value)) +
  scale_fill_gradient2(name = "Pr(RIP)",
                       limits = c(0,0.35), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "green",
                       breaks = c(0,0.1,0.2,0.3), 
                       labels = c(0,0.1,0.2,0.3)) +
  geom_tile(data = all_bins_count[all_bins_count$count < min.obs &
                                    all_bins_count$season %in% c("MAM") &
                                    all_bins_count$variable %in% unique(reg_grid_MAM_long$variable)[1:6],],
            aes(x = MHC_anom,
                y = EWD),
            fill = "grey") +
  coord_cartesian(xlim = c(-5.3,5.3), ylim = c(-235,235)) +
  labs(x = "MHC Index", y = "EWD Index") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ variable, nrow = 2)


obs_ens_plot = 
  ggplot() +
  geom_tile(data = reg_grid_MAM_long[reg_grid_MAM_long$variable == "ENS - OBS",],
            aes(x = MHC_anom,
                y = EWD,
                fill = value)) +
  scale_fill_gradient2(name = expression(Delta ~ "Pr(RIP)"),
                       limits = c(-0.12,0.12), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "green",
                       breaks = c(-0.1,0,0.1), 
                       labels = c(-0.1,0,0.1)) +
  geom_tile(data = all_bins_count[all_bins_count$count < min.obs &
                                    all_bins_count$season %in% c("MAM") &
                                    all_bins_count$variable == "ENS - OBS",],
            aes(x = MHC_anom,
                y = EWD),
            fill = "grey") +
  coord_cartesian(xlim = c(-5.3,5.3), ylim = c(-235,235)) +
  labs(x = "MHC Index", y = "EWD Index") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ variable)

lay = rbind(c(1,2))

pdf('Final figures/Figure_S8.pdf', height = 5, width = 12)
grid.arrange(obs_mod_plot,
             obs_ens_plot,
             ncol = 2,
             layout_matrix = lay)
dev.off()


########################################
############### Figure 10 ##############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")

# load RIP/EWD/MHC records
load(file = 'Processed_Data/RIP_EWD_MHC_all.RData')

# add EWD "+" indicator and retain the MAM season
RIP_EWD_MHC_all_MAM = RIP_EWD_MHC_all %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0),
                                                        RIP_lag1 = lag(RIP,1)) %>%
                                          dplyr::filter(season == "MAM")

# fit the model on the observations
RIP_EWD_MHC_mod = glm(RIP ~  MHC_anom : EWD.positive + factor(RIP_lag1) : EWD, 
                      data = RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model == "OBS",], family = binomial(link = "logit"))

n.sims = 25

# initialize the first RIP_lag1 as 0 for each model for each year
RIP_EWD_MHC_mod_MAM = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model != "OBS") %>%
                                              dplyr::select(-c(RIP, RIP_lag1)) %>% 
                                              dplyr::mutate(RIP_lag1 = ifelse(lubridate::month(date) == 3 & lubridate::day(date) == 1,
                                                                              0, NA))


RIP_sim_df = data.frame(date = unique(RIP_EWD_MHC_mod_MAM$date),
                        matrix(nrow = nrow(RIP_EWD_MHC_mod_MAM)/5, ncol = n.sims))

RIP_sim	= list(RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df)
names(RIP_sim) = unique(RIP_EWD_MHC_mod_MAM$model)

set.seed(0)
for(nn in 1:n.sims){
  for(mmod in unique(RIP_EWD_MHC_mod_MAM$model)){
    tmp_data = RIP_EWD_MHC_mod_MAM %>% dplyr::filter(model == mmod)
    for(rrow in 1:nrow(tmp_data)){
      RIP_sim[[mmod]][rrow,nn+1] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
      tmp_data[rrow + 1,]$RIP_lag1 = RIP_sim[[mmod]][rrow,nn+1]
    }
    print(paste0("model #", mmod, " complete (sim #", nn, ")"))
  }
}

# save this simulation
save(RIP_sim, file = 'Processed_Data/historical_sim.RData')

RIP_sim_df = do.call("rbind", RIP_sim) %>% dplyr::mutate(model = rep(1:5, each = nrow(RIP_EWD_MHC_mod_MAM)/5))

RIP_sim_long = melt(RIP_sim_df, id.vars = c("date","model"), variable.name = "sim", value.name = "RIP_sim")

RIP_sim_by_year = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                   dplyr::group_by(year, sim, model) %>%
                                   dplyr::summarise(RIP_sim = sum(RIP_sim))

RIP_EWD_MHC_obs_year = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model == "OBS") %>% 
                                               dplyr::mutate(year = lubridate::year(date)) %>%
                                               dplyr::group_by(year) %>%
                                               dplyr::summarise(RIP = sum(RIP))

# calculate the mean for each member and the full ensemble mean
RIP_pred_by_year_sim_mean = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                             dplyr::group_by(year, sim, model) %>%
                                             dplyr::summarise(RIP_sim = sum(RIP_sim)) %>%
                                             dplyr::group_by(year, model) %>%
                                             dplyr::summarise(RIP_sim = mean(RIP_sim))

RIP_pred_by_year_ens_mean = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                             dplyr::group_by(year, sim, model) %>%
                                             dplyr::summarise(RIP_sim = sum(RIP_sim)) %>%
                                             dplyr::group_by(year) %>%
                                             dplyr::summarise(RIP_sim = mean(RIP_sim))

# now compute the 10-year running mean for each of the records
source('R/ma.R')
smoothing_window = 10
RIP_sim_by_year = RIP_sim_by_year %>% dplyr::group_by(model,sim) %>%
                                      dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

RIP_pred_by_year_sim_mean = RIP_pred_by_year_sim_mean %>% dplyr::group_by(model) %>%
                                                          dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

RIP_EWD_MHC_obs_year = RIP_EWD_MHC_obs_year %>% dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window))

RIP_pred_by_year_ens_mean = RIP_pred_by_year_ens_mean %>% dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

line.width = 1
alpha.line = 1
col.line = "grey80"

RIP_mod_obs_compare_1 =
  ggplot() + 
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 1,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 2,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 3,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 4,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 5,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_pred_by_year_sim_mean,
            aes(year, y = RIP_sim_smooth, group = model),
            linetype = "dashed", size = line.width/2) +
  geom_line(data = RIP_pred_by_year_ens_mean,
            aes(year, y = RIP_sim_smooth),
            linetype = "dashed", size = line.width, col = "red") +
  geom_line(data = RIP_EWD_MHC_obs_year,
            aes(year, y = RIP_smooth),
            linetype = "solid", size = line.width) +
  ylab("# RIP") +
  coord_cartesian(xlim = c(1955,2006),
                  ylim = c(0,5.5)) +
  xlab("Year") +
  annotate("text", label = c("a)"), x = 1956, y = 5.3, size = 6, colour = "black") +
  theme_bw()


bin_breaks = seq(-0.5,12.5,1)
bin_labels = as.character(bin_breaks-0.5)
hist_lims = data.frame(xlim = c(0.5,11.6),
                       ylim = c(0,40))
RIP_mod_obs_compare_2 =
  ggplot() + 
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 1,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 2,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 3,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 4,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 5,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year,
           aes(x = RIP_sim, y =..count../(25*5)),
           geom = "step", linetype = "dashed", size = line.width,
           breaks = bin_breaks, col = "red") +
  stat_bin(data = RIP_EWD_MHC_obs_year,
           aes(x = RIP, y=..count..),
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  annotate("text", label = "b)", x = 0.75, y = 38.75, size = 6, colour = "black") +
  labs(x = "# RIP",
       y = "count") +
  coord_cartesian(xlim = hist_lims$xlim, 
                  ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()


pdf('Final figures/Figure_10.pdf', width = 11, height = 2.75)
grid.arrange(RIP_mod_obs_compare_1,
             RIP_mod_obs_compare_2,
             nrow = 1)
dev.off()

########################################
############### Figure 11 ##############  !! could use some better commenting in this section !!
########################################

# first define new variables of lagged RIPs
RIP_lag_obs = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model == "OBS", 
                                                    season == "MAM") %>%
                                      dplyr::mutate(RIP_lag1 = lag(RIP, 1),
                                                    RIP_lag_4_7 = ifelse(lag(RIP, 4) == 1, 1,
                                                                   ifelse(lag(RIP, 5) == 1, 1,
                                                                    ifelse(lag(RIP, 6) == 1, 1,
                                                                     ifelse(lag(RIP, 7) == 1, 1,0))))) %>%
                                      dplyr::select(c(date, RIP, season , RIP_lag1, RIP_lag_4_7))


# need to remove the first 7 days from each year 
# (because we cannot check the lagged days associated with those days in the simulation round)
RIP_lag_obs = RIP_lag_obs %>% dplyr::filter(!(lubridate::month(date) == 3 & lubridate::day(date) %in% 1:7))


RIP_sim_lag_mod = RIP_sim_long  %>% dplyr::group_by(model, sim) %>%
                                    dplyr::mutate(season = GetSeasonDate(date),
                                                  RIP_lag1 = lag(RIP_sim, 1),
                                                  RIP_lag_4_7 = ifelse(lag(RIP_sim, 4) == 1, 1,
                                                                 ifelse(lag(RIP_sim, 5) == 1, 1,
                                                                  ifelse(lag(RIP_sim, 6) == 1, 1,
                                                                   ifelse(lag(RIP_sim, 7) == 1, 1,0))))) %>%
                                    dplyr::select(c(date, RIP_sim, season, model, sim, RIP_lag1, RIP_lag_4_7))

RIP_sim_lag_mod = RIP_sim_lag_mod %>% dplyr::filter(!(lubridate::month(date) == 3 & lubridate::day(date) %in% 1:7))

RIP_lag_obs_long = melt(RIP_lag_obs, id.vars = c("date", "season", "RIP"))
RIP_sim_lag_mod_long = melt(RIP_sim_lag_mod, id.vars = c("date", "season", "model", "sim", "RIP_sim"))

# intialize a blank data.frame to store the conditional probs
RIP_cond_prob_obs = data.frame(lag = unique(RIP_sim_lag_mod_long$variable),
                               obs = NA)
RIP_cond_prob_mod_df = data.frame(lag = unique(RIP_sim_lag_mod_long$variable),
                                  mod1 = NA,
                                  mod2 = NA,
                                  mod3 = NA,
                                  mod4 = NA,
                                  mod5 = NA)
RIP_cond_prob_mod_list = list()
for(nn in 1:n.sims) RIP_cond_prob_mod_list[[nn]] = RIP_cond_prob_mod_df

RIP_lag_names = unique(RIP_sim_lag_mod_long$variable)

for(llag in 1:2){
  con_ting_table = table(RIP_lag_obs_long$RIP[RIP_lag_obs_long$variable == RIP_lag_names[llag]], 
                         RIP_lag_obs_long$value[RIP_lag_obs_long$variable == RIP_lag_names[llag]], 
                         deparse.level = 2)
  RIP_cond_prob_obs$obs[llag] = con_ting_table[4]/(con_ting_table[3] + con_ting_table[4])
  
  for(ss in 1:n.sims){
    ssim = unique(RIP_sim_lag_mod_long$sim)[ss]
    
    for(mmod in 1:5){
      con_ting_table = table(RIP_sim_lag_mod_long$RIP_sim[RIP_sim_lag_mod_long$model == mmod & 
                                                            RIP_sim_lag_mod_long$sim == ssim &
                                                            RIP_sim_lag_mod_long$variable == RIP_lag_names[llag]], 
                             RIP_sim_lag_mod_long$value[RIP_sim_lag_mod_long$model == mmod &
                                                          RIP_sim_lag_mod_long$sim == ssim &
                                                          RIP_sim_lag_mod_long$variable == RIP_lag_names[llag]], 
                             deparse.level = 2)
      
      RIP_cond_prob_mod_list[[ss]][llag,mmod+1] = con_ting_table[4]/(con_ting_table[3] + con_ting_table[4])
    }
    print(paste0("sim ", ss, " complete, (lag ",llag,")"))	
  }
  
}

RIP_cond_prob_mod = do.call("rbind", RIP_cond_prob_mod_list) %>% dplyr::mutate(sim = rep(unique(RIP_sim_lag_mod_long$sim), each = 2))
RIP_cond_prob_long = melt(RIP_cond_prob_mod, id.vars = c("lag","sim"), variable.name = "model") 

# add the observations to this
RIP_cond_prob_obs_long = data.frame(model = "obs",
                                    sim = "obs",
                                    lag = RIP_cond_prob_obs$lag,
                                    value = RIP_cond_prob_obs$obs)

RIP_cond_prob_mod_obs_long = rbind(RIP_cond_prob_long,RIP_cond_prob_obs_long)

CPC_marg_probs = RIP_lag_obs_long %>% dplyr::group_by(season) %>%
                                      dplyr::summarise(value = sum(RIP[!is.na(RIP)], na.rm = TRUE)/length(RIP[!is.na(RIP)])) %>%
                                      dplyr::mutate(model = "obs",
                                                    lag = "marg_prob",
                                                    sim = "obs") %>%
                                      dplyr::filter(season %in% c("MAM")) %>%
                                      data.frame()

mod_marg_probs = RIP_sim_lag_mod %>% dplyr::group_by(season, model, sim) %>%
                                     dplyr::summarise(value = sum(RIP_sim[!is.na(RIP_sim)], na.rm = TRUE)/length(RIP_sim[!is.na(RIP_sim)])) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate(model = paste0("mod",model),
                                                   lag = "marg_prob") %>%
                                     dplyr::filter(season %in% c("MAM")) %>%
                                     data.frame()

marg_probs = rbind(mod_marg_probs, CPC_marg_probs)

# rename the last colum 
marg_probs_new = reshape2::dcast(model + sim ~ lag, data = marg_probs, value.var	= "value")

RIP_mod_CPC_cond_marg_prob_long = merge(RIP_cond_prob_mod_obs_long, marg_probs_new, by = c("sim", "model")) %>%
                                        dplyr::mutate(cond_div_marg = value/ marg_prob)

# rename the lag
RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>% dplyr::mutate(lag_rename = ifelse(lag == "RIP_lag1", "1", "4-7"))

RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>%  dplyr::mutate(model = ifelse(model == "mod1", "GCM 1",
                                                                                                    ifelse(model == "mod2", "GCM 2",
                                                                                                           ifelse(model == "mod3", "GCM 3",
                                                                                                                  ifelse(model == "mod4", "GCM 4",
                                                                                                                         ifelse(model == "mod5", "GCM 5", "OBS"))))))

RIP_mod_CPC_cond_marg_prob_long = RIP_mod_CPC_cond_marg_prob_long %>%  dplyr::mutate(variable2 = ifelse(model == "OBS", "OBS", "GCM"))

pdf("Final figures/Figure_11.pdf", width = 5, height = 3)
ggplot() +
  geom_boxplot(data = RIP_mod_CPC_cond_marg_prob_long,
               aes(y = cond_div_marg, x = lag_rename, fill = variable2), width = 0.5, coef = 1.5) +
  labs(x = "lag (days)", y = expression(P(RIP[t] ~"|" ~ RIP[t-lag])/P(RIP))) +
  scale_fill_discrete(name = "GCM/OBS") +
  scale_color_discrete(name = "GCM/OBS") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,25)) +
  theme_bw()
dev.off()


########################################
############### Figure S9 ##############
########################################
rm(list = ls())
source("R/GetSeasonDate.R")

# load RIP/EWD/MHC records
load(file = 'Processed_Data/RIP_EWD_MHC_all.RData')

# add EWD "+" indicator and just retain the MAM season
RIP_EWD_MHC_all_MAM = RIP_EWD_MHC_all %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0),
                                                        RIP_lag1 = lag(RIP,1)) %>%
                                          dplyr::filter(season == "MAM")

# fit the model on the observations
RIP_EWD_mod = glm(RIP ~ factor(RIP_lag1) : EWD, 
                  data = RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model == "OBS",], family = binomial(link = "logit"))

n.sims = 25

# intialize the first RIP_lag1 as 0 for each model for each year
RIP_EWD_MHC_mod_MAM = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model != "OBS") %>%
                                              dplyr::select(-c(RIP, RIP_lag1)) %>%
                                              dplyr::mutate(RIP_lag1 = ifelse(lubridate::month(date) == 3 & lubridate::day(date) == 1,
                                                                              0, NA))

RIP_EWD_MHC_obs_MAM = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model == "OBS")


RIP_sim_df = data.frame(date = unique(RIP_EWD_MHC_mod_MAM$date),
                        matrix(nrow = nrow(RIP_EWD_MHC_mod_MAM)/5, ncol = n.sims))

RIP_sim	= list(RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df)
names(RIP_sim) = unique(RIP_EWD_MHC_mod_MAM$model)
time.nowwwww = proc.time()
set.seed(0)
for(nn in 1:n.sims){
  for(mmod in unique(RIP_EWD_MHC_mod_MAM$model)){
    tmp_data = RIP_EWD_MHC_mod_MAM %>% dplyr::filter(model == mmod)
    for(rrow in 1:nrow(tmp_data)){
      RIP_sim[[mmod]][rrow,nn+1] = rbinom(1, 1, prob = predict.glm(RIP_EWD_mod, newdata = tmp_data[rrow,], type = "response"))
      tmp_data[rrow + 1,]$RIP_lag1 = RIP_sim[[mmod]][rrow,nn+1]
    }
    print(paste0("model #", mmod, " complete (sim #", nn, ")"))
  }
}
proc.time() - time.nowwwww

RIP_sim_df = do.call("rbind", RIP_sim) %>% dplyr::mutate(model = rep(1:5, each = nrow(RIP_EWD_MHC_mod_MAM)/5))

RIP_sim_long = melt(RIP_sim_df, id.vars = c("date","model"), variable.name = "sim", value.name = "RIP_sim")

RIP_sim_by_year = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                   dplyr::group_by(year, sim, model) %>%
                                   dplyr::summarise(RIP_sim = sum(RIP_sim))

RIP_EWD_MHC_obs_year = RIP_EWD_MHC_obs_MAM %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                               dplyr::group_by(year) %>%
                                               dplyr::summarise(RIP = sum(RIP))

# calculate the mean for each member and the full ensemble mean
RIP_pred_by_year_sim_mean = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                             dplyr::group_by(year, sim, model) %>%
                                             dplyr::summarise(RIP_sim = sum(RIP_sim)) %>%
                                             dplyr::group_by(year, model) %>%
                                             dplyr::summarise(RIP_sim = mean(RIP_sim))

RIP_pred_by_year_ens_mean = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                             dplyr::group_by(year, sim, model) %>%
                                             dplyr::summarise(RIP_sim = sum(RIP_sim)) %>%
                                             dplyr::group_by(year) %>%
                                             dplyr::summarise(RIP_sim = mean(RIP_sim))


source('R/ma.R')
smoothing_window = 10

RIP_sim_by_year = RIP_sim_by_year %>% dplyr::group_by(model,sim) %>%
                                      dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

RIP_pred_by_year_sim_mean = RIP_pred_by_year_sim_mean %>% dplyr::group_by(model) %>%
                                                          dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

RIP_EWD_MHC_obs_year = RIP_EWD_MHC_obs_year %>% dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window))

RIP_pred_by_year_ens_mean = RIP_pred_by_year_ens_mean %>% dplyr::mutate(RIP_sim_smooth = ma(RIP_sim, smoothing_window))

line.width = 1
alpha.line = 1
col.line = "grey80"

RIP_mod_obs_compare_1 =
  ggplot() + 
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 1,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 2,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 3,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 4,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_sim_by_year[RIP_sim_by_year$model == 5,],
            aes(year, y = RIP_sim_smooth, group = sim), 
            linetype = "dashed", size = line.width/4, col = col.line) +
  geom_line(data = RIP_pred_by_year_sim_mean,
            aes(year, y = RIP_sim_smooth, group = model),
            linetype = "dashed", size = line.width/2) +
  geom_line(data = RIP_pred_by_year_ens_mean,
            aes(year, y = RIP_sim_smooth),
            linetype = "dashed", size = line.width, col = "red") +
  geom_line(data = RIP_EWD_MHC_obs_year,
            aes(year, y = RIP_smooth),
            linetype = "solid", size = line.width) +
  ylab("# RIP") +
  coord_cartesian(xlim = c(1955,2006),
                  ylim = c(0,5.5)) +
  xlab("Year") +
  annotate("text", label = c("a)"), x = 1956, y = 5.3, size = 6, colour = "black") +
  theme_bw()


bin_breaks = seq(-0.5,12.5,1)
bin_labels = as.character(bin_breaks-0.5)
hist_lims = data.frame(xlim = c(0.5,11.6),
                       ylim = c(0,40))
RIP_mod_obs_compare_2 =
  ggplot() + 
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 1,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 2,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 3,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 4,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year[RIP_sim_by_year$model == 5,],
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed", size = line.width/2,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year,
           aes(x = RIP_sim, y =..count../(25*5)),
           geom = "step", linetype = "dashed", size = line.width,
           breaks = bin_breaks, col = "red") +
  stat_bin(data = RIP_EWD_MHC_obs_year,
           aes(x = RIP, y=..count..),
           geom = "step", size = line.width,
           breaks = bin_breaks) +
  annotate("text", label = "b)", x = 0.75, y = 38.75, size = 6, colour = "black") +
  labs(x = "# RIP",
       y = "count") +
  coord_cartesian(xlim = hist_lims$xlim,
                  ylim = hist_lims$ylim) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()

pdf('Final figures/Figure_S9.pdf', width = 11, height = 2.75)
grid.arrange(RIP_mod_obs_compare_1,
             RIP_mod_obs_compare_2,
             nrow = 1)
dev.off()


########################################
############### Figure 12 ##############  !! up to here !!
########################################
rm(list = ls())
source("R/GetSeasonDate.R")

# Simulate the future projections based on the EWD and MHC anom indices from the future record (RCP 8.5)
# load RIP/EWD/MHC records
load(file = 'Processed_Data/RIP_EWD_MHC_all.RData')


# add EWD "+" indicator and retain the MAM season
RIP_EWD_MHC_all_MAM = RIP_EWD_MHC_all %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0),
                                                        RIP_lag1 = lag(RIP,1)) %>%
                                          dplyr::filter(season == "MAM")

# fit the model on the observations
RIP_EWD_MHC_mod = glm(RIP ~ MHC_anom : EWD.positive + factor(RIP_lag1) : EWD, 
                      data = RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model == "OBS",], family = binomial(link = "logit"))

# load and process the future records
load(file = "Processed_Data/EWD_mod_future.RData")
load(file = "Processed_Data/MHC_mod_future.RData")
load(file = 'Processed_Data/mod_RIP_future.RData')

EWD_MHC_mod_future = merge(EWD_mod_future,MHC_mod_future,by = c("date","season","model")) %>%
                           dplyr::select(c(date, season, model, EWD, MHC_anom)) %>%
                           dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0)) %>%
                           dplyr::filter(season == "MAM")

EWD_MHC_mod_future_sim = EWD_MHC_mod_future %>% dplyr::filter(model != "OBS")

# intialize the first RIP_lag1 as 0 for each model for each year
EWD_MHC_mod_future_sim = EWD_MHC_mod_future_sim %>% dplyr::mutate(RIP_lag1 = ifelse(lubridate::month(date) == 3 & lubridate::day(date) == 1,
                                                                                    0, NA))
n.sims = 25
RIP_sim_df = data.frame(date = unique(EWD_MHC_mod_future_sim$date),
                        matrix(nrow = nrow(EWD_MHC_mod_future_sim), ncol = n.sims))

RIP_sim	= list(RIP_sim_df)
names(RIP_sim) = unique(EWD_MHC_mod_future_sim$model)

set.seed(0)
for(nn in 1:n.sims){
  for(mmod in unique(EWD_MHC_mod_future_sim$model)){
    tmp_data = EWD_MHC_mod_future_sim %>% dplyr::filter(model == mmod)
    for(rrow in 1:nrow(tmp_data)){
      RIP_sim[[mmod]][rrow,nn+1] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
      tmp_data[rrow + 1,]$RIP_lag1 = RIP_sim[[mmod]][rrow,nn+1]
    }
    print(paste0("model #", mmod, " complete (sim #", nn, ")"))
  }
}

RIP_sim_df = do.call("rbind", RIP_sim)

RIP_sim_long = melt(RIP_sim_df, id.vars = c("date"), variable.name = "sim", value.name = "RIP_sim")

RIP_sim_by_year = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                   dplyr::group_by(year, sim) %>%
                                   dplyr::summarise(RIP_sim = sum(RIP_sim))

RIP_EWD_MHC_mod_raw_year = mod_RIP_future %>% dplyr::filter(season == "MAM") %>%
                                              dplyr::mutate(year = lubridate::year(date)) %>%
                                              dplyr::group_by(year) %>%
                                              dplyr::summarise(RIP = sum(RIP))

# calculate the mean for each member and the full ensemble mean
RIP_pred_by_year_sim_mean = RIP_sim_long %>% dplyr::mutate(year = lubridate::year(date)) %>%
                                             dplyr::group_by(year, sim) %>%
                                             dplyr::summarise(RIP_sim = sum(RIP_sim)) %>%
                                             dplyr::group_by(year) %>%
                                             dplyr::summarise(RIP_sim = mean(RIP_sim))


# mark whether the record is in the first 45 years (2006-2050)
# or the last 45 years (2056-2100)

RIP_sim_by_year = RIP_sim_by_year %>% dplyr::mutate(period = ifelse(year < 2051, "2006-2050",
                                                              ifelse(year > 2055,"2056-2100", "2051-2055")))

RIP_EWD_MHC_mod_raw_year = RIP_EWD_MHC_mod_raw_year %>% dplyr::mutate(period = ifelse(year < 2051, "2006-2050",
                                                                                ifelse(year > 2055,"2056-2100", "2051-2055")))

RIP_pred_by_year_sim_mean = RIP_pred_by_year_sim_mean %>% dplyr::mutate(period = ifelse(year < 2051, "2006-2050",
                                                                                  ifelse(year > 2055,"2056-2100", "2051-2055")))
# function to compute moving average
source('R/ma.R')
smoothing_window = 10

RIP_sim_by_year = RIP_sim_by_year %>% dplyr::group_by(sim) %>% 
                                      dplyr::mutate(RIP_smooth = ma(RIP_sim, smoothing_window))

RIP_EWD_MHC_mod_raw_year = RIP_EWD_MHC_mod_raw_year %>% dplyr::mutate(RIP_smooth = ma(RIP, smoothing_window))

RIP_pred_by_year_sim_mean = RIP_pred_by_year_sim_mean %>% dplyr::mutate(RIP_smooth = ma(RIP_sim, smoothing_window))

# look at the number of RIPs per year from the first and last year
# 2006-2025
mean(RIP_EWD_MHC_mod_raw_year[RIP_EWD_MHC_mod_raw_year$year < 2026,]$RIP)
# [1] 1.95

# 2081-2100
mean(RIP_EWD_MHC_mod_raw_year[RIP_EWD_MHC_mod_raw_year$year > 2080,]$RIP)
# [1] 3.9

# 2006-2025
mean(RIP_sim_by_year[RIP_sim_by_year$year < 2026,]$RIP_sim)
# [1] 1.248

# 2007-2026
mean(RIP_sim_by_year[RIP_sim_by_year$year > 2080,]$RIP_sim)
# [1] 2.496

line.width = 1
alpha.line = 0.1
col.line = "grey"

RIP_mod_future_sim_1 =
  ggplot() +
  geom_line(data = RIP_sim_by_year,
            aes(year, y = RIP_smooth, group = sim), 
            size = line.width/2, col = col.line, linetype = "dashed") +
  geom_line(data = RIP_pred_by_year_sim_mean,
            aes(year, y = RIP_smooth), 
            size = line.width, linetype = "dashed") +
  geom_line(data = RIP_EWD_MHC_mod_raw_year,
            aes(year, y = RIP_smooth), 
            size = line.width) +
  ylab("# RIP") +
  annotate("text", label = c("a)"), x = 2006, y = 5.3, size = 6, colour = "black") +
  coord_cartesian(ylim = c(0,5.6)) +
  xlab("Year") +
  theme_bw()


# exclude the middle few years between the first and last 45 years of the simulation
RIP_EWD_MHC_mod_raw_year_subset = RIP_EWD_MHC_mod_raw_year %>% dplyr::filter(period != "2051-2055")
RIP_sim_by_year_subset = RIP_sim_by_year %>% dplyr::filter(period != "2051-2055")

bin_breaks = seq(-0.5,13.5,1)
bin_labels = as.character(bin_breaks-0.5)
RIP_mod_future_sim_2 =
  ggplot() + 
  stat_bin(data = RIP_EWD_MHC_mod_raw_year_subset,
           aes(x = RIP, y =..count..),
           geom = "step",size = line.width,
           breaks = bin_breaks) +
  stat_bin(data = RIP_sim_by_year_subset,
           aes(x = RIP_sim, y =..count../25),
           geom = "step", linetype = "dashed",size = line.width,
           breaks = bin_breaks) +
  annotate("text", label = c("b)","c)"), period = c("2006-2050","2056-2100"), x = 0.5, y = 18, size = 6, colour = "black") +
  facet_wrap(~period,
             ncol = 1) +
  xlab("# RIP") +
  ylab("count") +
  coord_cartesian(xlim = c(0.5,12.6), ylim = c(0,20)) +
  scale_x_continuous(breaks = bin_breaks,
                     labels = bin_labels) +
  theme_bw()


lay = rbind(c(1,1,1,2,2))

pdf('Final figures/Figure_12.pdf', width = 11, height = 3.5)
grid.arrange(RIP_mod_future_sim_1,
             RIP_mod_future_sim_2,
             nrow = 1,
             layout_matrix = lay)
dev.off()

########################################
############### Figure 13 ##############
########################################
rm(list = ls())
load(file = 'Processed_Data/RIP_EWD_MHC_all.RData')

# add EWD "+" indicator and retain the MAM season
RIP_EWD_MHC_all_MAM = RIP_EWD_MHC_all %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0),
                                                        RIP_lag1 = lag(RIP,1)) %>%
                                          dplyr::filter(season == "MAM")

# fit the model on the observations
RIP_EWD_MHC_mod = glm(RIP ~ MHC_anom : EWD.positive + factor(RIP_lag1) : EWD,  
                      data = RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model == "OBS",], family = binomial(link = "logit"))

# load and process the historic future records
load(file = "Processed_Data/EWD_mod_future.RData")
load(file = "Processed_Data/MHC_mod_future.RData")
load(file = 'Processed_Data/mod_RIP_future.RData')

EWD_MHC_mod_future = merge(EWD_mod_future,MHC_mod_future,by = c("date","season","model")) %>%
                           dplyr::select(c(date, season, model, EWD, MHC_anom))

# add EWD "+" indicator and just retain the MAM season
EWD_MHC_mod_future_MAM = EWD_MHC_mod_future %>% dplyr::mutate(EWD.positive = ifelse(EWD > 0, 1, 0)) %>%
                                                dplyr::filter(season == "MAM")

# compute the old and future EWD and MHC averages -- try to partition the effects of the EWD and MHC
EWD_mean_1970_99 = mean(RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model != "OBS" &
                                            RIP_EWD_MHC_all_MAM$date > "1969-12-31" &
                                            RIP_EWD_MHC_all_MAM$date < "2000-01-01",]$EWD, na.rm = TRUE)

EWD_mean_2070_99 = mean(EWD_MHC_mod_future_MAM[EWD_MHC_mod_future_MAM$model == 1 &
                                               EWD_MHC_mod_future_MAM$date > "2069-12-31" &
                                               EWD_MHC_mod_future_MAM$date < "2100-01-01",]$EWD, na.rm = TRUE)

EWD_bias = EWD_mean_2070_99 - EWD_mean_1970_99

MHC_mean_1970_99 = mean(RIP_EWD_MHC_all_MAM[RIP_EWD_MHC_all_MAM$model != "OBS" &
                                            RIP_EWD_MHC_all_MAM$date > "1969-12-31" &
                                            RIP_EWD_MHC_all_MAM$date < "2000-01-01",]$MHC_anom, na.rm = TRUE)

MHC_mean_2070_99 = mean(EWD_MHC_mod_future_MAM[EWD_MHC_mod_future_MAM$model == 1 &
                                               EWD_MHC_mod_future_MAM$date > "2069-12-31" &
                                               EWD_MHC_mod_future_MAM$date < "2100-01-01",]$MHC_anom, na.rm = TRUE)

MHC_bias = MHC_mean_2070_99 - MHC_mean_1970_99

EWD_MHC_partition_hist = RIP_EWD_MHC_all_MAM %>% dplyr::filter(model != "OBS" &
                                                               date > "1969-12-31" &
                                                               date < "2000-01-01")

# intialize the first RIP_lag1 as 0 for each model for each year
EWD_MHC_partition_hist = EWD_MHC_partition_hist %>% dplyr::mutate(RIP_lag1 = ifelse(lubridate::month(date) == 3 & lubridate::day(date) == 1,
                                                                                    0, NA))
n.sims = 25
RIP_sim_df = data.frame(date = unique(EWD_MHC_partition_hist$date),
                        matrix(nrow = nrow(EWD_MHC_partition_hist)/5, ncol = n.sims))

RIP_sim	= list(RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df,RIP_sim_df)
names(RIP_sim) = unique(EWD_MHC_partition_hist$model)

time.nowwwww = proc.time()
set.seed(0)
for(nn in 1:n.sims){
  for(mmod in unique(EWD_MHC_partition_hist$model)){
    tmp_data = EWD_MHC_partition_hist %>% dplyr::filter(model == mmod)
    for(rrow in 1:nrow(tmp_data)){
      RIP_sim[[mmod]][rrow,nn+1] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
      tmp_data[rrow + 1,]$RIP_lag1 = RIP_sim[[mmod]][rrow,nn+1]
    }
    print(paste0("model #", mmod, " complete (sim #", nn, ")"))
  }
}
print(proc.time() - time.nowwwww)
RIP_sim_df = do.call("rbind", RIP_sim)

mean_1970_1999 = sum(RIP_sim_df[,-c(1)])/n.sims/5
mean_1970_1999
# [1] 25.656

EWD_MHC_partition_future = EWD_MHC_mod_future_MAM %>% dplyr::filter(date > "2069-12-31") %>%
                                                      dplyr::mutate(RIP_lag1 = ifelse(lubridate::month(date) == 3 & lubridate::day(date) == 1, 0, NA))

EWD_MHC_partition_EWD_change = EWD_MHC_partition_future %>% dplyr::mutate(MHC_anom = MHC_anom - MHC_bias)
EWD_MHC_partition_MHC_change = EWD_MHC_partition_future %>% dplyr::mutate(EWD = EWD - EWD_bias, 
                                                                          EWD.positive = ifelse(EWD > 0, 1, 0))



n.sims = 100

both_pred = data.frame(matrix(nrow = nrow(EWD_MHC_partition_future), ncol = n.sims))
EWD_pred = data.frame(matrix(nrow = nrow(EWD_MHC_partition_future), ncol = n.sims))
MHC_pred = data.frame(matrix(nrow = nrow(EWD_MHC_partition_future), ncol = n.sims))


# first both pred
set.seed(0)
for(nn in 1:n.sims){
  tmp_data = EWD_MHC_partition_future
  for(rrow in 1:nrow(tmp_data)){
    both_pred[rrow,nn] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
    tmp_data[rrow + 1,]$RIP_lag1 = both_pred[rrow,nn]
  }
  print(paste0("sim #", nn, " complete"))
}

# now EWD change
set.seed(0)
for(nn in 1:n.sims){
  tmp_data = EWD_MHC_partition_EWD_change
  for(rrow in 1:nrow(tmp_data)){
    EWD_pred[rrow,nn] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
    tmp_data[rrow + 1,]$RIP_lag1 = EWD_pred[rrow,nn]
  }
  print(paste0("sim #", nn, " complete"))
}

# now MHC change
set.seed(0)
for(nn in 1:n.sims){
  tmp_data = EWD_MHC_partition_MHC_change
  for(rrow in 1:nrow(tmp_data)){
    MHC_pred[rrow,nn] = rbinom(1, 1, prob = predict.glm(RIP_EWD_MHC_mod, newdata = tmp_data[rrow,], type = "response"))
    tmp_data[rrow + 1,]$RIP_lag1 = MHC_pred[rrow,nn]
  }
  print(paste0("sim #", nn, " complete"))
}

plot_future = data.frame(Model = c(rep("EWD & MHC increase (2070-2099)",n.sims),
                                   rep("EWD increase (2070-2099)",n.sims),
                                   rep("MHC increase (2070-2099)",n.sims)),
                         value = c(colSums(both_pred), colSums(EWD_pred), colSums(MHC_pred)) - mean_1970_1999)
plot_future_medians = plot_future %>% dplyr::group_by(Model) %>%
                                      dplyr::summarise(mean_value = mean(value))

save(plot_future, file = "Processed_Data/plot_future.RData")

pdf("Final figures/Figure_13.pdf", height = 3, width = 8)
ggplot(plot_future) +
  geom_histogram(aes(x = value, fill = Model), 
                 alpha = 0.5, position="identity", breaks = c(seq(-5,75,by = 5))) +
  geom_vline(data = plot_future_medians, aes(xintercept = mean_value, col = Model)) +
  labs(x = expression(Delta ~ "# RIPs relative to (1970-1999)"),
       y = "# of simulations") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

# median percentage increases?
round(plot_future_medians$mean_value/mean_1970_1999 * 100 / 1)*1
# [1] 188  66 104



