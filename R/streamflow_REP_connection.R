########################################
############### Figure 2 ###############
########################################
source("R/GetSeasonDate.R")
load(file = 'Processed_Data/HCDN.RData')
load(file = 'Processed_Data/gage_info.RData')


# plot the streamflow gauges

# load 1) the US state boundaries, 
#      2) the ORB basin shapefile, 
#      3) the the domain that we consider for the REPs
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
  geom_path(data=state, aes(x,y), alpha = 0.25) +
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

# rearrange so that the sites are ordered alphabetically
flow_seasons$site_ordered = factor(flow_seasons$site, levels = unique(flow_seasons$site))
  
# panel 2 for Figure 2
prob_str_S_by_season_plot =
  ggplot(data = flow_seasons) +
  geom_line(aes(x = season, y = prob_str_xtr, group = site_ordered, col = site_ordered), size = 1.25) +
  geom_point(aes(x = season, y = prob_str_xtr, group = site_ordered, shape = site_ordered, col = site_ordered), size = 3) +
  scale_color_discrete(name = "site") +
  scale_shape_discrete(name = "site") +
  theme(legend.position = "none") +
  ylab(expression(Pr(S^{s} > S[364/365]^{s}))) +
  ylim(c(0,0.01)) +
  theme_bw() +
  theme(legend.position = "bottom")

# find the days when a REP occured:
load('Processed_Data/CPC_mod_cell_REP.RData')

# compute the 15 day running sum for the REPs
REP_CPC = CPC_mod_cell_REP %>% dplyr::select(date, REP) %>%
  dplyr::mutate(REP_sum_15 = stats::filter(REP, rep(1, 15), method = "convolution", sides = 1))

# now merge the streamflow record and the REP record
precip_streamflow = merge(streamflows,REP_CPC, by = "date", all.y = TRUE)

# next we compute the odds ratio for REP_sum_15 > 0 vs. REP_sum_15 = 0 & 
# str_xtr = 1 vs. str_xtr = 0 (i.e. whether the streamflow is extreme)
odds_ratio = data.frame(expand.grid(site = unique(precip_streamflow$site),
                                    season = c("MAM"),
                                    REP_sum_15 = c(0)))

odds_ratio$odds_est = odds_ratio$odds_lower = odds_ratio$odds_upper = NA
odds_ratio_tmp = precip_streamflow %>% dplyr::mutate(REP_over = ifelse(REP_sum_15 > 0, 1, 0))

# exclude the NAs
odds_ratio_tmp = odds_ratio_tmp %>% dplyr::filter(!is.na(REP_over) &
                                                    !is.na(str_xtr))
for(rrow in 1:nrow(odds_ratio)){
  table_tmp = table(odds_ratio_tmp[odds_ratio_tmp$season == odds_ratio$season[rrow] & 
                                     odds_ratio_tmp$site == odds_ratio$site[rrow],]$REP_over, 
                    odds_ratio_tmp[odds_ratio_tmp$season == odds_ratio$season[rrow] &
                                     odds_ratio_tmp$site == odds_ratio$site[rrow],]$str_xtr)
  tryCatch({ # if there is an error, we want the loop to continue running	
    odds_ratio$odds_est[rrow] = oddsratio.wald(table_tmp)$measure[2,1]
    odds_ratio$odds_lower[rrow] = oddsratio.wald(table_tmp)$measure[2,2]
    odds_ratio$odds_upper[rrow] = oddsratio.wald(table_tmp)$measure[2,3]
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(rrow)
}

# make the # REP events label for plot
odds_ratio = odds_ratio %>% dplyr::mutate(REP_sum_15_lab = "1+")

# order the sites according to drainage basin size
odds_ratio = odds_ratio %>% dplyr::mutate(site = factor(site, levels = gage_info$Short.name[order(gage_info$D.A, decreasing = TRUE)]))

# rearrange so that the sites are ordered alphabetically
odds_ratio$site_ordered = factor(odds_ratio$site, levels = unique(flow_seasons$site))

# take the natural log of the odds ratio
odds_ratio = odds_ratio %>% dplyr::mutate(ln_odds_est = log(odds_est),
                                          ln_odds_lower = log(odds_lower),
                                          ln_odds_upper = log(odds_upper))



ann_text_pet <- data.frame(x = c(5, 3), y = c(1),lab = "Inf",
                           site_ordered = factor(c("Petersburg","MtCarmel")),
                           season = "MAM")

# panel 3 for Figure 2
odds_ratio_str_S_by_season_plot =
  ggplot(data = odds_ratio[odds_ratio$season == "MAM",]) +
  geom_pointrange(aes(x = site_ordered, y = ln_odds_est, ymin = ln_odds_lower, ymax = ln_odds_upper, col = site_ordered, shape = site_ordered), 
                  fatten = 1.75, size = 1.5) +
  # geom_text(aes(x = 1:6, y = -1.5, label = site_ordered),
  #                 position = position_dodge(width = 0.75), size = 3) +
  facet_grid( ~ season) +
  scale_y_continuous(name = expression((LOR^s ~ '|' ~ REP)), limits = c(-1,6.25)) +
  xlab("1+") +
#  scale_x_continuous(breaks = 1:6, labels = unique(flow_seasons$site)) +
  geom_text(data = ann_text_pet, aes(x,y,label = lab), col = c("#619CFF","#00BA38")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none")


############################

pdf(paste0("Final figures/Figure_2.pdf"), height = 5, width = 11)
grid.arrange(stream_gauge_locs_plot,
             prob_str_S_by_season_plot,
             odds_ratio_str_S_by_season_plot,
             nrow = 2,
             layout_matrix = rbind(c(1,2),c(1,2),c(1,2),c(1,3),c(1,3)))
dev.off()

rm(list = ls())