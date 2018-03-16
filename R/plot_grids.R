########################################
############### Figure 1 ###############
########################################

# compare the grids from the CPC and the GCM
load(file = 'Processed_Data/CPC_grid.RData')
load(file = 'Processed_Data/mod_ORB_grid.RData')


world <- data.frame(map("world", plot=FALSE)[c("x","y")])
state <- data.frame(map("state", plot=FALSE)[c("x","y")])

source("R/load_basin_boundary.R")
pdf('Final figures/Figure_1.pdf', width = 6, height = 4.25)
plot(
  ggplot() + 
  geom_tile(data = CPC_grid,
            aes(x = (lon-360),y = lat), fill = "transparent", col = "red", size = 0.2, alpha = 0.5) +
  geom_tile(data = mod_ORB_grid,
            aes(x = (lon-360),y = lat), fill = "transparent", col = "blue", size = 0.6) + 
  geom_path(data=state, aes(x,y), alpha = 0.75) +
  scale_y_continuous(limits = c(33,45)) +
  scale_x_continuous(limits = c(-93,-75)) +
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0.25, data = basin_points) +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw()
)
dev.off()

rm(list = ls())
