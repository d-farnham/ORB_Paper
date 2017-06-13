# read in shapefile and make a bounding box for the Ohio River Basin
basin_shapefile <- 'Raw_Data/BasinShapefile/FHP_Ohio_River_Basin_boundary'

basin_points <- basin_shapefile %>% 
	maptools::readShapePoly() %>% 
	ggplot2::fortify() %>% 
	data.table()

boundary <- make_bbox(basin_points$long, basin_points$lat, f = 0.0)
