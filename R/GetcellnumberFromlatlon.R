GetcellnumberFromlatlon = function(lat, lon, mod_lat_lon_cell){
  tmp_lat_lon = data.frame(lat = lat,
                           lon = lon)
  colnames(tmp_lat_lon) = c("lat", "lon")
  tmp_dist = as.matrix(dist(rbind(tmp_lat_lon,mod_lat_lon_cell[,c("lat","lon")])))[-1,1]
  
  return(mod_lat_lon_cell$cell[which.min(tmp_dist)])
}