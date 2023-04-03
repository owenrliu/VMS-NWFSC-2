bootstrapped_kerneloverlap= function(split,overlap.method="BA"){
  # matrix with coordinates
  vms_coords <- dplyr::select(analysis(split), LONGITUDE, LATITUDE)
  
  # data frame with IDs
  vms_ids <- analysis(split) %>%
    dplyr::select(subgroup) %>%
    mutate(subgroup=as.character(subgroup))
  # create the Spatial Points Data frame
  vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:4326"))
  
  yr.overlap <- kerneloverlap(vms_sp[,1], method=overlap.method, percent=0.9, grid=250, h=.06, conditional=F) 
  return(yr.overlap)
}

TESTbootstrapped_kerneloverlap = function(s){
  # matrix with coordinates
  vms_coords <- dplyr::select(s, LONGITUDE, LATITUDE)
  
  # data frame with IDs
  vms_ids <- s %>%
    dplyr::select(subgroup) %>%
    mutate(subgroup=as.character(subgroup))
  # create the Spatial Points Data frame
  vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:4326"))
  
  yr.overlap <- kerneloverlap(vms_sp[,1], method=overlap.method, percent=0.9, grid=250, h=0.06, conditional=F) 
  return(yr.overlap)
}
