create_vms_spdf <- function(dat, drvid, yrs=c(2012,2013)){
  # filter VMS data
  myvms <- filter(dat, crab_year %in% yrs & drvid == drvid)
  
  # matrix with coordinates
  vms_coords <- dplyr::select(myvms, X_COORD, Y_COORD)
  # data frame with IDs
  vms_ids <- dplyr::select(myvms, crab_year)
  vms_ids$crab_year <- as.character(vms_ids$crab_year)
  # create the Spatial Points Data frame
  vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:32610"))
  return(vms_sp)
}
