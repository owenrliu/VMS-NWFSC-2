create_vms_spdf <- function(dat, drvid, yrs=c(2012,2013)){
  # filter VMS data
  vms.sub <- filter(dat, crab_year %in% yrs & drvid == drvid)
  
  # matrix with coordinates
  vms_coords <- dplyr::select(vms.sub, X_COORD, Y_COORD)
  # data frame with IDs
  vms_ids <- dplyr::select(vms.sub, crab_year)
  vms_ids$crab_year <- as.character(vms_ids$crab_year)
  # create the Spatial Points Data frame
  vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:32610"))
  return(vms_sp)
}


create_groups_vms_spdf <- function(dat, crab_years, group_id=NA, list_by="year", clusters){
  if(list_by=="group"){
    if(length(crab_years) > 1){message("WARNING: collapsing multiple crab years to list by group. Did you mean list_by=year?")}
    # filter VMS data
    vms.sub <- filter(dat, crab_year %in% crab_years)
    vms.sub <- clusters %>%
      filter(crab_year %in% crab_years) %>%
      dplyr::select(drvid,subgroup, crab_year,group_area) %>%
      left_join(vms.sub,by=c("drvid","crab_year"),multiple="all") %>%
      filter(!is.na(LONGITUDE))
    
    
    ## check for confidential VMS
    sample_size <- vms.sub %>%
      group_by(subgroup) %>%
      summarise(n.vessels = length(unique(drvid)), n.trips = length(unique(Rec_ID))) %>%
      arrange(n.vessels,n.trips)
    confidential <- filter(sample_size,n.vessels < 3)
    
    vms.noncon <- vms.sub %>%
      filter(!(subgroup %in% confidential$subgroup))
      
    # matrix with coordinates
    vms_coords <- dplyr::select(vms.noncon, LONGITUDE, LATITUDE)
    
    # data frame with IDs
    vms_ids <- vms.noncon %>%
      dplyr::select(subgroup) %>%
      mutate(subgroup=as.character(subgroup))
    # create the Spatial Points Data frame
    vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:4326"))


    
  } else if(list_by=="year"){
    vms.sub <- filter(dat, crab_year %in% crab_years)
    vms.sub <- clusters %>%
      filter(crab_year %in% crab_years & subgroup==group_id) %>%
      dplyr::select(drvid,subgroup, crab_year,group_area) %>%
      left_join(vms.sub,by=c("drvid","crab_year"),multiple="all") %>%
      filter(!is.na(LONGITUDE))
    
    ## check for confidential VMS
    sample_size <- vms.sub %>%
      group_by(subgroup, crab_year) %>%
      summarise(n.vessels = length(unique(drvid)), n.trips = length(unique(Rec_ID))) %>%
      arrange(n.vessels,n.trips)
    confidential <- filter(sample_size,n.vessels < 3)
    
    vms.noncon <- vms.sub %>%
      filter(!(crab_year %in% confidential$crab_year))
      
    # matrix with coordinates
    vms_coords <- dplyr::select(vms.noncon, LONGITUDE, LATITUDE)
    # data frame with IDs
    vms_ids <- vms.noncon %>% dplyr::select(crab_year) %>%
      mutate(crab_year=as.character(crab_year))
    
    
    # create the Spatial Points Data frame
    vms_sp <- SpatialPointsDataFrame(coords=vms_coords, data=vms_ids, proj4string = CRS("+init=epsg:4326"))
    


  } else{error("\nunrecognized grouping variable. please choose [year / group]\n")}
  
  
  return(list(vms_sp,sample_size))
}
