
# vms: geolocations as a dataframe, with LONGITUDE,LATITUDE,crab_year,Rec_ID at least
# blocks_sf: blocks as an sf object 
# y : crab year
# plot: plot out the vms points overlaid on offshore management blocks


intersect_vms_block <- function(vms, blocks_sf, y=2014, plot=FALSE){
  # Subset vms data
  tmpvms <- filter(vms,crab_year==y)
  # make sf object and assign coord system
  tmpvms_sp <- st_as_sf(tmpvms,coords=c("LONGITUDE", "LATITUDE"),crs = st_crs(4326))
  
  # get block location of each vms point
  newvms_sp <- st_join(tmpvms_sp,blocks4326) 
  
  # save as data frame
  outvms <- newvms_sp %>%
    st_set_geometry(NULL) %>%
    dplyr::select(-Perimeter,-Area,-Acres,-Hectares)
  
  if(plot){
    blocks_coords <- as.data.frame(sf::st_coordinates(sf::st_point_on_surface(blocks4326))) %>%
      mutate(NAME=blocks4326$BLOCK10_ID)
    
    myplot <- ggplot() +
      geom_sf(data=blocks4326) +
      geom_sf(data=newvms_sp,aes(col=BLOCK10_ID)) +
      geom_text(data = blocks_coords, aes(X, Y, label = NAME), colour = "black") +
      labs(x="",y="")
    
    return(list(outvms,myplot))
  } else{
    
    return(outvms)
    
  }
}
