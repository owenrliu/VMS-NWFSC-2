subsetVMS <- function(dat, crab_years, cut="annual", seasons=season_dates_df){
  # filter for D. crab trips
  vms.sub <- filter(dat, removal_type_code %in% c("C","D","U") & crab_year %in% crab_years)
  if(cut=="annual"){
    return(return(vms.sub))
  } else{
    
    seasons <- filter(seasons,crab_year %in% crab_years)
    
    if(cut=="central"){
      seasons <- filter(seasons,pcdistrict=="central")
    } else if(cut=="northern"){
      seasons <- filter(seasons,pcdistrict=="northern")
    } else{stop("unrecognized time frame to subset data. please choose: [annual / central / northern]")}
    
    
    vms.sub <- left_join(vms.sub, seasons,by=c("crab_year")) %>%
      mutate(first6w=ifelse(date <= cutoff6w, "Y","N")) %>%
      filter(first6w=="Y") %>%
      dplyr::select(-first6w,-cutoff6w,-pcdistrict)
    return(vms.sub)
  }
}
