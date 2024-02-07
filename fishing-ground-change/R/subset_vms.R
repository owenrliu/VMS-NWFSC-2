subsetVMS <- function(dat, crab_years, cut="annual", seasons=season_dates_df){
  # filter for D. crab trips
  vms.sub <- filter(dat, removal_type_code %in% c("C","D","U") & crab_year %in% crab_years)
  if(cut=="annual"){
    return(return(vms.sub))
  } else{
    if(cut != unique(seasons$pcdistrict)){warning("WARNING: cut time scale does not match area in seasons data frame.")}
    vms.sub <- left_join(vms.sub, seasons,by=c("crab_year")) %>%
      mutate(first6w=ifelse(date <= cutoff6w, "Y","N")) %>%
      filter(first6w=="Y") %>%
      dplyr::select(-cutoff6w,-pcdistrict)
    return(vms.sub)
  }
}
