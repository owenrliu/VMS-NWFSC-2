
pull_crab_year <- function(matched_vms_data,choose_crabyr=2010){
  return(matched_vms_data %>%
           filter(tyear %in% seq(choose_crabyr-1,choose_crabyr)) %>%
           ## adjust date objects
           mutate(westcoastdate = parse_date_time(westcoastdate, orders = c("mdY_HM","Ymd_HMS"),tz= "America/Los_Angeles")) %>%
           mutate(UTCDATETIM = parse_date_time(UTCDATETIM, orders=c("mdY_HM","Ymd_HMS"))) %>%
           ## add in crab year
           mutate(tdate = ymd(date), tmonth = month(tdate), tyear=year(tdate),
                  crab_year = ifelse(tmonth > 9,tyear,tyear-1)) %>%
           ## filter for crab year
           filter(crab_year==choose_crabyr))
}