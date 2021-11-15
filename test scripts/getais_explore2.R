function (df, every_minutes = 10, status_codes_to_keep = c(0, 
                                                           7, 8, 9, 10, 11, 12, 13, 14, 15), SOG_threshold = 1, vessel_attr = c("VesselType", 
                                                                                                                                "Length"), voyage_attr = c("Destination"), raw = FALSE, 
          delete_gdb = TRUE) 
{
  if (!dir.exists(paste0(getwd(), "/filtered"))) {
    dir.create(paste0(getwd(), "/filtered"))
  }
  for (i in 1:nrow(df)) {
    if (!file.exists(paste0("filtered/", df$zone[i], 
                            "_", df$year[i], "_", df$month[i], ".csv"))) {
      charMonth = ifelse(df$month[i] < 10, paste0(0, df$month[i]), 
                         paste0(df$month[i]))
      month = c("January", "February", "March", 
                "April", "May", "June", "July", 
                "August", "September", "October", 
                "November", "December")
      if (df$year[i] == 2009) {
        url = paste0("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/AIS_FGDBs/Zone", 
                     df$zone[i], "/Zone", df$zone[i], "_", 
                     df$year[i], "_", charMonth, ".zip")
        download.file(url, destfile = "temp.zip", 
                      quiet = TRUE)
        unzip("temp.zip")
        char_month = ifelse(df$month[i] < 10, paste0("0", 
                                                     df$month[i]), paste0(df$month[i]))
        fname = paste0("Zone", df$zone[i], "_", 
                       df$year[i], "_", char_month, ".gdb")
        dat = sf::st_read(dsn = fname, layer = "Broadcast", 
                          quiet = TRUE)
        vessel = sf::st_read(dsn = fname, layer = "Vessel", 
                             quiet = TRUE)
        voyage = sf::st_read(dsn = fname, layer = "Voyage", 
                             quiet = TRUE)
      }
      if (df$year[i] == 2010) {
        char_month = ifelse(df$month[i] < 10, paste0("0", 
                                                     df$month[i]), paste0(df$month[i]))
        url = paste0("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/", 
                     df$year[i], "/", char_month, "_", 
                     month[df$month[i]], "_", df$year[i], 
                     "/Zone", df$zone[i], "_", df$year[i], 
                     "_", char_month, ".zip")
        download.file(url, destfile = "temp.zip", 
                      quiet = TRUE)
        unzip("temp.zip")
        fname = paste0("Zone", df$zone[i], "_", 
                       df$year[i], "_", char_month, ".gdb")
        dat = sf::st_read(dsn = fname, layer = "Broadcast", 
                          quiet = TRUE)
        vessel = sf::st_read(dsn = fname, layer = "Vessel", 
                             quiet = TRUE)
        voyage = sf::st_read(dsn = fname, layer = "Voyage", 
                             quiet = TRUE)
      }
      if (df$year[i] %in% c(2011, 2012, 2013)) {
        char_month = ifelse(df$month[i] < 10, paste0("0", 
                                                     df$month[i]), paste0(df$month[i]))
        url = paste0("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/", 
                     df$year[i], "/", char_month, "/Zone", 
                     df$zone[i], "_", df$year[i], "_", 
                     char_month, ".gdb.zip")
        download.file(url, destfile = "temp.zip", 
                      quiet = TRUE)
        unzip("temp.zip")
        fname = paste0("Zone", df$zone[i], "_", 
                       df$year[i], "_", char_month, ".gdb")
        if (df$year[i] %in% c(2011, 2012)) {
          dat = sf::st_read(dsn = fname, layer = "Broadcast", 
                            quiet = TRUE)
          vessel = sf::st_read(dsn = fname, layer = "Vessel", 
                               quiet = TRUE)
          voyage = sf::st_read(dsn = fname, layer = "Voyage", 
                               quiet = TRUE)
        }
        else {
          dat = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                        df$zone[i], "_", df$year[i], "_", 
                                                        char_month, "_Broadcast"), quiet = TRUE)
          vessel = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                           df$zone[i], "_", df$year[i], "_", 
                                                           char_month, "_Vessel"), quiet = TRUE)
          voyage = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                           df$zone[i], "_", df$year[i], "_", 
                                                           char_month, "_Voyage"), quiet = TRUE)
        }
      }
      if (df$year[i] == 2014) {
        char_month = ifelse(df$month[i] < 10, paste0("0", 
                                                     df$month[i]), paste0(df$month[i]))
        url = paste0("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/", 
                     df$year[i], "/", char_month, "/Zone", 
                     df$zone[i], "_", df$year[i], "_", 
                     char_month, ".zip")
        download.file(url, destfile = "temp.zip", 
                      quiet = TRUE)
        unzip("temp.zip")
        fname = paste0("Zone", df$zone[i], "_", 
                       df$year[i], "_", char_month, ".gdb")
        dat = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                      df$zone[i], "_", df$year[i], "_", 
                                                      char_month, "_Broadcast"), quiet = TRUE)
        vessel = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                         df$zone[i], "_", df$year[i], "_", 
                                                         char_month, "_Vessel"), quiet = TRUE)
        voyage = sf::st_read(dsn = fname, layer = paste0("Zone", 
                                                         df$zone[i], "_", df$year[i], "_", 
                                                         char_month, "_Voyage"), quiet = TRUE)
      }
      if (df$year[i] %in% c(2015, 2016, 2017)) {
        char_month = ifelse(df$month[i] < 10, paste0("0", 
                                                     df$month[i]), paste0(df$month[i]))
        char_zone = ifelse(df$zone[i] < 10, paste0("0", 
                                                   df$zone[i]), paste0(df$zone[i]))
        url = paste0("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/", 
                     df$year[i], "/AIS_", df$year[i], "_", 
                     char_month, "_Zone", char_zone, ".zip")
        download.file(url, destfile = "temp.zip", 
                      quiet = TRUE)
        unzip("temp.zip")
        fname = paste0("AIS_ASCII_by_UTM_Month/", 
                       df$year[i], "/AIS_", df$year[i], "_", 
                       char_month, "_Zone", char_zone, ".csv")
        dat = read.csv(file = fname, stringsAsFactors = FALSE)
      }
      unlink("temp.zip")
      if (delete_gdb) 
        unlink(fname)
      dat = dplyr::filter(as.data.frame(dat), SOG >= SOG_threshold)
      if (df$year[i] < 2015) {
        dat[, c(vessel_attr)] = vessel[match(dat$MMSI, 
                                             vessel$MMSI), c(vessel_attr)]
        unlink("Vessel.csv")
        dat[, c(voyage_attr)] = voyage[match(dat$MMSI, 
                                             voyage$MMSI), c(voyage_attr)]
        unlink("Voyage.csv")
      }
      dat$BaseDateTime = lubridate::as_datetime(as.POSIXlt(as.character(dat$BaseDateTime)))
      dat$keep = 0
      if (raw == FALSE) {
        dat$BaseDateTime_round = lubridate::round_date(dat$BaseDateTime, 
                                                       "minute")
        dat$minutes = lubridate::minute(dat$BaseDateTime_round) + 
          lubridate::second(dat$BaseDateTime)/60
        seq_min = seq(0, 60, by = every_minutes)
        seq_min[1] = -1
        dat$time_chunk = as.numeric(cut(dat$minutes, 
                                        seq_min))
        dat$diff_1 = abs(dat$minutes - seq_min[dat$time_chunk])
        dat$diff_2 = abs(dat$minutes - seq_min[dat$time_chunk + 
                                                 1])
        dat$time_1 = ifelse(dat$diff_1 < dat$diff_2, 
                            1, 0)
        dat$min_timediff = dat$diff_1 * dat$time_1 + 
          (1 - dat$time_1) * dat$diff_2
        dat$interval = dat$time_chunk * dat$time_1 + 
          (1 - dat$time_1) * (dat$time_chunk + 1)
        dat = select(dat, -BaseDateTime_round, -minutes, 
                     -time_chunk, -diff_1, -diff_2, -time_1)
        dat$chunk = as.numeric(as.factor(paste0(lubridate::month(dat$BaseDateTime), 
                                                ":", lubridate::day(dat$BaseDateTime), 
                                                ":", dat$interval)))
        dat$BaseDateTime = as.character(dat$BaseDateTime)
        dat = dplyr::group_by(dat, MMSI, chunk) %>% dplyr::mutate(mintime = ifelse(min_timediff == 
                                                                                     min(min_timediff), 1, 0)) %>% dplyr::filter(mintime == 
                                                                                                                                   1) %>% ungroup %>% dplyr::select(-mintime, 
                                                                                                                                                                    -chunk, -keep)
      }
      if (nrow(dat) > 0) {
        saveRDS(dat, paste0("filtered/Zone", df$zone[i], 
                            "_", df$month[i], "_", df$year[i], 
                            ".rds"))
      }
      unlink(paste0("Zone", df$zone[i], "_", 
                    df$year[i], "_", df$month[i], ".gdb"), 
             recursive = TRUE)
    }
  }
}