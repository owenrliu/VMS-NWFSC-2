# make time series of entanglements

library(tidyverse)
library(lubridate)
library(viridis)

# from Lauren Saez
# all data based on "entanglement reports from lauren 100920.xlsx"
# focus on all confirmed commercial Dungeness crab entanglements by year
path_entanglement_file_annual <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Entanglement Data/2000_19/all_confirmed_dcrb_entanglements.csv" 
path_entanglement_file_all <- here::here('vertical line model','entanglements','all_entanglements_102021.csv')

# all data based on "ForStates_WCR_Whale_entanglement_1982_2020_10.20.21.xlsx"
# focus on all confirmed commercial Dungeness crab entanglements by year
path_entanglement_file_all <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Entanglement Data/all_entanglements_102021.csv" 

###Initial processing of annual data

# most code from Make time series of entanglements.Rmd
entanglement_df <- readr::read_csv(path_entanglement_file_annual)
glimpse(entanglement_df)

entanglement_df_annual <- entanglement_df %>%
  pivot_longer(
    !Year, names_to = "common_nam", values_to = "count"
  ) %>%
  #group_by(Year) %>% #, Common_Nam
  #tally() %>%
  mutate(
    Year.as.date = as.Date(paste(Year, 1, 1, sep = "-")) 
  ) %>%
  #complete(Year.as.date = seq.Date(min(Year.as.date), max(Year.as.date), by="year"),
  #         fill = list(n=0)) %>%
  #complete(Year, nesting(Common_Nam)) %>%
  mutate(
    species = case_when(common_nam == "Gray" | common_nam == "Killer" | common_nam == "Unidentified"  ~ "Other / Unidentified",
                        common_nam == "Humpback" ~ "Humpback Whale",
                        common_nam == "Blue" ~ "Blue Whale",
                        TRUE ~ "ERROR")
  )
glimpse(entanglement_df_annual)

# complete the df with zeroes for each species category

#complete_species_year <- expand.grid(species = unique(entanglement_df_annual$species), Year.as.date = seq.Date(as.Date(paste(1982, 1, 1, sep = "-")), max(entanglement_df_annual$Year.as.date), by="year"))

entanglement_df_annual_complete <- entanglement_df_annual %>%
  group_by(Year, Year.as.date, species) %>%
  summarise(
    count = sum(count), 
    .groups = 'drop'
  )

glimpse(entanglement_df_annual_complete)

# write_csv(entanglement_df_annual_complete, 
#           paste0(here::here(), "/tradeoffs/full_analysis_samhourietal/output_dfs/entanglement_df_annual_complete.csv")
# )


### make some figures with annual data

# annual entanglements time series by species group
entanglements_ts <- ggplot(entanglement_df_annual_complete %>% filter (Year > 2009), 
                           aes(x=Year, y=count, group=species, colour=species)) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, position = position_dodge(0.4)) +
  geom_line(position = position_dodge(0.4)) + 
  #geom_vline(xintercept = c(2014.5, 2018.5), linetype=2) +
  ylab("Number of\nentanglements") +
  xlab("") + # "Year
  #labs(subtitle = "Data from NMFS WRO") +
  #ggtitle("Number of Whale\nEntanglements,US West Coast") +
  scale_colour_viridis_d(begin=0.1, end=0.9) + #, option = "plasma") +
  scale_x_continuous(breaks=seq(2010, 2019 , 1),limits=c(2009.5,2019.5))+
  scale_y_continuous(breaks=seq(0, 20, 5),limits=c(0,20))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1,size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = c(0.3, 0.8)
  )
entanglements_ts 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_annual_byspecies.png"), 
  width = 4, height = 4, units = "in", res = 300)
entanglements_ts
invisible(dev.off())

# annual entanglements time series humpbacks only
entanglements_ts_hump <- ggplot(entanglement_df_annual_complete %>% filter (Year > 2009) %>% filter (species == "Humpback Whale"), 
                           aes(x=Year, y=count)) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) +
  geom_line() + 
  #geom_vline(xintercept = c(2014.5, 2018.5), linetype=2) +
  ylab("Number of humpback\nwhale entanglements") +
  xlab("") + # "Year
  #labs(subtitle = "Data from NMFS WRO") +
  ggtitle("Confirmed Entanglements, US West Coast\nCommercial Dungeness Crab Fishery") +
  #scale_colour_viridis_d(begin=0.1, end=0.9) + #, option = "plasma") +
  scale_x_continuous(breaks=seq(2010, 2019 , 1),limits=c(2009.5,2019.5))+
  scale_y_continuous(breaks=seq(0, 20, 5),limits=c(0,20))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        #legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1,size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = "none"
  )
entanglements_ts_hump 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_annual_hump.png"), 
  width = 4, height = 4, units = "in", res = 300)
entanglements_ts_hump
invisible(dev.off())

# annual entanglements time series all species
entanglement_df_annual_complete_all <- entanglement_df_annual_complete %>% 
  filter (Year > 2009) %>%
  group_by(Year, Year.as.date) %>%
  summarise(
    count = sum(count)
  )

entanglements_ts_all <- ggplot(entanglement_df_annual_complete_all, 
                           aes(x=Year, y=count)) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) +
  geom_line() + 
  #geom_vline(xintercept = c(2014.5, 2018.5), linetype=2) +
  ylab("Number of\nentanglements") +
  xlab("") + # "Year
  #labs(subtitle = "Data from NMFS WRO") +
  #ggtitle("Number of Whale\nEntanglements,US West Coast") +
  #scale_colour_viridis_d(begin=0.1, end=0.9) + #, option = "plasma") +
  scale_x_continuous(breaks=seq(2010, 2019 , 1),limits=c(2009.5,2019.5))+
  scale_y_continuous(breaks=seq(0, 25, 5),limits=c(0,25))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        #legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1,size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = "none"
  )
entanglements_ts_all 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_annual_allspecies.png"), 
  width = 4, height = 4, units = "in", res = 300)
entanglements_ts_all
invisible(dev.off())

### Initial processing of full data frame

df_all <- readr::read_csv(path_entanglement_file_all)
glimpse(df_all)

# check for naming issues
unique(df_all$`Common Name`) # Gray Whale, Gray whale, Humpback Whale, Humpback whale, Blue Whale, Blue whale, Unidentified Whale, Unidentified whale, Sperm Whale, Sperm whale, Minke Whale, Minke whale, Fin Whale, Fin whale
unique(df_all$`Entanglement Fishery Code`)

# grab columns we want, make columns we need, including adding zeroes
df_all <- df_all %>%
  mutate(
    species = case_when(
      `Common Name` == "Humpback Whale" | `Common Name` == "Humpback whale" ~ "Humpback Whale",
      `Common Name` == "Blue Whale" | `Common Name` == "Blue whale" ~ "Blue Whale",
      TRUE ~ "Other / Unidentified"),
    plotting_date = as.Date(paste(Year, Month, 1, sep = "-")),
    count_confirmed = ifelse(
      `Entanglement Confirmation` == "C",1,0
      )
    ) %>%
  select(Year, plotting_date, State, species, `Entanglement Fishery Code`, count_confirmed) %>%
  complete(
    plotting_date = seq.Date(min(plotting_date), max(plotting_date), by="month"), species, `Entanglement Fishery Code`,
    fill = list(count_confirmed=0)
    )
glimpse(df_all)  

# grab commercial Dungeness crab entanglements only, summarise by species and month
df_dcc <- df_all %>%
  filter(`Entanglement Fishery Code` == "Dcc") %>%
  select(-`Entanglement Fishery Code`) %>%
  group_by(plotting_date, species) %>%
  summarise(
    count_confirmed = sum(count_confirmed, na.rm=TRUE)
  )
glimpse(df_dcc)
#unique(df_dcc$plotting_date)

basic_ts <- df_dcc %>% 
  ungroup() %>% 
  mutate(yr=year(plotting_date)) %>% 
  filter(yr>2010) %>% 
  mutate(species=factor(species,levels=c("Humpback Whale","Blue Whale","Other / Unidentified"))) %>% 
  group_by(yr,species) %>% 
  summarise(tot=sum(count_confirmed)) %>% 
  ggplot(aes(yr,tot,fill=species))+
  geom_col()+
  labs(x="Year",y="Total Confirmed Entanglements",title="Confirmed Entanglements in\nDungeness Crab Fishing Gear")+
  scale_fill_nejm(name="Species")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),
        legend.position = c(0.2,0.8),
        axis.text=element_text(size=14))
  

### make some plots

# monthly entanglements time series humpbacks only
monthly_ts_hump <- ggplot(
  df_dcc %>% 
    filter (plotting_date >= "2009-01-01" & plotting_date <= "2018-10-01") %>% 
    filter (species == "Humpback Whale"),
  aes(x=plotting_date, y=count_confirmed)) +
  geom_point(size=1) +
  geom_line() + 
  ylab("Number of humpback\nwhale entanglements") +
  xlab("") +
  ggtitle("Confirmed Entanglements, US West Coast\nCommercial Dungeness Crab Fishery") +
  #scale_x_continuous(breaks=seq(2009, 2018 , 1),limits=c(2008.5,2018.5)) +
  scale_x_date(date_breaks = "6 months",
               date_minor_breaks = "1 month",
               date_labels = "%B %Y") +
  scale_y_continuous(breaks=seq(0, 5, 1),limits=c(0, 5))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        #legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1,size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = "none"
  )
monthly_ts_hump 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_monthly_hump.png"), 
  width = 4, height = 4, units = "in", res = 300)
monthly_ts_hump
invisible(dev.off())

# quarterly entanglements time series humpbacks only
quarterly_ts_hump <- ggplot(
  df_dcc %>% 
    filter (plotting_date >= "2009-02-01" & plotting_date <= "2018-10-01") %>% 
    filter (species == "Humpback Whale") %>%
    mutate(
      plotting_quarter = lubridate::quarter(plotting_date, with_year = T, fiscal_start = 11)
    ) %>%
    group_by(plotting_quarter, species) %>%
    summarise(
      count_confirmed = sum(count_confirmed)
    ),
  aes(x=plotting_quarter, y=count_confirmed)) +
  geom_point(size=1) +
  geom_line() + 
  ylab("Number of entanglements") +
  xlab("") +
  ggtitle("Confirmed Humpback Whale Entanglements,\nUS West Coast Commercial Dungeness Crab Fishery") +
  #scale_x_continuous(breaks=seq(2009, 2018 , 1),limits=c(2008.5,2018.5)) +
  scale_x_date(date_breaks = "6 months",
               date_minor_breaks = "1 month",
               date_labels = "%B %Y") +
  scale_y_continuous(breaks=seq(0, 10, 2),limits=c(0, 10))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        #legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1,size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = "none"
  )
quarterly_ts_hump 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_quarterly_hump.png"), 
  width = 4, height = 4, units = "in", res = 300)
quarterly_ts_hump
invisible(dev.off())

# Nov-Dec vs May-Sep entanglements time series humpbacks only
early_late_season_ts_hump <- ggplot(
  df_dcc %>% 
    filter (plotting_date >= "2009-02-01" & plotting_date <= "2018-10-01") %>% 
    filter (species == "Humpback Whale") %>%
    mutate(
      time_of_year = case_when(
        month(plotting_date) > 10 ~ "Fall (Nov-Dec)",
        month(plotting_date) >= 5 & month(plotting_date) <= 9 ~ "Summer (May-Sep)", 
        TRUE ~ "Middle (Jan-Apr)"),
      plotting_year = as.Date(paste(year(plotting_date), 1, 1, sep = "-"))
      ) %>%
    group_by(plotting_year, time_of_year, species) %>%
    summarise(
      count_confirmed = sum(count_confirmed)
    ),
  aes(x=plotting_year, y=count_confirmed)) +
  geom_point(size=1) +
  geom_line() + 
  facet_grid(rows = vars(time_of_year)) +
  ylab("Number of entanglements") +
  xlab("") +
  ggtitle("Confirmed Humpback Whale Entanglements,\nUS West Coast Commercial Dungeness Crab Fishery") +
  #scale_x_continuous(breaks=seq(2009, 2018 , 1),limits=c(2008.5,2018.5)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(breaks=seq(0, 15, 3),limits=c(0, 15))+
  theme_classic() +
  #scale_fill_manual(values=c("lightskyblue", "coral")) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        #legend.text = element_text(size=11),
        axis.text.x = element_text(hjust = 1, size = 11, angle = 60),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = "none"
  )
early_late_season_ts_hump 

png(here::here(
  "vertical line model",
  "figures",
  "entanglements_ts_3partsofseason_hump.png"), 
  width = 4, height = 4, units = "in", res = 300)
early_late_season_ts_hump
invisible(dev.off())
