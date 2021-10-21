# make time series of entanglements

library(tidyverse)
library(viridis)

# from Lauren Saez
# all data based on "entanglement reports from lauren 100920.xlsx"
# focus on all confirmed commercial Dungeness crab entanglements by year
path_entanglement_file <- "/Users/jameal.samhouri/Documents/RAIMBOWT/Processed Data/Samhouri et al. whales risk/Input_Data/Entanglement Data/2000_19/all_confirmed_dcrb_entanglements.csv" 

###Initial processing

# most code from Make time series of entanglements.Rmd
entanglement_df <- readr::read_csv(path_entanglement_file)
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


### make some figures

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
