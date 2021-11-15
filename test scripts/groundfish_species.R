# Groundfish species by port/landed value/landed pounds

# libraries
library(tidyverse)
library(here)
here <- here::here
# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

# import fish tickets
tix <- read_rds(here('data','processed','fish tickets',"fish_tickets_long_allspecies.rds"))
glimpse(tix)
# species grouping table (from Jameal and Dan for CCIEA)
spp <- read_csv(here('test scripts','species_groupings.csv'))
spp_groups <- read_csv(here('test scripts','spgrpn2.csv'))
spp <- spp %>% left_join(spp_groups)

spp_by_port <- tix %>% 
  # # filter out non-groundfish gear types
  # filter(!(gear_group %in% c('DVG','MSC','TWS','DRG','USP')),
  #        !(gear_name %in% c("CRAB POT","SHELLFISH POT (CRAB)",'CRAB OR LOBSTER POT','CRAB POT','CRAB RING'))) %>%
  # filter for just groundfish
  left_join(spp,by=c('PACFIN_SPECIES_CODE' = 'SPID')) %>% 
  group_by(agency_code,port_group_code) %>% 
  mutate(tot_revenue_port=sum(tot_revenue_spp,na.rm=T)) %>% 
  mutate(spp_prop_rev = tot_revenue_spp/tot_revenue_port) %>% 
  ungroup() %>% 
  group_by(agency_code,port_group_code,PACFIN_SPECIES_CODE,PACFIN_SPECIES_COMMON_NAME) %>%
  filter(SPGRPN2 %in% c(1:4,50,55,110,200,710)) %>% 
  # calc proportion of revenue attributable to each species for each port
  summarise(rev_perc=sum(spp_prop_rev,na.rm=T)*100) %>%
  # select the top 20 groundfish species by percent of revenue for each port
  ungroup() %>% 
  group_by(agency_code,port_group_code) %>% 
  slice_max(order_by=rev_perc,n=20) %>%
  ungroup()

# all species that are >1% of at least 1 port's total revenue
spp_1p <- spp_by_port %>% 
  filter(rev_perc>1) %>% 
  group_by(PACFIN_SPECIES_CODE,PACFIN_SPECIES_COMMON_NAME) %>% 
  slice_max(order_by=rev_perc,n=1)

# some manual filtering
# gf <- c('SABL','CHL1','UHAG','PTRL','LCD1','SSP1','GPHR','BGL1','CLPR','YTR1','CLP1','LCOD','DOVR','LSPN','SSPN','CBZ1','BLGL','BCAC','BANK',
#         'PTR1','LSP1','UFLT','BLCK','BLK1','DVR1','BLUR','REX','LSKT','CNRY','WDOW','EGLS','BYL1','GPH1','CHLB','BRWN','BYEL','GRAS','BRW1',
#         'VRML','YLTL','CBZN','KGL1','PHLB','CHNA','CHN1','DBRK','PWHT','ARTH','YTRK','PCOD','OCTP','SSOL','WSTG','DSRK')
# spp_by_port <- spp_by_port %>% filter(PACFIN_SPECIES_CODE %in% gf)
# 
# # number of appearances in the top 20 species landed in a port
# spp_count <- spp_by_port %>% count(agency_code,PACFIN_SPECIES_CODE)

# nongf <- c('DCRB','CHNK','RURC','ALBC','RCRB','ASRK','BSRM','BTNA','')

