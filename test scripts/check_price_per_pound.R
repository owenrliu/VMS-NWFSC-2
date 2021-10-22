library(tidyverse)
library(magrittr)

plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

process_year=2013
tixfiles <- list.files(here::here('data','raw','fish tickets'),full.names = T)

# check one file first
# tixsamp <- read_csv(tixfiles)
# problems(tixsamp) %>% select(col) %>% distinct()
# adding PRICE_PER_POUND column

rawprice <- purrr::map_df(tixfiles,function(fl){
  read_csv(fl,col_types= cols_only(
    FISH_TICKET_ID = col_double(),
    PACFIN_SPECIES_CODE= col_character(), 
    LANDED_WEIGHT_LBS= col_double(), 
    EXVESSEL_REVENUE= col_double(),
    LANDING_YEAR= col_double(), 
    PRICE_PER_POUND=col_double()))
})

rawprice %<>% filter(LANDING_YEAR %in%  process_year)

# select only DCRB and compare price to landings/revenue

dcrb_rawprice <- rawprice %>% filter(PACFIN_SPECIES_CODE=='DCRB')

dcrb_rawprice %<>% mutate(price_calc=round(EXVESSEL_REVENUE/LANDED_WEIGHT_LBS,2))

dcrb_rawprice %>% 
  ggplot(aes(PRICE_PER_POUND,price_calc))+geom_point(size=2)+geom_smooth(method="lm")+
           labs(x='Price per pound (reported)',y='Price per pound (calculated)')

# Do the calculated vs reported prices ever differ?

dcrb_rawprice %<>% mutate(same_price=price_calc==PRICE_PER_POUND) 
sum(dcrb_rawprice$same_price)/nrow(dcrb_rawprice)

#98% of the prices are the same for this sample. look at the ones that arent
price_mismatch <- dcrb_rawprice %>% filter(!same_price)
price_mismatch %<>% mutate(price_difference=(PRICE_PER_POUND-price_calc)/PRICE_PER_POUND*100)

price_mismatch %>% 
  ggplot(aes(price_difference))+
  geom_density(fill='blue',alpha=0.5)+
  labs(x='Percentage Difference between Calculated and Reported Price')

# prices that are not the same are within 0.2%
