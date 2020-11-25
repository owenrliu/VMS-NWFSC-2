# Landings vs. revenue from fish tickets
library(here)
library(tidyverse)
library(magrittr)
library(lubridate)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        panel.grid.minor = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

# fish tickets
d <- read_rds(here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds')) %>% 
  filter(agency_code=="C")

# get weekly landings and revenue, and convert to price

monthly_prices <- d %>% 
  group_by(port_group_code,crab_season,year,month) %>% 
  #total landings and revenue
  summarise(lbs=sum(DCRB_lbs,na.rm=T),revenue=sum(DCRB_revenue,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(ppp=revenue/lbs) %>% 
  mutate(date=paste(year,month,"01",sep="_") %>% as_date())

monthly_prices %>% 
  filter(ppp<20,ppp!=0) %>% 
  ggplot(aes(date,ppp))+
  geom_point()+geom_line()+
  facet_wrap(~port_group_code)

# correlation

landings_v_revenue <- monthly_prices %>% 
  ggplot(aes(lbs/1000,revenue/1000))+
  geom_point(size=0.25)+
  geom_smooth(method='lm',se=F)+
  labs(x="Monthly Landings (thousand pounds)",y="Revenue (Thousand $)")

landings_v_revenue

cor(monthly_prices$lbs,monthly_prices$revenue,use='complete.obs')

# average PPP
summary(lm(revenue~0+lbs,data=monthly_prices))

# price with monthly deviations
monthly_prices %<>% mutate(mth=month.name[month(date)]) %>% mutate(yrdiff=year-2009)
price_vs_month <- lm(ppp~mth+yrdiff,data=monthly_prices)
summary(price_vs_month)


# average monthly prices
idx1 <- crossing(yrdiff=0:10,mth=month.name) %>% mutate(ppp=predict(price_vs_month,newdata=.)) %>% 
  mutate(yr=2009+yrdiff) %>% mutate(date=paste(yr,month.name,"01",sep="_") %>% as_date())
idx1 %>% 
  ggplot(aes(date,ppp))+
  geom_point()+geom_line()+
  labs(x="Month",y="Price per Pound")

# from data
monthly_price_index <-  d %>% 
  group_by(year,month) %>% 
  filter(!(month %in% c(7,8))) %>% 
  #total landings and revenue
  summarise(lbs=sum(DCRB_lbs,na.rm=T),revenue=sum(DCRB_revenue,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(ppp=revenue/lbs) %>% 
  mutate(date=paste(year,month,"01",sep="_") %>% as_date()) %>% 
  mutate(type="data")

monthly_price_index %>% 
  ggplot(aes(date,ppp))+
  geom_point()+geom_line()+
  labs(x="Month",y="Price per Pound")

# together
idx_combined <- idx1 %>% 
  select(date,ppp) %>% 
  mutate(type="model") %>% 
  bind_rows(select(monthly_price_index,date,ppp,type))

library(ggsci)
idx_combined %>% 
  ggplot(aes(date,ppp,col=type,group=type))+
  geom_point()+geom_line()+
  scale_color_aaas()+
  labs(x="Month",y="Price per Pound")

# the model kind of sucks
