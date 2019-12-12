# Plot port coordinates
library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)

# Ports (change the file path here if the files are somewhere else)
portlist_coords <- read_csv(here::here('data','raw','port_coords_fromBlake_edited.csv'),col_types='cddd') %>% 
  select(port_code,Lon,Lat) %>% 
  set_names(c('port_code','portlon','portlat')) %>% 
  st_as_sf(coords=c('portlon','portlat'),crs=4326) %>% 
  # west coast UTM projection
  st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")

# import a background/land layer from rnaturalearth package
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington')) %>% 
  
  # make sure CRS is the same as the port layer
  st_transform(st_crs(portlist_coords))

# bounding box for coastwide plotting
bbox <- st_bbox(portlist_coords)+c(-5e5,1e5,0,0)

# Plot
ggplot()+
  geom_sf(data=coaststates,fill='gray50')+
  geom_sf(data=portlist_coords,size=1)+
  geom_sf_text(data=portlist_coords,aes(label=port_code),vjust=0,hjust=1.1,size=2)+
  xlim(bbox[1],bbox[3])+ylim(bbox[2],bbox[4])+
  labs(x='',y='',title="West Coast Ports")+
  theme(axis.text.x = element_text(angle=90))

# For individual states
or_bbox <- coaststates %>% filter(name=="Oregon") %>% st_bbox()+c(-1e5,1e5,0,0)
ggplot()+
  geom_sf(data=coaststates,fill='gray50')+
  geom_sf(data=portlist_coords,size=1)+
  geom_sf_text(data=portlist_coords,aes(label=port_code),vjust=0,hjust=1.1,size=3)+
  xlim(or_bbox[1],or_bbox[3])+ylim(or_bbox[2],or_bbox[4])+
  labs(x='',y='',title="OR Ports")+
  theme(axis.text.x = element_text(angle=90))

wa_bbox <- coaststates %>% filter(name=="Washington") %>% st_bbox()+c(-1e5,1e5,0,0)
ggplot()+
  geom_sf(data=coaststates,fill='gray50')+
  geom_sf(data=portlist_coords,size=1)+
  geom_sf_text(data=portlist_coords,aes(label=port_code),vjust=0,hjust=1.1,size=3)+
  xlim(wa_bbox[1],wa_bbox[3])+ylim(wa_bbox[2],wa_bbox[4])+
  labs(x='',y='',title="WA Ports")+
  theme(axis.text.x = element_text(angle=90))

ca_bbox <- coaststates %>% filter(name=="California") %>% st_bbox()+c(-1e5,1e5,0,0)
ggplot()+
  geom_sf(data=coaststates,fill='gray50')+
  geom_sf(data=portlist_coords,size=1)+
  geom_sf_text(data=portlist_coords,aes(label=port_code),vjust=0,hjust=1.1,size=3)+
  xlim(ca_bbox[1],ca_bbox[3])+ylim(ca_bbox[2],ca_bbox[4])+
  labs(x='',y='',title="CA Ports")+
  theme(axis.text.x = element_text(angle=90))
