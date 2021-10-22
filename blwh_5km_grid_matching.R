## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libs,message=FALSE,warning=FALSE------------------------------------
library(raster)
library(sf)
library(fasterize)
library(tidyverse)
library(magrittr)
library(here)

rm(list=ls())


## ------------------------------------------------------------------------
# reference raster (the 5km grid)
grd <- read_sf(here::here('data','raw','grid','regions_master_final_lamb.shp'))
names(grd)
ext <- st_bbox(grd) %>% st_as_sfc() %>% as_Spatial() %>% raster::extent()
grd_r <- fasterize(grd,raster::raster(crs=st_crs(grd),resolution=5000,ext=ext),field="GRID5KM_ID")
grd_r

writeRaster(grd_r,here::here('data','raw','grid','raster5km'))
# example day of data
head(list.files(here::here('data','Blue whale predictions')))
r <- raster(here::here('data','Blue whale predictions', '2015-01-01_ensemble.grd'))



## ----warning=F,message=F-------------------------------------------------
r2 <- r %>% 
  # project to the new coordinate reference system
  projectRaster(grd_r) %>% 
  # resample to the 5km grid, using nearest neighbors
  resample(grd_r,method='ngb')

# see the results

r2
plot(r)
plot(r2)


## ------------------------------------------------------------------------
resample_5km_grd <- function(fn,returnRaster=FALSE){
  rast <- raster(here::here('data','Blue whale predictions', fn))
  raster_name <- paste0('day_',str_trunc(fn,10,ellipsis=""))
  names(rast) <- raster_name
  out_rast <- rast %>% 
    # project to the new coordinate reference system
    projectRaster(grd_r) %>% 
    # resample to the 5km grid, using nearest neighbors
    resample(grd_r,method='ngb')
  writeRaster(out_rast,paste0(here::here('data','Blue whale predictions','5km grid rasters'),'/',raster_name),overwrite=TRUE)
  if(returnRaster){return(out_rast)}
}

all_grd_files <-list.files(here::here('data','Blue whale predictions'))
all_grd_files <- all_grd_files[str_which(all_grd_files,'.grd')]
head(all_grd_files)

# 2009 January
p <- proc.time()
jan2009 <- all_grd_files[str_which(all_grd_files,'2009-01')] %>% 
  purrr::map(resample_5km_grd,returnRaster=T)
proc.time()-p


## ------------------------------------------------------------------------
p <- proc.time()
all_grd_files %>% 
  purrr::walk(resample_5km_grd,returnRaster=F)
proc.time()-p

