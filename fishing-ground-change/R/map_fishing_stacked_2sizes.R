map_fishing_stacked2 <- function(north_uds,central_uds,key,keep_years,states_df_coast,offset=1.4,north_fill=pnw_palette("Bay",n=5)[1],central_fill=pnw_palette("Bay",n=5)[4],label_year=TRUE){
  myplot <- ggplot()
  j = 0
  for(i in seq(1,length(keep_years))){
    yr <- as.numeric(keep_years[i])
    n_uds <- north_uds[north_uds$id==yr,]
    c_uds <- central_uds[central_uds$id==yr,]
    new_coast <- states_df_coast
    if(i > 1){
      n_uds <- fortify(n_uds) %>% mutate(long=long+j)
      c_uds <- fortify(c_uds) %>% mutate(long=long+j)
      new_coast <- new_coast %>% mutate(long=long+j)
    } 
    if(i < length(keep_years)){
      myplot <- myplot  +
        geom_path(data=new_coast,aes(x=long,y=lat,group=group),col="grey50") +
        geom_segment(aes(y=38.79, yend=38.79,x=-124,xend=-120), lty=3) +
        geom_polygon(data=n_uds, aes(x=long,y=lat, group=group), fill=north_fill,color=north_fill,alpha=0.3, size=0.5) +
        geom_polygon(data=fortify(c_uds), aes(x=long,y=lat, group=group), fill=central_fill,color=central_fill,alpha=0.3, size=0.5)
      j = j + offset
    } else{
      myplot <- myplot  +
        geom_polygon(data=states_geo_df, aes(x=long+j, y=lat, group=group), fill="grey87",linetype=1) +
        geom_path(data=new_coast,aes(x=long,y=lat,group=group),col="grey50") +
        geom_point(data=pg_df, aes(x=Lon+j,y=Lat), color="black") +
        geom_segment(aes(y=38.79, yend=38.79,x=-124,xend=-120+j), lty=3) +
        geom_polygon(data=n_uds, aes(x=long,y=lat, group=group), fill=north_fill,color=north_fill,alpha=0.3, size=0.5) +
        geom_polygon(data=fortify(c_uds), aes(x=long,y=lat, group=group), fill=central_fill,color=central_fill,alpha=0.3, size=0.5) +
        geom_text(data=pg_df, aes(x=Lon+j, y=Lat, label=port_group_label),size=4,
                  nudge_x=c(1.2,1.1,1.6,1,1,0.8,1.4),nudge_y=c(0.25,rep(0,6)))
    }
  }
  
  # finalize plot area / theme
  myplot <- myplot + theme_void() +
    theme(plot.margin=margin(l=0,r=0.5,unit="cm"),
          plot.title = element_text(hjust=0.5))  +
    coord_fixed(xlim=c(-125,-120+j),ylim=c(34.2,43))
  
  if(label_year){
    # data frame to label crab years
    nyrs <- length(keep_years)
    yrs_labels <- data.frame(yr = seq(as.numeric(first(keep_years)),as.numeric(last(keep_years)))) %>%
      mutate(ymin=42.5,ymax=42.5) %>%
      mutate(xmin=seq(-124, -124+((nyrs-1)*offset), by=offset),
             xmax=xmin)
    for(i in seq(1,nyrs)){
      myplot <- myplot +  annotation_custom(grob = textGrob(label = yrs_labels$yr[i], hjust = 0.25, vjust=1,
                                                            gp = gpar(cex = 1)),
                                            ymin = yrs_labels$ymin[i], ymax = yrs_labels$ymax[i],
                                            xmin = yrs_labels$xmin[i], xmax = yrs_labels$xmax[i])
    }
  } #end if(label_year)
  
  return(myplot)
  
}
