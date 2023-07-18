map_fishing_grid10 <- function(vms_sf,metric="activity",keep_group,keep_years,states_df_coast,max_vms){
  vms_sf <- vms_sf[vms_sf$subgroup==keep_group & !is.na(vms_sf$FID_),]
  
  if(metric=="activity"){
    if(!is.na(max_vms)){vms_sf <- vms_sf[vms_sf$n.vms < max_vms,]}
  
  vms_scale_max <- max(vms_sf$n.vms)
  vms_scale_min <- min(vms_sf$n.vms)
  
  myplot_list <- list()
  for(i in seq(1,length(keep_years))){
    yr <- as.numeric(keep_years[i])
    tmpvms_sf <- vms_sf[vms_sf$crab_year==yr,]
    if(i < length(keep_years)){
      if(i==1){
        myplot <- ggplot()  +
          geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
          geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-122), lty=3) +
          geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
          geom_sf(data=tmpvms_sf, aes(fill=n.vms), lwd=0, color=NA) + 
          scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vms_scale_min,vms_scale_max)) +
          ggtitle(yr) +
          theme_void() + 
          theme(legend.position="none",plot.margin=margin(l=0,r=-3,unit="cm"),
                plot.title = element_text(hjust=0.2,vjust=-5))
      } else{
      myplot <- ggplot()  +
        geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
        geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-122), lty=3) +
        geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
        geom_sf(data=tmpvms_sf, aes(fill=n.vms), lwd=0, color=NA) + 
        scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vms_scale_min,vms_scale_max)) +
        ggtitle(yr) +
        theme_void() + 
        theme(legend.position="none",plot.margin=margin(l=-2,r=-3,unit="cm"),
              plot.title = element_text(hjust=0.2,vjust=-5))
      }
    } else{
      myplot <- ggplot()  +
        geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
        geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-120), lty=3) +
        geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
        geom_sf(data=tmpvms_sf, aes(fill=n.vms), lwd=0, color=NA) +
        geom_text_repel(data=pg_df, aes(x=Lon, y=Lat, label=port_group_label),size=4,
                  nudge_x=c(1.2,1.1,1.6,1,1,0.6,1.4),nudge_y=c(0.25,rep(0,6)),
                  min.segment.length=0.1) + 
        scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vms_scale_min,vms_scale_max), name="VMS Density") +
        ggtitle(yr) +
        theme_void() +
        theme(plot.margin=margin(l=-2,r=0,unit="cm"),
              plot.title = element_text(hjust=0.2,vjust=-5))
      myplot
    }
    myplot_list[[i]] <- myplot
  }
  
  # finalize plot area / theme
  # myplot <- myplot + theme_void() +
  #   theme(legend.position="none",plot.margin=margin(l=0,r=0.5,unit="cm"),
  #         plot.title = element_text(hjust=0.5))  +
  #   coord_fixed(xlim=c(-127,-120+j),ylim=c(33,43))
  } else if(metric=="vessels"){
    if(!is.na(max_vms)){vms_sf <- vms_sf[vms_sf$n.vms < max_vms,]}
    
    vessel_scale_max <- max(vms_sf$n.vessels)
    vessel_scale_min <- min(vms_sf$n.vessels)
    
    myplot_list <- list()
    for(i in seq(1,length(keep_years))){
      yr <- as.numeric(keep_years[i])
      tmpvms_sf <- vms_sf[vms_sf$crab_year==yr,]
      if(i < length(keep_years)){
        if(i==1){
          myplot <- ggplot()  +
            geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
            geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-122), lty=3) +
            geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
            geom_sf(data=tmpvms_sf, aes(fill=n.vessels), lwd=0, color=NA) + 
            scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vessel_scale_min,vessel_scale_max)) +
            ggtitle(yr) +
            theme_void() + 
            theme(legend.position="none",plot.margin=margin(l=0,r=-3,unit="cm"),
                  plot.title = element_text(hjust=0.2,vjust=-5))
        } else{
          myplot <- ggplot()  +
            geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
            geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-122), lty=3) +
            geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
            geom_sf(data=tmpvms_sf, aes(fill=n.vessels), lwd=0, color=NA) + 
            scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vessel_scale_min,vessel_scale_max)) +
            ggtitle(yr) +
            theme_void() + 
            theme(legend.position="none",plot.margin=margin(l=-2,r=-3,unit="cm"),
                  plot.title = element_text(hjust=0.2,vjust=-5))
        }
      } else{
        myplot <- ggplot()  +
          geom_path(data=states_df_coast,aes(x=long,y=lat,group=group),col="grey50") +
          geom_segment(aes(y=38.79, yend=38.79,x=-124.5,xend=-120), lty=3) +
          geom_point(data=pg_df, aes(x=Lon, y=Lat), color="black") +
          geom_sf(data=tmpvms_sf, aes(fill=n.vessels), lwd=0, color=NA) +
          geom_text_repel(data=pg_df, aes(x=Lon, y=Lat, label=port_group_label),size=4,
                          nudge_x=c(1.2,1.1,1.6,1,1,0.6,1.4),nudge_y=c(0.25,rep(0,6)),
                          min.segment.length=0.1) + 
          scale_fill_viridis_c(option="magma", direction=-1, end=0.9, limits=c(vessel_scale_min,vessel_scale_max), name="Vessel Count") +
          ggtitle(yr) +
          theme_void() +
          theme(plot.margin=margin(l=-2,r=0,unit="cm"),
                plot.title = element_text(hjust=0.2,vjust=-5))
        myplot
      }
      myplot_list[[i]] <- myplot
    }
    
    # finalize plot area / theme
    # myplot <- myplot + theme_void() +
    #   theme(legend.position="none",plot.margin=margin(l=0,r=0.5,unit="cm"),
    #         plot.title = element_text(hjust=0.5))  +
    #   coord_fixed(xlim=c(-127,-120+j),ylim=c(33,43))
  } else{stop("ERROR: unknown metric. revisit what you are asking the fx to plot by grid cells.")}
  
  return(myplot_list)
  
}

