DTM.area.analysis.shiny <- function(target, dbase, shapes, sumname, date, target_file, 
                                    bline.is.dbase=T) {
  if (bline.is.dbase){ blined <- merge(target,dbase,by=c("Location.ID")
                                       ,all=TRUE,suffixes=c(".target",""))
  blined[which(is.na(blined$Latitude)),c("Longitude","Latitude")] <- 
    blined[which(is.na(blined$Latitude)),c("Longitude.target","Latitude.target")]
  } else { blined <- dbase}
  
  blined$District <- spatial.join.locs(SpatialPointsDataFrame(blined[,c("Longitude","Latitude")], 
                                                              blined, proj4string=get.wgs84()), shapes,
                                       sumname,"join")$join
  # summarize per district
  if (bline.is.dbase){
    sums <- names(blined)[rev(grep('families_abs',names(blined)))[1]]
    target_district <- ddply(blined, "District", summarise, 
                             sum_values=sum(!!sym(names(target)[grep("Families",names(blined))]),na.rm=T))
  } else { 
    sums <- names(blined)[ncol(blined)]
    target$District <- spatial.join.locs(SpatialPointsDataFrame(target[,c("Longitude","Latitude")], 
                                                                target, proj4string=get.wgs84()), shapes,
                                         sumname,"join")$join
    target_district <- ddply(target, "District", summarise, 
                             sum_values=sum(!!sym(names(target)[grep("Families",names(blined))]),na.rm=T))
  }
  bline_district <- ddply(blined, "District", summarise,  
                          sum_values=sum(!!sym(sums),na.rm=T))
  
  colnames(bline_district)[2] <- "bline_values"
  
  
  # compare target with bline and export
  newcols <- get.newcols(date)
  anal_table_dist <- merge(target_district,bline_district,by="District",all=T)
  anal_table_dist[is.na(anal_table_dist)] <- 0
  anal_table_dist[,newcols] <- compareReturnees(anal_table_dist[,c(1,3)],anal_table_dist[,c(1,2)])[,2:4]
  anal_table_dist <- anal_table_dist[,-2]
  
  anal_table_dist <- anal_table_dist[is.na(anal_table_dist$District)==FALSE,]
  write.csv(anal_table_dist,sprintf("%s/output/shiny_output/%s/analysis_%s_%s.csv",getwd(),
                                    date,sumname,substr(target_file,1,nchar(target_file)-5)),
            row.names=F)
  shapes <- join.results.to.shape(anal_table_dist, shapes, "District", sumname)
  matcher <- match(pull(anal_table_dist,District), shapes@data[,sumname])
  shapes$lbl <- ""
  shapes$lbl[matcher] <- paste(shapes@data[matcher,sumname],
                               shapes@data[matcher,"diff"],
                               paste(shapes@data[matcher,"perc"],"%",sep=""),sep=", ")
  plot_kml <- shapes[matcher,]
  graphs <- list()
  if(bline.is.dbase){
    dbase$District <- spatial.join.locs(SpatialPointsDataFrame(dbase[,c("Longitude","Latitude")], 
                                                               dbase, proj4string=get.wgs84()), shapes,
                                        sumname,"join")$join
    summarised <- summarize.returnees(dbase[,c(grep("District",names(dbase)),
                                               grep("families",names(dbase),ignore.case=T)[1]:ncol(dbase))], District)
    summarised <- merge(summarised, anal_table_dist[,c(1,(ncol(anal_table_dist)-2):ncol(anal_table_dist))], 
                        by="District",all=T)
    for(n in 1:nrow(summarised)){
      
      graphs[[as.character(summarised[n,1])]] <- plot.returnee.graph.shiny(summarised[n,])
      
    }
    
    # plot 
  } else {
    for(n in 1:nrow(plot_kml)){
      graphs[[as.character(plot_kml@data[n,sumname])]] <- 
        ggplot(plot_kml@data, aes(x=diff,y=perc, text=!!sym(sumname))) +#sprintf("name: %s",strsplit(lbl,",")[[1]][1]))) + 
        geom_point(colour=c(rep("black",n-1),"red",rep("black",nrow(plot_kml)-n))) +
        ggtitle(plot_kml$lbl[n]) + xlab("absolute difference") + ylab("percentual difference") +
        geom_hline(yintercept=0, linetype="dashed") + geom_vline(xintercept=0, linetype="dashed")
    }
  }
  centers <- data.frame(gCentroid(plot_kml, byid = TRUE))
  centers$lbl <- plot_kml$lbl
  pal <- colorNumeric("RdYlBu", replace(plot_kml$perc,is.infinite(plot_kml$perc),0),reverse=T)
  url <- "https://api.mapbox.com/v3/mapbox.iraq/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYm91a2VwaWV0ZXJvdHRvdyIsImEiOiJjanZrem82ZnAwdTliNDRtbDljdHptaXpkIn0.vlYoVkiHTdvkHONpNqy_Sg"
  
  m <- leaflet(plot_kml) %>%
    addTiles(urlTemplate=url) %>%
    addPolygons(layerId=plot_kml@data[,sumname], color="black",fillColor=~pal(plot_kml$perc),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                fillOpacity=0.75,
                popupOptions=popupOptions(maxWidth="100%",closeOnClick=T)) %>%
    addLabelOnlyMarkers(data = centers,
                        lng = ~x, lat = ~y, label = ~lbl,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))%>%
    addLegend(position = "bottomright", pal = pal, values = ~replace(perc,is.infinite(perc),0),
              title = "Returnee increase in percentage",
              opacity = 0.75, labFormat=labelFormat(suffix="%"))%>%
    addMiniMap(
      tiles = providers$Esri.OceanBasemap,
      position = 'topright', 
      width = 200, height = 200,
      toggleDisplay = FALSE)
  
  # write timeseries
  return(list(m=m,graphs=graphs,table=anal_table_dist))
}

DTM.point.analysis.shiny <- function(target, dbase, date, target_file, databasefolder=NULL, 
                                     bline.is.dbase=T) {
  if (bline.is.dbase){ bline <- extract.bline.from.dbase(dbase, target) 
  } else { bline <- dbase}
  
  # Compare points
  anal_table <- merge(target,bline[,c(1,ncol(bline))],by="Location.ID",all.x=T)
  anal_table[which(is.na(anal_table[,ncol(anal_table)])),ncol(anal_table)] <- 0
  newcols <- get.newcols(date)
  anal_table[,newcols] <- compareReturnees(anal_table[,c(1,ncol(anal_table))],
                                           anal_table[,c(1,(ncol(anal_table)-1))])[,2:4]
  anal_table <- anal_table[,-(ncol(anal_table)-4)]
  names(anal_table)[ncol(anal_table)-3] <- "bline"
  # export result
  write.csv(anal_table,sprintf("%s/%s/analysis_location_%s.csv","output/shiny_output",
                               date,substr(target_file,1,nchar(target_file)-5)), row.names=F)
  
  # plot result
  name_col <- grep("name",names(anal_table), ignore.case=T)[1]
  anal_table[,name_col] <- gsub("&","",anal_table[,name_col])
  anal_spdf <- SpatialPointsDataFrame(anal_table[,c("Longitude","Latitude")],
                                      anal_table,proj4string=get.wgs84())
  lbls <- paste(anal_spdf@data[,name_col],anal_spdf@data[,(ncol(anal_table)-1)],
                paste(anal_spdf@data[,ncol(anal_table)],"%",sep=""),sep=", ")
  url <- "https://api.mapbox.com/v3/mapbox.iraq/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYm91a2VwaWV0ZXJvdHRvdyIsImEiOiJjanZrem82ZnAwdTliNDRtbDljdHptaXpkIn0.vlYoVkiHTdvkHONpNqy_Sg"
  
  m <- leaflet(anal_spdf) %>%
    addTiles(urlTemplate=url) %>%
    addCircleMarkers(~Longitude,~Latitude, 
                     radius=ifelse(anal_spdf@data[,ncol(anal_spdf)]==0,2,
                                   ifelse(is.infinite(anal_spdf@data[,ncol(anal_spdf)]),25,
                                          ifelse(anal_spdf@data[,ncol(anal_spdf)]>100,50,
                                                 abs(anal_spdf@data[,ncol(anal_spdf)]/2)))),#sapply(abs(anal_spdf@data[,ncol(anal_spdf)]/2),function(y)min(y,50)),
                     color=ifelse(anal_spdf@data[,ncol(anal_spdf)]==0,"grey",
                                  ifelse(is.infinite(anal_spdf@data[,ncol(anal_spdf)]),"purple",
                                         ifelse(anal_spdf@data[,ncol(anal_spdf)]>0,"green","red"))), stroke=T,
                     fillOpacity=0.2, popup=lbls, #clusterOptions=markerClusterOptions(),
                     popupOptions=popupOptions(maxWidth="100%",closeOnClick=T)) %>%
    addMiniMap(
      tiles = providers$Esri.OceanBasemap,
      position = 'topright', 
      width = 200, height = 200,
      toggleDisplay = FALSE) %>%
    addLegendCustom(title="Percentage increase",colors=c(rep("green",3),rep("red",3),"purple","grey"), 
                    labels=c(">=100%","50%","5%","-5%","-50%","<=-100%","new location","no change"),
                    sizes=c(100,50,5,5,50,100,50,2), shapes=rep("circle",8),
                    borders=c(rep("green",3),rep("red",3),"purple","grey"))
  # addMarkers(~Longitude,~Latitude,#color="black",fillColor=~pal(plot_kml$perc),
  #            popup=lbls,clusterOptions=markerClusterOptions(),
  #            popupOptions=popupOptions(maxWidth="100%",closeOnClick=T)) #%>%
  # addLegend(position = "bottomright", pal = pal, values = ~perc,
  #           title = "Returnee increase in percentage",
  #           opacity = 0.75, labFormat=labelFormat(suffix="%"))
  
  plotKML(anal_spdf[,ncol(anal_table)], open.kml=F, plot.labpt=T,
          file.name=sprintf("%s/%s/analysis_location_%s.kml","output/shiny_output",
                            date,substr(target_file,1,nchar(target_file)-5)),
          folder.name=paste("analysis_location_",substr(target_file,1,nchar(target_file)-5),sep=""),
          points_names=paste(anal_spdf@data[,name_col]
                             ,anal_spdf@data[,(ncol(anal_table)-1)],
                             paste(anal_spdf@data[,ncol(anal_table)],"%",sep=""),sep=", "))
  
  # Write to database
  if (bline.is.dbase){
    missingRows <- which(anal_table$Location.ID %in% dbase$Location.ID == FALSE)
    dbase[(nrow(dbase)+1):(nrow(dbase)+length(missingRows)),1:10] <-
      anal_table[missingRows,c(1:10)]
    dbase <- dbase[order(dbase[,1]),]
    dbase[which(dbase$Location.ID %in% anal_table$Location.ID),newcols] <- anal_table[,newcols]
    write.csv(dbase,sprintf("%s/%s_location_database.csv","output/shiny_output",date),row.names=F)
    write.csv(dbase,row.names=F,
              sprintf("%s/%s/timeseries_location_%s.csv","output/shiny_output",date,substr(target_file,1,nchar(target_file)-5)))
  }
  return(list(m=m,table=anal_table))
}
addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, title="",opacity=0.5){
  make_shapes <- function(colors,sizes,borders,shapes){
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  legend_colors <- make_shapes(colors,sizes,borders,shapes)
  legend_labels <- make_labels(sizes,labels)
  # colorAdditions <- paste0(colors,"; width:",sizes,"px; height:",sizes,"px")
  # labelAdditions <- paste0("<div style='display: inline-block;height: ",sizes,
  #                          "px;margin-top: 4px;line-height: ",sizes,"px;'>",labels,"</div>")
  return(addLegend(position="bottomleft",title=title,map,colors=legend_colors,labels=legend_labels,opacity=opacity))
}