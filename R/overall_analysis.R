DTM.point.analysis <- function(target, dbase, outputfolder, date, target_file, databasefolder=NULL, 
                               bline.is.dbase=T) {
  if (bline.is.dbase){ bline <- extract.bline.from.dbase(dbase, target) 
  } else { bline <- dbase}
  
  # Compare points  
  anal_table <- target
  newcols <- get.newcols(date)
  anal_table[,newcols] <- compareReturnees(bline[,c(1,ncol(bline))],
                                           target[,c(1,ncol(target))])[,2:4]
  # export result
  write.csv(anal_table,sprintf("%s/%s/analysis_location_%s.csv",outputfolder,
                               date,substr(target_file,1,nchar(target_file)-5)),
            row.names=F)
  
  # plot result
  name_col <- grep("name",names(anal_table), ignore.case=T)[1]
  anal_table[,name_col] <- gsub("&","",anal_table[,name_col])
  anal_spdf <- SpatialPointsDataFrame(anal_table[which(anal_table[,ncol(anal_table)]!=0),c("Longitude","Latitude")],
                                      anal_table[which(anal_table[,ncol(anal_table)]!=0),],proj4string=WGS84)
  plotKML(anal_spdf[,ncol(anal_table)], open.kml=F, plot.labpt=T,
          file.name=sprintf("%s/%s/analysis_location_%s.kml",outputfolder,
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
    write.csv(dbase,sprintf("%s/%s_location_database.csv",databasefolder,date),row.names=F)
    write.csv(dbase,row.names=F,
              sprintf("%s/%s/timeseries_location_%s.csv",outputfolder,date,substr(target_file,1,nchar(target_file)-5)))
  }
}

DTM.subdistrict.analysis <- function(target, dbase, subdistricts, outputfolder, date, target_file, 
                                     bline.is.dbase=T) {
  if (bline.is.dbase){ blinesd <- extract.bline.from.dbase(dbase, target) 
  } else { blinesd <- dbase}
  
  newcols <- get.newcols(date)
  
  # spatial joins
  target$subdistrict <- DTM.extract.area(target, subdistricts)
  target_subdistrict <- ddply(target, "subdistrict", summarise, 
                              sum_values=sum(!!sym(names(target)[ncol(target)-1])))
  blinesd$subdistrict <- DTM.extract.area(blinesd, subdistricts)
  bline_subdistrict <- ddply(blinesd, "subdistrict", summarise, 
                             sum_values=sum(!!sym(names(blinesd)[ncol(blinesd)-1])))
  
  # compare
  matcher <- match(target_subdistrict[,1], subdistricts@data$ADM3_Eng_n)
  anal_table_subdist <- cbind(target_subdistrict, subdistricts@data[matcher,c(6,10)])
  anal_table_subdist[,newcols] <- compareReturnees(bline_subdistrict,target_subdistrict)[,2:4]
  anal_table_subdist[,2] <- NA
  anal_table_subdist[match(bline_subdistrict[,1],anal_table_subdist[,1]),2] <- bline_subdistrict$sum_values
  
  
  # export
  colnames(anal_table_subdist)[2] <- "bline_values"
  anal_table_subdist <- anal_table_subdist[,c(1,3,4,2,5:7)]
  write.csv(anal_table_subdist,sprintf("%s/%s/analysis_subdistrict_%s.csv",outputfolder,date,
                                       substr(target_file,1,nchar(target_file)-5)),
            row.names=F)
  
  # results to kml
  subdistricts <- join.results.to.shape(anal_table_subdist[-nrow(anal_table_subdist),], subdistricts, 
                                        "subdistrict", "ADM3_Eng_n")
  plot_kml <- subdistricts[which(is.na(subdistricts$diff)==FALSE),]
  plot_kml$lbl <- paste(plot_kml$ADM3_Eng_n,plot_kml$diff,
                        paste(plot_kml$perc,"%",sep=""),sep=", ")
  kml(plot_kml, colour=diff, open.kml=F, plot.labpt=T,
      file.name=sprintf("%s/%s/analysis_subdistrict_%s.kml",outputfolder,
                        date,substr(target_file,1,nchar(target_file)-5)),
      folder.name=paste("analysis_subdistrict_",substr(target_file,1,nchar(target_file)-5),sep=""),
      labels=lbl)
  
  # Timeseries export
  if(bline.is.dbase){
    dbase$subdistrict <- DTM.extract.area(dbase, subdistricts)
    summarised_sub <- summarize.returnees(dbase[,c(ncol(dbase),11:(ncol(dbase)-1))], subdistrict)
    summarised_sub <- merge(summarised_sub, anal_table_subdist[,c(1,5:7)], by="subdistrict", all=T)
    write.csv(summarised_sub, row.names=F,
              sprintf("%s/%s/timeseries_subdistrict_%s.csv",outputfolder,
                      date,substr(target_file,1,nchar(target_file)-5)))
  }
}

DTM.district.analysis <- function(target, dbase, district_shapes, outputfolder, date, target_file, 
                                  bline.is.dbase=T) {
  if (bline.is.dbase){ blined <- extract.bline.from.dbase(dbase, target) 
  } else { blined <- dbase}
  
  # summarize per district
  bline_district <- ddply(blined, "District", summarise,  
                          sum_values=sum(!!sym(names(blined)[ncol(blined)])))
  target_district <- ddply(target, "District", summarise,  
                           sum_values=sum(!!sym(names(target)[ncol(target)])))
  
  # add admin info
  matcher <- match(target_district[,1], target$District)
  target_district[,"Governorate"] <- target[matcher,"Governorate"]
  target_district <- target_district[,c(1,3,2)]
  colnames(target_district)[3] <- "bline_values"
  
  
  # compare target with bline and export
  newcols <- get.newcols(date)
  anal_table_dist <- target_district
  anal_table_dist[,3] <- NA
  anal_table_dist[match(bline_district[,1],anal_table_dist[,1]),3] <- bline_district$sum_values
  anal_table_dist[,newcols] <- compareReturnees(bline_district,target_district[,c(1,3)])[,2:4]
  write.csv(anal_table_dist,sprintf("%s/%s/analysis_district_%s.csv",outputfolder,
                                    date,substr(target_file,1,nchar(target_file)-5)),
            row.names=F)
  # prepare graphs
  lookup <- read.csv("database/lookupdists.csv",stringsAsFactors=F)
  anal_table_dist$JAU_distname <- lookup$JAU[match(anal_table_dist$District,lookup$DTM)]
  anal_table_dist <- anal_table_dist[c(ncol(anal_table_dist),1:(ncol(anal_table_dist)-1))]
  district_shapes <- join.results.to.shape(anal_table_dist, district_shapes, "JAU_distname", "A2NameEn")
  matcher <- match(pull(anal_table_dist,JAU_distname), district_shapes$A2NameEn)
  district_shapes$lbl <- ""
  district_shapes$lbl[matcher] <- paste(district_shapes@data[matcher,"A2NameEn"],
                                        district_shapes@data[matcher,"diff"],
                                        paste(district_shapes@data[matcher,"perc"],"%",sep=""),sep=", ")
  plot_kml <- district_shapes[matcher,]
  if(bline.is.dbase){
    summarised <- summarize.returnees(dbase[,c(grep("District",names(dbase)),
                                               grep("families",names(dbase),ignore.case=T)[1]:ncol(dbase))], District)
    summarised <- cbind(summarised, anal_table_dist[,(ncol(anal_table_dist)-2):ncol(anal_table_dist)])
    summarised$link <- NA
    for(n in 1:nrow(summarised)){
      loc <- sprintf('%s/%s/%s.png',outputfolder,date,summarised[n,1])
      summarised$link[n] <- plot.returnee.graph(summarised[n,],loc)
    }
    
    # plot to kml
    desc <- paste0(sprintf('<img src="%s"',summarised$link), 
                   "  width='400'  /><br/&gt; ", 
                   "Caption", '<br/>')
    kml(plot_kml, colour=diff, open.kml=F, plot.labpt=T,
        file.name=sprintf("%s/%s/analysis_district_%s.kml",outputfolder,
                          date,substr(target_file,1,nchar(target_file)-5)),
        folder.name=paste("analysis_district_",substr(target_file,1,nchar(target_file)-5),sep=""),
        labels=lbl, html.table=desc)
    
    # write timeseries
    write.csv(summarised[,c(1,ncol(summarised),2:(ncol(summarised)-1))],
              sprintf("%s/%s/timeseries_district_%s.csv",outputfolder,
                      date,substr(target_file,1,nchar(target_file)-5)),
              row.names=F)
  } else {
    kml(plot_kml, colour=diff, open.kml=F, plot.labpt=T,
        file.name=sprintf("%s/%s/analysis_district_%s.kml",outputfolder,
                          date,substr(target_file,1,nchar(target_file)-5)),
        folder.name=paste("analysis_district_",substr(target_file,1,nchar(target_file)-5),sep=""),
        labels=lbl)
  }
}
