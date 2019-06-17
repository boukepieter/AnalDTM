get.wgs84 <- function(){
  raster::crs("+init=epsg:4326")
} 

spatial.join.locs <- function(points, shapes, targetcolumn, column.names){
  if (!class(points)=="SpatialPointsDataFrame"){
    stop("Points must be of class SpatialPointsDataFrame")
    }
  if (!class(shapes)=="SpatialPolygonsDataFrame"){
    stop("Points must be of class SpatialPolygonsDataFrame")
    }
  if (is.na(raster::crs(points)) | is.na(raster::crs(shapes))){
    stop("CRS of both the points as the area has to be known")
    }
  shapes <- sp::spTransform(shapes, raster::crs(points))
  for (i in 1:length(targetcolumn)){
    points@data[,column.names[i]] <- sp::over(points, shapes)[,targetcolumn[i]]
  }
  points
}

DTM.extract.area <- function(dataset, areas) {
  ds_spdf <- sp::SpatialPointsDataFrame(dataset[,c("Longitude","Latitude")],dataset,proj4string=get.wgs84())
  ds_area <- sp::over(ds_spdf,areas)[,2]
  ds_area
}

join.results.to.shape <- function(table, area, tableColumn, areaColumn) {
  matcher <- match(table[,tableColumn], area@data[,areaColumn])
  area@data <- cbind(area@data,NA)
  area@data <- cbind(area@data,NA)
  names(area)[(ncol(area)-1):ncol(area)] <- c("diff","perc")
  area[matcher,(ncol(area)-1):ncol(area)] <- 
    table[,(ncol(table)-1):ncol(table)]
  area
}