get.wgs84 <- function(){
  raster::crs("+init=epsg:4326")
} 

#' Spatial join; join information of a shape to the points that it contains
#' 
#' @param points a SpatialPointsDataFrame. Projection cannot be unknown.
#' @param shapes a SpatialPolygonsDataFrame. Projection cannot be unknown.
#' @param targetcolumn a character string vector with the names of the columns in \emph{shapes} which need to
#'   be joined to \emph{points}.
#' @param column.names a character string vector with the column names that will be given to the joined 
#'   columns added to \emph{points}.
#' @return It returns the \emph{points} as SpatialPointsDataFrame with the new columns joined to the data.frame.
#' @examples 
#' pts = cbind(1.5:5.5, 1.5:5.5)
#' dimnames(pts)[[1]] = letters[1:5]
#' df = data.frame(a = 1:5)
#' row.names(df) = letters[5:1]
#' points <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)
#' 
#' grd <- GridTopology(c(1,1), c(1,1), c(10,10))
#' polys <- as(grd, "SpatialPolygons")
#' centroids <- coordinates(polys)
#' x <- centroids[,1]
#' y <- centroids[,2]
#' z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
#' pols <- SpatialPolygonsDataFrame(polys,
#'                                  data=data.frame(x=x, y=y, z=z, row.names=row.names(polys)))
#' result <- spatial.join.locs(points, pols)
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