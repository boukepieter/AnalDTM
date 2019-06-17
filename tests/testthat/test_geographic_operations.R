context("spatial.join.locs()")

# simple example, from scratch:set.seed(1331)
pts = cbind(1.5:5.5, 1.5:5.5)
dimnames(pts)[[1]] = letters[1:5]
df = data.frame(a = 1:5)
row.names(df) = letters[5:1]
points <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")
centroids <- coordinates(polys)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
pols <- SpatialPolygonsDataFrame(polys,
                                 data=data.frame(x=x, y=y, z=z, row.names=row.names(polys)))

test_that("testing valid input", {
  expect_error(spatial.join.locs(pts,pols), "Points must be of class SpatialPointsDataFrame")
  expect_error(spatial.join.locs(points,polys), "Points must be of class SpatialPolygonsDataFrame")
  expect_error(spatial.join.locs(points,pols), "CRS of both the points as the area has to be known")
})
projection(points) <- get.wgs84()
projection(pols) <- get.wgs84()
test_that("testing right output",{
  expect_equal(names(spatial.join.locs(points, pols, "z", "test"))[2],"test")
  expect_equal(spatial.join.locs(points, pols, "z", "test")@data[3, 2],2.518)
})

