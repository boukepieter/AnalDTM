library(testthat)
library(spatial)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
source("functions.R")

context("get.newcols()")

test_that("testing valid input", {
  expect_error(get.newcols("Jun-06-2019"),"give a 'Date' as input")
})
test_that("testing right output", {
  expect_equal(get.newcols(as.Date("2019-06-16")),
               c("families_abs_2019-06-16","families_diff_abs_2019-06-16","families_diff_perc_2019-06-16"))
})

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
projection(points) <- WGS84
projection(pols) <- WGS84
test_that("testing right output",{
  expect_equal(names(spatial.join.locs(points, pols, "z", "test"))[2],"test")
  expect_equal(spatial.join.locs(points, pols, "z", "test")@data[3, 2],2.518)
})

context("compareReturnees()")

test_that("testing valid input", {
  expect_error(compareReturnees(rbind(1:100),rbind(1:100)), "Input must have 2 columns exactly")
})
blineVec <- data.frame(a=1:100,b=rep(5,100))
targetVec <- data.frame(a=1:100,b=rep(8,100))
test_that("testing right output",{
  expect_equal(names(compareReturnees(blineVec, targetVec))[3:4],c("diff","diff_perc"))
  expect_equal(compareReturnees(blineVec, targetVec)[10,3],3)
  expect_equal(compareReturnees(blineVec, targetVec)[10,4],60)
})
