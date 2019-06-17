context("get.newcols()")

test_that("input is valid", {
  expect_error(get.newcols("Jun-06-2019"),"give a 'Date' as input")
})
test_that("output is as expected", {
  expect_equal(get.newcols(as.Date("2019-06-16")),
               c("families_abs_2019-06-16","families_diff_abs_2019-06-16","families_diff_perc_2019-06-16"))
})

context("compareReturnees()")

test_that("input is valid", {
  expect_error(compareReturnees(rbind(1:100),rbind(1:100)), "Input must have 2 columns exactly")
})
blineVec <- data.frame(a=1:100,b=rep(5,100))
targetVec <- data.frame(a=1:100,b=rep(8,100))
test_that("output is as expected",{
  expect_equal(names(compareReturnees(blineVec, targetVec))[3:4],c("diff","diff_perc"))
  expect_equal(compareReturnees(blineVec, targetVec)[10,3],3)
  expect_equal(compareReturnees(blineVec, targetVec)[10,4],60)
})
