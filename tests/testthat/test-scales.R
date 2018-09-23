context("scales")

test_that("scales work", {

  one_h <- round(behavr::hours(-1:1))
  three_h <- round(behavr::hours(-3:3))
  five_h <- round(behavr::hours(-5:5))
  twelve_h <- round(behavr::hours(-12:12))
  one_day <- round(-24:24  * 3600)
  three_day <- round(-3:3  * 24 * 3600)
  five_day <- round(-5:5  * 24 * 3600)


  htr <- ggetho:::hours_trans()

  #(three_day/10) / behavr::hours(1)

  expect_identical(as.numeric(htr$breaks(one_h)) / 3600 , c(-1,-.5,0,.5,1))
  #as.numeric(htr$breaks(one_h/3)   /
  expect_identical(as.numeric(htr$breaks(three_h)) / 3600, c(-3,-1.5,0,1.5,3))
  expect_identical(as.numeric(htr$breaks(twelve_h)) / 3600, c(-12,-6,0,6,12))
  expect_identical(as.numeric(htr$breaks(one_day)) / 3600, c(-12,-6,0,6,12) * 2)
  expect_identical(as.numeric(htr$breaks(three_day)) / 3600, c(-72,-48,-24,0,24,48,72))


T
  expect_identical(as.numeric(htr$breaks(three_day/10)) / 3600, c(-3,-1.5,0,1.5,3) * 2)
  expect_identical(as.numeric(htr$breaks(three_day/13)) / 3600, c(-3,-1.5,0,1.5,3) * 2)
  expect_identical(as.numeric(htr$breaks(five_day/13)) / 3600, c(-9,-6,-3,0,3,6,9) )


  dtr <- ggetho:::days_trans()
  as.numeric(dtr$breaks(one_h)) / 3600 / 24
  as.numeric(dtr$breaks(one_h/3))/ 3600 / 24
  as.numeric(dtr$breaks(three_h))/ 3600 / 24
  as.numeric(dtr$breaks(twelve_h))/ 3600 / 24
  as.numeric(dtr$breaks(one_day))/ 3600 / 24
  as.numeric(dtr$breaks(three_day))/ 3600 / 24
  as.numeric(dtr$breaks(three_day/10))/ 3600 / 24
  as.numeric(dtr$breaks(three_day/13))/ 3600 / 24
  as.numeric(dtr$breaks(five_day * 1.7))/ 3600 / 24
  as.numeric(dtr$breaks(five_day * 6))/ 3600 / 24

})


test_that("scales work", {

  df <- data.frame(y=rnorm(1000), t=1:1000 * 10)
  ggplot(df, aes(t, y)) + scale_x_days()
  ggplot(df, aes(t, y)) + scale_x_hours()
  ggplot(df, aes(t, y)) + scale_x_seconds()

  ggplot(df, aes(y,t)) + scale_y_days()
  ggplot(df, aes(y,t)) + scale_y_hours()
  ggplot(df, aes(y,t)) + scale_y_seconds()

})


test_that("scales handles log transform", {

  df <- data.frame(t= 1:1000, y=seq(from=1,to=days(.1), length.out=1000))
  pl <- ggplot(df, aes(t, y))
  pl + geom_point()
  testthat::expect_silent(pl+ scale_y_days(log=T) +geom_point())
  testthat::expect_silent(pl+ scale_x_days(log=T) +geom_point())

  testthat::expect_silent(pl+ scale_y_hours(log=T) +geom_point())
  testthat::expect_silent(pl+ scale_x_hours(log=T) +geom_point())

  testthat::expect_silent(pl+ scale_y_seconds(log=T) +geom_point())
  testthat::expect_silent(pl+ scale_x_seconds(log=T) +geom_point())
})

