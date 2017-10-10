context("sclales")

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

  as.numeric(htr$breaks(one_h)) / 3600
  htr$breaks(one_h/3)
  htr$breaks(three_h)
  htr$breaks(twelve_h)
  htr$breaks(one_day)
  htr$breaks(three_day)
  htr$breaks(three_day/10)
  htr$breaks(three_day/13)
  htr$breaks(five_day/13)

  dtr <- ggetho:::days_trans()
  dtr$breaks(one_h)
  dtr$breaks(one_h/3)
  dtr$breaks(three_h)
  dtr$breaks(twelve_h)
  dtr$breaks(one_day)
  dtr$breaks(three_day)
  dtr$breaks(three_day/10)
  dtr$breaks(three_day/13)
  dtr$breaks(five_day * 1.7)
  dtr$breaks(five_day * 6)

})
