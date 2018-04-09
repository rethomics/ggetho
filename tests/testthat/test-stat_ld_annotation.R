context("ld annotation")

test_that("annotations work", {
  t <- mins(0: 60 * 24 * 10)
  dt <- behavr(data.table(id=1, t=t, x=runif(length(t)), key="id"),
               data.table(id=1,key="id"))
  dt
  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations()

  plh <- pl + scale_x_hours()
  pld <- pl + scale_x_days()
  plc <- pl + scale_x_continuous()

  testthat::expect_equal(ggplot_build(pld)$data[[1]], ggplot_build(plh)$data[[1]])
  testthat::expect_equal(ggplot_build(plc)$data[[1]], ggplot_build(plh)$data[[1]])
  pld


  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(colour=NA)
  pl
})


test_that("annotations work with unbound limits", {
  t <- mins(0: 60 * 24 * 10)
  dt <- behavr(data.table(id=1, t=t, x=runif(length(t)), key="id"),
               data.table(id=1,key="id"))
  dt
  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(2.5), NA)) + geom_point()
  pl

  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(NA, days(3.4))) + geom_point()
  pl

  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(1), days(3.4))) + geom_point()
  pl


  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(10), days(3.4))) + geom_point()
  testthat::expect_error(print(pl), "limits are not in order")
})




#
# library(ggetho)
# metadata <- data.table(id=sprintf("toy_experiment|%02d" , 1:40), region_id=1:40,
#                        condition=c("A","B"),
#                        sex=c("M","M", "F", "F"))
# head(metadata)
#
# dt <- toy_activity_data(metadata, seed=107)
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(-16)) +
#   stat_pop_etho()
# pl
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(0)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(-1)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(+1)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(l_duration = hours(16)) +
#   stat_pop_etho()
# pl
#
