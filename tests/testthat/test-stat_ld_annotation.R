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
  expect_equal(ggplot_build(pld)$data[[1]], ggplot_build(plh)$data[[1]])
  expect_equal(ggplot_build(plc)$data[[1]], ggplot_build(plh)$data[[1]])
  pld


  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(colour=NA)
  pl
})
