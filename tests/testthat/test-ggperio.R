context("ggperio")

test_that("ggperio works", {
  load("per_dt.rda")

  out <- ggperio(per_dt, aes(y=power, colour=condition))

  out <- out +  stat_pop_etho()
  print(out)
  testthat::expect_is(out$layers[[1]], "ggproto")

  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition))
  out <- out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)
  print(out)
  testthat::expect_is(out$layers[[1]], "ggproto")


  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition, peak=peak))
  out <- out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)
  print(out)
  testthat::expect_is(out$layers[[1]], "ggproto")


    per_dt[, peak:= NULL]
  out <- ggperio(per_dt, aes(y=power, colour=condition))
  out <- out +  stat_pop_etho()
  print(out)
  testthat::expect_is(out$layers[[1]], "ggproto")

  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition, peak=NULL))
  out <- out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)
  print(out)
  testthat::expect_is(out$layers[[1]], "ggproto")
})
