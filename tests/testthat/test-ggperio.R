context("ggperio")

test_that("ggperio works", {
  load("per_dt.rda")
  out <- ggperio(per_dt, aes(y=power, colour=condition))
  out +  stat_pop_etho()

  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition))
  out +  stat_pop_etho()
  #  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition))
  out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)

})
