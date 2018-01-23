context("geom_peak")

test_that("geom_peak works", {
  load("per_dt.rda")
  out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition, peak=peak)) +
                      stat_pop_etho() +
                      facet_wrap( ~ id, labeller = id_labeller)
  out
  out + geom_peak(colour="black")


  # with default values for aes
  out <- ggperio(per_dt) +
    stat_pop_etho() +
    facet_wrap( ~ id, labeller = id_labeller)+
   geom_peak(colour="black")
  out
})
