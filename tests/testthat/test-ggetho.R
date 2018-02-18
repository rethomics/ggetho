context("ggetho")

test_that("ggetho works well with a variety of expressions", {
  metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:20),
                    condition=c("A","B"))
  dt <- toy_activity_data(metadata, 3)
  pl <- ggetho(dt, aes(y=moving))
  testthat::expect_is(pl, "ggplot")
  pl <- ggetho(dt, aes(y=1-moving ))
  testthat::expect_is(pl, "ggplot")
  pl <- ggetho(dt, aes(y=!moving ))
  testthat::expect_is(pl, "ggplot")

  pl <- ggetho(dt, aes(z=moving))
  testthat::expect_is(pl, "ggplot")
  pl <- ggetho(dt, aes(z=1-moving ))
  testthat::expect_is(pl, "ggplot")
  pl <- ggetho(dt, aes(z=!moving ))
  testthat::expect_is(pl, "ggplot")

  # sorting with paste
  pl <- ggetho(dt, aes(z=moving,y=paste(condition, id)))
  testthat::expect_is(pl, "ggplot")

  pl <- ggetho(dt, aes(z=moving, y=paste(condition, id)))
  testthat::expect_is(pl, "ggplot")
})




test_that("ggetho works well with a variety of expressions", {



  metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:3))
  dt <- toy_activity_data(metadata, 3,duration = days(20))
  dt[id == "toy_experiment|02", t := t  * (25/ 24)]

  pl <- ggetho(dt, aes(z=moving), multiplot = 2, multiplot_period = hours(24))
  pl  + stat_bar_tile_etho(fill="blue")+ facet_grid(id ~ .)
  pl  + stat_tile_etho()+ facet_grid(id ~ .)

  pl <- ggetho(dt, aes(z=moving), multiplot = 2, multiplot_period = hours(25))
  pl  + stat_bar_tile_etho(fill="blue")+ facet_grid(id ~ .)
  pl  + stat_tile_etho()+ facet_grid(id ~ .)

  pl <- ggetho(dt, aes(z=moving), multiplot = 3, multiplot_period = hours(24))
  pl  + stat_bar_tile_etho(fill="blue")+ facet_grid(id ~ .)
  pl  + stat_tile_etho()+ facet_grid(id ~ .)


  expect_error(ggetho(dt, aes(z=moving, y = id), multiplot = 2, multiplot_period = hours(24)),
               regexp = "Cannot use y AND multiplot")
  expect_error(ggetho(dt, aes(z=moving), time_wrap = hours(24), multiplot = 2, multiplot_period = hours(24)),
               regexp = "Cannot use time wrapping")
  expect_error(ggetho(dt, aes(z=moving), multiplot = -1, multiplot_period = hours(24)),
               regexp = "multiplot must be an integer >1")
  expect_error(ggetho(dt, aes(z=moving), multiplot = 1.5, multiplot_period = hours(24)),
               regexp = "multiplot must be an integer >1")

})


test_that("ggetho's time_offset works", {

  metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:20),
                         condition=c("A","B"))
  dt <- toy_activity_data(metadata, 3)
  dt[, moving := ifelse(t %% hours(24) > hours(12), moving & rnorm(.N) > -.2, moving)]

  my_layers <- list(stat_pop_etho(), stat_ld_annotations())
  pl_a <- ggetho(dt, aes(y=moving), time_wrap=hours(24)) + my_layers
  pl_b <- ggetho(dt, aes(y=moving), time_wrap=hours(24), time_offset = hours(12)) + my_layers
  pl_c <- ggetho(dt, aes(y=moving), time_wrap=hours(24), time_offset = hours(6)) + my_layers
  pl_d <- ggetho(dt, aes(y=moving), time_wrap=hours(24), time_offset = hours(-6)) + my_layers

  #cowplot::plot_grid(pl_a,pl_b,pl_c,pl_d, labels=letters[1:4])

  expect_warning(ggetho(dt, aes(y=moving), time_offset = 1),
                 "Time offset only relevant when using time_wrap")
  expect_equal(range(ggplot2::ggplot_build(pl_a)$data[[1]]$x), c(0,days(1)  -mins(30)))
  expect_equal(range(ggplot2::ggplot_build(pl_b)$data[[1]]$x), c(hours(-12),days(.5)  -mins(30)))
  expect_equal(range(ggplot2::ggplot_build(pl_c)$data[[1]]$x), c(hours(-6),hours(18)  -mins(30)))
  expect_equal(range(ggplot2::ggplot_build(pl_d)$data[[1]]$x), c(hours(-18),hours(6)  -mins(30)))



})






