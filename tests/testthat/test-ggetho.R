context("ggteho")

test_that("ggetho works well with a variety of expressions", {
  metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:20),
                    condition=c("A","B"))
  dt <- toy_activity_data(metadata, 3)
  pl <- ggetho(dt, aes(y=moving))
  pl
  pl <- ggetho(dt, aes(y=1-moving ))
  pl
  pl <- ggetho(dt, aes(y=!moving ))
  pl


  pl <- ggetho(dt, aes(z=moving))
  pl
  pl <- ggetho(dt, aes(z=1-moving ))
  pl
  pl <- ggetho(dt, aes(z=!moving ))
  pl

  # sorting with paste
  pl <- ggetho(dt, aes(z=moving,y=paste(condition, id)))
  pl

  pl <- ggetho(dt, aes(z=moving, y=paste(condition, id)))
  pl
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
