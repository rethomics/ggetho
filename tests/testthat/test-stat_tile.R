context("stat_tile and bar tile work")

test_that("stat_bar_tile_works", {

  metadata <- data.frame(id = paste0("toy_experiment|",1:30),
                         condition = c("A", "B", "C"))
  dt <- toy_dam_data(metadata,  duration = days(5))

  ggetho(dt, aes(z=activity)) + stat_tile_etho()
  ggetho(dt, aes(z= activity  , y=condition)) +
    stat_tile_etho()

  ggetho(dt, aes(z=activity)) + stat_bar_tile_etho()
  ggetho(dt, aes(z=activity)) + stat_bar_tile_etho(fill="blue")
  ggetho(dt, aes(z=activity)) + stat_bar_tile_etho(aes(fill=..value..))
  #ggetho(dt, aes(z=activity)) + stat_bar_tile_etho(aes(fill=condition))
  ggetho(dt, aes(z=sqrt(activity))) + stat_bar_tile_etho()
  ggetho(dt, aes(z=sqrt(activity), y= condition)) + stat_bar_tile_etho()
  ggetho(dt, aes(z=sqrt(activity), y= condition), time_wrap = hours(24)) + stat_bar_tile_etho()
})



