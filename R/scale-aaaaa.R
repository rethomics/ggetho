#' Scales for durations
#'
#' Scales used to represent behaviour durations
#' @inheritParams ggplot2::scale_x_time
#' @examples
#' # we generate some data
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition=c("A","B"))
#' dt <- toy_activity_data(metadata,3)
#' # then, a simple plot
#' pl <-  ggetho(dt, aes(y=asleep)) + stat_pop_etho()
#' pl + scale_x_hours(breaks = days(c(1, 2)))
#' pl + scale_x_hours()
#' pl + scale_x_days(breaks = days(c(1, 2)))
#'  pl + scale_x_days()
#' # on a shorter time scale
#' pl <-  ggetho(dt[t < hours(5)], aes(z=asleep)) + stat_tile_etho()
#' pl + scale_x_hours()
#' pl + scale_x_hours(breaks = hours(1:4))
#' @name time_scales
NULL
