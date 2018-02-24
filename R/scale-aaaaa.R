#' Scales for durations
#'
#' Scales used to represent behaviour durations.
#' @param time_wrap duration (in seconds) used to wrap the labels of the time axis
#' @param unit the unit to be use in the label (e.g. `"second"` instead of `"s"`)
#' @details `time_wrap` is useful, for instance, to express time within a day (ZT).
#' @inheritParams ggplot2::scale_x_time
#' @examples
#' # we generate some data
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition = c("A","B"))
#' dt <- toy_activity_data(metadata,3)
#' # then, a simple plot
#' pl <-  ggetho(dt, aes(y=asleep)) + stat_pop_etho()
#' pl + scale_x_hours(breaks = days(c(1, 2)))
#' pl + scale_x_hours()
#' pl + scale_x_days(breaks = days(c(1, 2)))
#'  pl + scale_x_days()
#' # on a shorter time scale
#' pl <-  ggetho(dt[t < hours(5)], aes(z = asleep)) + stat_tile_etho()
#' pl + scale_x_hours()
#' pl + scale_x_hours(breaks = hours(1:4))
#' pl + scale_x_seconds(breaks = hours(1:4))
#'
#' # time wraping
#' pl <-  ggetho(dt[t < days(2)], aes(y = asleep)) + stat_pop_etho()
#' pl + scale_x_hours(time_wrap = hours(24))
#' @seealso
#' * [ggetho] to generate a plot object
#' * [ggplot2::scale_x_continuous], the defaut ggplot scale, to understand limits, breaks, labels and name
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#coordinate-and-scales)
#' @name time_scales
NULL
