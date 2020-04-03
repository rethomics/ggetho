#' Scales for durations
#'
#' A set of scales used to represent experimental durations.
#' @param time_wrap duration (in seconds) used to wrap the labels of the time axis
#' @param unit the name of unit (string) to be used in the label (e.g. one could use `"second"` instead of `"s"`)
#' @param log logical, whether axis should be on a log-transformed
#' @param expand Vector of range expansion constants used to add some
#' padding around the data, to ensure that they are placed some distance
#' away from the axes. Use the convenience function `ggplot2::expand_scale()`
#' to generate the values for the `expand` argument. The defaults are to
#' expand the scale by 5\% on each side for continuous variables, and by
#' 0.6 units on each side for discrete variables.
#' @details `time_wrap` is useful, for instance, to express time within a day (ZT), instead of absolute time.
#' @inheritParams ggplot2::scale_x_time
#' @return A ggplot scale.
#' @examples
#' # We generate some data
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition = c("A","B"))
#' dt <- toy_activity_data(metadata, 3)
#' # Then, a simple plot
#' pl <-  ggetho(dt, aes(y = asleep)) + stat_pop_etho()
#' pl + scale_x_hours(breaks = days(c(1, 2)))
#' pl + scale_x_hours()
#' pl + scale_x_days(breaks = days(c(1, 2)))
#' pl + scale_x_days()
#'
#' # To express time modulus `time_wrap`
#' # e.g. time n the day
#' pl + scale_x_hours(time_wrap = hours(24)) +
#'      coord_cartesian(xlim=c(0, days(2)))
#'
#' # On a shorter time scale
#' pl <-  ggetho(dt[t < hours(5)], aes(z = asleep)) + stat_tile_etho()
#' pl + scale_x_hours()
#' pl + scale_x_hours(breaks = hours(1:4))
#' pl + scale_x_seconds(breaks = hours(1:4))

#' @seealso
#' * [ggetho] to generate a plot object
#' * [ggplot2::scale_x_continuous], the defaut ggplot scale, to understand limits, breaks, labels and name
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#coordinate-and-scales)
#' @name time_scales
NULL
