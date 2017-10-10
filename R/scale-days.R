#' Scales for durations
#'
#' Scales used to represent behaviour durations
#'
#' @inheritParams ggplot2::scale_x_time
#' @examples
#' # we generate some data
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition=c("A","B"))
#' dt <- toy_activity_data(metadata,3)
#' # then, a simple plot
#'pl <-  ggetho(dt, aes(y=asleep)) + stat_pop_etho()
# pl + scale_x_hours(breaks = days(c(1, 2)))
# pl + scale_x_hours()
# pl + scale_x_days(breaks = days(c(1, 2)))
# pl + scale_x_days()
# # on a shorter time scale
# pl <-  ggetho(dt[t < hours(5)], aes(z=asleep)) + stat_tile_etho()
# pl + scale_x_hours()
# pl + scale_x_hours(breaks = hours(1:4))

#' @seealso
#' * [ggplot2::scale_x_time] -- to print the time as HH:MM:SS
#' @export
scale_x_days <- function(name = "Time (day)",
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = scales::censor,
                         na.value = NA_real_,
                         position = "bottom") {

  scale_x_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    position = position,
    trans = days_trans()
  )
}


#' hms <- round(1:12 * 8640)
#' t <- days_trans()
#' t$transform(hms)
#' t$inverse(t$transform(hms))
#' t$breaks(hms)
#' @noRd
days_trans <- function() {
  scales::trans_new(
    "days",
    transform = function(x){ structure(as.numeric(x), names = names(x))},
    inverse = function(x){x},
    breaks = days_breaks(),
    format = function(x)format(as.numeric(x) / 86400)
  )
}

days_breaks <- function(n = 5) {
  function(x) {
    rng <- as.numeric(range(x))
    diff <- rng[2] - rng[1]
    scale <- 86400


    rng <- rng / scale
    breaks <- labeling::extended(
      rng[1], rng[2], n,

            Q = c(1, 5, 2, 2.5, 3),
      only.loose = FALSE
    )
    as.character(breaks * scale)
  }
}



