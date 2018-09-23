#' @rdname time_scales
#' @export
scale_x_days <- function(name = "Time",
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = scales::censor,
                         na.value = NA_real_,
                         position = "bottom",
                         time_wrap = NULL,
                         unit = "day",
                         log = FALSE) {

  name <- sprintf("%s (%s)", name, unit)
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
    trans = days_trans(time_wrap, log_tr = log)
  )
}


#' @rdname time_scales
#' @export
scale_y_days <- function(name = "Time",
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         labels = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = scales::censor,
                         na.value = NA_real_,
                         position = "left",
                         time_wrap = NULL,
                         unit="day",
                         log = FALSE) {
  name <- sprintf("%s (%s)", name, unit)
  scale_y_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    position = position,
    trans = days_trans(time_wrap, log_tr = log)
  )
}


#' hms <- round(1:12 * 8640)
#' t <- days_trans()
#' t$transform(hms)
#' t$inverse(t$transform(hms))
#' t$breaks(hms)
#' @noRd
days_trans <- function(time_wrap = NULL,log_tr=FALSE) {
  if(is.null(time_wrap))
    formater <- function(x)format(as.numeric(x) / 86400)
  else
    formater <- function(x)format((as.numeric(x) %% time_wrap) / 86400)
  foo <- ifelse(log_tr,log10, identity)
  foo_inv <- ifelse(log_tr,function(x){10^x}, identity)
  scales::trans_new(
    "days",
    transform = function(x){ structure(foo(as.numeric(x)), names = names(x))},
    inverse = function(x){foo_inv(x)},
    breaks = days_breaks(),
    format = formater
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



