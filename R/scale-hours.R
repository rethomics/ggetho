#' @rdname time_scales
#' @export
scale_x_hours <- function(name = "Time",
                          breaks = waiver(),
                          minor_breaks = waiver(),
                          labels = waiver(),
                          limits = NULL,
                          expand = waiver(),
                          oob = scales::censor,
                          na.value = NA_real_,
                          position = "bottom",
                          time_wrap = NULL,
                          unit = "h",
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
    trans = hours_trans(time_wrap, log_tr = log)
  )
}

#' @rdname time_scales
#' @export
scale_y_hours <- function(name = "Time",
                          breaks = waiver(),
                          minor_breaks = waiver(),
                          labels = waiver(),
                          limits = NULL,
                          expand = waiver(),
                          oob = scales::censor,
                          na.value = NA_real_,
                          position = "left",
                          time_wrap = NULL,
                          unit="h",
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
    trans = hours_trans(time_wrap, log_tr = log)
  )
}


hours_trans <- function(time_wrap  = NULL, log_tr = FALSE) {
  if(is.null(time_wrap))
    formater <- function(x)format(as.numeric(x) / 3600)
  else
    formater <- function(x)format((as.numeric(x) %% time_wrap) / 3600)
  foo <- ifelse(log_tr,log10, identity)
  foo_inv <- ifelse(log_tr,function(x){10^x}, identity)

  scales::trans_new(
    "hours",
    transform = function(x){ structure(foo(as.numeric(x)) , names = names(x))},
    inverse = function(x){as.numeric(foo_inv(x)) },
    breaks = hours_breaks(),
    format = formater
  )
}

hours_breaks <- function(n = 5) {
  function(x) {
    rng <- as.numeric(range(x))
    diff <- rng[2] - rng[1]
    scale <- 3600
    rng <- rng / scale

    if(diff > 3600 * 24 * 3){
      q <- c(24, 1)
    }
    else if(diff > 3600 * 3){
      q <- c(6, 3, 1.5, 12, 1)
    }
    else{
      q <- c( 0.5, 0.25, 0.125, 1,  3)
    }

    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = q,
      only.loose = FALSE
    )

    as.character(breaks * scale)
  }
}
