#' @rdname time_scales
#' @export
scale_x_seconds <- function(name = "Time",
                          breaks = waiver(),
                          minor_breaks = waiver(),
                          labels = waiver(),
                          limits = NULL,
                          expand = waiver(),
                          oob = scales::censor,
                          na.value = NA_real_,
                          position = "bottom",
                          time_wrap = NULL,
                          unit="s") {
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
    trans = seconds_trans(time_wrap)
  )
}


#' @rdname time_scales
#' @export
scale_y_seconds <- function(name = "Time",
                            breaks = waiver(),
                            minor_breaks = waiver(),
                            labels = waiver(),
                            limits = NULL,
                            expand = waiver(),
                            oob = scales::censor,
                            na.value = NA_real_,
                            position = "left",
                            time_wrap = NULL,
                            unit="s") {
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
    trans = seconds_trans(time_wrap)
  )
}


seconds_trans <- function(time_wrap = NULL) {
  if(is.null(time_wrap))
    formater <- function(x)format(as.numeric(x))
  else
    formater <- function(x)format((as.numeric(x) %% time_wrap))

  scales::trans_new(
    "seconds",
    transform = function(x){structure(as.numeric(x), names = names(x))},
    inverse = function(x){as.numeric(x) },
    format = formater
  )
}
