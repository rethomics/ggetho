#' Prepare a ggplot object to represent spectrogram data
#'
#' This function summarises spectrogram data (containing spectrograms of multiple individual),
#' to show period on the `y` axis, time on the `x` axis and power on the `z` axis (*e.g.* as a colour).
#'
#' @inheritParams ggetho
#'
#'@examples
#' \donttest{
#' library(zeitgebr)
#' data(dams_sample)
#' dt <- dams_sample
#' spect_dt <- spectrogram(activity, dt)
#' pl <- ggspectro(spect_dt,time_wrap = hours(24)) + stat_tile_etho() + scale_y_hours(log=T) +
#'   stat_ld_annotations(ld_colours = c("grey","black"))
#' pl + facet_grid(period_group ~ .)
#' pl + facet_wrap(~ id)
#'}
#' @export
ggspectro <- function(data,
                   mapping = aes(),
                   summary_FUN = mean,
                   summary_time_window = mins(30),
                   time_wrap = NULL,
                   time_offset = 0,
                   ...){


  # trick to avoid NOTES from R CMD check:
  x_off = x_name = y =  . = NULL

  if(time_offset != 0 & is.null(time_wrap))
    warning("Time offset only relevant when using time_wrap.
            Ignoring argument")
  else
    time_offset <- time_offset %% time_wrap


  #mapping_list <- as.list(as.character(mapping))
  mapping_list <- make_labels(mapping)
  aes_names <- names(mapping_list)

  if(!"x" %in% aes_names)
    mapping_list$x = "t"

  if(!"y" %in% aes_names)
    mapping_list$y = "period"

  if(!"z" %in% aes_names)
    mapping_list$z= "power"

  x_name <- mapping_list$x
  y_name <- mapping_list$y
  z_name <- mapping_list$z

  var_of_interest = mapping_list$z

  foo <- function(data){ behavr::bin_apply_all(data,
                                         var_of_interest,
                                         x = x_name,
                                         x_bin_length = summary_time_window,
                                         wrap_x_by = time_wrap,
                                         FUN = summary_FUN)}

  o <- data[, foo(.SD),by=y_name]
  sdt <- meta(data)[o,on=data.table::key(data)]

  if(!is.null(time_wrap)){
    sdt[, x_off := eval(parse(text = x_name)) ]
    sdt[, x_off := ((x_off + time_offset) %% time_wrap ) - time_offset]
    sdt[, x_name] <- sdt[, x_off]
    sdt[, x_off := NULL]
  }
  #sdt[,,.SD,keyby=c("id", "x_name")]


  scale_x_FUN <- auto_x_time_scale(sdt[[mapping_list$x]])
  mapping_list <- lapply(mapping_list,
                         function(x){
                           if(x %in% colnames(sdt))
                             paste0("`", x, "`")
                           else
                             x
                         })

  mapping = do.call(aes_string, mapping_list)
  out <- ggplot(sdt, mapping,...)
  if(!is.null(time_wrap))
    return( out + scale_x_FUN(limits=c(- time_offset, time_wrap- time_offset)))

  out + scale_x_FUN()
}

