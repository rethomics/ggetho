#' Prepare a ggplot object to represent behavioural data
#'
#' This function summarises a variable of interest (y or z axis)
#' in order to subsequently represent it over time (x axis)
#' (either using `ggplot2` or the of plotting functions provided in `ggetho``).
#'
#' @param data [behavr] table containing the data and metadata
#' @param mapping default list of aesthetic mappings to use for plot
#' @param summary_FUN method (function) used to summarise `variable` over time (typically, the mean)
#' @param summary_time_window width (in seconds) of the time window to compute a summary on
#' @param time_wrap time (in seconds) used to wrap the data (see details)
#' @param time_offset time offset (i.e. phase, in seconds) when using `time_wrap`
#' @param ... additional arguments to be passed to [ggplot2::ggplot()]
#' @details `time_wrap` is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day.
#' In this case, `time_wrap = hours(24)`.
#' Instead of representing data from the start of the day, it can be done from any offset, using `time_offset`.
#' For instance,  `time_offset = hours(12)` puts the circadian reference (ZT0) in the middle of the plot.
#' @return an initial plot object that can be further edited.
#' @examples
#' # We start by making a to dataset with 20 animals
#' metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:20),
#'                    condition=c("A","B"))
#' dt <- toy_activity_data(metadata, 3)
#' # We build a plot object with **nothing inside** (just the axis)
#' # we want to show proportion of time sleeping  on the y axis:
#' pl <- ggetho(dt, aes(y=asleep))
#' pl
#' # Sometimes, the variable of interest in not on the y axis, but on z axis (colour scale).
#' # When we do not provide a y axis,
#' # ggetho will make a ID fo each animal and display them on separate rows
#' pl <- ggetho(dt, aes(z=asleep))
#' pl
#' # this one is the same type, but groups the animals by condition
#' pl <- ggetho(dt, aes(z=asleep,y=condition))
#' pl
#' # sorting with paste
#' pl <- ggetho(dt, aes(z=asleep,y=paste(condition, id)))
#' pl
#'
#' # we want to summarise (wrap) data along a circadian day:
#' pl <- ggetho(dt, aes(y=asleep), time_wrap=hours(24))
#' pl
#' @seealso
#' * [stat_pop_etho] to show population trend by aggregating individuals over time
#' * [stat_tile_etho] to show variable of interest as colour intensity
#' * [stat_ld_annotations] to show light and dark phases on the plot
# TODO * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @export
#' @author Quentin Geissmann (\email{qgeissmann@@gmail.com})
ggetho <- function(data,
                    mapping,
                    summary_FUN = mean,
                    summary_time_window = mins(30),
                    #time_conversion=hours,
                    time_wrap=NULL,
                    time_offset=NULL,
                    # todo add time wrap offset / double plotting
                    ...){

  if(!is.null(time_offset))
    stop("Not implemented") #todo
  #todo check argument types!!

  mapping_list <-as.list(as.character(mapping))
  aes_names <- names(mapping_list)

  has_colour = "colour" %in% aes_names
  has_fill = "fill" %in% aes_names

  # if has only colour Xor fill deffined
  if( xor(has_fill, has_colour)){
     col = c(mapping_list$fill, mapping_list$colour)[[1]]
     mapping_list$fill <- col
     mapping_list$colour <- col
  }

  if(!"x" %in% aes_names)
    mapping_list$x = "t"

  x_name <- mapping_list$x

  if("z" %in% aes_names)
    var_of_interest = mapping_list$z
  else if("y" %in% aes_names)
    var_of_interest = mapping_list$y
  else
    stop("Either `y` or `z` should be provided as variable of interest")

  sdt <- behavr::bin_apply_all(data,
                               var_of_interest,
                               x = x_name,
                               x_bin_length = summary_time_window,
                               wrap_x_by = time_wrap,
                               FUN = summary_FUN)

  data.table::setnames(sdt, mapping_list$x, "t__")
  sdt[,t__ := hms::as.hms(t__) ]

  rng <- as.numeric(range(sdt[,t__]))

  data.table::setnames(sdt,"t__", mapping_list$x)


  sdt <- rejoin(sdt)

  # when no `y`` is provided, the default is to have a
  # discrete/factor axis with individuals as rows
  if(!"y" %in% aes_names){
    # todo check those columns exist
    mapping_list$y = "id"
  }


  #mapping_list$x <-  paste0("`", mapping_list$x, "`")


  mapping_list <- lapply(mapping_list,
                         function(x){
                                      if(x %in% colnames(sdt))
                                        paste0("`", x, "`")
                                      else
                                        x
                                    })

  diff <- rng[2] - rng[1]

  if(diff > behavr::days(3)){
    scale_x_FUN <- scale_x_days
  }
  else if(diff > behavr::hours(3)){
    scale_x_FUN <- scale_x_hours
  }
  else{
    scale_x_FUN <- scale_x_seconds
  }

  mapping = do.call(aes_string, mapping_list)
  out <- ggplot(sdt, mapping,...)

  if(!is.null(time_wrap))
    return( out + scale_x_FUN(limits=c(0, time_wrap)))

  out + scale_x_FUN()
}
