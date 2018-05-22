#' Prepare a ggplot object to represent periodogram data
#'
#' This function summarises periodogram data (containing periodogram of multiple individual),
#' to show period on the `x` axis, and power (or equivalent) on the `y` axis.
#'
#' @inheritParams ggetho
#'
#' @examples
#' \donttest{
#' require(zeitgebr)
#' # We make toy data
#' metadata <- data.table(id = sprintf("toy_experiment|%02d", 1:40),
#'                        region_id = 1:40,
#'                        condition = c("A", "B"),
#'                        sex = c("M", "M", "F", "F"))
#' dt <- toy_activity_data(metadata, seed = 107)
#' # We shift period of the group "A" by 0.01
#' dt[, t := ifelse(xmv(condition) == "A", t, t * 1.01)]
#' # We  compute a periodogram for each individual
#' per_dt <- periodogram(moving, dt, FUN = chi_sq_periodogram)
#'
#' # Then we display them as an average
#' out <- ggperio(per_dt, aes(y = power, colour = condition))
#' out +  stat_pop_etho()
#'
#' out <- ggperio(per_dt, aes(y = power - signif_threshold, colour = condition))
#' out +  stat_pop_etho()
#
#' out <- ggperio(per_dt, aes(y = power - signif_threshold, colour = condition))
#' out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)
#' }
#' @seealso
#' * [ggetho] to plot time series
#' * [geom_peak] to draw peaks on a periodogram
#' * [zeitgebr::periodogram] to compute periodograms in a first place
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#periodograms)
#' @export
ggperio <- function(data,
                    mapping = aes(x = period, y = power),
                   ...){
  mapping_list <- make_labels(mapping)
  #mapping_list <-as.list(as.character(mapping))
  aes_names <- names(mapping_list)
  if(!"x" %in% aes_names)
    mapping_list$x = "period"
  if(!"y" %in% aes_names)
    mapping_list$y = "power"
  if(!"peak" %in% aes_names & "peak" %in% colnames(data))
    mapping_list$peak = "peak"

  has_colour = "colour" %in% aes_names
  has_fill = "fill" %in% aes_names

  # if has only colour Xor fill defined
  if( xor(has_fill, has_colour)){
    col = c(mapping_list$fill, mapping_list$colour)[[1]]
    mapping_list$fill <- col
    mapping_list$colour <- col
  }
  x_name <- mapping_list$x
  # rejoin(data)[,
  #              summary_FUN(eval(parse(text=x_name))),
  #              by=key(data)]
  #mapping_list
  scale_x_FUN <- auto_x_time_scale(data[[mapping_list$x]])

  mapping_list <- lapply(mapping_list,
                         function(col){
                           if(col %in% colnames(data))
                             paste0("`", col, "`")
                           else
                             col
                         })

  # mapping_list$group <-  key(data)[1]
  mapping = do.call(aes_string, mapping_list)

  ggplot(rejoin(data), mapping = mapping,...) + scale_x_FUN(name = "Period")
}
