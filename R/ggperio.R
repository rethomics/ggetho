#' Prepare a ggplot object to represent periodogram data
#'
#' TODO
#'
#' @inheritParams ggetho
#'
#' @examples
#' \dontrun{
#' library(zeitgebr)

#' metadata <- data.table(id=sprintf("toy_experiment|%02d" , 1:40), region_id=1:40,
#'                        condition=c("A","B"),
#'                        sex=c("M","M", "F", "F"))
#' dt <- toy_activity_data(metadata, seed=107)
#' dt[, t := ifelse(xmv(condition) == "A", t, t * 1.01)]
#' per_dt <- periodogram(moving, dt, FUN = chi_sq_periodogram)

#' out <- ggperio(per_dt, aes(y=power, colour=condition))
#' out +  stat_pop_etho()
#
#' out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition))
#' out +  stat_pop_etho()
#
#' out <- ggperio(per_dt, aes(y=power - signif_threshold, colour=condition))
#' out +  stat_pop_etho() + facet_wrap( ~ id, labeller = id_labeller)
#' }
ggperio <- function(data,
                    mapping = aes(x = period, y = power),
                   ...){
  mapping_list <-as.list(as.character(mapping))
  aes_names <- names(mapping_list)
  if(!"x" %in% aes_names)
    mapping_list$x = "period"
  if(!"y" %in% aes_names)
    mapping_list$y = "power"

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
