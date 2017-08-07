#@include
#' Display a behavioural variable of interest as colour intensity value
#'
#' This function shows the temporal trend (time on the x axis) of a varible of interest as colour instensity (z axis).
#' The y axis is a discrete variable such as a treatment or the id of animals.
#'
#'
#' @family layers
#' @inheritParams ggplot2::stat_summary_2d
#' @inheritParams stat_pop_etho
#' @param method function used to compute the aggregate, when grouping individuals on the same row.
#' The default is [mean]. [median], [min], [max] are other examples of other functions one can use.
#' @examples
#' library(behavr)
#' # we start by making a to dataset with 20 animals
#' query<- data.frame(experiment_id="toy_experiment",
#'                    region_id=1:20,
#'                    condition=c("A","B"),
#'                    age=c(1, 5, 10, 20))
#' print(query)
#' dt <- toy_activity_data(query,seed=3)
#' # We build a plot object
#' pl <-  ggetho(dt, aes(z=asleep))
#' # A standard plot one row per animal:
#' pl + stat_tile_etho()
#' # We can also group animals per condition and calculate the average sleep
#' pl <-  ggetho(dt, aes(z=asleep, y=condition))
#' pl + stat_tile_etho()
#' # Instead, of the average, maybe we want to show the highest (max)
#' # posible value of sleep for any time point
#' pl + stat_tile_etho(method=max)
#' @seealso
#' * [ggetho] to generate a plot object
#' * [stat_pop_etho] to show population trend by aggregating individuals over time
#' * [stat_ld_annotations] to show light and dark phases on the plot
#' * TODO Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @export
stat_tile_etho <- function(mapping = NULL, data = NULL,
                            geom = "raster", position = "identity",
                            ...,
                           method = mean,
                           method.args = list(),
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTileEtho,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = method.args,
      ...
    )
  )
}


StatTileEtho <- ggproto("StatTileEtho", Stat,
                         default_aes = aes(fill = ..value..),
                         required_aes = c("x", "y", "z"),
                         compute_group = function(data, scales, method, method.args = list()){
                           data <- data.table::as.data.table(data)
                           foo <- function(z){
                             all_args <- append(list(z), method.args)
                             do.call(method, all_args)
                           }
                           out <- data[,.(value=foo(z)),by="x,y"]
                           out
                         }
)

