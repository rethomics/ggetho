#' Display a variable of interest either as colour intensity value or a bar height
#'
#' These functions show the temporal trend (time on the x axis) of a variable of interest (z axis)
#' as either colour intensity (`stat_tile_etho`) or using the hight of the tiles (`stat_bar_tile_etho`).
#' In both cases, the y axis is a discrete variable such as a treatment or the id of individuals.
#'
#' @family layers
#' @inheritParams ggplot2::stat_summary_2d
#' @inheritParams stat_pop_etho
#' @param method function used to compute the aggregate, when grouping individuals on the same row.
#' The default is [mean]. [median], [min], [max] are examples of other functions that can be used.
#' @examples
#' # We start by making a to dataset with 20 animals
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    age = c(1, 5, 10, 20),
#'                    condition = c("A", "B"))
#' print(metadata)
#' dt <- toy_activity_data(metadata, 3)
#' # We build a plot object
#' pl <-  ggetho(dt, aes(z = asleep))
#' # A standard plot one row per animal:
#' pl + stat_tile_etho()
#' # We can also group animals per condition and calculate the average sleep
#' pl <-  ggetho(dt, aes(z = asleep, y = condition))
#' pl + stat_tile_etho()
#'
#' # We can sort by adding condition AND id on the y axis:
#' pl <-  ggetho(dt, aes(z = asleep, y = interaction(id, condition)))
#' pl + stat_tile_etho()
#' # Same if we want to sort by age
#' pl <-  ggetho(dt, aes(z = asleep, y = interaction(id, age)))
#' pl + stat_tile_etho()
#'
#' # Instead, of the average, maybe we want to show the highest (max)
#' # posible value of sleep for any time point
#' pl + stat_tile_etho(method = max)
#' # We can also use stat_bar_tile as an alternative
#' pl + stat_bar_tile_etho()
#' @seealso
#' * [ggetho] to generate a plot object
#' * [stat_pop_etho] to show population trend by aggregating individuals over time
#' * [stat_ld_annotations] to show light and dark phases on the plot
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#tile-plots)
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


StatTileEtho <- ggproto("StatTileEtho",
                           StatBarTileEtho,
                            default_aes = aes(fill = ..value..)
                           )

