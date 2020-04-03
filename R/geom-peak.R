#' Visualise peaks in a power spectrum or periodogram
#'
#' This function draws points on the x-y coordinates of selected peaks and write their (y) value on the bottom of the plot.
#'
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param peak_rank numerical vector specifying the rank(s) of peak(s) to draw
#' @param conversion function to convert values of `x` to a specific unit.
#' The default, `hours`, will write x (time) in decimal hours.
#' @details
#' In the input data, peaks are encoded as an additional column/aesthetic with values
#' corresponding to peak ranks (and `0` when the point is not a peak).
#' In other word, the mapping must provide `x`, `y` and `peak`.
#' Only peaks matching `peak_rank` will be drawn (see example).
#' @return A ggplot layer.
#' @examples
#' # We make a data frame by hand with five rows
#' # There are two peaks: in position 4 and 2
#'
#' df <- data.frame(x = hours(1:5),
#'                  y = c(1, 2, 0, 4, 1),
#'                  peak = c(0, 2, 0, 1, 0))
#'#  We draw the plot as a line
#' pl <-  ggplot(df, aes(x, y, peak = peak)) +
#'                   geom_line() +
#'                   scale_x_hours()
#' pl
#' # Now we could add the peak values as an extra layer:
#' # The first peak
#' pl + geom_peak()
#' # The first ans second peak
#' pl + geom_peak(peak_rank = 1:2)
#' # The second only
#' pl + geom_peak(peak_rank = 2)
#'
#' # Just like with other geoms,
#' # we can change colour, size, alpha, shape, ... :
#' pl + geom_peak(colour = "red", size = 10, alpha = .5, shape = 20)
#'
#' ## In the context of circadian analysis,
#' # Using the zeitgebr package:
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
#' per_dt <- find_peaks(per_dt)
#' out <- ggperio(per_dt, aes(y = power - signif_threshold, colour = condition, peak = peak)) +
#'                     stat_pop_etho() +
#'                     facet_wrap( ~ id, labeller = id_labeller)
#' out
#' out + geom_peak(colour="black")
#' }
#' @seealso
#' * [ggperio] to create a periodogram
#' * [zeitgebr::find_peaks] to automatically add a `peak` column on periodogram data
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#periodograms)
#' @export
geom_peak <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      peak_rank = 1,
                      conversion = hours) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPeak,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      peak_rank = peak_rank,
      conversion = conversion,
      ...
    )
  )
}


GeomPeak <- ggproto("GeomPeak", GeomPoint,
                    required_aes = c("x", "y", "peak"),
                    non_missing_aes = c("size", "shape", "colour"),
                    default_aes = aes(
                      shape = 4, colour = "blue", size = 3, fill = NA,
                      alpha = NA, stroke = 0.5,  angle = 0, hjust = .5,
                      vjust = 0, family = "", fontface = 1, lineheight = 1.2
                    ),

                    draw_panel = function(data, panel_params, coord, na.rm = FALSE, peak_rank = 1, conversion=hours) {
                      data <- data[data$peak %in% peak_rank, ]
                      if(nrow(data) < 1)
                        return(NULL)
                      coords <- coord$transform(data, panel_params)
                      label <- sprintf( "%.2f",data$x / conversion(1))
                      #ggplot2:::ggname("geom_point",
                      grid::gList(
                        grid::pointsGrob(
                          coords$x, coords$y,
                          pch = coords$shape,
                          gp = grid::gpar(
                            col = alpha(coords$colour, coords$alpha),
                            fill = alpha(coords$fill, coords$alpha),
                            # Stroke is added around the outside of the point
                            fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                            lwd = coords$stroke * .stroke / 2
                          )
                        ),
                        grid::textGrob(
                          label,
                          coords$x,
                          unit(0.05, "npc"),
                          hjust = data$hjust, vjust = data$vjust,
                          rot = data$angle,
                          gp = grid::gpar(
                            col = alpha(data$colour, data$alpha),
                            fontsize = data$size * .pt,
                            fontfamily = data$family,
                            fontface = data$fontface,
                            lineheight = data$lineheight
                          )
                        )
                      )
                    },

                    draw_key = draw_key_point
)
