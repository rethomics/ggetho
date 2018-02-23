#' Compute and display light/dark annotations onto a plot object
#'
#' This function is used to show light and dark (L and D) phases as boxes on top a plot.
#'
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param ld_colours character vector of length two naming
#' the colours for light and dark phases, respectively.
#' The default is `c("white", "black")`.
#' @param ypos position and height of the annotation on the y axis.
#' It can be either `"top"` of `"bottom"`.
#' The default, `"bottom"` will put the labels below any data.
#' @param height relative height of the rectangles. The default is 3 percent (0.03).
#' @param outline colour of the border of the rectangles. `NA` means no border.
#' @param x_limits numerical vector of length 2 for the start and end of the annotations (in seconds).
#' The default, `c(NA, NA)`, uses the full range of the plotted data.
#' @param period,phase period and phase (in seconds) of the LD cycle.
#' @examples
#' library(behavr)
#' # we start by making a to dataset with 20 animals
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition = c("A","B"))
#' dt <- toy_activity_data(metadata, 3)
#' # We build a plot object
#' pl <-  ggetho(dt, aes(y = asleep)) + stat_pop_etho()
#' pl + stat_ld_annotations()
#' # We can also put the annotations in the background:
#' pl <-  ggetho(dt, aes(y = asleep)) +
#'                  stat_ld_annotations(outline = NA) +
#'                  stat_pop_etho()
#' pl
#' # different colours (e.g. DD)
#' pl + stat_ld_annotations(ld_colour = c("grey", "black"))
#' # shorter period
#' pl + stat_ld_annotations(period = hours(22), phase = hours(3))
#' # on a tile plot:
#' pl <-  ggetho(dt, aes(z = asleep)) + stat_tile_etho()
#' pl + stat_ld_annotations()
#' @seealso
#' * [ggetho] to generate a plot object
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#ld-annotations)
#' @export
stat_ld_annotations <- function (mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 ld_colours = c("white", "black"),
                                 ypos = "bottom",
                                 height = 0.03,
                                 period = hours(24),
                                 phase = 0,
                                 outline = "black",
                                 x_limits = c(NA, NA),
                                 ...,
                                 na.rm = FALSE,
                                 show.legend = FALSE,
                                 inherit.aes = TRUE)
{
  layer(data = data, mapping = mapping, stat = StatLDAnnotation,
        geom = GeomLD,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ld_colours=ld_colours, ypos=ypos,height=height,
                      phase=phase, period=period,ld_boxes=NULL, outline=outline,x_limits =x_limits, ...))
}

StatLDAnnotation <- ggplot2::ggproto("StatLDannotation", ggplot2::Stat,
                            default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,
                                              alpha = .67),
                            setup_params = function(data, params){
                              if(length(params$x_limits) != 2 ){
                                stop("`x_limits` should be of length 2")
                              }


                              default_limits <- c(min(data$x), max(data$x))
                              x <- ifelse(is.na(params$x_limits), default_limits, params$x_limits)


                              if(diff(x) <=0 ){
                                stop("x limits are not in order: `x_limits[1] >= x_limits[2]`")
                              }

                              out <- ldAnnotation(x, params$period, params$phase)
                              params$ld_boxes <-out
                              params
                            },

                            compute_group = function(data, scales,ld_colours, ld_boxes,ypos,
                                                     height,phase,period, outline, x_limits, ...) {
                              ld_boxes
                            },

                            finish_layer = function(data, params) {
                              data$fill <- params$ld_colours[(data$ld=="L")+1]
                              data$colour=params$outline
                              data
                            },
                            required_aes = c("x"),
                            draw_key = ggplot2::draw_key_polygon
)

ldAnnotation <- function(x, period=1, phase=0){
  if(!(abs(phase) <= period))
    stop("Phase should be lower or equal to period!")
  left <- min(x)
  right <- max(x)
  p2 <- period/2
  box_pos <- p2 * floor(seq(from=left-p2, to=right+p2, by=p2) /p2) + phase %%period
  ld <- ifelse(((box_pos - phase) %% period)/p2, "L","D")
  out <- data.table::data.table(ld=ld, xmin=box_pos, xmax=box_pos + p2)
  out <- out[left < xmax & right >xmin]
  out[1, xmin := left]
  out[.N, xmax := right]
  out
}

geom_ld <- function(mapping = NULL, data = NULL,
                    stat = "identity", position = "identity",
                    ...,
                    sides = "bl",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLD,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )
}



GeomLD <- ggproto("GeomLD", Geom,
                  required_aes = c("xmin", "xmax"),

                  draw_panel = function(data, panel_params, coord, ypos = "bottom", height = 0.03) {

                    ymin <-ifelse(ypos == "top", 1, 0)
                    ymax <-ifelse(ypos == "top",  ymin - height,  ymin + height)
                    if (!coord$is_linear()) {
                      data$ymin <- unit(ymin, "npc")
                      data$ymax <-  unit(ymax, "npc")
                      aesthetics <- setdiff(
                        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                      )

                      polys <- plyr::alply(data, 1, function(row) {
                        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                        aes <- as.data.frame(row[aesthetics],
                                             stringsAsFactors = FALSE)[rep(1,5), ]

                        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
                      })

                      ggname("bar", do.call(grid::grobTree, polys))
                    } else {

                      coords <- coord$transform(data, panel_params)

                      coords$ymin <- unit(ymin, "npc")
                      coords$ymax <-  unit(ymax, "npc")

                      ggname("geom_rect", grid::rectGrob(
                        coords$xmin, coords$ymax,
                        width = coords$xmax - coords$xmin,
                        height = coords$ymax - coords$ymin,
                        default.units = "native",
                        just = c("left", "top"),
                        gp = grid::gpar(
                          col = coords$colour,
                          fill = alpha(coords$fill, coords$alpha),
                          lwd = coords$size * .pt,
                          lty = coords$linetype,
                          lineend = "butt"
                        )
                      ))
                    }
                  },

                  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

                  draw_key = draw_key_path
)


ggname <- function (prefix, grob){
  grob$name <- grid::grobName(grob, prefix)
  grob
}
rect_to_poly <- function (xmin, xmax, ymin, ymax){
  data.frame(y = c(ymax, ymax, ymin, ymin, ymax),
             x = c(xmin, xmax, xmax, xmin, xmin))
}
