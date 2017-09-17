#' Compute and display light/dark annotations onto a plot object
#'
#' This function is used to show light and dark (L and D) phases as boxes on top a plot.
#'
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param ld_colours character vector of length 2 naming the colours for light and dark phases, respectively.
#' The default is white and black.
#' @param ypos,height The position and height of the annotation on the y axis.
#' The defaults, "auto" will put the labels below any data.
#' @param period,phase period and phase (in seconds) of the LD cycle.
#' @examples
 #' library(behavr)
#' # we start by making a to dataset with 20 animals
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    condition=c("A","B"))
#' dt <- toy_activity_data(metadata,3)
#' # We build a plot object
#' pl <-  ggetho(dt, aes(y=asleep)) + stat_pop_etho()
#' pl + stat_ld_annotations()
#' # different colours (e.g. DD)
#' pl + stat_ld_annotations(ld_colour=c("grey", "black"))
#' # shorter period
#' pl + stat_ld_annotations(period=hours(22), phase=hours(3))
#' # on a tile plot:
#' pl <-  ggetho(dt, aes(z=asleep)) + stat_tile_etho()
#' pl + stat_ld_annotations()
#' @seealso  Useful links:
#' * [ggetho] to generate a plot object
#' * TODO Tutorial for this function
#' @export
stat_ld_annotations <- function (mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 ld_colours = c("white", "black"),
                                 ypos = "auto",
                                 height = "auto",
                                 period = hours(24),
                                 phase = 0,
                                 ...,
                                 na.rm = FALSE,
                                 show.legend = FALSE,
                                 inherit.aes = TRUE)
{
  layer(data = data, mapping = mapping, stat = StatLDAnnotation,
        geom = ggplot2::GeomRect,
        #geom = GeomRect,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ld_colours=ld_colours, ypos=ypos,height=height,
                      phase=phase, period=period,ld_boxes=NULL, ...))
}

StatLDAnnotation <- ggplot2::ggproto("StatLDannotation", ggplot2::Stat,
                            default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,
                                              alpha = .66),
                            setup_params = function(data, params){
                              out <- ldAnnotation(data$x,params$period,params$phase)
                              if(params$ypos != "auto")
                                out[,ypos:=params$ypos]
                              else{
                                range <- max(data$y) - min(data$y)
                                out[,ypos := min(data$y) - range * .05]
                              }
                              if(params$height != "auto")
                                out[,height:=params$height]
                              else{
                                range <- max(data$y) - min(data$y)
                                out[, height :=  range * .02]
                              }
                              out[,ymin:= ypos-height/2]
                              out[,ymax:= ypos+height/2]
                              params$ld_boxes <-out
                              params
                            },

                            compute_group = function(data, scales,ld_colours, ld_boxes,ypos,
                                                     height,phase,period,...) {
                              ld_boxes
                            },

                            finish_layer = function(data, params) {
                              data$fill <- params$ld_colours[(data$ld=="L")+1]
                              data$colour="black"
                              data
                            },
                            required_aes = c("x","y"),
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

