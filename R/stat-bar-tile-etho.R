#' @rdname stat_tile_etho
#' @export
stat_bar_tile_etho <- function(mapping = NULL, data = NULL,
                               geom = "bar_tile",
                               position = "identity",
                               ...,
                               method = mean,
                               method.args = list(),
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBarTileEtho,
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


StatBarTileEtho <- ggproto("StatBarTileEtho", Stat,
                           default_aes = aes(height = ..value..),
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

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

GeomBarTile <- ggproto("GeomBarTile", GeomRect,
                       extra_params = c("na.rm","width", "fill"),
                       required_aes = c("x", "y", "height"),
                       default_aes = aes(height = ..value..,
                                         fill = "grey20",
                                         colour = NA,
                                         size = 0.1, linetype = 1,
                                         alpha = NA),
                       setup_data = function(data, params) {
                         data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                         data$fill <-  params$fill %||% data$fill
                         data$z_rel <- data$height / max(data$height)
                         transform(data,
                                   xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                   ymin = y - 1 / 2,
                                   ymax = y - 1/2 + z_rel
                         )
                       },
                       draw_key = draw_key_polygon
                       # draw_panel = function(self, data, panel_params, coord, width = NULL) {
                       #   # Hack to ensure that width is detected as a parameter
                       #   ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
                       # }
)


geom_bar_tile <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          fill="grey20") {

  par <-list(
    na.rm = na.rm,
    fill=fill,
    ...
  )
  par <- par [sapply(par, function(x)!is.null(x))]
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = par
  )
}

