match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  if(utils::packageVersion("ggplot2") <= "2.2.1")
    return(as.list(as.character(mapping)))
  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess"))
    if(is.null(mapping))
      return(NULL)
    if (is.atomic(mapping)) {
      aesthetic
    } else {
      x <- rlang::quo_text(strip_dots(mapping))
      if (length(x) > 1) {
        x <- paste0(x[[1]], "...")
      }
      x
    }
  }
  out <- Map(default_label, names(mapping), mapping)
  #  out[!is.null(out)]
  out[!sapply(out, is.null)]
}

# Strip dots from expressions
strip_dots <- function(expr) {
  if (is.atomic(expr)) {
    expr
  } else if (is.name(expr)) {
    expr_ch <- as.character(expr)
    if (nchar(expr_ch) > 0) {
      as.name(gsub(match_calculated_aes, "\\1", expr_ch))
    } else {
      expr
    }
  } else if (is.call(expr)) {
    if (identical(expr[[1]], quote(stat))) {
      strip_dots(expr[[2]])
    } else {
      expr[-1] <- lapply(expr[-1], strip_dots)
      expr
    }
  } else if (is.pairlist(expr)) {
    # In the unlikely event of an anonymous function
    as.pairlist(lapply(expr, strip_dots))
  } else if (is.list(expr)) {
    # For list of aesthetics
    lapply(expr, strip_dots)
  } else {
    stop("Unknown input:", class(expr)[1])
  }
}

