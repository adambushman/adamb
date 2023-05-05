#' Main color object
#'
#' `main_color_obj()` supplies a color object responsible for styling the
#' `theme_adamb()` ggplot theme layer when no other object is passed thereto.
#'
#' @seealso [theme_adamb()]
#' @returns A list, or color object, featuring four named elements
#' ("core', "background", "text", "lines")
#' @examples
#' main_color_obj()
#' @export
main_color_obj <- function() {
  list(
    core = "#708090",
    background = "#F5F5F5",
    text = "#2A3439",
    lines = "#D9D9D9"
  )
}

#' Adam B ggplot2 theme
#'
#' `theme_adamb()` applies styling to ggplot objects per the preferences of
#' Adam B
#'
#' @param color_obj A list, featuring at least four elements, each
#' containing a hexadecimal color value: "core", "background", "text",
#' and "lines". May be ignored in which case the main color object will
#' be used.
#' @seealso [main_color_obj()]
#' @returns A theme element for laying onto a ggplot object.
#' @examples
#' theme_adamb()
#'
#' # Optionally, pass a color object, featuring at least four named elements
#' # ("core', "background", "text", "lines"), each featuring a hexadecimal color
#' # value:
#' theme_adamb(list(core = "#1b7e64", background = "#e4f1ed", text = "#24423a", lines = "#c7d1ce"))
#' @import ggplot2
#' @export
theme_adamb <- function(color_obj = NULL) {
  if(is.null(color_obj)) {
    color_obj = main_color_obj()
  }

  '%+replace%' <- ggplot2::'%+replace%'

  ggplot2::theme_minimal() %+replace%

    ggplot2::theme(
      # Grid elements
      plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"),
      panel.grid = ggplot2::element_line(color = color_obj$lines),
      panel.grid.major = ggplot2::element_line(linewidth = 0.5),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.5),

      # Text elements
      text = ggplot2::element_text(color = color_obj$text),
      plot.title = ggplot2::element_text(size = 20, vjust = 4, hjust = 0, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 13, vjust = 4, hjust = 0, face = "italic"),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0.1, vjust = -5, face = "italic"),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(color = color_obj$text, size = 10),
      strip.text = ggplot2::element_text(
        color = color_obj$text, size = 10, face = "bold",
        margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm")
      ),

      # Color elements
      plot.background = ggplot2::element_rect(fill = color_obj$background, color = NA),
      strip.background = ggplot2::element_rect(fill = color_obj$lines, color = NA),

      # Axis elements
      axis.title = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(vjust = -5),
      axis.text.x = ggplot2::element_text(vjust = -1)
    )
}

#' HSL color to HEX color
#'
#' `hsl_to_hex()` turns a set of "hue", "saturation", and "lightness" values to
#' its equivalent hexadecimal code value.
#'
#' @param h An integer between 0 and 360, representing the color's "hue"
#' @param s An integer between 0 and 100, representing the color's "saturation"
#' @param l An integer between 0 and 100, representing the color's "lightness"
#' @returns A string representing the hexadecimal color code, prepended with a "#"
#' @examples
#' hsl_to_hex(115, 80, 50)
#' @export
hsl_to_hex = function(h, s, l) {
  check_val <- function(val) {
    val >= 0 & val <= 360
  }

  if(!is.numeric(h) | !is.numeric(s) | !is.numeric(l)) {
    stop("Parameters 'h', 's', and 'l' must be integers between 0 and 360")
  }

  if(!(h >= 0 & h <= 360)) {
    stop("Parameters 'h' must be an integers between 0 and 360")
  }

  if(!(s >= 0 & s <= 100) | !(l >= 0 & l <= 100)) {
    stop("Parameters 'h' must be an integers between 0 and 360")
  }

  s = as.integer(s) / 100
  l = as.integer(l) / 100
  h = as.integer(h)

  c = (1 - abs(2 * l - 1)) * s
  x = c * (1 - abs((h / 60) %% 2 - 1))
  m = l - c / 2
  r = 0
  g = 0
  b = 0

  if (0 <= h & h < 60) {
    r = c
    g = x
    b = 0
  } else if (60 <= h & h < 120) {
    r = x
    g = c
    b = 0
  } else if (120 <= h & h < 180) {
    r = 0
    g = c
    b = x
  } else if (180 <= h & h < 240) {
    r = 0
    g = x
    b = c
  } else if (240 <= h & h < 300) {
    r = x
    g = 0
    b = c
  } else if (300 <= h & h < 360) {
    r = c
    g = 0
    b = x
  }

  # Having obtained RGB, convert channels to hex
  r = sprintf("%02x", round((r + m) * 255))
  g = sprintf("%02x", round((g + m) * 255))
  b = sprintf("%02x", round((b + m) * 255))

  return(paste("#", r, g, b, sep = ""))
}
