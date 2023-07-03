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
#' `theme_adamb_cam()` applies styling to ggplot objects per the preferences of
#' Adam B. It includes size adjustments for various elements and should be used
#' in conjuction with a `camcorder` recording.
#'
#' @param color_obj A list, featuring at least four elements, each
#' containing a hexadecimal color value: "core", "background", "text",
#' and "lines". May be ignored in which case the main color object will
#' be used.
#' @seealso [main_color_obj()]
#' @returns A theme element for laying onto a ggplot object.
#' @examples
#' # Make sure to start a `camcorder` recording
#' theme_adamb_cam()
#'
#' # Optionally, pass a color object, featuring at least four named elements
#' # "core', "background", "text", "lines", each featuring a hexadecimal color
#' # value:
#' theme_adamb_cam(list(core = "#1b7e64", background = "#e4f1ed", text = "#24423a", lines = "#c7d1ce"))
#' @import ggplot2
#' @export
theme_adamb_cam <- function(color_obj = NULL) {
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

#' Adam B ggplot2 theme
#'
#' `theme_adamb()` applies styling to ggplot objects per the preferences of
#' Adam B, It has no size adjustments to it for scaling nicely in the "Plots"
#' tab.
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
#' # "core', "background", "text", "lines", each featuring a hexadecimal color
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
      panel.grid = ggplot2::element_line(color = color_obj$lines),

      # Text elements
      text = ggplot2::element_text(color = color_obj$text),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(face = "italic"),
      plot.caption = ggplot2::element_text(face = "italic"),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(color = color_obj$text),
      strip.text = ggplot2::element_text(
        color = color_obj$text, face = "bold"
      ),

      # Color elements
      plot.background = ggplot2::element_rect(fill = color_obj$background, color = NA),
      strip.background = ggplot2::element_rect(fill = color_obj$lines, color = NA)
    )
}

#' Main color object
#'
#' `start_camcorder()` begins a recording of the "Viewer" tab, saving copies
#' of the rendered plot in the directory of your choosing. It uses defaults
#' for the device, units, and dpi. To change those, use `camcorder::gg_record()`
#' instead of this function. It's designed to be used in conjunction with
#' `theme_adamb_cam()`. Execute `camcorder::gg_stop_recording()` to end the
#' recording.
#'
#' @seealso [theme_adamb_cam()]
#' @param dir A string representing a valid directory on the machine for which
#' the camcorder will save rendered plots
#' @param width An integer for the width in cm of the rendered and saved plot
#' @param height An integer for the height in cm of the rendered and saved plot
#' @returns Nothing
#' @examples
#' # Pass in desired directory and dimensions
#' start_camcorder("/my_directory", 16, 9)
#' @import camcorder
#' @export
start_camcorder <- function(dir = NULL, width = NULL, height = NULL) {
  if(is.null(dir) | is.null(height) | is.null(width)) {
    stop("Please pass valid arguments before executing the function.")
  }

  if(!is.character(dir)) {
    stop("Please enter a valid directory string.")
  }

  if(!is.numeric(height) | !is.numeric(width)) {
    stop("Please enter a valid height and width integer (in cm) for the recording.")
  }

  height = as.integer(height)
  width = as.integer(width)

  tryCatch({
    camcorder::gg_record(
      dir = dir,
      device = "png",
      width = width,
      height = height,
      units = "cm",
      dpi = 300
    )

  }, error = function(e) {
    message("ERROR: ", e)
  }, warning = function(w) {
    message("WARNING: ", w)
  })
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
  if(!is.numeric(h) | !is.numeric(s) | !is.numeric(l)) {
    stop("Parameters 'h', 's', and 'l' must be integers between 0 and 360")
  }

  if(!(h >= 0 & h <= 360)) {
    stop("Parameters 'h' must be an integers between 0 and 360")
  }

  if(!(s >= 0 & s <= 100) | !(l >= 0 & l <= 100)) {
    stop("Parameters 's' and 'l' must be an integers between 0 and 100")
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

#' Make a color object
#'
#' `make_color_obj()` creates a color palette wrapped in a list of named elements from
#' a target hue or color name.
#'
#' @param type The style of palette, either "Light" or "Dark"
#' @param hue An integer between 0 and 360, representing the target palette
#' color's "hue"
#' @param name A valid string representing a color name from `common_colors`
#' @param print Whether to print the palette to the console, TRUE or FALSE
#' @returns A list of named elements making up a color palette
#' @examples
#' # In it's simplest form, specify a type and a default hue or valid color name from `common_colors`:
#' make_color_obj("Light", hue = 10)
#' make_color_obj("Dark", name = "Orange")
#'
#' # Additionally, you can opt to print the HSL values inspiring the palette to the console:
#' make_color_obj("Light", name = "Blue", print = TRUE)
#' @export
make_color_obj <- function(type = c("Light", "Dark"), hue = NULL, name = NULL, print = FALSE) {

  type <- match.arg(type)

  if(is.null(hue) & is.null(name)) {
    stop("Enter a valid 'hue' value (integer 0 to 360) or a valid 'name' value from `common_colors`.")
  }

  if(!is.null(hue) & !is.null(name)) {
    stop("Pass either a 'hue' or 'name' value, not both.")
  }

  common = adamb::common_colors

  if(!is.null(hue)) {
    if(!(is.numeric(hue) & hue >= 0 & hue <= 360)) {
      stop("Parameter 'hue' must be an integer between 0 and 360")
    }
  }

  if(!is.null(name)) {
    if(!(is.character(name) & name %in% common$color)) {
      stop("Parameter 'name' must be a string matching valid color names. Check `common_colors` for possible values.")
    }
    hue = common$hue[common$color == name] + (15 * stats::runif(1, -1, 1))
    hue = ifelse(hue < 0, 360 + hue, hue)
  }

  if(!(print %in% c(TRUE, FALSE))) {
    stop("Parameter 'print' should be TRUE or FALSE.")
  }

  hue = as.integer(hue)

  # Core Color
  if(type == "Light") {
    core = round(c(hue, 65, 30))
  } else {
    core = round(c(hue, 65, 70))
  }

  # Background
  if(type == "Light") {
    back = round(c(hue, 30, 92))
  } else {
    back = round(c(hue, 30, 20))
  }

  # Text
  if(type == "Light") {
    txt = round(c(hue, 30, 20))
  } else {
    txt = round(c(hue, 30, 92))
  }

  # Lines
  if(type == "Light") {
    lyns = round(c(hue, 10, 80))
  } else {
    lyns = round(c(hue, 10, 35))
  }

  if(print) {
    print(paste("Core HSL: ", core[1], ", ", core[2], ", ", core[3], sep = ""))
  }

  pal = list(
    core = hsl_to_hex(core[1], core[2], core[3]),
    background = hsl_to_hex(back[1], back[2], back[3]),
    text = hsl_to_hex(txt[1], txt[2], txt[3]),
    lines = hsl_to_hex(lyns[1], lyns[2], lyns[3])
  )
}


#' Make a color object
#'
#' `contrast_text_color()` finds the best black/white text contrast for a
#' specified color background.
#'
#' @param hex_code The target color background
#' @returns A hexadecimal color code best for contrast with the specified color
#' background.
#' @examples
#' # White background returns black
#' contrast_text_color("#FFFFFF")
#'
#' # Dark red background returns white
#' contrast_text_color("#AC192D")
#' @export
contrast_text_color <- function(hex_code) {
  # Remove the "#" symbol if present
  hex_code <- gsub("#", "", hex_code)

  # Convert hexadecimal to decimal
  r <- strtoi(substr(hex_code, 1, 2), base = 16) / 255
  g <- strtoi(substr(hex_code, 3, 4), base = 16) / 255
  b <- strtoi(substr(hex_code, 5, 6), base = 16) / 255

  # Linear RGB
  r = ifelse(r > 0.03928, ((r + 0.055) / 1.055)^2.4, r / 12.92)
  g = ifelse(g > 0.03928, ((g + 0.055) / 1.055)^2.4, g / 12.92)
  b = ifelse(b > 0.03928, ((b + 0.055) / 1.055)^2.4, b / 12.92)

  # Calculate relative luminance using the sRGB color space formula
  luminance <- (0.2126 * r + 0.7152 * g + 0.0722 * b)

  # Check contrast against black (luminance: 0)
  contrast_black <- (luminance + 0.05) / (0 + 0.05)

  # Check contrast against white (luminance: 1)
  contrast_white <- (1 + 0.05) / (luminance + 0.05)

  # Determine the best contrasting text color
  if (contrast_black > contrast_white) {
    text_color <- "#000000"  # Black
  } else {
    text_color <- "#FFFFFF"  # White
  }

  return(text_color)
}
