#' Clip a rectangular area in a \code{\link[lidR]{LAS}} object
#'
#' This function differs from \code{\link[lidR]{lasclipRectangle}} in that it
#' allows the user to specify the rectangle relative to the bottom left corner
#' of the provided las data set.
las_clip_relative_rectangle <- function(LAS,
                                        width,
                                        height = width,
                                        x_left = 0,
                                        y_bottom = 0) {
  return(lidR::clip_rectangle(
    LAS,
    xleft =   LAS@bbox["x", "min"] + x_left,
    ybottom = LAS@bbox["y", "min"] + y_bottom,
    xright =  LAS@bbox["x", "min"] + x_left + width,
    ytop =    LAS@bbox["y", "min"] + y_bottom + height,
  ))
}

#' Plot a las data set with axes and a bigger default point size
#'
#' This function merely provides custom defaults for some arguments to the
#' \code{\link[lidR]{plot}} function.
#'
#' @param ... Arguments passed on to lidR::plot
plot_las <- function(LAS, axis = TRUE, size = 3, ...) {
  lidR::plot(LAS, axis = axis, size = size, ...)
}

#' Generate random colors for a set of IDs
#'
#' Generates n randomly shuffled colors, where n is the highest ID. One color
#' value that corresponds to the white color is attached to the beginning of
#' these shuffled colors. This happens because it is assumed that there is one
#' ID that is lesser than or equal to zero and indicates some kind of "invalid"
#' element.
#'
#' @section TODO:
#'   Use the number of unique IDs greater than a "maximum invalid ID" instead of
#'   the highest ID for the number of to-be-generated colors.
#'
#' @param crown_ids Numeric Vector. A vector of numeric IDs. The highest ID is
#'   used to determine the number of returned colors. See the details for more
#'   information.
#' @param use_brewer_palette If TRUE, picks random colors from those returned by
#'   a call to \code{\link[RColorBrewer]{brewer.pal}} with the palette "Paired".
#'   Otherwise the colors from a coll to \code{\link[grDevices]{hcl.colors}}
#'   with palette "Berlin" are used.
#' @param invalid_color The color that should be used for "invalid" IDs. The
#'   format of the color should be the same as colors returned by the
#'   \code{\link[grDevices]{rgb}} function. The color is prepended to the other
#'   random colors. See the description for more information on this.
#'
random_crown_colors <- function(crown_ids,
                                use_brewer_palette = TRUE,
                                invalid_color = rgb(
                                  255, 255, 255, maxColorValue = 255
                                )) {
  highest_crown_id <- max(crown_ids, na.rm = TRUE)

  if (use_brewer_palette) {
    random_crown_colors <- sample(
      RColorBrewer::brewer.pal(n = 12, name = "Paired"),
      size = highest_crown_id, replace = TRUE
    )
  } else {
    random_crown_colors <-
      sample(hcl.colors(highest_crown_id, palette = "Berlin"), highest_crown_id)
  }

  return(c(invalid_color, random_crown_colors))
}
