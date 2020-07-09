#' Clip a rectangular area in a lidR::LAS object
las_clip_relative_rectangle <- function(LAS,
                                        width, height = width,
                                        x_left = 0, y_bottom = 0) {
  lidR::lasclipRectangle(
    LAS,
    xleft =   LAS@bbox["x", "min"] + x_left,
    ybottom = LAS@bbox["y", "min"] + y_bottom,
    xright =  LAS@bbox["x", "min"] + x_left + width,
    ytop =    LAS@bbox["y", "min"] + y_bottom + height,
  )
}

plot_las <- function(LAS, size = 3, ...) {
  lidR::plot(LAS, axis = TRUE, size = size, ...)
}

random_crown_colors <- function(crown_ids, use_brewer_palette = TRUE) {
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

  c(rgb(255, 255, 255, maxColorValue = 255), random_crown_colors)
}
