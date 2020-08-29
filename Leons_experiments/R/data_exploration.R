library(tidyverse)


# Plot Table Data ---------------------------------------------------------

trees <- sf::read_sf("../../Data/output/crown_hulls_with_data.gpkg")

ggplot(trees) +
  geom_point(aes(x = convex_area, y = max_z), size = 0.25)

ggplot(trees) +
  geom_point(aes(x = terrain_height_max - terrain_height_min, y = max_z))

ggplot(trees) +
  geom_point(aes(x = slope_mean, y = max_z), size = 0.25)

ggplot(trees) +
  geom_hex(aes(x = slope_mean, y = max_z))

ggplot(trees) +
  geom_point(aes(x = slope_mean, y = convex_area), size = 0.25)

ggplot(trees %>% filter(convex_area < 100)) +
  geom_point(
    aes(x = slope_mean, y = convex_area, color = num_points),
    size = 0.25
  )

ggplot(trees) +
  geom_point(
    aes(x = terrain_height_mean, y = convex_area, color = num_points),
    size = 0.25
  )


# Plot Point Data ---------------------------------------------------------

source("R/utility.R")

segmented_points_with_data <- lidR::readLAS(
  "../../Data/output/segmentation/segmented_points_with_data.las"
)

trees <- sf::read_sf("../../Data/output/crown_hulls_with_data.gpkg")

segmented_points_with_data@data <- data.table::merge.data.table(
  x = segmented_points_with_data@data,
  y = data.table::as.data.table(trees) %>% select(crown_id, convex_area),
  by = "crown_id", all.x = TRUE
)

lidR_plot_custom(
  lidR::unnormalize_height(lidR::filter_poi(
    segmented_points_with_data,
    convex_area < 500
  )),
  color = "convex_area", legend = TRUE
)


# Plot the Crown Hulls --------------------------------------------------

trees <- sf::read_sf("../../Data/output/crown_hulls_with_data.gpkg")

# Plotting with ggplot2
ggplot(trees %>% filter(convex_area > 50 & convex_area < 500)) +
  geom_sf(aes(fill = convex_area), size = 0.05) +
  ggspatial::annotation_scale(
    location = "br",
    plot_unit = "m",
    width_hint = 0.5
  )
ggsave("../../Graphics/convex_crowns_convex_area.pdf")

# Plotting with base R graphics
pdf("../../Graphics/convex_crowns_convex_area")
plot(
  trees %>% select(convex_area) %>% filter(convex_area > 50),
  axes = TRUE, lwd = .1
)
dev.off()
