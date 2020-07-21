source("R/utility.R")


# Read in Data ------------------------------------------------------------

las_data <- lidR::readLAScatalog(
  list.files("../../Data/laz_files/", pattern = "*CIR.laz", full.names = TRUE)
)

# A nice way of displaying LASCatalogues
# lidR::plot(las_data, mapview = TRUE, map.type = "Esri.WorldImagery")

# Clip a subset of the data
sample_tile <- lidR_clip_relative_rectangle(las_data,
  width = 2000,
  x_left = 2000,
  y_bottom = 0
)


# Remove Noise, Overlapping, and Duplicated Points ------------------------

filtered_tile <- lidR::filter_poi(sample_tile, !(Classification %in% c(7, 12)))
filtered_tile <- lidR::filter_duplicates(filtered_tile)

remove(sample_tile)


# Normalize Height Values -------------------------------------------------

normalized_tile <- lidR::normalize_height(filtered_tile,
  algorithm = lidR::tin()
)

remove(filtered_tile)


# Remove all Points Below and Above Certain Heights -----------------------

without_outliers <- lidR::filter_poi(normalized_tile, Z > 1)
# I chose 1 m as the lower threshold because I wanted to make sure that small
# trees in the higher places would still be included. I compared the number of
# points that remained after using 1 m as a threshold and after using 2 m as a
# threshold. There was no big difference. I also think that we can keep more
# points because the processing doesn't take that long anyway (about 1 - 2 min
# for 4 km2 on my laptop).

# With one tile I observed outliers at the edge of the tile after normalization.
# If this is something that happens regularly, we should think about removing
# the edges of data sets before segmenting trees. A buffer of 0.5 or 1 meter
# should probably be enough. I did not see many of these outliers, though. Maybe
# around a dozen.

# When processing a tile that reaches into the central crater of la Palma, I
# could also see this phenomenon of stretched/deformed trees, that Akie had
# first observed. For me, such trees seemed to be mainly appearing at the side
# of steep cliffs/ridges. Just excluding points above a certain height might not
# be enough to account for this behaviour, since there might still be some
# deformed trees in the final data set.

# Maybe a different way to deal with deformed trees could be to first leave them
# in the data set, calculate their metrics, like their height, diameter and so
# on, and then exclude these trees based on such metrices. For example, we could
# exclude all trees greater than a certain height, or with a weird height to
# diameter ratio. Maybe we could even include the slope or min and max height of
# the surrounding terrain into this culling process.

edge_width <- 0.5
without_outliers <- lidR_clip_relative_rectangle(without_outliers,
  width = 2000 - 2 * edge_width,
  x_left = edge_width,
  y_bottom = edge_width
)
# lidR_plot_custom(without_outliers)


# Homogenize the Density of the Remaining Points --------------------------

homogenized_tile <- lidR::decimate_points(without_outliers,
  algorithm = lidR::homogenize(density = 0.5, res = 4)
)


# Segment Individual Trees ------------------------------------------------

segmented_coordinates <- crownsegmentr::segment_tree_crowns(
  point_cloud = homogenized_tile@data,
  crown_diameter_2_tree_height = 1/2,
  crown_height_2_tree_height = 1
)

segmented_tile <- lidR::add_attribute(homogenized_tile,
  segmented_coordinates$crown_id,
  name = "crown_id"
)


# Plot the Segmentation ---------------------------------------------------

mountaineous_segmented_tile <- lidR::unnormalize_height(segmented_tile)

lidR_plot_custom(mountaineous_segmented_tile,
  color = "crown_id",
  colorPalette = random_crown_colors(mountaineous_segmented_tile@data$crown_id),
  size = 4
)


# Create a Data Set out of Trees and Ground Points ------------------------

only_trees <- lidR::filter_poi(
  lidR::unnormalize_height(segmented_tile),
  crown_id > 0
)
only_ground <- lidR::filter_poi(
  lidR::unnormalize_height(normalized_tile),
  Classification == 2
)
only_ground <- lidR::add_attribute(only_ground, 0, "crown_id")

trees_plus_ground <- only_trees
trees_plus_ground@data <- rbind(trees_plus_ground@data, only_ground@data)

lidR_plot_custom(trees_plus_ground,
  color = "crown_id",
  colorPalette = random_crown_colors(mountaineous_segmented_tile@data$crown_id),
  size = 6
)


# Calculate and Merge Data for Analysis (by Akie) -------------------------

dtm <- lidR::grid_terrain(only_ground, algorithm = lidR::tin())

trees_w_data <- lidR::merge_spatial(only_trees,
  source = dtm,
  attribute = "terrain_height"
)

trees_w_data <- lidR::merge_spatial(trees_w_data,
  source = raster::terrain(dtm, "slope", "degrees", neighbors = 8),
  attribute = "slope"
)

trees_w_data <- lidR::merge_spatial(trees_w_data,
  source = raster::terrain(dtm, "aspect", "degrees", neighbors = 8),
  attribute = "aspect"
)

lidR_plot_custom(trees_w_data, color = "slope")


# Calculate Tree Metrics --------------------------------------------------

mean_tree_heights <- lidR::tree_metrics(trees_w_data,
  func = ~mean(Z) - mean(terrain_height),
  attribute = "crown_id"
)
names(mean_tree_heights)[2] <- "mean_tree_height"

sp::spplot(mean_tree_heights, zcol = "mean_tree_height")

z_extent_below_trees <- lidR::tree_metrics(trees_w_data,
 func = ~max(Z) - min(Z),
 attribute = "crown_id"
)
names(z_extent_below_trees)[2] <- "z_extent"

sp::spplot(z_extent_below_trees, zcol = "z_extent")
