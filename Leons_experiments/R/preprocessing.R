source("R/utility.R")
library(tidyverse)


# Read in Data ------------------------------------------------------------

# Read a .las file and plot it
sample_tile <- lidR::readLAS(
  "../Data/PNOA_2016_CANAR-LP_212-3184_ORT-CLA-RGB.laz"
)
lidR_plot_custom(sample_tile)
lidR_plot_custom(sample_tile, color = "RGB", size = 2)


# Digital Terrain Model(s) ------------------------------------------------

# Calculate DTM rasters
dtm_tin     <- lidR::grid_terrain(sample_tile, algorithm = lidR::tin())
dtm_knnidw  <- lidR::grid_terrain(sample_tile, algorithm = lidR::knnidw())
# Takes very long:
# dtm_kriging <- lidR::grid_terrain(sample_tile, algorithm = lidR::kriging())

# Plot the DTMs
raster::plot(dtm_tin, main = "tin")
raster::plot(dtm_knnidw, main = "knnidw")
# raster::plot(dtm_kriging, main = "kriging")

# Calculate the difference between the DTMs and plot it
dtm_diff <- dtm_tin - dtm_knnidw
raster::plot(dtm_diff, main = "tin - knnidw")
raster::hist(dtm_diff,
  main = "tin - knnidw",
  xlab = "delta_Z"
)
raster::hist(dtm_diff,
  main = "tin - knnidw",
  xlab = "delta_Z",
  ylim = c(0, 2e5)
)


# Normalization with DTM --------------------------------------------------

# Normalize the .las file and plot it
sample_normalized <- lidR::normalize_height(
  sample_tile, algorithm = lidR::tin()
)
lidR_plot_custom(sample_normalized)
lidR_plot_custom(sample_normalized, color = "RGB")

# Plot a histogram of the .las file's Z values
# (but only use a random subset since plotting all values takes quite long)
set.seed(123)
sample_row_indices <- sample(seq_len(nrow(sample_normalized@data)), size = 1e6)

full_histogram <- ggplot(data = sample_normalized@data[sample_row_indices]) +
    geom_histogram(aes(x = Z), binwidth = 1)
full_histogram

cropped_histogram <- full_histogram +
    coord_cartesian(xlim = c(-5, 40), ylim = c(0, 1e5))
cropped_histogram


# Segment Individual Trees ------------------------------------------------

test_rectangle <- lidR_clip_relative_rectangle(sample_tile, width = 500)
lidR::las_check(test_rectangle)
lidR_plot_custom(test_rectangle, color = "RGB")

test_rectangle_normalized <- lidR::normalize_height(test_rectangle,
  algorithm = lidR::tin()
)
lidR::las_check(test_rectangle_normalized)
lidR_plot_custom(test_rectangle_normalized, color = "RGB")

test_rectangle_wo_ground <- lidR::filter_poi(test_rectangle_normalized,
  Classification != 2
)
lidR::las_check(test_rectangle_wo_ground)
lidR_plot_custom(test_rectangle_wo_ground, color = "RGB")

test_rectangle_above_0 <- lidR::filter_poi(test_rectangle_wo_ground,
  Z >= 0
)
lidR::las_check(test_rectangle_above_0)
lidR_plot_custom(test_rectangle_above_0)

segmented_trees <- crownsegmentr::segment_tree_crowns(
  test_rectangle_wo_ground@data,
  crown_diameter_2_tree_height = 3 / 10,
  crown_height_2_tree_height = 5 / 10,
  return_modes = TRUE
)

segmented_las <- lidR::add_attribute(test_rectangle_wo_ground,
  segmented_trees$crown_id,
  name = "crown_id"
)
lidR_plot_custom(
  segmented_las,
  color = "crown_id",
  colorPalette = random_crown_colors(segmented_trees$crown_id)
)

modes_las <- lidR::LAS(
  segmented_trees[!(is.na(mode_x) | is.na(mode_y) | is.na(mode_z)),
                  .(X = mode_x, Y = mode_y, Z = mode_z, crown_id)])
lidR_plot_custom(segmented_las, color = "crown_id")
