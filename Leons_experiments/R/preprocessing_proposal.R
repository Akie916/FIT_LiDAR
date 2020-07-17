source("R/utility.R")


# Read in Data ------------------------------------------------------------

las_data <- lidR::readLAScatalog(
  list.files("../../Data/laz_files/", pattern = "*CIR.laz", full.names = TRUE)
)

# A nice way of displaying LASCatalogues
lidR::plot(las_data, mapview = TRUE, map.type = "Esri.WorldImagery")

# Clip a subset of the data
sample_tile <- lidR_clip_relative_rectangle(las_data,
  width = 2000,
  x_left = 4000,
  y_bottom = 2000
)


# Remove Noise, Overlapping, and Duplicated Points ------------------------

filtered_tile <- lidR::filter_poi(sample_tile, !(Classification %in% c(7, 12)))
filtered_tile <- lidR::filter_duplicates(filtered_tile)


# Normalize Height Values -------------------------------------------------

normalized_tile <- lidR::normalize_height(filtered_tile,
  algorithm = lidR::tin()
)


# Remove all Points Below and Above Certain Heights -----------------------

without_outliers <- lidR::filter_poi(normalized_tile, 1 < Z & Z < 75)
# I chose 1 m as the lower threshold because I wanted to make sure that small
# trees in the higher places would still be included. I compared the number of
# points that remained after using 1 m as a threshold and after using 2 m as a
# threshold. There was no big difference. I also think that we can keep more
# points because the processing doesn't take that long anyway (about 1 - 2 min
# for 4 km2 on my laptop).
# As for the upper threshold: I chose 75 , because it definitely doesn't cut off
# any tree tops and the outliers that still remain aren't segmented anyway so
# that should hopefully be just fine.


# Homogenize the Density of the Remaining Points --------------------------

homogenized_tile <- lidR::decimate_points(without_outliers,
  algorithm = lidR::homogenize(density = 0.5, res = 4)
)


# Segment Individual Trees ------------------------------------------------

segmented_coordinates <- crownsegmentr::segment_tree_crowns(
  point_cloud = homogenized_tile@data,
  crown_diameter_2_tree_height = 1/2,
  crown_height_2_tree_height = 4/5
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


# Calculate and Merge Data for Analysis -----------------------------------

lidR::merge_spatial(segmented_tile, source = , attribute = "")
lidR::tree_metrics(segmented_tile, attribute = "crown_id")
