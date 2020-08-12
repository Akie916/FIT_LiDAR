# Bulk Processing with LASTools -------------------------------------------

# The command line call below demonstrates how LASTools can be used from within
# R. I chose to use the lidR::LASCatalog framework instead, because certain
# tasks can be parallelized and I wanted to see how much can be done without
# using LASTools.

# lastools_bin_dir <- "../../../../Apps/LAStools/bin/"
# input_laz_dir <- "../../Data/input_laz_files/"
# output_dir <- "../../Data/output/dropped_class_7_12/"
#
# system2(
#   command = shQuote(paste0(lastools_bin_dir, "las2las")), # the executable
#   args = c(
#     "-drop_class", 7, 12,
#     "-i", shQuote(paste0(input_laz_dir, "*.laz")), # input files
#     "-olaz", "-odir", shQuote(output_dir) # output format and directory
#   )
# )


# The LASCatalog procedures used below should run faster when using LAS files
# instead LAZ files and even more so when also using LAX files. I chose to use
# the LAZ files because the LAS files take up way more disk space and the
# processing time is still acceptable for me.

catalog_output_directory <- "../../Data/output/catalog_processing/"

# Set paralellization parameters ------------------------------------------

future::plan(strategy = future::multisession)


# Create LASCatalog and Exclude Noise and Overlapping Points --------------

filtered_las_data <- lidR::readLAScatalog(
  # the documentation of lidR::readLAScatalog says that list.files options can
  # be passed directly, but there is an error when using the pattern parameter
  list.files("../../Data/input_laz_files/",
    pattern = "*CIR\\.laz",
    full.names = TRUE
  ),
  # I use a smaller chunk size because my machine only has 8 GB of memory
  chunk_size = 1000,
  filter = "-drop_class 7 12"
)

lidR::plot(filtered_las_data, mapview = TRUE, map.type = "Esri.WorldImagery")


# Filter Duplicates -------------------------------------------------------

# This procedure requires the creation of output files
lidR::opt_output_files(filtered_las_data) <-
  paste0(catalog_output_directory, "filtered_x_{XLEFT}_y_{YBOTTOM}")

# Store as LAZ files in order to save disk space
filtered_las_data@output_options$drivers$LAS$extension <- ".laz"

# This takes at most 5 minutes on my machine
filtered_las_data <- lidR::filter_duplicates(filtered_las_data)


# Normalize Height Values -------------------------------------------------

# Read the previously created intermediary files into a new LASCatalog object.
# This allows for repeating processing steps without first having to perform all
# previous processing steps.
filtered_las_data <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = "filtered_.*\\.laz",
    full.names = TRUE
  )
)

# This procedure requires the creation of output files
lidR::opt_output_files(filtered_las_data) <-
  paste0(catalog_output_directory, "normalized_x_{XLEFT}_y_{YBOTTOM}")

filtered_las_data@output_options$drivers$LAS$extension <- ".laz"

# This took less than 5 minutes on my machine and produced a warning about
# degenerated ground points for each chunk. I think we can ignore these warnings
# but I am not sure. One thing that may be interesting: There seem to be more
# degenerated points in the western half than in the eastern one.
normalized_las_data <- lidR::normalize_height(filtered_las_data,
  algorithm = lidR::tin(),
  add_lasattribute = TRUE
)


# Remove Points Below 1m --------------------------------------------------

# Use the lidR::readLASCatalog function for this because the lidR::filter_poi
# function doesn't support catalog processing
no_ground_las_data <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = "normalized_.*\\.laz",
    full.names = TRUE
  ),
  filter = "-drop_z_below 1"
)


# Remove Edge Effects of the Normalization --------------------------------

# This procedure does not require the creation of an output file but the
# resulting LAS object is too big to be kept in my limited memory
lidR::opt_output_files(no_ground_las_data) <-
  paste0(catalog_output_directory, "no_ground_edge_clipped")

no_ground_las_data@output_options$drivers$LAS$extension <- ".laz"

edge_width <- 0.5

# This took about a minute on my machine
no_ground_las_data <- lidR::clip_rectangle(no_ground_las_data,
  xleft = no_ground_las_data@bbox["x", "min"] + edge_width,
  ybottom = no_ground_las_data@bbox["y", "min"] + edge_width,
  xright = no_ground_las_data@bbox["x", "max"] - edge_width,
  ytop = no_ground_las_data@bbox["y", "max"] - edge_width,
)


# Homogenize Remaining Points ---------------------------------------------

no_ground_las_data <- lidR::readLAScatalog(
  paste0(catalog_output_directory, "no_ground_edge_clipped.laz"),
  chunk_size = 2000
)

# This procedure requires the creation of output files
lidR::opt_output_files(no_ground_las_data) <-
  paste0(catalog_output_directory, "homogenized_x_{XLEFT}_y_{YBOTTOM}")

no_ground_las_data@output_options$drivers$LAS$extension <- ".laz"

# This takes less than 5 minutes on my machine
homogenized_las_data <- lidR::decimate_points(no_ground_las_data,
  algorithm = lidR::homogenize(density = 0.5, res = 4)
)


# Segment Individual Trees ------------------------------------------------

homogenized_points <- lidR::readLAS(
  list.files(catalog_output_directory,
    pattern = "homogenized_.*\\.laz",
    full.names = TRUE
  )
)

# This takes somewhere between 5 to 10 minutes on my machine
segmented_points <- crownsegmentr::segment_tree_crowns(
  point_cloud = homogenized_points@data,
  crown_diameter_2_tree_height = 1/2,
  crown_height_2_tree_height = 1
)

segmented_points <- lidR::add_lasattribute(homogenized_points,
  segmented_points$crown_id,
  name = "crown_id",
  desc = "Tree Crown ID"
)

lidR::writeLAS(segmented_points,
  file = "../../Data/output/segmentation/segmented.laz"
)

rm(homogenized_points)


# Calculate and Merge Data for Analysis (by Akie) -------------------------

filtered_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = "filtered_.*\\.laz",
    full.names = TRUE
  )
)

# This took a bit less than five minutes on my machine
terrain_height <- lidR::grid_terrain(filtered_points,
  res =  0.5,
  algorithm = lidR::tin()
)
# I'm not sure if we should remove the edges of this raster in order to account
# for the edge effects or if having removed the points at the edge of the point
# cloud was already enough. At least the plot looks good:
# raster::plot(terrain_height).

raster::writeRaster(terrain_height,
  filename = paste0(catalog_output_directory, "terrain_height")
)


segmented_points <- lidR::readLAS(
  "../../Data/output/segmentation/segmented.laz"
)
terrain_height <- raster::raster(
  paste0(catalog_output_directory, "terrain_height")
)

# Calculate the terrain height of each crow point with the Zref attribute that
# was created by the lidR::normalize_height function
segmented_points <- lidR::add_lasattribute(segmented_points,
  segmented_points@data$Zref - segmented_points@data$Z,
  name = "terrain_height",
  desc = "terrain height"
)

# This took less than five minutes on my machine
slope_and_aspect <- raster::terrain(terrain_height,
  opt = c("slope", "aspect"),
  unit = "degrees",
  neighbors = 8
)

# This took about five minutes on my machine
segmented_points <- lidR::merge_spatial(segmented_points,
  source = slope_and_aspect$slope,
  attribute = "slope"
)
# when using this function instead of lidR::add_attribute, the attribute is
# retained when a LAS file is written
segmented_points <- lidR::add_lasattribute(segmented_points,
  name = "slope",
  desc = "slope"
)

# This took less than five minutes on my machine
segmented_points <- lidR::merge_spatial(segmented_points,
  source = slope_and_aspect$aspect,
  attribute = "aspect"
)
segmented_points <- lidR::add_lasattribute(segmented_points,
  name = "aspect",
  desc = "aspect"
)

lidR::writeLAS(segmented_points,
  "../../Data/output/segmentation/segmented_with_data.laz",
)

