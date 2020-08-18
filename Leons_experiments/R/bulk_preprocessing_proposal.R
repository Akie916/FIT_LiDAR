
# Note: If you have enough memory, you should be able to execute this script
# line by line. If you don't have as much, just restart R (keyboard shortcut
# Ctrl+Shift+F10) every time after writing intermediate output data to disk and
# before performing the next processing step. Prior to every calculation for
# which it might make sense to first restart R, I have included calls to
# lidR::readLAScatalog, lidR::readLAS and/or other file loading functions that
# read in the required data. Don't forget to execute all of the code in section
# "Setup" after every restart!

# Note: For some calculations I explicitly set the chunk size to something
# smaller than the size of the original files (I usually use 1000, the original
# file size is 2000). I do this because my 8GB of memory are sometimes not
# enough when my processor tries to work on 4 or 5 chunks simultaneously and
# wants to load all of them into memory. Sometimes the processing is also faster
# with smaller chunks.

# Note: The LASCatalog procedures used below should run faster when using LAS
# files instead of LAZ files and even more so when also using LAX files (see
# vignette("lidR-computation-speed-LAScatalog") for details). I chose to still
# use LAZ files most of the time because LAS files take up way more disk space
# and the processing time is still acceptable for me.


# Setup -------------------------------------------------------------------

# The following packages need to have been installed prior to executing this
# script:
# - lidR
# - crownsegmentr

# Please note that most packages in this script are used with qualified calls
# and therefore not attached with a call to library().
library("glue")   # for glueing strings, especially file paths, together
library("dplyr")  # mainly for investigating and removing duplicated crown IDs

# Parallelization settings
# (This is done with the future package because lidR uses that package for
# parallelization)
future::plan(strategy = future::multisession)

# Output directories
output_directory <- "../../Data/output/"
catalog_output_directory <- glue(output_directory, "catalog_processing/")
segmentation_output_directory <- glue(output_directory, "segmentation/")

# Output file prefixes/names
filtered_points_prefix = "filtered_points"
normalized_points_prefix <- "normalized_points"
edge_clipped_points_name <- "edge_clipped_points"
homogenized_points_prefix <- "homogenized_points"
segmented_points_name <- "segmented_points"
segmented_points_with_data_name <- "segmented_points_with_data"
terrain_height_grid_name <- "terrain_height_grid"
tree_metrics_points_name <- "tree_metrics_points"
convex_crown_hulls_prefix <- "convex_crown_hulls"
crown_hulls_with_data_prefix <- "crown_hulls_with_data"

output_file_pattern <- "x_{XLEFT}_y_{YBOTTOM}"


# Exclude Noise, Overlapping Points and Duplicated Points -----------------

filtered_points <- lidR::readLAScatalog(
  # the documentation of lidR::readLAScatalog says that list.files options can
  # be passed directly, but there is an error when using the pattern parameter
  list.files("../../Data/input_laz_files/",
    pattern = "*CIR\\.laz",
    full.names = TRUE
  ),
  filter = "-drop_class 7 12",
  chunk_size = 1000
)

# lidR::plot(filtered_points, mapview = TRUE, map.type = "Esri.WorldImagery")

# The filter_duplicates procedure requires the creation of output files
lidR::opt_output_files(filtered_points) <- glue(
  catalog_output_directory,
  "{filtered_points_prefix}_{output_file_pattern}"
)

# The default is ".las" but I want to use LAZ files
lidR::opt_laz_compression(filtered_points) <- TRUE

# This takes at most 5 minutes on my machine
filtered_points <- lidR::filter_duplicates(filtered_points)


# Normalize Height Values -------------------------------------------------

filtered_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{filtered_points_prefix}_.*\\.laz"),
    full.names = TRUE
  )
)

# The normalize_height procedure requires the creation of output files
lidR::opt_output_files(filtered_points) <- glue(
  catalog_output_directory,
  "{normalized_points_prefix}_{output_file_pattern}"
)
lidR::opt_laz_compression(filtered_points) <- TRUE

# This took less than 5 minutes on my machine and produced a warning about
# degenerated ground points for each chunk. I think we can ignore these warnings
# but I am not sure. One thing that may be interesting: There seem to be more
# degenerated points in the western half compared to the eastern one.
normalized_points <- lidR::normalize_height(filtered_points,
  algorithm = lidR::tin(),
  add_lasattribute = TRUE
)


# Remove Points Below 1m --------------------------------------------------

# Use lidR::readLASCatalog for this instead of lidR::filter_poi because the
# latter doesn't support catalog processing
above_ground_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{normalized_points_prefix}_.*\\.laz"),
    full.names = TRUE
  ),
  filter = "-drop_z_below 1"
)


# Remove Points with Possible Normalization Edge Effects ------------------

# This procedure does not require the creation of an output file but the
# resulting LAS object is too big to be kept in my limited memory
lidR::opt_output_files(above_ground_points) <- glue(
  catalog_output_directory,
  edge_clipped_points_name
)
lidR::opt_laz_compression(above_ground_points) <- TRUE

edge_width <- 0.5

# This took about a minute on my machine
edge_clipped_points <- lidR::clip_rectangle(above_ground_points,
  xleft = above_ground_points@bbox["x", "min"] + edge_width,
  ybottom = above_ground_points@bbox["y", "min"] + edge_width,
  xright = above_ground_points@bbox["x", "max"] - edge_width,
  ytop = above_ground_points@bbox["y", "max"] - edge_width,
)


# Homogenize Points -------------------------------------------------------

edge_clipped_points <- lidR::readLAScatalog(
  glue(catalog_output_directory, edge_clipped_points_name, ".laz"),
  chunk_size = 2000
)

# The decimate_points procedure requires the creation of output files
lidR::opt_output_files(edge_clipped_points) <- glue(
  catalog_output_directory,
  "{homogenized_points_prefix}_{output_file_pattern}"
)
lidR::opt_laz_compression(edge_clipped_points) <- TRUE

# This takes less than 5 minutes on my machine
homogenized_points <- lidR::decimate_points(edge_clipped_points,
  algorithm = lidR::homogenize(density = 0.5, res = 4)
)


  # Segment Individual Trees ------------------------------------------------

  homogenized_points <- lidR::readLAS(
    list.files(catalog_output_directory,
      pattern = glue("{homogenized_points_prefix}_.*\\.laz"),
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
  segmented_points[, crown_id],
  name = "crown_id",
  desc = "Tree Crown ID"
)

# Remove any unsegmented points
segmented_points <- lidR::filter_poi(segmented_points, crown_id > 0)

lidR::writeLAS(segmented_points,
  file = glue(segmentation_output_directory, segmented_points_name, ".laz")
)

rm(homogenized_points, segmented_points)


# Add Terrain Data to Segmented Points (by Akie) --------------------------

## First Calculate a Terrain Height Grid
filtered_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{filtered_points_prefix}_.*\\.laz"),
    full.names = TRUE
  )
)

# This took a bit less than five minutes on my machine
terrain_height_grid <- lidR::grid_terrain(filtered_points,
  res =  0.5,
  algorithm = lidR::tin()
)
# I'm not sure if we should remove the edges of this raster in order to account
# for the edge effects or if having removed the points at the edge of the point
# cloud was already enough. At least the plot looks good:
# raster::plot(terrain_height_grid).

raster::writeRaster(terrain_height_grid,
  filename = glue(catalog_output_directory, terrain_height_grid_name)
)


## Then Add Grid Data to the Tree Points
segmented_points <- lidR::readLAS(
  glue(segmentation_output_directory, segmented_points_name, ".laz")
)
terrain_height_grid <- raster::raster(
  glue(catalog_output_directory, terrain_height_grid_name)
)

# Calculate the terrain height of each crown point with the Zref attribute that
# was created by the lidR::normalize_height function
segmented_points <- lidR::add_lasattribute(segmented_points,
  segmented_points@data[, Zref] - segmented_points@data[, Z],
  name = "terrain_height",
  desc = "terrain height"
)

# This took about a minute on my machine
slope_and_aspect_grid <- raster::terrain(terrain_height_grid,
  opt = c("slope", "aspect"),
  unit = "degrees",
  neighbors = 8
)

# This took less than five minutes on my machine
segmented_points <- lidR::merge_spatial(segmented_points,
  source = slope_and_aspect_grid[["slope"]],
  attribute = "slope"
)
# When using this function instead of lidR::add_attribute, the attribute is
# retained when a LAS file is written
segmented_points <- lidR::add_lasattribute(segmented_points,
  name = "slope",
  desc = "slope"
)
# TODO Suggest lidR maintainer to add a add_lasattribute parameter to
# merge_spatial like in normalize_height

# This took less than five minutes on my machine
segmented_points <- lidR::merge_spatial(segmented_points,
  source = slope_and_aspect_grid[["aspect"]],
  attribute = "aspect"
)
segmented_points <- lidR::add_lasattribute(segmented_points,
  name = "aspect",
  desc = "aspect"
)

lidR::writeLAS(segmented_points,
  glue(segmentation_output_directory, segmented_points_with_data_name, ".las"),
  index = TRUE
)

rm(segmented_points, terrain_height_grid, slope_and_aspect_grid)


# Calculate Tree Data -----------------------------------------------------

# I tried to combine the crown delineation with the metrics calculation but for
# some reason I just get the error message "could not find function
# "calculate_tree_metrics"". Maybe it's a bug in lidR.

# # This takes about 5 minutes on my machine
# lidR::delineate_crowns(segmented_points_with_data,
#   type = "convex",
#   func = ~calculate_tree_metrics(X, Y, Z, terrain_height, slope, aspect),
#   attribute = "crown_id"
# )


# Calculate Tree Metrics --------------------------------------------------

segmented_points_with_data <- lidR::readLAScatalog(
  glue(segmentation_output_directory, segmented_points_with_data_name, ".las"),
  chunk_size = 2000
)

calculate_tree_metrics <- function(x, y, z, terrain_height, slope, aspect) {

  max_z_index <- which.max(z)

  return(list(
    min_x = min(x),
    max_x = max(x),

    min_y = min(y),
    max_y = max(y),

    min_z = min(z),
    max_z = z[max_z_index],

    x_at_max_z = x[max_z_index],
    y_at_max_z = y[max_z_index],

    num_points = length(x),

    terrain_height_at_max_z = terrain_height[max_z_index],
    terrain_height_max = max(terrain_height),
    terrain_height_median = median(terrain_height),
    terrain_height_mean = mean(terrain_height),
    terrain_height_min = min(terrain_height),

    slope_at_max_z = slope[max_z_index],
    slope_max = max(slope),
    slope_median = median(slope),
    slope_mean = mean(slope),
    slope_min = min(slope),

    aspect_at_max_z = aspect[max_z_index],
    aspect_max = max(aspect),
    aspect_median = median(aspect),
    aspect_mean = mean(aspect),
    aspect_min = min(aspect)
  ))
}

tree_metrics_points <- lidR::tree_metrics(segmented_points_with_data,
  func = ~calculate_tree_metrics(X, Y, Z, terrain_height, slope, aspect),
  attribute = "crown_id"
)

# Convert the points to an sf object because I don't know how to write the data
# to disk directly
tree_metrics_points <- sf::st_as_sf(tree_metrics_points)

sf::write_sf(tree_metrics_points,
 glue(catalog_output_directory, tree_metrics_points_name, ".gpkg")
)

rm(tree_metrics_points)

# plot(
#   dplyr::filter(tree_metrics_points["terrain_height_range"],
#     terrain_height_range < 30
#   ),
#   axes = TRUE,
#   cex = .25,
#   pch = 16
# )


# Calculate Convex 2D (XY) Crown Hull Data ---------------------------

segmented_points_with_data <- lidR::readLAScatalog(
  glue(segmentation_output_directory, segmented_points_with_data_name, ".las"),
  chunk_size = 1000
)

lidR::opt_output_files(segmented_points_with_data) <- glue(
  catalog_output_directory,
  "{convex_crown_hulls_prefix}_{output_file_pattern}"
)
segmented_points_with_data@output_options[[
  "drivers"]][["Spatial"]][["extension"]] <- ".gpkg"

# This takes about 5 minutes on my machine
lidR::delineate_crowns(segmented_points_with_data,
  type = "convex",
  attribute = "crown_id"
)
# TODO ask for documentation of default argument to type parameter being convex

# Merge the partial crown hull files
convex_crown_hulls <- dplyr::bind_rows(lapply(
  list.files(catalog_output_directory,
    pattern = glue("{convex_crown_hulls_prefix}_.*\\.gpkg"),
    full.names = TRUE
  ),
  sf::read_sf
))

# Calculate the crown area
convex_crown_hulls <- convex_crown_hulls %>%
  mutate(convex_area = sf::st_area(convex_crown_hulls))

# Write the output file
sf::write_sf(convex_crown_hulls,
  glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
)

# Remove the partial files
file.remove(
  list.files(catalog_output_directory,
    pattern = glue("{convex_crown_hulls_prefix}_.*"),
    full.names = TRUE
  )
)

# I was trying to also calculate bounding boxes until I noticed that calculating
# the min and max of x and y with the tree metrics should be just as good. I am
# leaving the code that I used for documentary purposes.

# convex_crown_hulls <- sf::read_sf(
#   glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
# )
#
# # Calculate bounding boxes
# # This takes about half a minute on my machine
# bboxes <- lapply(sf::st_geometry(convex_crown_hulls),
#   FUN = sf::st_bbox
# )
#
# bboxes <- data.table::data.table(
#   x_min = sapply(bboxes, function(bbox) bbox["xmin"]),
#   x_max = sapply(bboxes, function(bbox) bbox["xmax"]),
#   y_min = sapply(bboxes, function(bbox) bbox["ymin"]),
#   y_max = sapply(bboxes, function(bbox) bbox["ymax"])
# )
#
# convex_crown_hulls <- convex_crown_hulls %>%
#   mutate(
#     x_extent = bboxes[, x_max] - bboxes[, x_min],
#     y_extent = bboxes[, y_max] - bboxes[, y_min]
#   )
#
# # Write the output file
# sf::write_sf(convex_crown_hulls,
#   glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
# )


# Also calculate concave hulls

# Well, at least I tried with the default settings but it looks like this would
# take a few hours so I think that we can omit concave tree hulls. Especially
# because our trees often don't consist of many points which might lead to very
# thin concave hulls?


# Deal with Duplicated Crown IDs ------------------------------------------

segmented_points <- lidR::readLAS(
  glue(segmentation_output_directory, segmented_points_name, ".laz")
)

tree_metrics_points <- sf::read_sf(
  glue(catalog_output_directory, tree_metrics_points_name, ".gpkg")
)

convex_crown_hulls <- sf::read_sf(
  glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
)

# Get the unique IDs returned by the segmentation
unique_segmentation_ids <- unique(segmented_points@data[, crown_id])
# Get the unique IDs in the tree metrics data
unique_tree_metrics_ids <- tree_metrics_points %>%
  as_tibble() %>% # drop the geometry column of the sf object
  select(crown_id) %>%
  unique() %>%
  pull() # convert the resulting column to a vector
# Get the unique IDs of the crown hulls
unique_crown_hull_ids <- convex_crown_hulls %>%
  as_tibble() %>%
  select(crown_id) %>%
  unique() %>%
  pull()

# Compare the ID vectors
# First check whether they have the same length
length(unique_segmentation_ids) == length(unique_tree_metrics_ids)
#> TRUE
length(unique_segmentation_ids) == length(unique_crown_hull_ids)
#> TRUE

# If they do, check whether they contain the same IDs
all(sort(unique_segmentation_ids) == sort(unique_tree_metrics_ids))
#> TRUE
all(sort(unique_segmentation_ids) == sort(unique_crown_hull_ids))
#> TRUE

# -> Both the tree metrics and the crown hulls feature all of the IDs that
#    appear in the segmented points but since they have more rows than unique
#    IDs, some IDs have to have been duplicated

## First deal with the tree metrics data
# Identify the duplicated IDs
duplicated_tree_metrics_ids <- tree_metrics_points %>%
  as_tibble() %>%
  select(crown_id) %>%
  filter(duplicated(crown_id)) %>%
  pull()

# Have a look at all rows with duplicated IDs
tree_metrics_points %>%
  filter(crown_id %in% duplicated_tree_metrics_ids) %>%
  arrange(crown_id)

# Check whether the duplicated rows are identical
tree_metrics_points %>%
  filter(crown_id %in% duplicated_tree_metrics_ids) %>%
  unique() %>%
  arrange(crown_id)
# unique() just "tests for identity of character representations" (according to
# the documentation) but that should be enough for our purposes

# -> The rows with the same IDs are identical
# -> They are all located at the border of a chunk

# Remove the duplicated rows in the tree metrics data
tree_metrics_points <- tree_metrics_points %>%
  filter(!duplicated(crown_id))

## Now for the crown hull data
# Identify duplicated IDs
duplicated_crown_hull_ids <- convex_crown_hulls %>%
  as_tibble() %>%
  select(crown_id) %>%
  filter(duplicated(crown_id)) %>%
  pull()

# Have a look at all rows with duplicated IDs
convex_crown_hulls %>%
  filter(crown_id %in% duplicated_crown_hull_ids) %>%
  arrange(crown_id)

# -> It's similar to the tree metrics in that every ID appears exactly twice and
#    the tree top coordinate is located at a chunk edge
# -> This issue seems to be related to the chunk edges because the duplicated
#    crowns in the tree metrics data also appear in the crown hull data and only
#    at chunk edges whose coordinates ends in an even thousands number. The
#    duplicated crowns that only appear in the crown hull data all have
#    coordinates where one value ends in an odd thousands number.
#    Also, when the tree metrics are also calculated with 1000m chunks they
#    feature the same duplicated crowns as the crown hull data.
#    TODO test if this is a reproducible bug and if yes, create an issue on the
#    lidR GitHub

# Check whether the duplicated rows are identical
convex_crown_hulls %>%
  filter(crown_id %in% duplicated_crown_hull_ids) %>%
  unique() %>%
  arrange(crown_id)
# -> they are

# Remove duplicated rows
convex_crown_hulls <- convex_crown_hulls %>%
  filter(!duplicated(crown_id))

rm(segmented_points)

sf::write_sf(tree_metrics_points,
  glue(catalog_output_directory, tree_metrics_points_name, ".gpkg")
)

sf::write_sf(convex_crown_hulls,
  glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
)


# Merge Crown Hulls with Metrics ------------------------------------------

convex_crown_hulls <- sf::read_sf(
  glue(catalog_output_directory, convex_crown_hulls_prefix, ".gpkg")
)

tree_metrics_points <- sf::read_sf(
  glue(catalog_output_directory, tree_metrics_points_name, ".gpkg")
)

crown_hulls_with_data <- convex_crown_hulls %>%
  select(crown_id, convex_area) %>%
  left_join(
    tree_metrics_points %>% as_tibble() %>% select(-geom),
    by = "crown_id"
  )

sf::write_sf(crown_hulls_with_data,
  glue(output_directory, crown_hulls_with_data_prefix, ".gpkg")
)

rm(convex_crown_hulls, tree_metrics_points)


# [Plot the Crown Hulls] --------------------------------------------------

crown_hulls_with_data <- sf::read_sf(
  glue(output_directory, crown_hulls_with_data_prefix, ".gpkg")
)

# I am saving this plot to a pdf file because this allows zooming into the plot
# This takes about a minute on my machine
pdf("../../Graphics/convex_crowns")
plot(crown_hulls_with_data["max_z"], axes = TRUE, lwd = .1)
dev.off()


# [Plot the Segmented Point Coud] -----------------------------------------

source("R/utility.R")

segmented_points_with_data <- lidR::readLAS(
  glue(segmentation_output_directory, segmented_points_with_data_name, ".laz")
)

lidR_plot_custom(segmented_points_with_data,
  color = "crown_id",
  colorPalette = random_crown_colors(
    segmented_points_with_data@data[, "crown_id"]
  )
)


# [Bulk Processing with LASTools] -----------------------------------------

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