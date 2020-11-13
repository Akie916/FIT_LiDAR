
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
# use LAZ files in most places because LAS files take up way more disk space and
# the processing time is still acceptable for me.


# Setup -------------------------------------------------------------------

# The following packages need to have been installed prior to executing this
# script:
# - lidR
# - crownsegmentr

# Please note that most packages in this script are used with qualified calls
# and are therefore not attached with a call to library().
library("glue")   # for glueing strings, especially file paths, together
library("dplyr")  # mainly for investigating and removing duplicated crown IDs

# Parallelization settings
# (This is done with the future package because lidR uses that package for
# parallelization)
future::plan(strategy = future::multisession)

# Segmentation settings
cd2th = 0.5  # crown diameter / tree height
ch2th = 1  # crown height / tree height

# Output directories
output_directory <- "../../Data/output/"
catalog_output_directory <- glue(output_directory, "catalog_processing/")
segmentation_output_directory <- glue(
  output_directory, "segmentation/cd2th_{cd2th}_ch2th_{ch2th}/"
)
# replace the dots in decimal numbers with underscores
segmentation_output_directory <- stringr::str_replace_all(
  segmentation_output_directory,
  pattern = "(?<=[:digit:])\\.(?=[:digit:])",
  replacement = "_"
)

# Output file prefixes/names
without_noise_and_duplicates_prefix = "without_noise_and_duplicates"
normalized_points_prefix <- "normalized_points"
canopy_height_grid_prefix <- "canopy_height_grid"
edge_clipped_points_name <- "edge_clipped_points"
homogenized_points_prefix <- "homogenized_points"
edge_clipped_points_with_data_name <- "edge_clipped_points_with_data"
segmented_points_name <- "segmented_points"
unsegmented_points_name <- "unsegmented_points"
segmented_points_with_data_name <- "segmented_points_with_data"
terrain_height_grid_name <- "terrain_height_grid"
ground_point_density_grid_name <- "ground_point_density_grid"
land_use_grid_path <- "../../Data/land_use/Akies_LU_grid.tif"
tree_metrics_points_name <- "tree_metrics_points"
convex_crown_hulls_prefix <- "convex_crown_hulls"
crown_hulls_with_data_prefix <- "crown_hulls_with_data"

output_file_pattern <- "x_{XLEFT}_y_{YBOTTOM}"


# Remove Noise and Duplicated Points --------------------------------------

raw_data <- lidR::readLAScatalog(
  list.files("../../Data/input_laz_files/",
    pattern = "*CIR\\.laz",
    full.names = TRUE
  ),
  filter = "-drop_class 7",
  chunk_size = 1000
)

# The filter_duplicates procedure requires the creation of output files
lidR::opt_output_files(raw_data) <- glue(
  catalog_output_directory,
  "{without_noise_and_duplicates_prefix}_{output_file_pattern}"
)

# The default is ".las" but I want to use LAZ files
lidR::opt_laz_compression(raw_data) <- TRUE

# This takes at most 5 minutes on my machine
no_noise_or_duplicates <- lidR::filter_duplicates(raw_data)


# Calculate Terrain Data --------------------------------------------------

no_noise_or_duplicated_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue(without_noise_and_duplicates_prefix, "_.*\\.laz"),
    full.names = TRUE
  )
)

# This took about 10 minutes on my machine
lidR::grid_terrain(no_noise_or_duplicated_points,
  res =  0.5,
  algorithm = lidR::tin()
) %>% raster::writeRaster(.,
    filename = glue(catalog_output_directory, terrain_height_grid_name),
    overwrite = TRUE
  )

# Keep only the ground points
lidR::opt_filter(no_noise_or_duplicated_points) <- "-keep_class 2"

# This took less than 5 minutes on my machine
ground_point_density <- lidR::grid_density(no_noise_or_duplicated_points,
  res = 5
)

ground_point_density[is.na(ground_point_density)] <- 0

raster::writeRaster(ground_point_density,
  glue(catalog_output_directory, ground_point_density_grid_name),
  overwrite = TRUE
)


# Normalize Height Values -------------------------------------------------

no_noise_or_duplicates <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue(without_noise_and_duplicates_prefix, "_.*\\.laz"),
    full.names = TRUE
  ),
  chunk_size = 1000
)

# The normalize_height procedure requires the creation of output files
lidR::opt_output_files(no_noise_or_duplicates) <- glue(
  catalog_output_directory,
  "{normalized_points_prefix}_{output_file_pattern}"
)
lidR::opt_laz_compression(no_noise_or_duplicates) <- TRUE

# This took less than 5 minutes on my machine and produced a warning about
# degenerated ground points for each chunk. I think we can ignore these warnings
# but I am not sure.
normalized_points <- lidR::normalize_height(no_noise_or_duplicates,
  algorithm = lidR::tin(),
  add_lasattribute = TRUE
)


# Homogenize Pulses -------------------------------------------------------

normalized_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{normalized_points_prefix}_.*\\.laz"),
    full.names = TRUE
  ),
  chunk_size = 2000
)

# The decimate_points procedure requires the creation of output files
lidR::opt_output_files(normalized_points) <- glue(
  catalog_output_directory,
  "{homogenized_points_prefix}_{output_file_pattern}"
)
lidR::opt_laz_compression(normalized_points) <- TRUE

homogenize_pulses <- function(las, bbox) {
  set.seed(1234)
  lidR::retrieve_pulses(las) %>%
    lidR::decimate_points(.,
      lidR::homogenize(density = 0.5, res = sqrt(8), use_pulse = TRUE)
    ) %>%
    lidR::filter_poi(dplyr::near(buffer, 0))
}

# This takes about 8 minutes on my machine
lidR::catalog_sapply(normalized_points,
  FUN = homogenize_pulses,
  .options = list(automerge = TRUE, autoread = TRUE)
)


# Remove Ground Points and Points <= 0m -----------------------------------

# Use lidR::readLASCatalog for this instead of lidR::filter_poi because the
# latter doesn't support catalog processing
homogenized_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{homogenized_points_prefix}_x.*\\.laz"),
    full.names = TRUE
  ),
  filter = "-drop_z_below 0.0001 -drop_class 2"
)
# The z threshold should be sufficiently small since the scale factor of the Z
# values, i.e. their precision is given with 0.001.


# Remove Points with Possible Normalization Edge Effects ------------------

# This procedure does not require the creation of an output file but the
# resulting LAS object is too big to be kept in my limited memory
lidR::opt_output_files(homogenized_points) <- glue(
  catalog_output_directory,
  edge_clipped_points_name
)
lidR::opt_laz_compression(homogenized_points) <- TRUE

edge_width <- 0.5

# This took about a minute on my machine
edge_clipped_points <- lidR::clip_rectangle(homogenized_points,
  xleft = homogenized_points@bbox["x", "min"] + edge_width,
  ybottom = homogenized_points@bbox["y", "min"] + edge_width,
  xright = homogenized_points@bbox["x", "max"] - edge_width,
  ytop = homogenized_points@bbox["y", "max"] - edge_width,
)


# Get Terrain Data at Each Point (by Akie) --------------------------------

edge_clipped_points <- lidR::readLAS(glue(
  catalog_output_directory,
  edge_clipped_points_name, ".laz"
))

ground_point_density <- raster::raster(
  glue(catalog_output_directory, ground_point_density_grid_name)
)
land_use_grid <- raster::raster(land_use_grid_path)

# This took about a minute on my machine
slope_and_aspect_grid <- raster::raster(
  glue(catalog_output_directory, terrain_height_grid_name)
) %>%
  raster::aggregate(., fact = 4) %>%
  raster::terrain(.,
  opt = c("slope", "aspect"),
  unit = "degrees",
  neighbors = 8
)

# All of this took about three minutes on my machine
lidR::add_lasattribute(edge_clipped_points,
  # Calculate the terrain height of each crown point with the Zref attribute
  # that was created by the lidR::normalize_height function
  edge_clipped_points@data[["Zref"]] - edge_clipped_points@data[["Z"]],
  name = "terrain_height",
  desc = "terrain height"
) %>%
  lidR::merge_spatial(.,
    source = slope_and_aspect_grid[["slope"]],
    attribute = "slope"
  ) %>%
  lidR::add_lasattribute(., name = "slope", desc = "slope") %>%
  lidR::merge_spatial(.,
    source = slope_and_aspect_grid[["aspect"]],
    attribute = "aspect"
  ) %>%
  lidR::add_lasattribute(., name = "aspect", desc = "aspect") %>%
  lidR::merge_spatial(.,
    source = ground_point_density,
    attribute = "ground_point_density"
  ) %>%
  lidR::add_lasattribute(.,
    name = "ground_point_density",
    desc = "ground point density"
  ) %>%
  lidR::merge_spatial(.,
    source = land_use_grid,
    attribute = "land_use"
  ) %>%
  lidR::add_lasattribute(., name = "land_use", desc = "land use") %>%
  lidR::writeLAS(.,
    glue(
      catalog_output_directory,
      edge_clipped_points_with_data_name, ".las"
    ),
    index = TRUE
  )

rm(
  edge_clipped_points,
  terrain_height_grid,
  slope_and_aspect_grid,
  land_use_grid
)


# Segment Individual Trees ------------------------------------------------

homogenized_points <- lidR::readLAScatalog(
  glue(catalog_output_directory, edge_clipped_points_with_data_name, ".las"),
  chunk_size = 2000
)

# This takes somewhere between 5 to 10 minutes on my machine
segmented_points <- crownsegmentr::segment_tree_crowns(
  point_cloud = homogenized_points,
  crown_diameter_2_tree_height = cd2th,
  crown_height_2_tree_height = ch2th
)

# Store segmented and unsegmented points separately
segmented_points %>%
  lidR::filter_poi(., !near(crown_id, 0)) %>%
  lidR::writeLAS(.,
    file = glue(segmentation_output_directory, segmented_points_name, ".laz")
  )

segmented_points %>%
  lidR::filter_poi(., near(crown_id, 0)) %>%
  lidR::writeLAS(.,
    file = glue(segmentation_output_directory, unsegmented_points_name, ".laz")
  )

rm(homogenized_points, segmented_points)


# [Calculate Tree Data] ---------------------------------------------------

# I tried to combine the crown delineation with the metrics calculation but for
# some reason I just get the error message "could not find function
# "calculate_tree_metrics"". Maybe it's a bug in lidR.


# Calculate Tree Metrics --------------------------------------------------

segmented_points_with_data <- lidR::readLAScatalog(
  glue(segmentation_output_directory, segmented_points_name, ".laz"),
  chunk_size = 2000
)

# The min and max functions would otherwise return -Inf or Inf respectively if
# all values in x were NA and removed because of na.rm = TRUE.
# There are still warnings about that when running calculate_tree_metrics, but I
# checked the output and couldn't find any Inf or -Inf so it should be fine.
min_of_nas_is_na <- function(x) {
  suppressWarnings(
    dplyr::if_else(all(is.na(x)), true = NA_real_, false = min(x, na.rm = TRUE))
  )
}
max_of_nas_is_na <- function(x) {
  suppressWarnings(
    dplyr::if_else(all(is.na(x)), true = NA_real_, false = max(x, na.rm = TRUE))
  )
}

calculate_tree_metrics <- function(x, y, z,
                                   gpstime,
                                   terrain_height,
                                   slope,
                                   aspect,
                                   ground_point_density,
                                   land_use) {

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

    num_pulses = data.table::uniqueN(gpstime),

    land_use_at_max_z = land_use[max_z_index],
    # This is cooler but takes too long to be worth the wait
    # land_use = dplyr::if_else(
    #   condition = dplyr::n_distinct(land_use) == 1,
    #   true = land_use[1],
    #   false = as.numeric(sample(
    #       dplyr::slice_max(dplyr::as_tibble(table(land_use)), n)[["land_use"]],
    #       size = 1
    #   ))
    # ),

    terrain_height_at_max_z = terrain_height[max_z_index],
    terrain_height_max = max(terrain_height),
    terrain_height_median = median(terrain_height),
    terrain_height_mean = mean(terrain_height),
    terrain_height_min = min(terrain_height),
    terrain_height_range = max(terrain_height) - min(terrain_height),

    slope_at_max_z = slope[max_z_index],
    slope_max = max_of_nas_is_na(slope),
    slope_median = median(slope, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    slope_min = min_of_nas_is_na(slope),
    slope_range = max_of_nas_is_na(slope) - min_of_nas_is_na(slope),
    slope_na_count = sum(is.na(slope)),

    aspect_at_max_z = aspect[max_z_index],
    aspect_max = max_of_nas_is_na(aspect),
    aspect_median = median(aspect, na.rm = TRUE),
    aspect_mean = mean(aspect, na.rm = TRUE),
    aspect_min = min_of_nas_is_na(aspect),
    aspect_na_count = sum(is.na(aspect)),

    ground_point_density_at_max_z = ground_point_density[max_z_index],
    ground_point_density_max = max(ground_point_density),
    ground_point_density_median = median(ground_point_density),
    ground_point_density_mean = mean(ground_point_density),
    ground_point_density_min = min(ground_point_density),
    ground_point_density_range = max(ground_point_density) -
      min(ground_point_density)
  ))
}

set.seed(1234)
tree_metrics_points <- lidR::tree_metrics(segmented_points_with_data,
  func = ~calculate_tree_metrics(
    X, Y, Z,
    gpstime,
    terrain_height,
    slope,
    aspect,
    ground_point_density,
    land_use
  ),
  attribute = "crown_id"
)

# Convert the points to an sf object because I don't know how to write sp
# objects to disk directly
sf::st_as_sf(tree_metrics_points) %>%
  sf::write_sf(
   glue(segmentation_output_directory, tree_metrics_points_name, ".gpkg")
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

segmented_points <- lidR::readLAScatalog(
  glue(segmentation_output_directory, segmented_points_name, ".laz"),
  chunk_size = 1000
)

lidR::opt_output_files(segmented_points) <- glue(
  catalog_output_directory, "{convex_crown_hulls_prefix}_{output_file_pattern}"
)
segmented_points@output_options[[
  "drivers"]][["Spatial"]][["extension"
]] <- ".gpkg"

# This takes somewhere between 5 to 10 minutes on my machine
lidR::delineate_crowns(segmented_points,
  type = "convex",
  attribute = "crown_id"
)
# TODO ask for documentation of default argument to type parameter being convex

# Merge the partial crown hull files
convex_crown_hulls <- dplyr::bind_rows(lapply(
  list.files(catalog_output_directory,
    pattern = glue(convex_crown_hulls_prefix, "_.*\\.gpkg"),
    full.names = TRUE
  ),
  sf::read_sf
))

# Calculate max diameters
# This took about two minutes on my machine
convex_crown_hull_diameter_max <- sapply(convex_crown_hulls[["geom"]],
  function(convex_polygon) {
    max(dist(sf::st_coordinates(convex_polygon)[, c("X", "Y")]))
  }
)

# Calculate the crown area and mean diameter and write the data to disk
convex_crown_hulls %>%
  mutate(
    area_convex = sf::st_area(geom),
    diameter_convex_max = convex_crown_hull_diameter_max,
    diameter_convex_mean =
      sf::st_cast(geom, to = "LINESTRING") %>% sf::st_length() / pi
  ) %>%
  sf::write_sf(
    glue(segmentation_output_directory, convex_crown_hulls_prefix, ".gpkg")
  )

# Remove the partial files
file.remove(
  list.files(catalog_output_directory,
    pattern = glue(convex_crown_hulls_prefix, "_.*"),
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


# [Get Terrain Data for Crown Polygons] -----------------------------------

# This works but would probably take a few hours so let's skip this part

# convex_crown_hulls <- sf::read_sf(
#   glue(segmentation_output_directory, convex_crown_hulls_prefix, ".gpkg")
# )
# terrain_height_grid <- raster::raster(
#   glue("{catalog_output_directory}{terrain_height_grid_name}")
# )
#
# # This takes about a minute on my machine
# terrain_height_grid_res_2 <- raster::aggregate(terrain_height_grid, fact = 4)
#
# slope_and_aspect_grid <- raster::terrain(terrain_height_grid_res_2,
#   opt = c("slope", "aspect"),
#   unit = "degrees",
#   neighbors = 8
# )
#
# num_processed_polygons <- 0
# test <- raster::extract(terrain_height_grid, convex_crown_hulls,
#   function(terrain_height, na.rm) {
#     num_processed_polygons <<- num_processed_polygons + 1
#     if (num_processed_polygons %% 100  == 0) {
#       cat(num_processed_polygons, "\n")
#     }
#     return(mean(terrain_height, na.rm = TRUE))
#   })


# Deal with Duplicated Crown IDs ------------------------------------------

segmented_points <- lidR::readLAS(
  glue(segmentation_output_directory, segmented_points_name, ".laz")
)
tree_metrics_points <- sf::read_sf(
  glue(segmentation_output_directory, tree_metrics_points_name, ".gpkg")
)
convex_crown_hulls <- sf::read_sf(
  glue(segmentation_output_directory, convex_crown_hulls_prefix, ".gpkg")
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
isTRUE(all.equal(sort(unique_segmentation_ids), sort(unique_tree_metrics_ids)))
#> TRUE
isTRUE(all.equal(sort(unique_segmentation_ids), sort(unique_crown_hull_ids)))
#> TRUE

# -> Both the tree metrics and the crown hulls feature all of the IDs that
#    appear in the segmented points but since they have more rows than unique
#    IDs, some IDs have to have been duplicated


## First deal with the tree metrics data

# Have a look at all rows with duplicated IDs
tree_metrics_points %>%
  filter(crown_id %in% crown_id[duplicated(crown_id)]) %>%
  arrange(crown_id)

# Check whether the duplicated rows are identical
tree_metrics_points %>%
  filter(crown_id %in% crown_id[duplicated(crown_id)]) %>%
  unique() %>%
  arrange(crown_id)
# unique() just "tests for identity of character representations" (according to
# the documentation) but that should be enough for our purposes

# -> The rows with the same IDs are identical
# -> They are all located at the border of a chunk

# Remove the duplicated rows in the tree metrics data
tree_metrics_points <- tree_metrics_points %>% filter(!duplicated(crown_id))


## Now for the crown hull data

# Have a look at all rows with duplicated IDs
convex_crown_hulls %>%
  filter(crown_id %in% crown_id[duplicated(crown_id)]) %>%
  arrange(crown_id)

# Check whether the duplicated rows are identical
convex_crown_hulls %>%
  filter(crown_id %in% crown_id[duplicated(crown_id)]) %>%
  unique() %>%
  arrange(crown_id)
# -> they are

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
#    lidR GitHub. More evidence for a bug: This happened for my MT data as well.

# Remove duplicated rows
convex_crown_hulls <- convex_crown_hulls %>% filter(!duplicated(crown_id))

rm(segmented_points)

sf::write_sf(tree_metrics_points,
  glue(segmentation_output_directory, tree_metrics_points_name, ".gpkg")
)

sf::write_sf(convex_crown_hulls,
  glue(segmentation_output_directory, convex_crown_hulls_prefix, ".gpkg")
)


# Merge Crown Hulls with Tree Metrics -------------------------------------

# convex_crown_hulls <- sf::read_sf(
#   glue(segmentation_output_directory, convex_crown_hulls_prefix, ".gpkg")
# )
#
# tree_metrics_points <- sf::read_sf(
#   glue(segmentation_output_directory, tree_metrics_points_name, ".gpkg")
# )

convex_crown_hulls %>%
  select(-(XTOP:ZTOP)) %>%
  left_join(
    tree_metrics_points %>% sf::st_drop_geometry(.),
    by = "crown_id"
  ) %>%
  sf::write_sf(
    glue(segmentation_output_directory, crown_hulls_with_data_prefix, ".gpkg")
  )

rm(convex_crown_hulls, tree_metrics_points)


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
