library(tidyverse)

future::plan(strategy = future::multisession)


# Explore the original LAS data -------------------------------------------

file_paths <- list.files("../../Data/output/catalog_processing",
  pattern = "without_noise_and_duplicates_.*\\.laz",
  full.names = TRUE
)

las_data <- lidR::readLAScatalog(file_paths)

lidR::summary(las_data)
lidR::las_check(las_data)

las_file <- lidR::readLAS(file_paths[[1]])

lidR::summary(las_file)
lidR::las_check(las_file)

las_header <- lidR::readLASheader(file_paths[[1]])

cat(
  las_file@header@PHB$`Number of points by return`[[1]],
  data.table::uniqueN(las_file@data$gpstime),
  table(las_file@data$ReturnNumber)[[1]],
  las_header@PHB$`Number of points by return`[[1]],
  sep = "\n"
)
las_header@PHB$`Number of points by return`[[1]] -
  data.table::uniqueN(las_file@data$gpstime)

las_headers <- list()
for (i in seq_along(file_paths)) {
  las_headers[[i]] <- lidR::readLASheader(file_paths[[i]])
}


# Look at Point Classification --------------------------------------------

las_data <- lidR::readLAS(
  "../../Data/output/catalog_processing/normalized_points_x_227000_y_3184000.laz",
  filter = "-drop_class 2 -drop_z_below 1"
)

lidR::summary(las_data)
lidR::las_check(las_data)

las_data %>% lidR::filter_duplicates() %>% lidR::las_check()

# 0: Unclassified
# 1: Unclassified
# 2: Ground
# 3: Low Vegetation
# 4: Medium Vegetation
# 5: Low Vegetation
# 6: Building
# 7: Noise
# 12: Overlap

table(las_data@data$Classification)

las_class_palette <- rgb(
  t(col2rgb(c(
    "white", "brown",
    "lightgreen", "green", "darkgreen",
    "gray", "red", rep("yellow", times = 5)))),
  maxColorValue = 255
)

lidR::plot(las_data,
  color = "Classification",
  colorPalette = las_class_palette,
  size = 4, axis = TRUE, legend = TRUE
)


# Create Point Density Rasters for Exploration in QGIS --------------------

las_data <- lidR::readLAScatalog(
  # list.files("../../Data/output/catalog_processing",
  #   pattern = "without_noise_and_duplicates_.*\\.laz",
  #   full.names = TRUE
  # ),
  list.files("../../Data/output/catalog_processing",
    pattern = "homogenized_points_.*\\.laz",
    full.names = TRUE
  ),
  # list.files("../../Data/input_laz_files/",
  #   pattern = "*CIR\\.laz",
  #   full.names = TRUE
  # ),
  filter = "-first_only",
  # chunk_size = 1000
)

# This usually takes at least a few minutes depending on the input data
point_density <- lidR::grid_density(las_data, res = 5)

point_density[is.na(point_density)] <- 0

raster::writeRaster(point_density,
  filename = paste0(
    "../../Data/output/exploration/",
    "first_only_homogenized_dens075_sqrt4_point_density_res_5"
  ),
  overwrite = TRUE
)


# Investigate the Homogenization Results ----------------------------------

homogenized_points <- lidR::readLAScatalog(
  list.files("../../Data/output/catalog_processing",
    pattern = "homogenized_points_.*\\.laz",
    full.names = TRUE
  )
)

ideal_n_points <- lidR::area(homogenized_points) * 0.5
lost_points_percentage <-
  100 - lidR::npoints(homogenized_points) / ideal_n_points * 100

# This usually takes at least a few minutes depending on the input data
point_density <- lidR::grid_density(homogenized_points, res = 2)

point_density[is.na(point_density)] <- 0

raster::writeRaster(point_density,
  filename = paste0(
    "../../Data/output/exploration/",
    "homogenized_at_res_2_wo_noise_n_dupes_point_density_res_2"
  ),
  overwrite = TRUE
)


# Create Terrain Height Grids ---------------------------------------------

las_data <- lidR::readLAScatalog(
  list.files("../../Data/output/catalog_processing",
    pattern = "without_noise_and_duplicates_.*\\.laz",
    full.names = TRUE
  ),
  chunk_size = 1000
)

# This took about 5 minutes on my machine
terrain_height_grid <- lidR::grid_terrain(las_data,
  res = 1,
  algorithm = lidR::tin()
)

raster::writeRaster(terrain_height_grid,
  filename = (
    "../../Data/output/exploration/ground_height_wo_noise_n_dupes_res_1"
  ),
  overwrite = TRUE
)


# Create a Canopy Height Model --------------------------------------------

raster::writeRaster(
  raster::raster(
    "../../Data/output/exploration/terrain_height_wo_noise_n_dupes_res_1.grd"
  ) -
  raster::raster(
    "../../Data/output/exploration/ground_height_wo_noise_n_dupes_res_1.grd"
  ),
  filename = "../../Data/output/exploration/vegetation_height_wo_noise_n_dupes_res_1.grd",
  overwrite = TRUE
)

normalized_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
    pattern = glue("{normalized_points_prefix}_.*\\.laz"),
    full.names = TRUE
  ),
  chunk_size = 500
)

# This took about 2 minutes on my machine
canopy_height_grid <- lidR::grid_canopy(normalized_points,
  res = 0.5,
  algorithm = lidR::p2r(na.fill = lidR::tin())
)

canopy_height_grid <- raster::raster(
  "../../Data/output/exploration/vegetation_height_wo_noise_n_dupes_res_1.grd"
)

elevation_matrix <- rayshader::raster_to_matrix(canopy_height_grid)

canopy_hillshade <- elevation_matrix %>%
  rayshader::sphere_shade(.) %>%
  rayshader::add_shadow(.,
    shadowmap = rayshader::ray_shade(elevation_matrix),
    max_darken = 0.5
  )

raster::brick(canopy_hillshade,
  xmn = raster::xmin(canopy_height_grid),
  xmx = raster::xmax(canopy_height_grid),
  ymn = raster::ymin(canopy_height_grid),
  ymx = raster::ymax(canopy_height_grid),
  crs = raster::crs(canopy_height_grid)
) %>%
  raster::writeRaster(
    "../../Data/output/exploration/canopy_height_raytraced_res_1.tif"
  )


# Comparing Slope on Coarser and Finer Scales -----------------------------

filtered_points <- lidR::readLAScatalog(
  list.files("../../Data/output/catalog_processing",
    pattern = "filtered_points_.*\\.laz",
    full.names = TRUE
  )
)

# First create a height grid at the target resolution and save it
# This took a bit less than five minutes on my machine
terrain_height_grid <- lidR::grid_terrain(filtered_points,
  res = 2,
  algorithm = lidR::tin()
)
raster::writeRaster(terrain_height_grid,
  "../../Data/output/exploration/terrain_height_grid_res_2"
)


# Then calculate and save a slope grid
slope_grid_res_0_5 <- raster::terrain(terrain_height_grid,
  opt = c("slope"),
  unit = "degrees",
  neighbors = 8,
  filename = "../../Data/output/exploration/slope_grid_res_2"
)


# Compare the histograms of the differently scaled slope values
slope_grid_res_0_5 <- raster::raster(
  "../../Data/output/exploration/slope_grid_res_0_5"
)
slope_grid_res_2 <- raster::raster(
  "../../Data/output/exploration/slope_grid_res_2"
)
slope_grid_res_5 <- raster::raster(
  "../../Data/output/exploration/slope_grid_res_5"
)

slope_values <- tibble(
  slope = raster::sampleRandom(slope_grid_res_0_5, size = 1e5),
  resolution = 0.5
) %>%
  bind_rows(tibble(
    slope = raster::sampleRandom(slope_grid_res_2, size = 1e5),
    resolution = 2
  )) %>%
  bind_rows(tibble(
    slope = raster::sampleRandom(slope_grid_res_5, size = 1e5),
    resolution = 5
  ))

ggplot(slope_values) +
  geom_density(aes(x = slope, color = as_factor(resolution))) +
  geom_vline(
    aes(xintercept = slope_median, color = as_factor(resolution)),
    data = slope_values %>%
      group_by(resolution) %>%
      summarise(slope_median = median(slope, na.rm = TRUE))
    )
# -> The difference between the resolutions is very small


# Plot Point Data ---------------------------------------------------------

source("R/utility.R")

segmentation_output_directory <-
  "../../Data/output/segmentation/cd2th_0_4_ch2th_0_75/"

segmented_points <- lidR::readLAS(
  paste0(segmentation_output_directory, "segmented_points.laz")
)
crown_hulls <- sf::read_sf(
  paste0(segmentation_output_directory, "crown_hulls_with_data.gpkg")
)

point_cloud_with_data <- crown_hulls %>%
  sf::st_drop_geometry() %>%
  data.table::as.data.table() %>%
  .[segmented_points@data, on = "crown_id"] %>%
  mutate(diameter_convex_mean_log = log(diameter_convex_mean))

for (attribute_name in c("diameter_convex_mean")) {
  segmented_points <- lidR::add_lasattribute(segmented_points,
    point_cloud_with_data[[attribute_name]],
    name = attribute_name,
    desc = attribute_name
  )
}

system.file("extdata", "Topography.laz", package = "lidR") %>%
  lidR::readLAS(.) %>%
  lidR::normalize_height(., lidR::tin(), add_lasattribute = FALSE) %>%
  lidR::unnormalize_height(.) %>%
  lidR::writeLAS(., paste0(tempfile(), ".laz"))

lidR::writeLAS(
  segmented_points,
  "../../Data/output/exploration/segmented_points_with_data_cd2th_0_4_ch2th_0_75.laz"
)

segmented_points %>%
  lidR_clip_relative_rectangle(width = 6000, height = 6000, x_left = 2000) %>%
  lidR::unnormalize_height() %>%
  lidR::plot(color = "diameter_convex_mean", axis = TRUE, legend = TRUE)

terrain_height_grid <- raster::raster(
  "../../Data/output/catalog_processing/terrain_height_grid.grd"
) %>% raster::aggregate(fact = 60)

slope_grid <- raster::terrain(terrain_height_grid,
  opt = "slope",
  unit = "degrees",
  neighbors = 8
)

# Note that some trees will be counted multiple times by this approach
tree_density_grid <- lidR::grid_metrics(segmented_points,
  ~ list(tree_density = length(unique(crown_id))),
  res = slope_grid
)

# Note this isn't ideal either
land_use_grid <- raster::raster("../../Data/land_use/Akies_LU_grid.tif") %>%
  raster::aggregate(fact = 30, function(x, na.rm) sample(x, size = 1))

grid_stack <- raster::stack(list(
  tree_density = tree_density_grid,
  slope = slope_grid,
  land_use = land_use_grid
))

trees_vs_slope_density <- tibble(
  tree_density = raster::values(grid_stack$tree_density),
  slope = raster::values(grid_stack$slope),
  land_use = factor(raster::values(grid_stack$land_use))
) %>%
  # filter(!is.nan(slope)) %>%
  mutate(tree_density = ifelse(is.na(tree_density), 0, tree_density)) %>%
  mutate(slope_interval = cut_width(slope,
    width = 5,
    boundary = 0,
    closed = "left"
    )
  ) %>%
  group_by(land_use, slope_interval) %>%
  summarise(n_pixels = n(), n_trees = sum(tree_density)) %>%
  mutate(
    relative_tree_density = n_trees / n_pixels,
    slope_interval_val = as.integer(slope_interval)
  )

ggplot(trees_vs_slope_density) +
  geom_line(
    aes(x = slope_interval_val, y = relative_tree_density, color = land_use)
  ) +
  scale_x_continuous(
    breaks = unique(trees_vs_slope_density$slope_interval_val),
    labels = unique(trees_vs_slope_density$slope_interval)
  )


# A failed attempt to create a rayshader plot with a picture mapped on top of
# the 3D model.

# crown_diameter_grid <- segmented_points %>%
#   lidR::grid_metrics(
#     ~ list(crown_diameter =  max(diameter_convex_mean)),
#     res = 5
#   )
#
# terrain_height_grid <- raster::raster(
#   "../../Data/output/catalog_processing/terrain_height_grid.grd"
# ) %>%
#   raster::aggregate(10)
#
# my_colors <- colorRamp(c("blue", "red"), space = "Lab")
#
# raster_colors <- raster::getValues(
#   crown_diameter_grid / raster::cellStats(crown_diameter_grid, "max")
# ) %>%
#   my_colors()
#
# raster_colors[which(is.na(raster_colors[, 1])), ] <- 0
#
# raster_colors / 255
#
# terrain_height_grid %>%
#   # raster::crop(raster::extent(., 1, 6000, 1, 6000)) %>%
#   # raster::aggregate(10) %>%
#   rayshader::raster_to_matrix() %>%
#   rayshader::sphere_shade(texture = "desert") %>%
#   rayshader::add_overlay(raster_colors / 255) %>%
#   rayshader::plot_map()


ground_points <- lidR::readLAScatalog(
  list.files("../../Data/output/catalog_processing",
    pattern = "without_noise_and_duplicates_.*\\.laz",
    full.names = TRUE
  ),
  filter = "-keep_class 2",
  chunk_size = 1000
)

lidR_plot_custom(
  segmented_points %>%
    lidR::unnormalize_height() %>%
    lidR::filter_poi(crown_id > 0),
  color = "crown_id",
  colorPalette = random_crown_colors(segmented_points[["crown_id"]])
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


# Explore the Crown Hulls on a Base Map -----------------------------------

trees <- sf::read_sf("../../Data/output/crown_hulls_with_data.gpkg")

bbox_map <- mapview::mapview(sf::st_bbox(trees), map.type = "Esri.WorldImagery")

tree_clip <- sf::st_bbox(trees)
tree_clip["xmin"] <- tree_clip["xmax"] - 500
tree_clip["ymax"] <- tree_clip["ymin"] + 500

bbox_map + mapview::mapview(tree_clip)

trees %>%
  sf::st_intersection(sf::st_as_sfc(tree_clip)) %>%
  mapview::mapview(
    map.type = "Esri.WorldImagery",
    zcol = "convex_area",
    legend = TRUE,
    canvas = TRUE
  )

