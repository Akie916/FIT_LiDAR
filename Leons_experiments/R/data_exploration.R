library(tidyverse)

future::plan(strategy = future::multisession)


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
  list.files("../../Data/output/catalog_processing",
    pattern = "without_noise_and_duplicates_.*\\.laz",
    full.names = TRUE
  ),
  # list.files("../../Data/input_laz_files/",
  #   pattern = "*CIR\\.laz",
  #   full.names = TRUE
  # ),
  filter = "-drop_class 12",
  # chunk_size = 1000
)

# This usually takes at least a few minutes depending on the input data
point_density <- lidR::grid_density(las_data, res = 5)

point_density[is.na(point_density)] <- 0

raster::writeRaster(point_density,
  filename = paste0(
    "../../Data/output/exploration/",
    "no_overlap_wo_noise_n_dupes_point_density_res_5"
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

segmented_points <- lidR::readLAS(
  "../../Data/output/segmentation/segmented_points_cd2th_0_3_ch2th_1.laz"
)

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

