library(tidyverse)

file_paths <- list.files(
  "../../Data/output/segmentation/crown_hulls_with_data_homogenized_pulse_density",
  recursive = TRUE,
  full.names = TRUE
)

crowns <- list()

# This takes a few minutes on my machine
for (i in seq_along(file_paths)) {

  # Extract the segmentation parameters from the file names
  segmentation_parameters <- tibble(file_path = file_paths[[i]]) %>%
    mutate(
      cd_2_th = str_match(file_path,
        pattern = "cd2th_([:digit:](_[:digit:]+)?)")[, 2],
      ch_2_th = str_match(file_path,
        pattern = "ch2th_([:digit:](_[:digit:]+)?)")[, 2],
    ) %>%
    mutate(across(cd_2_th:ch_2_th,
      ~ str_replace(.x, pattern = "_", replacement = ".")
    ))

  # Add the data to the list
  crowns[[i]] <- sf::read_sf(file_paths[[i]]) %>%
    sf::st_drop_geometry() %>%
    mutate(
      cd_2_th = segmentation_parameters %>% pull(cd_2_th),
      ch_2_th = segmentation_parameters %>% pull(ch_2_th)
    )

  # Indicata progress
  cat("Processed file ", i, "\n")
}

crowns <- bind_rows(crowns) %>%
  mutate(
    land_use_at_max_z = factor(land_use_at_max_z) %>% fct_recode(
      `Bare rocks` = "1",
      `Broad-leaved forest` = "2",
      `Coniferous forest (sparse)` = "3",
      `Coniferous forest (crater)` = "4",
      `Coniferous forest (dense)` = "5",
      `Moors and heathland` = "6",
      `Sclerophyllous vegetation` = "7",
      `Sparsely vegetated areas` = "8",
      `Sparsely vegetated areas (crater)` = "9",
      `Transitional woodland-shrub` = "10"
    ),
    cd_2_th = factor(cd_2_th),
    ch_2_th = factor(ch_2_th),
    cd2th_x_ch2th = fct_cross(cd_2_th, ch_2_th),
    ch2th_x_cd2th = fct_cross(ch_2_th, cd_2_th)
  )

readr::write_rds(crowns, "../../Data/output/crown_hulls_with_data.gpkg")

crowns <- readr::read_rds("../../Data/output/crown_hulls_with_data.gpkg")

land_use_colors <- c(
  "azure3", # Bare rocks
  "limegreen", # Broad-leaved forest
  "springgreen3", # Coniferous forest (sparse)
  "darkgreen",
  "springgreen4",
  "darkorchid3", # Moors and heathland
  "darkorchid1", # Sclerophyllous vegetation
  "tan3", # Sparsely vegetated areas
  "tan4",
  "yellowgreen"
)


# Land Use
land_use_grid <- raster::raster("../../Data/land_use/Akies_LU_grid.tif")

land_use_areas <- tibble(land_use = raster::values(land_use_grid)) %>%
  mutate(land_use = factor(land_use) %>% fct_recode(
    `Bare rocks` = "1",
    `Broad-leaved forest` = "2",
    `Coniferous forest (sparse)` = "3",
    `Coniferous forest (crater)` = "4",
    `Coniferous forest (dense)` = "5",
    `Moors and heathland` = "6",
    `Sclerophyllous vegetation` = "7",
    `Sparsely vegetated areas` = "8",
    `Sparsely vegetated areas (crater)` = "9",
    `Transitional woodland-shrub` = "10"
  )) %>%
  count(land_use, name = "area_m2") %>%
  mutate(area_hectare = area_m2 / 1e4)


# Tree density
crowns %>%
  filter(
    area_convex > 8,
    max_z < 50,
    diameter_convex_mean / max_z < 1.5
  ) %>%
  group_by(ch2th_x_cd2th, land_use_at_max_z) %>%
  count() %>%
  left_join(land_use_areas, by = c(land_use_at_max_z = "land_use")) %>%
  mutate(n_per_hectare = n / area_hectare) %>%
ggplot() +
  geom_jitter(
    aes(
      x = land_use_at_max_z,
      y = n_per_hectare,
      color = ch2th_x_cd2th
    ),
    size = 1.5,
    width = 0.15
  ) +
  scale_color_viridis_d() +
  theme(text = element_text(size = 16)) +
  guides(x = guide_axis(n.dodge = 2))


# Trees with < 10 m2 area
crowns %>%
  filter(area_convex < 50) %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(aes(x = area_convex, y = num_pulses), size = 0.2)

crowns %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_histogram(aes(area_convex), binwidth = 0.25) +
  coord_cartesian(xlim = c(0, 50))

ggplot() +
  geom_line(aes(x = segmentation_params, y = n),
    data = crowns %>%
      filter(area_convex < 10) %>%
      count(segmentation_params),
    color = "red"
  ) +
  geom_line(aes(x = segmentation_params, y = n),
    data = crowns %>% count(segmentation_params),
    color = "blue",
  )

crowns %>%
  filter(area_convex < 30) %>%
  group_by(segmentation_params) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(aes(x = area_convex, y = max_z), size = 0.1)

segmented_points <- lidR::readLAS(
    "../../Data/output/segmentation/cd2th_0_5_ch2th_1/"
  )

crown_hulls <- sf::read_sf(
  "../../Data/output/segmentation/cd2th_0_5_ch2th_0_5/crown_hulls_with_data.gpkg"
)

segmented_points@data <- crown_hulls %>%
  sf::st_drop_geometry() %>%
  select(c(crown_id, diameter_convex_mean)) %>%
  data.table::as.data.table() %>%
  .[segmented_points@data, on = "crown_id"] %>%
  mutate(diameter_convex_mean_log = log(diameter_convex_mean))

segmented_points %>%
  lidR_clip_relative_rectangle(width = 6000, height = 6000, x_left = 2000) %>%
  lidR::unnormalize_height() %>%
  lidR::plot(color = "diameter_convex_mean", axis = TRUE, legend = TRUE)


crowns %>%
  filter(between(max_z, 5, 40)) %>%
ggplot() +
  geom_freqpoly(
    aes(x = max_z, color = land_use_at_max_z),
    size = 2,
    binwidth = 1
  ) +
  scale_color_manual(values = land_use_colors) +
  facet_grid(rows = vars(ch_2_th), cols = vars(cd_2_th))

ggplot(crowns) +
  geom_freqpoly(
    aes(x = ground_point_density_mean, color = land_use_at_max_z),
    size = 2,
    binwidth = 0.05
  ) +
  scale_color_manual(values = land_use_colors) +
  coord_cartesian(xlim = c(0, 1.5))

ggplot(crowns) +
  geom_histogram(aes(x = sqrt(area_convex)), binwidth = 0.1) +
  geom_vline(xintercept = 1, color = "red")

crowns %>%
  filter(area_convex > 1) %>%
  group_by(land_use_at_max_z) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(
    aes(
      x = ground_point_density_mean,
      y = terrain_height_range,
      color = land_use_at_max_z
    ),
    size = 0.5
  )

ggplot(crowns) +
  geom_freqpoly(
    aes(x = terrain_height_range, color = land_use_at_max_z),
    binwidth = 0.5,
    size = 2
  ) +
  scale_color_manual(values = land_use_colors) +
  coord_cartesian(xlim = c(0, 20))
# -> terrain height ranges of 5 m are not uncommon!


# Trees > 30m
crowns %>%
  # filter(max_z > 30) %>%
ggplot() +
  geom_freqpoly(aes(x = max_z, color = segmentation_params), binwidth = 1)


# Pulse and Point Counts
ggplot(crowns) +
  geom_point(aes(x = area_convex, y = num_points / num_pulses), size = 0.1)


# CD / H ratio
ggplot(crowns) +
  geom_freqpoly(
    aes(x = diameter_convex_mean / max_z, color = land_use_at_max_z),
    binwidth = 0.01,
    size = 2
  ) +
  scale_color_manual(values = land_use_colors) +
  coord_cartesian(xlim = c(0, 1))

crowns %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(
    aes(x = diameter_convex_mean, y = max_z, color = area_convex >= 10),
    size = 0.2
  )

crowns %>%
  filter(diameter_convex_mean / max_z > 1) %>%
ggplot() +
  # geom_point(aes(x = max_z, y = diameter_convex_mean / max_z), size = 0.2)
  geom_point(aes(x = area_convex, y = diameter_convex_mean / max_z), size = 0.2)
  # geom_point(aes(x = diameter_convex_mean, y = diameter_convex_mean / max_z), size = 0.2)


# Crown Lengthiness
circles <- tibble(
  d = seq(0, 50, by = 0.5),
  area = (d / 2)^2 * pi
)

crowns %>%
  mutate(
    diameter_min_estimate = 2 * diameter_convex_mean - diameter_convex_max,
    lengthiness = area_convex / diameter_convex_max
  ) %>%
  slice_sample(n = 1e5) %>%
ggplot() +
  # geom_point(aes(x = area_convex, y = diameter_convex_max), size = 0.5) +
  # geom_line(data = circles, aes(x = area, y = d), color = "red") +
  geom_point(
    aes(x = area_convex, y = diameter_convex_max / diameter_min_estimate),
    size = 0.5
  ) +
  # scale_x_log10() + scale_y_log10() +
  facet_grid(rows = vars(ch_2_th), cols = vars(cd_2_th))
# -> Have I already filtered for crown lengthiness?


# Tree Height Distribution
crowns %>%
  filter(
    area_convex > 9,
    # area_convex < 50,
    diameter_convex_mean < 15,
    max_z < 50
  ) %>%
  mutate(
    cd2th_x_ch2th = fct_cross(cd_2_th, ch_2_th),
    ch2th_x_cd2th = fct_cross(ch_2_th, cd_2_th)
  ) %>%
ggplot() +
  geom_freqpoly(
    aes(x = diameter_convex_mean, color = ch2th_x_cd2th),
    binwidth = 0.5,
    size = 1.2
  ) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  theme(text = element_text(size = 16))

crowns %>%
  filter(area_convex >= 10 & max_z <= 60) %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
  group_by(cd_2_th, ch_2_th, land_use_at_max_z) %>%
  group_modify(~ .x %>%
    arrange(desc(max_z)) %>%
    mutate(index = seq_len(nrow(.x)))
  ) %>%
ggplot() +
  geom_line(aes(x = index, y = max_z, color = segmentation_params)) +
  facet_wrap(vars(land_use_at_max_z)) +
  scale_x_log10()
