library(tidyverse)


# Combine the crown polygon datasets into one (without polygons) ----------

file_paths <- list.files(
  paste0(
    "../../Data/output/segmentation/",
    "crown_hulls_with_data_v3_1_deal_with_na_terrain_values"
  ),
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
    cd2th_x_ch2th = fct_cross(cd_2_th, ch_2_th, sep = " | ") %>%
      fct_relevel(levels(.) %>% sort()),
    ch2th_x_cd2th = fct_cross(ch_2_th, cd_2_th, sep = " | ") %>%
      fct_relevel(levels(.) %>% sort())
  )

write_rds(crowns,
  "../../Data/output/crown_hulls_with_data_v3_1_deal_with_na_terrain_values.rds"
)


# Load Data ---------------------------------------------------------------

crowns <- read_rds(
  "../../Data/output/crown_hulls_with_data_v3_1_deal_with_na_terrain_values.rds"
) %>%
  mutate(across(c(cd2th_x_ch2th, ch2th_x_cd2th),
    ~fct_relabel(.x, ~str_replace(., pattern = ":", replacement = " | "))
  ))

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

land_use_areas <- tibble(land_use = raster::values(
  raster::raster("../../Data/land_use/Akies_LU_grid.tif")
)) %>%
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


# ggplot Theme Settings ---------------------------------------------------

# Increase the default text size for all ggplots (the ggplot_default_theme
# variable is assigned the unmodified theme)
ggplot_default_theme <- theme_update(text = element_text(size = 16))


# Analysis ----------------------------------------------------------------

# Define outliers
crowns_w_outliers <- crowns %>%
  mutate(
    outlier =
      !(max_z %>% between(2, 60)) |
      !((diameter_convex_mean / max_z) %>% between(0.05, 1.5)) |
      area_convex < 2 |
      diameter_convex_mean < sqrt(2) * 4 / pi
  )

crowns <- crowns_w_outliers %>% filter(!outlier)

crowns <- crowns %>%
  add_count(land_use_at_max_z, name = "land_use_n") %>%
  mutate(
    land_use_at_max_z = fct_reorder(land_use_at_max_z, land_use_n, .desc = TRUE)
  )

# Tree density ----
crowns %>%
  filter(
    area_convex > 8,
    max_z < 50,
    diameter_convex_mean / max_z < 1.5
  ) %>%
  group_by(cd2th_x_ch2th, land_use_at_max_z) %>%
  count() %>%
  left_join(land_use_areas, by = c(land_use_at_max_z = "land_use")) %>%
  mutate(n_per_hectare = n / area_hectare) %>%
ggplot() +
  geom_jitter(
    aes(
      x = fct_reorder(land_use_at_max_z, n_per_hectare),
      y = n_per_hectare,
      color = cd2th_x_ch2th
    ),
    size = 1.5,
    width = 0.15
  ) +
  scale_color_viridis_d(name = "CD | CH / TH") +
  theme(text = element_text(size = 16)) +
  guides(x = guide_axis(n.dodge = 2)) +
  xlab("Land Use") + ylab("# Trees / Hectare")
# -> This shows clear patterns but only because all trees with a crown area < 8
# have been excluded. If you disable that filter the segmentation parameters are
# not as clearly separated.


# Tree Height Distribution ----
ggplot(crowns) +
  geom_density(aes(x = max_z, color = ch2th_x_cd2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 40))

crowns %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
  group_by(cd_2_th, ch_2_th, land_use_at_max_z) %>%
  group_modify(
    ~ .x %>%
      arrange(desc(max_z)) %>%
      mutate(index = seq_len(nrow(.x)))
  ) %>%
ggplot() +
  geom_line(aes(x = index, y = max_z, color = ch2th_x_cd2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  scale_x_log10() +
  coord_cartesian(ylim = c(0, 60))


# Diameter Distribution ----
ggplot(crowns) +
  geom_density(aes(x = diameter_convex_mean, color = ch2th_x_cd2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 12))


# Point and Pulse Count Density ----
ggplot(crowns) +
  geom_freqpoly(
    aes(x = num_points, y = after_stat(density), color = ch2th_x_cd2th),
    binwidth = 1
  ) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(5, 25))

ggplot(crowns) +
  geom_freqpoly(
    aes(x = num_pulses, y = after_stat(density), color = ch2th_x_cd2th),
    binwidth = 1
  ) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 25))

ggplot(crowns) +
  geom_density(aes(x = num_points / num_pulses, color = cd2th_x_ch2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(1, 1.03))

# Pulse density per tree area
crowns %>%
  mutate(pulse_density = num_pulses / area_convex) %>%
  filter(pulse_density < 20) %>%
ggplot() +
  geom_density(aes(x = pulse_density, color = ch2th_x_cd2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 7))

# Point density per tree volume
crowns %>%
  mutate(point_density = num_points / (area_convex * max_z)) %>%
  filter(point_density < 20) %>%
ggplot() +
  geom_density(aes(x = point_density, color = ch2th_x_cd2th)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 0.7))

# Ground point density
crowns %>%
  filter(ground_point_density_mean < 1.5) %>%
ggplot() +
  geom_freqpoly(
    aes(x = ground_point_density_mean, color = land_use_at_max_z),
    binwidth = 0.05
  ) +
  coord_cartesian(xlim = c(0, 1))
# -> I'm not yet satisfied with this plot but it reflects the distribution of
# ground points quite nicely.


# Terrain Height Range -> Mean Slope ----

# So the terrain height range is likely to be smaller when the tree is smaller.
# Therefore it has to be shown in combination with e.g. the crown diameter.
# Instead of simply using the ratio of height range and diameter, I chose to
# calculate an inclination from them.

ggplot(crowns) +
  geom_density(aes(
    x = atan(terrain_height_range / diameter_convex_mean) / pi * 180,
    color = ch2th_x_cd2th
  )) +
  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z))

# The mean slope matches this calculated inclination quite well:
crowns %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(
    aes(
      x = atan(terrain_height_range / diameter_convex_mean) / pi * 180,
      y = slope_mean
    ),
    size = 0.5
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z))

# I guess it makes more sense then to use the mean slope instead
ggplot(crowns) +
  geom_density(aes(x = slope_mean, color = ch2th_x_cd2th)) +
  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z))

crowns %>%
  group_by(cd_2_th, ch_2_th) %>%
  slice_sample(n = 1e4) %>%
ggplot() +
  geom_point(aes(x = max_z, y = slope_median, color = ch2th_x_cd2th), size = 0.5) +
  scale_y_continuous(breaks = c(0, 30, 60, 90)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z))


# Trees > 30m ----

crowns %>%
  filter(max_z > 29) %>%
ggplot() +
  geom_point(
    aes(x = max_z, y = slope_mean, color = cd2th_x_ch2th),
    size = 0.5
  ) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(30, max(crowns$max_z)))
# -> Slopes above 55 look like outlier candidates


# CD / H ratio ----
ggplot(crowns) +
  geom_freqpoly(
    aes(x = diameter_convex_mean / max_z, color = land_use_at_max_z),
    binwidth = 0.01,
    size = 2
  ) +
  scale_color_manual(values = land_use_colors) +
  coord_cartesian(xlim = c(0, 1))

crowns %>%
  filter(diameter_convex_mean / max_z > 1.5) %>%
ggplot() +
  geom_point(aes(x = area_convex, y = diameter_convex_mean / max_z), size = 0.2)
  # geom_point(
  #   aes(x = diameter_convex_mean, y = diameter_convex_mean / max_z),
  #   size = 0.2
  # )
# -> It's not nany trees but I think we can use this as an outlier threshold

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


# Sandbox ----

crowns %>%
  filter(area_convex < pi * (sqrt(2) / 2)^2) %>%
ggplot() +
  geom_density(aes(x = diameter_convex_mean), size = 0.2) +
  geom_vline(xintercept = sqrt(2), color = "red") +
  facet_wrap(vars(land_use_at_max_z))
# -> The diameters excluded by this area size filter are actually a bit bigger
# than sqrt(2) but since these are very small diameters anyway it doesn't
# really matter


# Temporary: Compare crowns_v2 with crowns_v3 -----------------------------

crowns <- bind_rows(
  readr::read_rds(paste0(
    "../../Data/output/crown_hulls_with_data_v2_",
    "homogenized_pulse_instead_of_point_density.rds"
  )) %>%
    mutate(version = "v2"),
  readr::read_rds(paste0(
    "../../Data/output/crown_hulls_with_data_v3_",
    "homogenize_before_removing_normalization_effects.rds"
  )) %>%
    mutate(version = "v3")
) %>%
  mutate(version = factor(version))

crowns %>%
  count(version, land_use_at_max_z, cd2th_x_ch2th) %>%
  pivot_wider(names_from = version, values_from = n) %>%
  mutate(
    v2_minus_v3 = v2 - v3,
    percent_change = (v2_minus_v3 / v2) * 100
  ) %>%
  arrange(desc(v2_minus_v3)) %>%
ggplot() +
  geom_boxplot(aes(
    x = fct_reorder(land_use_at_max_z, v2, .desc = TRUE),
    y = percent_change
  )) +
  guides(x = guide_axis(n.dodge = 2))
# -> Change in number of detected trees is somewhere around 2 to 10 percent less
# trees in v3

ggplot(crowns) +
  geom_density(aes(x = max_z, color = cd_2_th, linetype = version)) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim= c(0, 30))
# -> Tree height distribution seems to be shifted a very tiny bit to the right

ggplot(crowns) +
  geom_density(aes(
    x = diameter_convex_mean,
    color = cd_2_th,
    linetype = version
  )) +
  scale_color_viridis_d() +
  facet_wrap(vars(land_use_at_max_z)) +
  coord_cartesian(xlim = c(0, 10))
# -> Same for diameter distribution
