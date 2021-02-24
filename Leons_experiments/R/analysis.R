
# Load Packages -----------------------------------------------------------

library(tidyverse)


# Combine the crown polygon datasets into one (without polygons) ----------

# Get the file names of the polygon data sets
file_paths <- list.files(
  paste0(
    "../../Data/output/segmentation/",
    "crown_hulls_with_data_v3_1_deal_with_na_terrain_values"
  ),
  full.names = TRUE
)

# Create an empty list that will be filled in the loop below
crowns <- list()

# Loop over the polygon files
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
  crowns[[i]] <- sf::read_sf(file_paths[[i]]) %>% # read the data
    sf::st_drop_geometry(.) %>% # remove the polygons
    mutate( # add the segmentation parameters as additional columns
      cd_2_th = segmentation_parameters %>% pull(cd_2_th),
      ch_2_th = segmentation_parameters %>% pull(ch_2_th)
    )

  # Indicate progress with a message to the console
  cat("Processed file ", i, "\n")
}

# Combine the data.frames in the list into one big data.frame and store it in an
# R data file
bind_rows(crowns) %>%
  write_rds(
    "../../Data/output/crown_hulls_with_data_v3_1_deal_with_na_terrain_values.rds"
  )


# Load Data ---------------------------------------------------------------

crowns <- read_rds( # read the previously written data file
  "../../Data/output/crown_hulls_with_data_v3_1_deal_with_na_terrain_values.rds"
) %>%
  mutate(
    # translate the land use numbers into descriptions
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
    # convert the numeric segmentation parameters into factors
    cd_2_th = factor(cd_2_th),
    ch_2_th = factor(ch_2_th),
    # create combination factors for the segmentation parameters and sort the
    # factor levels alphabetically so that they are meaningfully ordered in plot
    # legends
    cd2th_x_ch2th = fct_cross(cd_2_th, ch_2_th, sep = " | ") %>%
      fct_relevel(levels(.) %>% sort()), # this sorts the factor levels
    ch2th_x_cd2th = fct_cross(ch_2_th, cd_2_th, sep = " | ") %>%
      fct_relevel(levels(.) %>% sort())
  )

# Set up some colors for plotting the different land use classes (I don't really
# use them though)
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

# Get the area of each land use class for later analysis
land_use_areas <- tibble(
  # load the raster, extract its values and put them into a tidyverse data.frame
  land_use = raster::raster("../../Data/land_use/Akies_LU_grid.tif") %>%
    raster::values()
  ) %>%
  mutate(
    # translate the land use numbers into descriptions
    land_use = factor(land_use) %>% fct_recode(
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
    )
  ) %>%
  # Calculate the areas by counting the values because each value corresponds to
  # a 1m^2 pixel
  count(land_use, name = "area_m2") %>%
  # also calculate hectare values from the m^2 values
  mutate(area_hectare = area_m2 / 1e4)

# Fetch an allometry database that will be used for outlier definitions
allometry_db <- readxl::read_excel( # read the relevant part of the excel file
  paste0("../../../Master_Thesis/Data/Jucker2016_GlobalAllometricDatabase/",
         "Jucker2016_GlobalAllometricDatabase.xlsx"),
  sheet = "Data",
  na = "NA",
  col_types = c(rep("guess", 2), "numeric", rep("guess", 6))
) %>%
  mutate(across(Functional_type:Biome, factor)) %>%
  filter( # select only trees from the same kind of region as La Palma
    Biogeographic_zone == "Palearctic",
    Biome == "Woodlands and savannas"
  )


# ggplot Theme Settings ---------------------------------------------------

# Increase the default text size for all ggplots (the ggplot_default_theme
# variable is assigned the unmodified theme)
ggplot_default_theme <- theme_update(text = element_text(size = 16))


# > Overview over Raw Results ---------------------------------------------

# Plot the number of trees per land use and parameter combination ----
crowns %>%
  # count the number of cases for each combination of land use and segmentation
  # parameters
  count(land_use_at_max_z, cd2th_x_ch2th) %>%
  # Sort the land use class factor by the count for a sorted visualization
  mutate(land_use_at_max_z = fct_reorder(land_use_at_max_z, n)) %>%
ggplot() +
  # use geom_jitter instead of geom_point so that the individual points in the
  # plot don't overlap
  geom_jitter(
    aes(x = land_use_at_max_z, y = n, color = cd2th_x_ch2th),
    size = 1.5,
    width = 0.15 # restrict the "jittering" of the points to a certain width
  ) +
  # use viridis colors for the segmentation parameters
  scale_color_viridis_d(name = "Segmentation\nParameters\n(CD | CH / TH)") +
  # tilt the land use class descriptions, otherwise they would overlap
  guides(x = guide_axis(title = NULL, angle = 45)) +
  ylab("Num. Segmented Objects")
ggsave("../Leons_experiments/graphics/segmentation_unfiltered_count.pdf",
  width = 9, height = 9 / 1.618 # use the golden ratio
)


# > Outlier Definition ----------------------------------------------------

# Identify Value Ranges of Allometric Database ----
allometry_plot <- cowplot::plot_grid(
  ggplot(allometry_db) +
    geom_point(aes(x = CD / H, y = H), size = 0.8),
  ggplot(allometry_db) +
    geom_point(aes(x = CD, y = H), size = 0.8),
  ncol = 2
)
cowplot::save_plot("graphics/allometry_CD_n_CD2H.pdf", allometry_plot, ncol = 2)

allometry_db %>%
  select(CD) %>%
  # arrange(CD) %>%
  arrange(desc(CD)) %>%
  unique()

allometry_db %>%
  transmute(CD2H = CD / H) %>%
  # arrange(CD2H) %>%
  arrange(desc(CD2H)) %>%
  unique()

# Check out tree height distribution to find possible outlier definition ----
crowns %>%
  add_count(land_use_at_max_z, name = "land_use_n") %>%
  mutate(land_use_at_max_z = fct_reorder(land_use_at_max_z, land_use_n)) %>%
  group_by(land_use_at_max_z, cd2th_x_ch2th) %>%
  # calculate different tree height percentile values for every combination of
  # land use and segmentation parameters
  summarize(
    percentile = c(0, 0.001, 0.01, 0.05, 0.1, 0.2),
    tree_height = quantile(max_z, probs = percentile),
    percentile = factor(percentile * 100)
  ) %>%
ggplot() +
  geom_jitter(
    aes(x = land_use_at_max_z, y = tree_height, color = cd2th_x_ch2th),
    size = 1.5,
    width = 0.15
  ) +
  scale_color_viridis_d(name = "Segmentation\nParameters\n(CD | CH / TH)") +
  # create an individual plot for each percentile
  facet_wrap(~percentile,
    # add a percent symbol "%" to every subplot title
    labeller = labeller(percentile = function(labels) paste(labels, "%"))
  ) +
  guides(x = guide_axis(title = NULL, angle = 45)) +
  ylab("Tree Height at Percentile") +
  # add a margin on the left so that the tilted land use descriptions fit into
  # the plot
  theme(plot.margin = margin(l = 40))
ggsave("../Leons_experiments/graphics/tree_height_lower_percentiles.pdf",
  width = 13, height = 13 / 1.618
)

# Same thing as above for some upper percentiles
crowns %>%
  add_count(land_use_at_max_z, name = "land_use_n") %>%
  mutate(land_use_at_max_z = fct_reorder(land_use_at_max_z, land_use_n)) %>%
  group_by(land_use_at_max_z, cd2th_x_ch2th) %>%
  summarize(
    percentile = c(0.8, 0.9, 0.95, 0.99, 0.999, 1),
    tree_height = quantile(max_z, probs = percentile),
    percentile = factor(percentile * 100)
  ) %>%
  ggplot() +
    geom_jitter(
      aes(x = land_use_at_max_z, y = tree_height, color = cd2th_x_ch2th),
      size = 1.5,
      width = 0.15
    ) +
    scale_color_viridis_d(name = "Segmentation\nParameters\n(CD | CH / TH)") +
    facet_wrap(~percentile,
      labeller = labeller(percentile = function(labels) paste(labels, "%"))
    ) +
    guides(x = guide_axis(title = NULL, angle = 45)) +
    ylab("Tree Height at Percentile") +
    theme(plot.margin = margin(l = 40))
ggsave("../Leons_experiments/graphics/tree_height_upper_percentiles.pdf",
  width = 13, height = 13 / 1.618
)

# Define outliers ----
crowns_w_outliers <- crowns %>%
  mutate(
    # don't register outliers where the slope is NA by setting all NA slope
    # values to 0 (but just here, for the outlier definition)
    slope_wo_na = if_else(slope_mean %>% is.na(), 0, slope_mean),
    # determine all outliers
    outlier = !(
      max_z %>% between(6, 40) &
      diameter_convex_mean %>% between(5, 17) &
      slope_wo_na <= 75 &
      diameter_convex_mean / max_z %>% between(0.039, 1.26)
    ),
    # determine outliers for individual criteria
    outlier_bad_height = !(max_z %>% between(6, 40)),
    outlier_bad_diameter = !(diameter_convex_mean %>% between(5, 17)),
    outlier_bad_cd_2_h = !(diameter_convex_mean / max_z %>% between(0.039, 1.26)),
    outlier_too_small = max_z < 6,
    outlier_too_tall = max_z > 40,
    outlier_too_thin = diameter_convex_mean < 5,
    outlier_too_wide = diameter_convex_mean > 17,
    outlier_too_steep = slope_wo_na > 75,
    outlier_too_thin_for_height = diameter_convex_mean / max_z < 0.039,
    outlier_too_wide_for_height = diameter_convex_mean / max_z > 1.26,
    # remove the slope values where NAs where converted to zeros
    slope_wo_na = NULL
  )

# Analyse outliers ----
crowns_w_outliers %>%
  # count the number of cases for each land use and segmentation parameter
  # combination
  count(land_use_at_max_z, cd2th_x_ch2th, name = "n_total") %>%
  # count the number of outliers for each combination separately and add them to
  # the total counts with a join
  left_join(
    crowns_w_outliers %>%
      filter(outlier) %>%
      count(land_use_at_max_z, cd2th_x_ch2th, name = "n_outlier"),
    by = c("land_use_at_max_z", "cd2th_x_ch2th")
  ) %>%
  mutate(
    outlier_share_percent = n_outlier / n_total * 100,
    land_use = fct_reorder(land_use_at_max_z, n_total)
  ) %>%
ggplot() +
  geom_jitter(
    aes(x = land_use, y = outlier_share_percent, color = cd2th_x_ch2th),
    size = 1.5,
    width = 0.15
  ) +
  scale_color_viridis_d(name = "Segmentation\nParameters\n(CD | CH / TH)") +
  guides(x = guide_axis(title = NULL, angle = 30)) +
  ylab("Outlier Share [%]") +
  theme(plot.margin = margin(l = 40))
ggsave("../Leons_experiments/graphics/outlier_share.pdf",
  width = 13, height = 13 / 1.618
)

# Counts and percentages of different outliers
crowns_w_outliers %>%
  group_by(cd2th_x_ch2th, land_use_at_max_z) %>%
  summarize(
    n = n(),
    # the sums of the TRUE/FALSE outlier values give the number of TRUE values
    percent_cd_outlier = sum(outlier_bad_diameter) / n * 100,
    percent_h_outlier = sum(outlier_bad_height) / n * 100,
    n_cd_2_h_outlier = sum(outlier_bad_cd_2_h),
    percent_slope_outlier = sum(outlier_too_steep) / n * 100,
  ) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  print(n = nrow(.))

# Overlap between different outliers (unfinished)
# crowns_w_outliers %>%
#   filter(cd_2_th == "0.4" & ch_2_th == "0.5") %>%
#   select(c(crown_id, starts_with("outlier_"))) %>%
#
# venn_diagram <- nVennR::plotVenn()

# Finally exclude outliers ----
crowns <- crowns_w_outliers %>%
  filter(!outlier) %>%
  add_count(land_use_at_max_z, name = "land_use_n") %>%
  mutate(
    land_use_at_max_z = fct_reorder(land_use_at_max_z, land_use_n, .desc = TRUE)
  )


# > Analysis --------------------------------------------------------------

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
  scale_color_viridis_d(name = "Segmentation\nParameters\n(CD | CH / TH)") +
  guides(x = guide_axis(title = NULL, angle = 45)) +
  ylab("Tree Density [1 / ha]") +
  theme(plot.margin = margin(l = 40))
ggsave("../Leons_experiments/graphics/tree_densities.pdf",
  width = 9, height = 9 / 1.618
)
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
  geom_line(aes(x = index, y = max_z, color = cd2th_x_ch2th)) +
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
