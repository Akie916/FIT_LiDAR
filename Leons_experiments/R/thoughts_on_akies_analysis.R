library(tidyverse)

crown_data <- sf::read_sf("../../Data/output/crown_hulls_with_data.gpkg") %>%
  as_tibble() %>% select(-geom)
terrain_height_grid <- raster::raster(
  "../../Data/output/catalog_processing/terrain_height_grid"
)


# Slope vs. Tree Height ---------------------------------------------------

# On assumption 1): I think it makes sense for trees to be shorter on slopes
# that are so steep that it becomes difficult to grow there. In those places,
# there might be less vertical growth because there is less competition and/or
# less soil. If the reduced competition is a significant factor, trees might
# show an increased horizontal growth. In order to analyze that, we would have
# to generate tree density data or calculate metrics like
# distance-to-nearest-neighbor or number-of-neighbors-within-a-certain-distance
# for each tree.
#
# I think it's also interesting to swich between slope_at_max_z and slope_mean.
# The latter gives a less "noisy" picture, although I don't really have an idea
# why that happens. In any case, it looks as if there is also a weak trend for
# trees to be smaller at verly gentle slopes and that's really weird because
# flat areas shouldn't cause trees to be smaller. Maybe that effect is due to a
# majority of the flat area being next to the crater at big heights, where less
# trees grow. I will investigate the terrain data some more below. On the other
# hand, there are only very few trees in that area and they only start to appear
# when the slope gets a bit steeper further downhill.
#
# The last interesting thing for me is that trees with only a few points (I
# arbitrarily chose the threshold 10) take up the extreme ends of the slope
# spectrum. This is not as clear for the lower end but more so for the upper
# one. Maybe we can find out whether these are distortions or are caused by
# ecological things, e.g. smaller trees having an advantage on steep slopes
# because of less stability issues. In that context the plot of max_z against
# num_points is also interesting because it shows that small trees always
# consist of only a few points. (Although it isn't true that trees with few
# points are always small.)

# Using slope_at_max_z vs. using slope_mean
crown_data %>%
  mutate(more_than_10_points = num_points > 10) %>%
  pivot_longer(
    cols = c(slope_mean, slope_at_max_z),
    names_to = "slope_variable",
    values_to = "slope"
  ) %>%
ggplot() +
  geom_hex(aes(x = slope, y = max_z), binwidth = c(0.5, 0.5)) +
  # takes way longer than geom_hex:
  # geom_point(aes(x = slope, y = max_z), size = 0.01) +
  facet_grid(rows = vars(slope_variable))

# See where trees with few points are
crown_data %>%
  mutate(more_than_10_points = num_points > 10) %>%
ggplot() +
  geom_point(
    aes(x = slope_mean, y = max_z),
    size = 0.01
  ) +
  facet_grid(
    cols = vars(more_than_10_points),
    labeller = as_labeller(c(`TRUE` = ">= 10 points", `FALSE` = "< 10 points"))
  )

# Relationship between number-of-points-per-tree and tree size
ggplot(crown_data) +
  geom_hex(aes(x = num_points, y = max_z), binwidth = c(3, 1)) +
  coord_cartesian(xlim = c(0, 250)) # excluding the extremes here


# Terrain Analysis --------------------------------------------------------

# Create a raster stack that contains height, terrain, and slope
# This takes about a minute on my machine
slope_and_aspect_grid <- raster::terrain(terrain_height_grid,
  opt = c("slope", "aspect"),
  unit = "degrees",
  neighbors = 8
)
terrain_grid <- raster::stack(terrain_height_grid, slope_and_aspect_grid)

# Take a sample of the values because there would be too much data to plot
# otherwise
sample_size = 1e5
set.seed(1234)
# This takes about half a minute on my machine
terrain_sample <- raster::sampleRandom(terrain_grid, sample_size) %>%
  as_tibble()

ggplot(terrain_sample) +
  geom_hex(aes(x = slope, y = Z), binwidth = c(1, 20))

# Without the sample but a little less detailed:
raster::plot(terrain_grid$slope, terrain_grid$Z)

# -> This shows that there is a small bias of the slope distribution along
# height in our study area, i.e. there are more gentle slopes in very high areas
# compared to lower altitudes

# Distribution of the slope values for the whole area vs. for the trees only
set.seed(1234)
terrain_sample %>%
  select(slope) %>%
  mutate(slope_source = "terrain") %>%
  bind_rows(
    crown_data %>%
      select(slope = slope_mean) %>%
      mutate(slope_source = "trees") %>%
      slice_sample(n = sample_size)
  ) %>%
ggplot() +
  geom_histogram(aes(slope), binwidth = 1) +
  facet_grid(rows = vars(slope_source))

# -> It looks as if there are slightly less trees with gentle slopes than
# terrain pixels

# The same as before but now for the terrain_height_mean of the whole terrain
# compared to that of the trees
set.seed(1234)
terrain_sample %>%
  select(terrain_height = Z) %>%
  mutate(terrain_height_source = "terrain") %>%
  bind_rows(
    crown_data %>%
      select(terrain_height = terrain_height_mean) %>%
      mutate(terrain_height_source = "trees") %>%
      slice_sample(n = sample_size)
  ) %>%
ggplot() +
  geom_histogram(aes(terrain_height), binwidth = 1) +
  facet_grid(rows = vars(terrain_height_source))


# Back to Tree Size vs. Terrain Height ------------------------------------

crown_data %>%
  filter(num_points > 10) %>%
  # filter(convex_area < 100) %>%
ggplot() +
  geom_hex(aes(x = terrain_height_mean, y = max_z), binwidth = c(20, 1))
# -> This checks out with the 20m Baumheide from 1000 to 1500m but the Canary
# Pine doesn't really fit in there. However, maybe that's just because Canary
# Pine doesn't grow as tall in heights above 1500m?

crown_data %>%
  mutate(slope_category = dplyr::case_when(
    slope_mean < 20 ~ "<20",
    20 <= slope_mean & slope_mean < 40 ~ "20-40",
    40 <= slope_mean & slope_mean < 60 ~ "40-60",
    60 <= slope_mean ~ ">=60"
  )) %>%
  filter(!is.na(slope_category)) %>% # There were like 9 NAs generated
ggplot() +
  # geom_histogram(
  #   aes(x = max_z, color = slope_category, fill = slope_category),
  #   position = "identity", bins = 30, alpha = 0.4
  # ) +
  geom_density(aes(x = max_z, color = slope_category))


# Irregularities with Point Counts per Crown ------------------------------

ggplot(crown_data) +
  geom_histogram(aes(x = num_points), binwidth = 2)
# -> huge outliers here

ggplot(crown_data) +
  geom_hex(aes(x = terrain_height_mean, y = num_points), binwidth = c(10, 1)) +
  coord_cartesian(ylim = c(0, 200))
# -> This is still weird. There seems to be a "hotspot" of crowns with only 5 to
# 10 points each between 1000 and 1500m

ggplot(crown_data) +
  geom_hex(aes(x = slope_mean, y = num_points), binwidth = c(1, 3)) +
  coord_cartesian(ylim = c(0, 200))
# -> similar


# Let's see about the crown area ------------------------------------------

ggplot(crown_data) +
  geom_histogram(aes(x = convex_area), binwidth = 2)
# -> Also huge outliers

ggplot(crown_data) +
  geom_hex(aes(x = terrain_height_mean, y = convex_area), binwidth = c(10, 2.5)) +
  coord_cartesian(ylim = c(0, 500))
# -> A very similar weird to that of the num_points variable


ggplot(crown_data) +
  geom_hex(aes(x = convex_area, y = num_points), binwidth = c(20, 6))
