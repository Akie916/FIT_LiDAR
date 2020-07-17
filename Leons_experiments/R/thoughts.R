source("R/utility.R")


# Read in data ------------------------------------------------------------

sample_tile <- lidR::readLAS(
  "../Data/PNOA_2016_CANAR-LP_212-3184_ORT-CLA-RGB.laz"
)
# There is no CRS included in the data but the epsg code 4083 seems reasonable:
# https://epsg.io/4083
lidR::epsg(sample_tile) <- 4083


# Point density -----------------------------------------------------------

# Plot all points
lidR_plot_custom(sample_tile,
  color = "RGB",
  size = 1
)

# Plot without ground points
lidR_plot_custom(lidR::lasfilter(sample_tile, Classification != 2),
  color = "RGB",
  size = 1
)
## -> The ground points do not visibly dissappear except for some areas where
##    there are weird stripes with visibly less ground points.

# Plot all the points again but with smaller point size
lidR_plot_custom(sample_tile,
  color = "RGB",
  size = 0.3
)
## -> There seem to be areas with a lower point density

# Plot point density
point_density <- lidR::grid_density(sample_tile, res = 7)
raster::plot(point_density)
## -> This confirms the previous suspicion

# Plot only ground points
ground_points <- lidR::lasfilter(sample_tile, Classification == 2)
lidR_plot_custom(
  ground_points,
  color = "RGB",
  size = 0.3
)
ground_density <- lidR::grid_density(ground_points, res = 7)
raster::plot(ground_density)
## -> The ground points, however, seem less affected by this irregularity

ground_elevation <- lidR::grid_terrain(sample_tile, algorithm = lidR::tin())
slope <- raster::terrain(ground_elevation)
raster::plot(slope)
## -> There seems to be no connection between slope and point density.

# Omit extreme densities
raster::freq(ground_density)

lower_ground_density <- raster::calc(ground_density, fun = function(x) {ifelse(x < 4, x, 4)})
raster::plot(lower_ground_density)

ground_density <- lidR::grid_density(ground_points, res = 2)
na_density <- raster::calc(ground_density, fun = function(x) {ifelse(is.na(x), 0, 1)})
raster::plot(na_density)
## -> NA values seem to be relatively equally distributed

## Conclusion:
## The classification of ground points seems to be not very good. Next steps
## could be to try out ground point classification with the lidR package or just
## omit all points below a certain height and move on to individual tree
## detection and the assessment of their properties.


# Ideas for hypotheses ----------------------------------------------------

# + Investigate individual tree properties over different slopes, heights and
#   point densities
