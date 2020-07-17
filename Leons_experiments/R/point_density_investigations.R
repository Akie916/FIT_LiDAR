source("R/utility.R")
library(tidyverse)


# Read in Data ------------------------------------------------------------

las_tile <- lidR::readLAS(
  "../../Data/laz_files/PNOA_2016_CANAR-LP_212-3184_ORT-CLA-RGB.laz"
)

# According to the vignette("lidR-computation-speed-LAScatalog") some
# computations are faster when using .las files instead of .laz files and can
# become even faster when also creating .lax files.
# This wasn't true when I ran a ground classification algorithm on a subset of a
# lidar data tile. It might be interesting when processing more data in one go
# on the cluster computer, but for experimenting it looks like there is no
# benefit in doing this (at least with the ground classification).
#
# lidR::writeLAS(las_tile,
#   "../../Data/las_files/PNOA_2016_CANAR-LP_212-3184_ORT-CLA-RGB.las",
#   index = TRUE
# )
# las_tile <- lidR::readLAScatalog(
#   "../../Data/las_files/"
# )

# When I tried to perform a calculation on a LASCatalog object that contained a
# whole .las file, I was prompted to provide an output file path. After cropping
# the las data this was no longer an issue. I am keeping the two lines below for
# when we will try to perform calculations on bigger data sets on the cluster
# computer.
#
# lidR::opt_output_files(las_tile) <-
#   "../../Data/output/ground_classification/{ORIGINALFILENAME}"

# I am experimenting on a small subset of the original data because otherwise
# the computations take too long on my machine.
las_subtile <- las_clip_relative_rectangle(las_tile, width = 700)


# Remove Noise and Overlap Points (suggested by Akie) ---------------------

las_filtered <- lidR::filter_poi(las_subtile, !(Classification %in% c(7, 12)))

# Use lascheck to check for duplicated points
lidR::las_check(las_subtile)
lidR::las_check(las_filtered)
# -> most of them are gone in the filtered point cloud

# Plot before and after filtering
plot_las(las_subtile, color = "Classification", legend = TRUE)
plot_las(las_filtered, color = "Classification", legend = TRUE)
# -> From just looking at it, removal of these points seems fine.

# Count point classifications
table(las_subtile@data$Classification)
table(las_filtered@data$Classification)
# -> Apparently there are nearly no noise points but the overlapping points make
# up about half of the whole data set!


# Investigate the Point Density Issue -------------------------------------

# Plot point density before and after removing overlapping points
point_density_original <- lidR::grid_density(las_subtile)
point_density_filtered <- lidR::grid_density(las_filtered)

raster::plot(point_density_original)
raster::plot(point_density_filtered)

# Check point density histograms
# (I didn't use raster::hist here because ggplot allows for plotting multiple
# diagrams in such a way that they are easy to compare.)
point_densities <- bind_rows(
  data.frame(
    source = "original",
    density = point_density_original@data@values
  ),
  data.frame(
    source = "filtered",
    density = point_density_filtered@data@values
  )
)

density_histograms <- ggplot(data = point_densities) +
  geom_histogram(aes(x = density),
                 binwidth = 0.25,
                 color = "black",
                 fill = "grey55"
  ) +
  geom_vline(xintercept = 3, color = "blue") +
  facet_grid(rows = vars(source)) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 30, by = 5),
    minor_breaks = seq(from = 0, to = 30, by = 1))

density_histograms

# Zoom into the right part of the plot where the bars are hard to see.
density_histograms +
  coord_cartesian(xlim = c(3, 30), ylim = c(0, 20))

# I see a few choices that we have to make in order to decide on a strategy for
# homogenizing the point density:

# 1. We can remove the noise and overlapping points beforehand, like you
# suggested. I think we should do that, but the only reasons I have are that
# "the plots look good" and that the LAS specification
# (https://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf, pg. 11)
# states for overlapping points: "Overlap Points are those points that were
# immediately culled during the merging of overlapping flight lines.". I haven't
# looked for processing details in the data documentation yet.
#
# 2. We have to decide on a point density at which we want to sample the point
# clouds for homogenization.
# We could choose something like a minimum density where most square meters
# would have at least that many points. Maybe that would be around 0.5 or 1
# point/m2. Probably 0.5 would be the absolute minimum since the documentation
# lists that value.
# Another option would be to choose a point density that results in something
# similar to a normal distribution of densities. The density histogram of the
# point cloud that had its overlapping points removed looks very similar to a
# normal distribution. If we take this approach and limit point density to
# somewhere around 3 to 4 points/m2 we would have more points for the tree
# detection but we would also have to make sure that the different point
# densities are distributed homogenously.

# This could be the minimum density homogenization:
las_homogenized <- lidR::decimate_points(las_filtered,
                                         lidR::homogenize(density = 1, res = 4)
)
# This results in point densities that look somewhat normally distributed:
las_homogenized_to_normal <- lidR::decimate_points(las_filtered,
                                                   lidR::homogenize(density = 2.5, res = 6)
)

point_density_homogenized <- lidR::grid_density(las_homogenized, res = 4)
point_density_homogenized_to_normal <- lidR::grid_density(
  las_homogenized_to_normal,
  res = 6
)

raster::plot(point_density_homogenized)
raster::plot(point_density_homogenized_to_normal)

table(point_density_homogenized@data@values)
# The density that was homogenized to follow the normal distribution is not
# distributed homogenously in the whole area. In most places it follows the
# valleys and ridges.
# The most homogenous density distribution is returned with the minimum density
# approach for the parameters density = 0.5, res = 4 for the lidR::homogenize
# algorithm. At this point about 99 percent of the area has a point density of
# 0.5 points/m2, the rest is below that value. This "very good" result is
# probably also caused by the fact that the lidR::grid_density function uses a
# resolution of 4.
# However, the point cloud that was homogenized to normal has nearly three times
# more points than the one that was homogenized to 0.5 points/m2.

point_densities_extended <- bind_rows(point_densities,
                                      data.frame(
                                        source = "homogenized",
                                        density = point_density_homogenized@data@values
                                      ),
                                      data.frame(
                                        source = "homogenized to normal",
                                        density = point_density_homogenized_to_normal@data@values
                                      )
)

density_histograms$data <- point_densities_extended
density_histograms

density_histograms +
  coord_cartesian(xlim = c(3, 30), ylim = c(0, 20))

# I think for this normal distribution approach, ideally we would need an
# algorithm that doesn't choose points until the target density is reached but
# an algorithm that removes points as long as there are more than the target
# density requires. This could be programmed by us, but let's first check the
# difference in individual tree detection between the minimum homogenous point
# density and the higher, pseudo-normally distributed density.

# Conclusion:
# If we need point densities higher than 0.5 points/m2 for the individual tree
# detection, then we first need to investigate the relationship between point
# density and slope/slope orientation.


# Classify Ground Points --------------------------------------------------

# Because the ground classification that is included in the data set does not
# seem to be very good, I tried out the ground classification algorithms that
# are provided by the lidR package. One of them came up with a classification
# that looks better than the original one. However, I have not yet checked how
# much of a difference the "better" ground classification makes for the
# normalization. If we perform the ground classification on our own, we also
# need to think about whether we want to homogenize the point density before or
# after that.

# There are two different algorithms that can be used with the function below.
# See ?lidR::classify_ground for details. Apparently both are from the same
# author but the CSF algorithm is from 2016 while the PMF algorithm is from
# 2003. I tried both and the CSF algorithm seems to perform better, even though
# it takes longer to compute. It also looks like this algorithm classifies
# ground returns better than the original classification.
own_ground_classification <- lidR::classify_ground(
  las_subtile,
  algorithm = lidR::csf(sloop_smooth = TRUE)
)

# Plot the "original" classification and the one performed by the algorithm
plot_las(las_subtile, color = "Classification", legend = TRUE)
plot_las(own_ground_classification, color = "Classification", legend = TRUE)


# Homogenize Point Density ------------------------------------------------

# Running lidR::lascheck(las_subtile) reveals that there are some duplicate
# points. Maybe we should use the function lidR::lasfilterduplicates() before we
# randomly remove any points.
