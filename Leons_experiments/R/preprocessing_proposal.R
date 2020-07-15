source("R/utility.R")


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
#   "../../Data/las_files/PNOA_2016_CANAR-LP_212-3184_ORT-CLA-RGB.las"
# )
# las_tile <- lidR::readLAScatalog(
#   "../../Data/las_files/"
# )
# Create .lax files with the lasindex command line tool that comes with LAStools

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
las_subtile <- las_clip_relative_rectangle(las_tile, width = 500)


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
# See ?lidR::lasground for details. Apparently both are from the same author but
# the CSF algorithm is from 2016 while the PMF algorithm is from 2003. I tried
# both and the CSF algorithm seems to perform better, even though it takes
# longer to compute. It also looks like this algorithm classifies ground returns
# better than the original classification.
own_ground_classification <- lidR::lasground(
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
