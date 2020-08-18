remove(list = ls())
## version AFTER second meeting on 12nd August 2020

## Package "crownsegmentr" is an updated version at 18th Aug 2020
## Package "lidR" is version 3.0.3

## Preprocess .Las data ###########################

## What we discussed
## 1. Polygon (delineate crown) will give information on crown size, but let's do points first
## 2. First, include all trees regardless of the error, then afterward somehow remove?
## 3. Tree segmentation parameter? 1/2 and ...? Need to check

## Set paralellization parameters (from Leon) ------------------------------------------
future::plan(strategy = future::multisession)


## Read .las data using LAScatalog engine --------------------------------------------------------------

## Even though we discussed check out LAStools,
## on Mac OS, I could not use LAStools ><
## reference: https://jean-romain.github.io/lidRbook/engine.html

# readLAS(filter = "-help")
las_data <- lidR::readLAScatalog(list.files("../../Data/",
                                            pattern = "*CIR\\.laz",
                                            full.names = TRUE),
                                 filter = "-drop_class 7 12 -drop_overlap",
                                 select = "xyzic",
                                 chunk_size = 1000,
                                 chunk_buffer = 200
)

## 2) Filter duplicates  -------------------------------------------------------

## for chunk processing (0 = the same size as the tile size)
lidR::opt_chunk_size(las_data) <- 1000
lidR::opt_chunk_buffer(las_data) <- 30
lidR::opt_select <- "xyzic"
## To save in .laz format (with this, you don't have to set output option to .laz every time)
lidR::opt_laz_compression(las_data) <- TRUE 

## Create folder to save preprocess files (from Leon)
catalog_output_directory <- "../../Data/output/catalog_processing/"
## set the path to save in a tamp folder
lidR::opt_output_files(las_data) <- paste0(catalog_output_directory, "filtered_x_{XLEFT}_y_{YBOTTOM}")

filtered_las_data <- lidR::filter_duplicates(las_data)

## 3) Normalize with the tin algorithm  -------------------------------------------------------

## set the path to save in a tamp folder
lidR::opt_output_files(filtered_las_data) <- paste0(catalog_output_directory, "normalized_x_{XLEFT}_y_{YBOTTOM}")

normalized_las_data<- lidR::normalize_height(filtered_las_data,
                                             algorithm = lidR::tin())

## All chunks got a warning saying some degenerated ground points about 500-2000
## According to StackExchange: 1) Multiple points in the cloud at the same location are dropped. 
## 2) Multiple points with the same XY location but different Z location (height) are 
## inconsistent with the idea of a planar 2.5D terrain model, 
## and in this case grid_terrain will take the smallest value of Z.
## So we don't have to think too much about this warning imo


## 4) Remove outliers  -------------------------------------------------------

## Drop points less than 1 m as an outlier
lidR::opt_filter(normalized_las_data) <- "-drop_z_below 1"


## 6) remove buffer around  -------------------------------------------------------

lidR::opt_output_files(normalized_las_data) <- paste0(catalog_output_directory, "no_ground_edge_clipped")

Buffer_width <- 0.5

## Removing buffer
no_ground_las_data <- lidR::clip_rectangle(normalized_las_data,
                                           xleft = normalized_las_data@bbox["x", "min"] + Buffer_width,
                                           ybottom = normalized_las_data@bbox["y", "min"] + Buffer_width,
                                           xright = normalized_las_data@bbox["x", "max"] - Buffer_width,
                                           ytop = normalized_las_data@bbox["y", "max"] - Buffer_width)

## Just to make sure if the drop Z was working
# test <- lidR::readLAS(paste0(catalog_output_directory, "no_ground_edge_clipped.laz"))
# range(test@data$Z)

## 7) Homozineze point density -------------------------------------------------------

lidR::opt_output_files(no_ground_las_data) <- paste0(catalog_output_directory, "homogenized_x_{XLEFT}_y_{YBOTTOM}")
## reset the chunk size because after removing the buffer the number of chunks became 1
lidR::opt_chunk_size(no_ground_las_data) <- 1000
lidR::opt_chunk_buffer(no_ground_las_data) <- 30

homogenized_las_data <- lidR::decimate_points(no_ground_las_data, 
                                              algorithm = lidR::homogenize(density = 0.5, res = 4))

## 8) Segment individual trees  -------------------------------------------------------

## Tree segmentation can be done in LAS format not LAScatalog format
homogenized_points <- lidR::readLAS(
  list.files(catalog_output_directory,
             pattern = "homogenized_.*\\.laz",
             full.names = TRUE
  )
)

## Segment trees
segmented_points <- crownsegmentr::segment_tree_crowns(point_cloud = homogenized_points@data,
                                                       crown_diameter_2_tree_height = 1 / 2,
                                                       crown_height_2_tree_height = 1)


segmented_points <- lidR::add_lasattribute(homogenized_points,
                                           segmented_points$crown_id,
                                           name = "treeID",
                                           desc = "Tree Crown ID"
)

## treeID 0 is the collection of points which were not segmented as tree


lidR::writeLAS(segmented_points,
               file = "../../Data/output/segmentation/segmented.laz"
)



## To compare if we have the same data, I'll list how my data look like below.
## Segmented tree ID: 0 - 534469
## For example, treeID 1 consists of 8 points: x coordinate = 220998.5 - 221000.0,
## treeID 1000 consists of 5 points: x coordinate = 220892.1 - 220895.7
# range(segmented_points@data$treeID)
# segmented_points@data[segmented_points@data$treeID == 1000]


# Calculate and Merge Data for Analysis (Based on Leon's code) -------------------------

filtered_points <- lidR::readLAScatalog(
  list.files(catalog_output_directory,
             pattern = "filtered_.*\\.laz",
             full.names = TRUE
  )
)

# Create DTM from filtered(without duplicates and class 7,12) points
terrain_height <- lidR::grid_terrain(filtered_points,
                                     res =  0.5,
                                     algorithm = lidR::tin()
)
## >>>I'm not sure if we should remove the edges of this raster in order to account
## >>>for the edge effects or if having removed the points at the edge of the point
## >>>cloud was already enough. At least the plot looks good:
## >>>raster::plot(terrain_height).
## I think we don't have to think about the edge because as you said, 
## we will not use the edge of this raster anyway

# raster::writeRaster(terrain_height,
#                     filename = paste0(catalog_output_directory, "terrain_height")
# )


segmented_points <- lidR::readLAS(
  "../../Data/output/segmentation/segmented.laz"
)
terrain_height <- raster::raster(
  paste0(catalog_output_directory, "terrain_height")
)
raster::plot(terrain_height)


segmented_points <- lidR::merge_spatial(segmented_points,
                                        source = terrain_height,
                                        attribute = "terrain_height")

# when using this function instead of lidR::add_attribute, the attribute is
# retained when a LAS file is written
segmented_points <- lidR::add_lasattribute(segmented_points,
                                           name = "terrain_height",
                                           desc = "terrain height"
)

# Create slope and aspect based on DTM
slope_and_aspect <- raster::terrain(terrain_height,
                                    opt = c("slope", "aspect"),
                                    unit = "degrees",
                                    neighbors = 8
)

segmented_points <- lidR::merge_spatial(segmented_points,
                                        source = slope_and_aspect$slope,
                                        attribute = "slope"
)

segmented_points <- lidR::add_lasattribute(segmented_points,
                                           name = "slope",
                                           desc = "slope"
)

segmented_points <- lidR::merge_spatial(segmented_points,
                                        source = slope_and_aspect$aspect,
                                        attribute = "aspect"
)
segmented_points <- lidR::add_lasattribute(segmented_points,
                                           name = "aspect",
                                           desc = "aspect"
)

lidR::writeLAS(segmented_points,
               "../../Data/output/segmentation/segmented_with_data.laz",
)


## Maybe delineate crown and add the area of crown, or canppy cover as another attribute?
## But I really cannot perform that with my laptop now, so let me skip that


## Calculate metrics for all points ------------------------------------------------

## What I am trying to do: Extract datafrmae then join it later to LAS

segmented_points_dataframe <- segmented_points@data

library(dplyr)
segmented_points_metrics <- segmented_points_dataframe %>%
  group_by(treeID) %>%
  summarise_at(vars(c(Z,terrain_height,slope,aspect)), list(min = min, max = max, mean = mean))

segmented_points_metrics <- as.data.frame(segmented_points_metrics)

## update values for treeID 0 as NA
segmented_points_metrics[1,c(2:13)] <- NA

## save the dataframe
saveRDS(segmented_points_metrics, file = "../../Data/output/segmentation/points_metrics_dataframe.rds")

## this can be merged by crown polygon by treeID as a key
## haven't tried it yet but sp::merge should do the job
?sp::merge

