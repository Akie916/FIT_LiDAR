## version AFTER second meeting on 23rd July 2020
require(crownsegmentr)
require(lidR)
require(rgl)
require(sp)
## Preprocess .Las data ###########################


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
                                 chunk_size = 2000,
                                 chunk_buffer = 200
                                 )

## 2) Filter duplicates  -------------------------------------------------------

## for chunk processing (0 = the same size as the tile size)
lidR::opt_chunk_size(las_data) <- 0
lidR::opt_chunk_buffer(las_data) <- 30
lidR::opt_select <- select = "xyzic"
## To save in .laz format
lidR::opt_laz_compression(las_data) = TRUE

## set the path to save in a tamp folder
lidR::opt_output_files(las_data) <- paste0(tempdir(), "/{*}_wo_duplicate")

las_data_WOduplicate <- lidR::filter_duplicates(las_data)

## 3) Normalize with the tin algorithm  -------------------------------------------------------

## set the path to save in a tamp folder
lidR::opt_output_files(las_data_WOduplicate) <- paste0(tempdir(), "/{*}_Normalized")

las_data_Normalized <- lidR::normalize_height(las_data_WOduplicate,
                                              algorithm = lidR::tin())

## All chunks got a warning saying some degenerated ground points about 500-2000
## According to StackExchange: 1) Multiple points in the cloud at the same location are dropped. 
## 2) Multiple points with the same XY location but different Z location (height) are 
## inconsistent with the idea of a planar 2.5D terrain model, 
## and in this case grid_terrain will take the smallest value of Z.
## So we don't have to think too much about this warning imo


## 4) Remove outliers  -------------------------------------------------------

## set the temp path for output files
lidR::opt_filter(las_data_Normalized) <- "-drop_z_below 1"

## 6) remove buffer around  -------------------------------------------------------

lidR::opt_output_files(las_data_Normalized) <- paste0(tempdir(), "/WObuffer")

Buffer_width <- 0.5

## Removing buffer
las_data_woBuffer <- lidR::clip_rectangle(las_data_Normalized,
                                          xleft = las_data_Normalized@bbox["x", "min"] + Buffer_width,
                                          ybottom = las_data_Normalized@bbox["y", "min"] + Buffer_width,
                                          xright = las_data_Normalized@bbox["x", "max"] - Buffer_width,
                                          ytop = las_data_Normalized@bbox["y", "max"] - Buffer_width)



## 7) Homozineze point density -------------------------------------------------------

lidR::opt_output_files(las_data_woBuffer) <- paste0(tempdir(), "/{XLEFT}_{YTOP}_Homogenized")
## reset the chunk size because after removing the buffer the number of chunks became 1
lidR::opt_chunk_size(las_data_woBuffer) <- 2000

las_data_Homogenized <- lidR::decimate_points(las_data_woBuffer, 
                                              algorithm = lidR::homogenize(density = 0.5, res = 4))

## Save this not in temp but actual folder
# dir.create("../../Data/Preprocessed")
# file.copy(from = paste0(tempdir(),list.files(tempdir(),"_Homogenized\\.laz")),
#           to   = "../../Data/Preprocessed/")


## 8) Segment individual trees  -------------------------------------------------------

## I cannot find the way to do segmentation from LAScatalog
## Therefore, anyway first read as LAS
Preprocessed_las <- lidR::readLAS(list.files("../../Data/Preprocessed/",
                                  pattern = "_Homogenized*\\.laz",
                                  full.names = TRUE),
                                  select = "xyzic")

## Segment trees
segmented_trees <- crownsegmentr::segment_tree_crowns(Preprocessed_las@data,
                                                      crown_diameter_2_tree_height = 5 / 10,
                                                      crown_height_2_tree_height = 5 / 10,
                                                      return_modes = TRUE)


segmented_trees_las <- lidR::lasadddata(Preprocessed_las,
                                        segmented_trees$crown_id,
                                        name = "treeID")

## 8) Delineate crowns  -------------------------------------------------------

## Create subset to test it because it will take forever....
subset_las <- lidR::clip_rectangle(segmented_trees_las,
                                   xleft = segmented_trees_las@bbox["x", "min"] +600,
                                   ybottom = segmented_trees_las@bbox["y", "min"] +600,
                                   xright = segmented_trees_las@bbox["x", "min"] + 1100,
                                   ytop = segmented_trees_las@bbox["y", "min"] + 1100 )


f <- function(z) {
  list(
    Zmean = mean(z), 
    Zmax = max(z)
  )  
}
lidR::plot(subset_las)
crowns <- lidR::delineate_crowns(subset_las,
                                 type = "convex",
                                 func = ~f(Z))
crowns$Area_sqm <- raster::area(crowns)

min(crowns$Area_sqm)
max(crowns$Area_sqm)

sp::spplot(crowns, "Zmax")

## Area of tree crown looks very diverse, which I am not sure is okay to proceed.
## Well, I think it can happen some trees are only detected around tree top,
## and they'll show small crown area compared to ones which recieved points from lower port of crown.
## So the question is: Crown area in this small 500x500m^2 subset ranges 0.00067 - 249866.8 m^2 (mean 113 m^2)
## What do you think? Is it feasible? Or will be somehow remove them here?

## 9) Merge attributes to polygon ----------------------------------------------------------------

## Create slope and aspect data from dtm
lidR::opt_output_files(las_data_WOduplicate) <- paste0(tempdir(), "/{*}_tin")
DTM <- lidR::grid_terrain(las_data_WOduplicate,
                                          algorithm = lidR::tin())

## Extract DTM values on polygon with Zonal Statistics
crowns_with_DTM <- raster::extract(DTM, crowns, fun=mean, na.rm=TRUE, df=TRUE)

## However it took too long, could not process with my laptop....



## In conclusion, 
## 1) How to remove errors?
## 2) Should we do polygon? or from point only?
