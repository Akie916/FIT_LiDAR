## version AFTER second meeting on 16th July 2020
require(crownsegmentr)
require(lidR)
require(rgl)


## Preprocess .Las data ###########################

## Steps are below:
## 1) Remove class 7 and 12
## 2) Filter duplicates
## 3) Normalize with the tin algorithm
## 4) exclude all points below a certain height and above a certain heigt
## 5) homogenize points
## 6) individual tree segmentation
## 7) combine terrain information with point cloud


## Read .las data --------------------------------------------------------------

tile <- lidR::readLAS("../../Data/PNOA_2016_CANAR-LP_218-3178_ORT-CLA-RGB.laz") 
## Data downloaded from http://centrodedescargas.cnig.es/CentroDescargas/locale?request_locale=en

## Make subet for faster analysis
subset_las <- lidR::clip_rectangle(tile,
                                   tile@header@PHB$`Min X`,
                                   tile@header@PHB$`Min Y`,
                                   tile@header@PHB$`Min X` + 500,
                                   tile@header@PHB$`Min Y` + 500)

## 1) Remove class 7 and 12  -------------------------------------------------------

## remove class 12 and 7 which are overlap and error
filtered <- lidR::lasfilter(subset_las, Classification != 12 & Classification != 7)
lidR::lascheck(filtered)

## 2) Filter duplicates  -------------------------------------------------------

## remove duplicated points
filtered_wo_duplicates <- lidR::lasfilterduplicates(filtered)


## 3) Normalize with the tin algorithm  -------------------------------------------------------

Normalized <- lidR::normalize_height(filtered_wo_duplicates,algorithm = lidR::tin())
lidR::plot(Normalized, color = "Classification")


## 4) Remove outliers  -------------------------------------------------------

## remove outliers above 1 m
normalized_above1 <- lidR::lasfilter(Normalized, 1 < Z)

## I would agree with Leon that the outlier for below can be Z < 1m because
## there was not huge difference found in terms of processing time on my computer either.
## However, I would say outlier for above should be smaller. In my tile, there are not
## extreme outlier like Z = 75, but outliers around 30m. It seems that trees along the valley
## are extended. I tried segmentation without removing those outliers, but they were recognized
## as point for tree crowns. Therefore, somehow we need to remove them.

## Remove those outliers by filtering out the edge of distribution
hist(normalized_above1@data$Z)
Z_mean <- mean(normalized_above1@data$Z)
Z_std <- sd(normalized_above1@data$Z)

normalized_woOutliers <- lidR::lasfilter(normalized_above1, Z < (Z_mean + 3 * Z_std))
## that reduced about 400 points
hist(normalized_woOutliers@data$Z)
lidR::plot(normalized_woOutliers, color = "Classification")


## 5) Homogenize points  -------------------------------------------------------

Homogenized <- lidR::decimate_points(normalized_woOutliers, algorithm = lidR::homogenize(density = 0.5, res = 4))

raster::plot(lidR::grid_density(Homogenized))
lidR::plot(Homogenized, color = "Classification")



## 6) Segment individual trees  -------------------------------------------------------

## Segment trees
segmented_trees <- crownsegmentr::segment_tree_crowns(Homogenized@data,
                                                      crown_diameter_2_tree_height = 1 / 2,
                                                      crown_height_2_tree_height = 4 / 5,
                                                      return_modes = TRUE)

segmented_trees_las <- lidR::lasadddata(Homogenized,
                                        segmented_trees$crown_id,
                                        name = "crown_id")
col <- random.colors(500)
lidR::plot(lidR::lasfilter(segmented_trees_las,crown_id != 0), color = "crown_id", colorPalette = col)
rgl.close()


## 7) combine terrain information with point cloud -------------------------------------------------------

## Here I tried to create slope, aspect and canopy cover and combine with point cloud
## To use raster::terrain, input data had to have projection crs 
## La palma sits in utm zone 28N = EPSG:32628

crs <- sp::CRS("+init=epsg:32628")
projection(filtered_wo_duplicates) <- crs

## Create slope and aspect data from dtm
DTM <- lidR::grid_terrain(filtered_wo_duplicates,algorithm = tin())
raster::plot(DTM)
Slope <- raster::terrain(DTM, opt = "slope", unit="degrees", neighbors=8)
raster::plot(Slope)
Aspect <- raster::terrain(DTM, opt = "aspect", unit="degrees", neighbors=8)
raster::plot(Aspect)

## according the documentation for raster::terrain
## "When neighbors=4, slope and aspect are computed according to Fleming 
## and Hoffer (1979) and Ritter (1987). When neigbors=8, slope and aspect
## are computed according to Horn (1981). The Horn algorithm may be best 
## for rough surfaces, and the Fleming and Hoffer algorithm may be better 
## for smoother surfaces (Jones, 1997; Burrough and McDonnell, 1998)."


## Next, canopy cover was created by (non-ground points/all points)*100
## This attribute was just a trial. Reference is "ACCURACY ASSESSMENT OF 
## LIDAR-DERIVED DIGITAL TERRAIN MODEL (DTM)WITH DIFFERENT SLOPE AND CANOPY
## COVER IN TROPICAL FOREST REGION" by Mohd Radhie Mohd Salleh 

non_ground <- lidR::filter_poi(filtered_wo_duplicates, Classification != 2)
canopy_cover <- lidR::grid_density(non_ground, res = 5) *100 / lidR::grid_density(filtered_wo_duplicates, res = 5)
raster::plot(canopy_cover)


## Finally combine those new attributes to segmented points
projection(segmented_trees_las) <- crs

trees_with_attributes <- lidR::merge_spatial(lidR::filter_poi(segmented_trees_las, crown_id != 0), Slope, "slope")
trees_with_attributes <- lidR::merge_spatial(trees_with_attributes, Aspect, "aspect")
trees_with_attributes <- lidR::merge_spatial(trees_with_attributes, canopy_cover, "canopy_cover")

head(trees_with_attributes@data)

## Now, points which share the same tree crown id having different attribute values
## They need to be set as one value for one tree id somehow.
