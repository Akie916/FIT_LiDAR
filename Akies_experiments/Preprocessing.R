# rm(list=ls())
require(lidR)
lidR::set_lidr_threads(0)
require(rgl)
require(crownsegmentr)

## Preprocess .Las data ###########################

## Read .las data -----------------------------------

tile <- lidR::readLAS("../Data/PNOA_2016_CANAR-LP_218-3178_ORT-CLA-RGB.laz") 
## Data downloaded from http://centrodedescargas.cnig.es/CentroDescargas/locale?request_locale=en

## Make subet for faster analysis
tile_sample <- lidR::clip_rectangle(tile,
                                    tile@header@PHB$`Min X`,
                                    tile@header@PHB$`Min Y`,
                                    tile@header@PHB$`Min X` + 500,
                                    tile@header@PHB$`Min Y` + 500
                                    )



## Play around the data ----------------------------------------------------

## Visualize with 3d plot
lidR::plot(tile_sample, color = "Classification")
rgl.close()

## Explore point cloud
lidR::lasflightline(tile_sample,dt=30) 
## density = 1.92/unit even though the officital document says 0.5/unit

hist(tile_sample@data$Classification)
## more than 1/3 points are classified as class 12 (= overlap) 
## classification ID [2:ground, 3,4,5:low-, medium-, high-vegetation, 6:buildings, 7:noise, 12:Overlap]

## Plot point density
plot(lidR::grid_density(tile_sample,res=10))

## It seems like class 7&12 casuses heterogeneous point density
## In order to have homogeneous data, either 1) reduce points, or 2) create points should be done



## Try reduce points ----------------------------------------------------------

## Attempt 1: homogenize with function in lidR
thinned_1 <- lidR::decimate_points(tile_sample, homogenize(1,20)) 
# thinned_1_2 <- lidR::decimate_points(tile_sample, random(1))
# thinned_1_3 <- lidR::decimate_points(tile_sample,highest(1))
## Among 3 algorithms, homogenize looked the best
lidR::plot(thinned_1, color = "Classification")
rgl.close()
plot(lidR::grid_density(thinned_1, res=10))

## Attempt 2: remove points classified as class 7&12 > then homogenize
sample_reduced <- lidR::filter_poi(tile_sample, Classification != 12 | Classification != 12)
## Check if it looks homogeneous
lidR::plot(sample_reduced, color = "Classification")
rgl.close()
plot(lidR::grid_density(sample_reduced, res=10))
## Still some parts (probably ridge)
## Homoginize it
thinned_2 <- lidR::decimate_points(sample_reduced, homogenize(1,20))
lidR::plot(thinned_2, color = "Classification")
rgl.close()
plot(lidR::grid_density(thinned_2, res=10))
## Compare 2 attempts, the latter way gave more homogenious point density


## Try add points ----------------------------------------------------------------

## I could not find the way with lidR tool to generate points...



## Get point cloud normalized ----------------------------------------------------

## Normalize point cloud with the ground at 0 (subtract DTM created with tin algorithm)
## In order to see how homogenizing will affect delineating crowns,... 
## ...thinned and non-thinned data were normalized separately here
thinned_normalized <- lidR::normalize_height(thinned_2,
                                             algorithm = tin()
                                             )
lidR::plot(thinned_normalized, color = "Classification")
rgl.close()
## outliers (z<0 and some extended trees) along valley 

unthinned_normalized <-lidR::normalize_height(tile_sample,
                                              algorithm = tin()
                                              )
lidR::plot(unthinned_normalized, color = "Classification")
rgl.close()
## outliers (z<0 and some extended trees) along valley 


## Delineate crowns --------------------------------------------------------------

## Attempt 1: thinned data #######
## Reduce outliers
thin_normalized_above0 <- lidR::lasfilter(thinned_normalized,Z>0)
hist(thin_normalized_above0@data$Z)
## Z>35 looks like na error

thin_segmented <- crownsegmentr::segment_tree_crowns(lidR::lasfilter(thin_normalized_above0, Z < 35)@data,
                                                     crown_diameter_2_tree_height = 3 / 10,
                                                     crown_height_2_tree_height = 5 / 10,
                                                     return_modes = TRUE)

thin_segmented_las <- lidR::lasadddata(lidR::lasfilter(thin_normalized_above0, Z < 35),
                                       thin_segmented$crown_id,
                                       name = "crown_id"
                                       )
col <- random.colors(500)
lidR::plot(lidR::lasfilter(thin_segmented_las,crown_id != 0), color = "crown_id", colorPalette = col)
rgl.close()
## Error-looked line in the plot matches with flight line which could not removed (you can see in plot(lidR::grid_density(thinned_2, res=10)))




## Attempt 2: unthinned data #########
## Reduce outliers
unthin_normalized_above0 <- lidR::lasfilter(unthinned_normalized,Z>0)
hist(unthin_normalized_above0@data$Z)
## Z>35 looks like na error

unthin_segmented <- crownsegmentr::segment_tree_crowns(lidR::lasfilter(unthin_normalized_above0, Z < 35)@data,
                                                       crown_diameter_2_tree_height = 3 / 10,
                                                       crown_height_2_tree_height = 5 / 10,
                                                       return_modes = TRUE
                                                       )

unthin_segmented_las <- lidR::lasadddata(lidR::lasfilter(unthin_normalized_above0, Z < 35),
                                         unthin_segmented$crown_id,
                                         name = "crown_id"
                                         )

col <- random.colors(500)
lidR::plot(lidR::lasfilter(unthin_segmented_las,crown_id != 0), color = "crown_id", colorPalette = col)
rgl.close()
## Error-looked line in the plot matches with flight line which could not removed (you can see in plot(lidR::grid_density(thinned_2, res=10)))


## In conclusion, by comparing 2 segmented tree crowns, thinning, removing class 12 and 7 should be done, in my opinion.



