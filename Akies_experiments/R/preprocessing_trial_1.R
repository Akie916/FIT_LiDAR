## version before second meeting on 16th July 2020
require(crownsegmentr)


## Preprocess .Las data ###########################

## Read .las data --------------------------------------------------------------

tile <- lidR::readLAS("../../Data/PNOA_2016_CANAR-LP_218-3178_ORT-CLA-RGB.laz") 
## Data downloaded from http://centrodedescargas.cnig.es/CentroDescargas/locale?request_locale=en

## Make subet for faster analysis
subset_las <- lidR::clip_rectangle(tile,
                                   tile@header@PHB$`Min X`,
                                   tile@header@PHB$`Min Y`,
                                   tile@header@PHB$`Min X` + 500,
                                   tile@header@PHB$`Min Y` + 500)

## filter overlap data  -------------------------------------------------------

## remove class 12 and 7 which are overlap and error
filtered <- lidR::lasfilter(subset_las, Classification != 12 & Classification != 7)
lidR::lascheck(filtered)

## remove duplicated points
filtered_wo_duplicates <- lidR::lasfilterduplicates(filtered)

## So, I would conclude ground classification should not be done manumally
lidR::plot(lidR::lasfilter(filtered_wo_duplicates, Classification == 2), color = "Classification")



## Ground classification  -------------------------------------------------------

## My thoughts: Ground classifiation algorithm classifies all points again.
## So, without removing unwanted points (such as class 12, 7) before reclassification,
## points which were originally classified 12 or 7 would be classified as ground. It would make 
## ground points more dense, but I am not sure... If we use them(class 12&7) for ground, data
## will be more heterogeneous.
## However, ground points will be only used for making DTM, so more points for ground would be
## better even though it's heterogeneous???


## re-classify ground after filtering out class 7 & 12, and duplicates
filtered_own_classified_ground <- lidR::lasground(
  filtered_wo_duplicates,
  algorithm = lidR::csf(sloop_smooth = TRUE)
)

lidR::plot(lidR::filter_poi(filtered_own_classified_ground, Classification == 2), color = "Classification")

## Memo:: Oops ground points looks so sparse
## So, I would conclude ground classification should not be done manumally
lidR::plot(lidR::lasfilter(filtered_wo_duplicates, Classification == 2), color = "Classification")



## preprocess proposal ----------------------------------------------------------------


## After normalization, there would be outliers depending on the terrain.
## So, maybe homogenize should be the last, which means just before the segmentation.
## Remove class 12&7 >>> Remove duplicates >>> Normalize >>> Homogenize would be the best(?)
## However, I'm not quite sure how to evaluate the quality of results


## Get point cloud normalized
normalized <- lidR::normalize_height(filtered_wo_duplicates,algorithm = lidR::tin())
lidR::plot(normalized, color = "Classification")
## outliers (z<0 and some extended trees) along valley 

## Remove outliers
normalized_RemoveOutliers <- lidR::lasfilter(normalized,Z<28&Z>2)
lidR::plot(normalized_RemoveOutliers, color = "Classification")


## Homogenize thorought area
normalized_RemoveOutliers_homogenized <- lidR::decimate_points(normalized_RemoveOutliers, lidR::homogenize(density = 0.5, res = 4))
raster::plot(lidR::grid_density(normalized_RemoveOutliers_homogenized))
lidR::plot(normalized_RemoveOutliers_homogenized, color = "Classification")


## Segment trees
segmented_trees <- crownsegmentr::segment_tree_crowns(normalized_RemoveOutliers_homogenized@data,
                                                      crown_diameter_2_tree_height = 3 / 10,
                                                      crown_height_2_tree_height = 5 / 10,
                                                      return_modes = TRUE)

segmented_trees_las <- lidR::lasadddata(reduce_duplicate_normalized_RemmoveOutlier_thinned,
                                        segmented_trees$crown_id,
                                        name = "crown_id")
col <- random.colors(500)
lidR::plot(lidR::lasfilter(segmented_trees_las,crown_id != 0), color = "crown_id", colorPalette = col)
rgl.close()




