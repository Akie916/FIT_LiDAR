### This is a trial on statistic analysis with crown polygon
### , which created by Leon on 20 Aug 2020
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
## Read file ---------------------------------------------------
crown_polygon <- readOGR("../../Data/crown_hulls_with_data.gpkg")

## Extract dataframe for further analysis
crown_dataframe <- crown_polygon@data
crown_dataframe[,]
## See what the data look like and plan which step to be taken


########################################
## First, plot "Slope vs tree height" ##
########################################

## Assumption 1) tree height is somehow correlated to Slope
## Like, steep slope <=> less trees <=> shorter?
## (Other relationship was not very clearly seen by my eyes)

## First of all, let's plot the data to visually judge the relationship
plot(crown_dataframe$slope_at_max_z,crown_dataframe$max_z)
## -> From the plot "slope VS tree height"", it is assumed that
##    the bigger the slpe is, the higher the possibility of the distorsion
##    (the obvious errors such as Z > 35 are seen on the steep area more often than flat area) 

## -> The question here is that is the error somehow occurred in a predictable way?
## It is converted into "Are there any differences in mean of tree height depending on the slope?"
## To discuss this, slope values were splitted in 4 classes 0-20, 20-40, 40-60, 60-, and then visualized

## Create and assign slope category 
crown_dataframe$slope_category <- ifelse(crown_dataframe$slope_at_max_z < 20, "0-20",
                                         ifelse(crown_dataframe$slope_at_max_z >= 20 & crown_dataframe$slope_at_max_z < 40, "20-40",
                                                ifelse(crown_dataframe$slope_at_max_z >= 40 & crown_dataframe$slope_at_max_z < 60, "40-60","60-")))
## See the number for each class
crown_dataframe %>% count(slope_category)
## -> So the number of segmented trees are most found in slope 20-40 m. 
##    This is understandable, because trees would prefer moderate slope, but 0-20m can be
##    around ridges and valley. 
## -> thoughts: Should those groups have the same amount of data to discuss????

## Plot histogram group by the slope category which is created above
ggplot(crown_dataframe, aes(x = max_z)) +
  geom_histogram(aes(color = slope_category, fill = slope_category), 
                 position = "identity", bins = 30, alpha = 0.4)
## -> from this, mean tree height seems same-ish regardess of the slope, interestingly.
##    (maybe the peak of tree height for category 0-20 and 60- seem to be a bit smaller)
## -> As it was assumed from the "slope VS tree height" plot, steeper slopes seems to show
##    more distorted trees; like tree height = 80m


## Also, for furher usage I just created the categories for terrain height and aspect as well

## For terrain height
crown_dataframe$terrain_heigt_category <- ifelse(crown_dataframe$terrain_height_at_max_z < 1000, "-1000", 
                                                                ifelse(crown_dataframe$terrain_height_at_max_z < 1500 & crown_dataframe$terrain_height_at_max_z >= 1000 , "1000-1500",
                                                                       ifelse(crown_dataframe$terrain_height_at_max_z < 2000 & crown_dataframe$terrain_height_at_max_z >= 1500 , "1500-2000",
                                                                              "2000-"
                                                                       )))
## For aspect (Aspect values: 0 = North)
range(crown_dataframe$aspect_at_max_z) ## no -1 (= flat)

crown_dataframe$aspect_category <- ifelse(crown_dataframe$aspect_at_max_z <= 22.5 | crown_dataframe$aspect_at_max_z > 337.5, "North", 
                                                 ifelse(crown_dataframe$aspect_at_max_z > 22.5 & crown_dataframe$aspect_at_max_z <= 67.5 , "North East",
                                                        ifelse(crown_dataframe$aspect_at_max_z > 67.5 & crown_dataframe$aspect_at_max_z <= 112.5 , "East",
                                                               ifelse(crown_dataframe$aspect_at_max_z > 112.5 & crown_dataframe$aspect_at_max_z <= 157.5 , "South East",
                                                                      ifelse(crown_dataframe$aspect_at_max_z > 157.5 & crown_dataframe$aspect_at_max_z <= 202.5 , "South",
                                                                             ifelse(crown_dataframe$aspect_at_max_z > 202.5 & crown_dataframe$aspect_at_max_z <= 247.5 , "South West",
                                                                                    ifelse(crown_dataframe$aspect_at_max_z > 247.5 & crown_dataframe$aspect_at_max_z <= 292.5 , "West",
                                                                                           "North West"
                                                        )))))))


## Split data into errors and non-errors --------------------------------------------------------

## So, next, let's split the data into good-looking data and errors, but the question is how?
## I think we can exclude errrors using 1) tree height, then 2) crown size.

## 1) Split data with tree height -------------------------------

## It is very interesting but Wikipedia on La Palma is described way more detailed in German than in English
## Anyway, according to Wikipedia (https://de.wikipedia.org/wiki/La_Palma#Flora_und_Vegetation),
## the vegetation is of course depending on elevation and they are categorized into 5 classes.
## 1) Küstenzone (bis 500 m), 2) Lorbeerwälder (500–1000 m), 3) Baumheide (1000–1500 m)
## 4) Kiefernwald (1500–2000 m), 5) Subalpine Hochgebirgsformen (ab 2000 m)
range(crown_dataframe$terrain_height_at_max_z)
## Because our study area's elevation is 585 - 2338 m, we just need to think about class 2,3 and 4.
## And on Wikipedia, they say that Lorbeerwälder grows up to 30m, and Baumheide up to 20m.
## For Kiefernwald, the main spiece is Canary Pine, and again I asked 
## Wikipedia (https://en.wikipedia.org/wiki/Pinus_canariensis), and it says they grow up to 40m
## Also, I checked a few papers and I think tree height less than 2.5m can also be omitted

## So, first, I set threshold Z < 40 & Z > 2.5.
## Here, the new field, "error_by_z" is created and numbers were assigned as
## Z < 2.5: 1, Z > 40: 2, and others: 0
crown_dataframe$error_by_z <- ifelse(crown_dataframe$max_z < 2.5, 1,
                                     ifelse(crown_dataframe$max_z > 40, 2, 0))

## Number of trees excluded by this threshold
nrow(crown_dataframe)-nrow(crown_dataframe[crown_dataframe$error_by_z==0,])
## -> 2084 (Z>40: 1298, Z<2.5: 786)

## Plot histograms after applying threshold of height
ggplot(crown_dataframe[crown_dataframe$error_by_z==0,], aes(x = max_z)) +
  geom_histogram(aes(color = slope_category, fill = slope_category), 
                 position = "identity", bins = 30, alpha = 0.4)
## All slope category seem to follow normal distribution.

## Create histogram of the errors to see how erros of height occurred
ggplot(crown_dataframe[crown_dataframe$error_by_z!=0,], aes(x = max_z)) +
  geom_histogram(aes(color = slope_category, fill = slope_category), 
                 position = "identity", bins = 30, alpha = 0.4)
## -> from this, trees distorted larger is seen most in category 60-
##    On the other hand, small trees detected was less in that category compared to others
## -> Which makes sense. Areas with steep slope probably didn't have enogh ground points
##    and holes on the terrain were filled by TIN processing. So those areas can have unreliable 
##    terrain height compered


## -------------------------------------------------------------------------------------------
## Below, I tried to give threshold depending on the terrain heihgt considering vegetation category from Wikipedia
## First of all, new column which have information if the tree should be considered as an error (= 1) is created
# crown_dataframe$errors <- ifelse(crown_dataframe$terrain_height_at_max_z < 1000 & crown_dataframe$max_z > 30, 1,
#                                  ifelse(crown_dataframe$terrain_height_at_max_z >= 1000 & crown_dataframe$terrain_height_at_max_z < 1500 & crown_dataframe$max_z > 20, 1,
#                                         ifelse(crown_dataframe$terrain_height_at_max_z >= 1500 & crown_dataframe$max_z > 40, 1, 0)))
## Let's see where those errors occur
# ggplot(crown_dataframe, aes(x = max_z)) +
#   geom_histogram(aes(color = as.factor(errors), fill = as.factor(errors)),
#                  position = "identity", bins = 30, alpha = 0.4)
# ggplot(crown_dataframe, aes(x = convex_area)) +
#   geom_histogram(aes(color = as.factor(errors), fill = as.factor(errors)), 
#                  position = "identity", bins = 30, alpha = 0.4)
## Create subsets without errors
# crown_dataframe_without_errors <- subset(crown_dataframe, errors == 0)
## -> Well, I guess it excluded too many trees. Transition of vegetation is continuous,
##    so I would think this would make more sence than just having Z>40 as a threshold
## -------------------------------------------------------------------------------------------


## 2) Split data with crown size -------------------------------

## Create field for crown size error
crown_dataframe$error_by_crown <- ifelse(crown_dataframe$convex_area > 100, 1, 0)
## Reference on crown size were not found, so I just picked 100.
## -> Need to be search more....


## Number of trees excluded by this threshold
nrow(crown_dataframe)-nrow(crown_dataframe[crown_dataframe$error_by_crown==0,])
## -> 16506 

## Plot histogram (group by the crown error)
ggplot(crown_dataframe, aes(x = max_z)) +
  geom_histogram(aes(color = as.factor(error_by_crown), fill = as.factor(error_by_crown)), 
                 position = "identity", bins = 30, alpha = 0.4)
## -> from this, it looks like the crown size error not only occurs for tall trees,
##    but also some "normal height" trees.

## Number of trees excluded by both (height and crown size) thresholds
nrow(crown_dataframe[crown_dataframe$error_by_z != 0 & crown_dataframe$error_by_crown != 0,])
## -> 733

## -> So, there are not many overlaps between those errors.
##    It is assumed that tree height errors are mostly caused by missing terrain, judging from
##    the fact that steeper the slope is the more distortion there are.
##    However, the errors of crown area is probably caused by tree segmentation process...?

## create field for error combined
crown_dataframe$errors <- ifelse(crown_dataframe$error_by_z != 0 | crown_dataframe$error_by_crown != 0, 1, 0)
nrow(crown_dataframe[crown_dataframe$errors !=0,])

## Anyway, until here I somehow label trees either error or not depending on their tree height and crown size
## So, next step would be analyze the non-error data (Like "Are there any correlation between variables?")
## Plus, let's analyze where those errors were seen, by visualizing in QGIS (For my machine QGIS was way faster to handle polygons, so...)



## Statistic analysis for the "non-error" data --------------------------------------------------

## Create subset of the non-error data
crown_dataframe_without_errors <- subset(crown_dataframe, crown_dataframe$error_by_crown == 0 & crown_dataframe$error_by_z == 0 )


## Plot variables
## Crown area VS Tree height
plot(crown_dataframe_without_errors$max_z,crown_dataframe_without_errors$convex_area)
## -> It looks there are not much can be said...
##    At least, shorter trees didn't have big crowns.

## Terrain height VS tree height
plot(crown_dataframe_without_errors$terrain_height_at_max_z,crown_dataframe_without_errors$max_z)
## Slope VS tree height
plot(crown_dataframe_without_errors$slope_at_max_z,crown_dataframe_without_errors$max_z)
## Aspect vs tree height
plot(crown_dataframe_without_errors$aspect_at_max_z,crown_dataframe_without_errors$max_z)
## -> From these plots, my conclusion would be that seeing plots as it is would not give 
##    that much information. The best would be analyze the trend by grouping varialbes.
##    So, here, the data ware split grouped into categories (made above) and compared

###################################
## Slope category VS tree height ##
###################################
ggplot(crown_dataframe_without_errors, aes(x = max_z)) +
  geom_histogram(aes(color = slope_category, fill = slope_category), 
                 position = "identity", bins = 30, alpha = 0.4)

## One-way Anova to compare the mean of tree height on each slope category
slope.group.anova <- aov(max_z ~ slope_category, data = crown_dataframe_without_errors)
summary(slope.group.anova)
TukeyHSD(slope.group.anova)
## -> all pairs show P-value ≈ 0 <=> all paris having different means
##    So the conclusion is that the mean of tree heights are different 
##    among the slope category which I randomly created.
##    However, I cannot say if it is caused by biology or the impact of
##    preprocess was different depending on slope???

############################################
## terrain height category VS tree height ##
############################################
ggplot(crown_dataframe_without_errors, aes(x = max_z)) +
  geom_histogram(aes(color = terrain_heigt_category, fill = terrain_heigt_category), 
                 position = "identity", bins = 30, alpha = 0.4)

## One-way Anova to compare the mean of tree height on each terrain height category
terrain.group.anova <- aov(max_z ~ terrain_heigt_category, data = crown_dataframe_without_errors)
summary(terrain.group.anova)
TukeyHSD(terrain.group.anova)
## -> all pairs show P-value ≈ 0 <=> all paris having different means
mean(crown_dataframe_without_errors$max_z[crown_dataframe_without_errors$terrain_heigt_category == "-1000"])
mean(crown_dataframe_without_errors$max_z[crown_dataframe_without_errors$terrain_heigt_category == "1000-1500"])
mean(crown_dataframe_without_errors$max_z[crown_dataframe_without_errors$terrain_heigt_category == "1500-2000"])
mean(crown_dataframe_without_errors$max_z[crown_dataframe_without_errors$terrain_heigt_category == "2000-"])
## Means for tree height are as below:
## -1000    : 16.16
## 1000-1500: 14.85
## 1500-2000: 16.68
## 2000-    : 14.33
## So, the results aren't inconsistent with vegetation discription on Wikipedia as mentioned above 
## -> "Lorbeerwälder (which are seen at Elevation 500-1000m) grows up to 30m, 
##     and Baumheide(which are seen at Elevation 1000-1500m) up to 20m. Then canary pines can grow up to 40m,
##     which are seen at elevation 1500-2000m. Above 2000m, it is beyond the tree line."
## That is something at least:)

####################################
## Aspect category VS tree height ##
####################################
ggplot(crown_dataframe_without_errors, aes(x = max_z)) +
  geom_histogram(aes(color = aspect_category, fill = aspect_category), 
                 position = "identity", bins = 30, alpha = 0.4)

## One-way Anova to compare the mean of tree height on each terrain height category
aspect.group.anova <- aov(max_z ~ aspect_category, data = crown_dataframe_without_errors)
summary(aspect.group.anova)
TukeyHSD(aspect.group.anova)
## -> Most of pairs showed p-value < 0.05, but pairs "NW-E", "SE-NW", and "SW-S" showed 
##    larger p-value, which means null hypothesis was rejected for those pairs.
## However, I am not very much sure what we can say from that....Thoughts?


## ....Overall, I don't know if it is feasible to create category for continuous values in the first place.
## What's your opinion on that? Or is there any way to somehow analyze this data?



###################################################
## Error analysis: Is it predictable in any way? ##
###################################################

## Just tried to visualize as a map to locate where errors occur
crown_polygons <- sf::read_sf("../../Data/crown_hulls_with_data.gpkg") ## To use join, data should be read by sf
joined_polygon <- dplyr::left_join(crown_polygons,crown_dataframe)
write_sf(joined_polygon,"../../Data/crown_hulls_with_data_field_updated.gpkg")

## is there any ground truth data available?????
