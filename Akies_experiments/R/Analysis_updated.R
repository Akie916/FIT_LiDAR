## modified on 7 Sep, 2020


## NOTE BEFORE THE SCRIPT -----------------------------------------------------

##############################
## Workflow of the project  ##
##############################

## 1) Create segmented tree crown polygons
## 2) Group the crown based on Land use data (modified CORINE 2018 data)
## -------↑ those parts were done in other scrpits or QGIS-----------
## -------↓ this script is working on below -----------
## 3) Split data into error/non-error by threshold <- NEED TO TEST AROUND
## 4) Analysis of error data 
##    -> Where? (Is there any trend depending on LU group?)
##    -> Visualize what was going on to those errors?
##    -> Comparison with non-error data (Location etc.)
## 5) Summarize the charasteristics of the study area
##    -> elevation, slope values on each LU
##    -> Error rate on each LU
##    -> Some visualization with AOIs <- in QGIS
## 6) Analysis of non-error data
##    -> Height wihtin group
##    -> Biomass estimation



###############################
## Data used in this script  ##
###############################

## 1) ../../Data/output/segmentation/crown_hulls_with_data.gpkg
##    -> Segmented crown polygons with geospatial data ( created by Leon on 20 Aug 2020)
##    -> It may be updated especially slope values (by changing the resolution)

## 2) ./../../Data/clc2018/LU_modified.tif
##    -> Raster image that stored LU info as pixel values
##    -> How it was prepared:
##      1) CORINE 2018 were downloaded (Shapefile)
##      2) Subclass for inside crater were introduced (ex) Coniferous -> coniferous, coniferous inside crater)
##      3) Coniferous was splitted into Dense and Sparse (I wrote line with the help of basemap)
##      4) It was rasterized and pixel value of it is stored as below
##          1: Bare rocks
##          2: Broad-leaved forest
##          3: Coniferous forest (Sparse)
##          4: Coniferous forest inside crater
##          5: Coniferous forest (Dense)
##          6: Moors and heathland
##          7: Sclerophyllous vegetaion
##          8: Sparsely vegetated areas
##          9: Sparsely vegetated areas inside crater
##         10: Transitional woodland-shrub (<- Honestly this group just looks as Broad-leaved forest)
##    -> This field was combined already with polygon as LU_majority field

## 3) ../../Data/Map_materials/shapefile/Corine2018_detailLU.shp
##    -> Polygon of raster above that stored terrain/slope information for each LU
##    -> How it was prepared:
##      1) CORINE 2018 were downloaded (Shapefile)
##      2) Subclass for inside crater were introduced (ex) Coniferous -> coniferous, coniferous inside crater)
##      3) Coniferous was splitted into Dense and Sparse (I wrote line with the help of basemap)
##      4) terrain/slope (raster) values were extracted on LU polygons in QGIS


## Note:
## Input files may be updated in the future, but the structure of this script
## will be applicable to the newer virsion.


###################################
## Libraries used in the scripts ##
###################################
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(hexbin)
library(stats)

## THings to change when used in different env ------------------

path_to_crown_polygon <- "../../Data/output/segmentation/crown_hulls_with_data.gpkg"
path_to_LU_raster <- "../../Data/clc2018/LU_modified.tif"
path_to_laz <- "../../Data/output/catalog_processing/"
path_to_LU_polygon <- "../../Data/Map_materials/shapefile/Corine2018_detailLU.shp"
path_to_output <- "../../Data/output/analysis/"




## Read file --------------------------------------------------------------------------------------

## Crown polygons 
crown_polygon <- rgdal::readOGR(path_to_crown_polygon) ## created by Leon on 20 Aug 2020

## Extract dataframe of polygons
crown_dataframe <- crown_polygon@data
## LU is stored in numbers so convert it into string
crown_dataframe <- crown_dataframe  %>% 
  mutate(LU = case_when(LU_majority == 1 ~ "Bare rocks",
                        LU_majority == 2 ~ "Broad-leaved forest",
                        LU_majority == 3 ~ "Coniferous forest (Sparse)",
                        LU_majority == 4 ~ "Coniferous forest inside crater",
                        LU_majority == 5 ~ "Coniferous forest (Dense)",
                        LU_majority == 6 ~ "Moors and heathland",
                        LU_majority == 7 ~ "Sclerophyllous vegetaion",
                        LU_majority == 8 ~ "Sparsely vegetated areas",
                        LU_majority == 9 ~ "Sparsely vegetated areas inside crater",
                        LU_majority == 10 ~ "Transitional woodland-shrub",
                        TRUE ~ "others"))


## My first plan was reading LU raster and extract the value on polygons in R 
## However, it took so much time so this extraction was done in QGIS
## So, this polygon already consists of LU value in "LU_majority"

## -- Below was the way to extract in R------

## Extract raster values (LU values) on polygon, and save as dataframe
# crown_dataframe <- data.frame()
## raster::extract will take time, so extraction was performed after cutting rasters into samll pieces
# for (i in 1:nrow(crown_polygon)) { #this is the number of polygons to iterate through
#   single_crown <- crown_polygon[i,] #selects a single polygon
#   clip1 <- raster::crop(LU_raster, raster::extent(single_crown)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
#   ext<-raster::extract(clip1,single_crown,fun=raster::modal, na.rm=TRUE) #extracts data from the raster based on the polygon bound
#   matrix<- as.data.frame(ext) 
#   final<-cbind(single_crown@data,matrix) #combines into single dataframe
#   crown_dataframe<-rbind(crown_dataframe,final)
#   print(paste0(i," completed!"))
# }
## it took soooo long... >>>> This part was done in QGIS instead

## ------------------------------------------




## 3) Split data into error/non-error by threshold ------------------------------------------------

#############################################
## Error threshold by tree height (max_z)  ##
#############################################

## As an obvious error, trees Z > 40 should be categorized as errors (<- References #1)
## So, first, I set threshold Z < 40 
## Here, the new field, "error_by_z" is created and numbers were assigned as
crown_dataframe$error_by_z <- ifelse(crown_dataframe$max_z > 40, "Error", "Non-error")

## Lets's see if there any other way to exclude errors
## ex) crown size, # of points

## Plot Crown area vs number of points
plot_points_vs_crown <-
  ggplot(data=crown_dataframe, aes(num_points, convex_area, col = max_z)) +
  geom_point() + 
  labs(title = "Crown area vs Points used for segmentation", x = expression("Crown size / m"^2) , y = "Number of points") + 
  theme_bw()
## So, crown size and number of points are very much correlated
## -> It will be enogh to use either of them as threshold

## Plot crown area vs tree height
plot_crown_vs_height <-
ggplot(data=crown_dataframe, aes(max_z, convex_area)) +
  geom_hex(bins = 100) + 
  scale_fill_continuous(type = "viridis") +
  geom_vline(xintercept = 40 , linetype="dashed", color = "red") + 
  labs(title = "Crown area vs Tree height", x = "Tree height / m" , y = expression("Crown size / m"^2)) + 
  theme_bw()
## Plot number of points vs tree height
plot_points_vs_height <-
ggplot(data=crown_dataframe, aes(max_z, num_points)) +
  geom_hex(bins = 100) + 
  scale_fill_continuous(type = "viridis") +
  geom_vline(xintercept = 40 , linetype="dashed", color = "red") + 
  labs(title = "Points used for segmentation vs Tree height", x = "Tree height / m" , y = "Number of points") + 
  theme_bw()

## -> Threshold can be the value gained by extrapolating the dense part in the plot above?
##    But don't know how actually

## So, this time, I decided to exclude trees statistically (crown size > mean + 3σ)
mean_crown <- mean(crown_dataframe$convex_area)
sd_crown <- sd(crown_dataframe$convex_area)
## Create error information on crown size
crown_dataframe$error_by_crown <- ifelse(crown_dataframe$convex_area > mean_crown + 3 * sd_crown, "Error", "Non-error")
## So the threshold is convex_area > 119.5819 


####################################################################
## After splitting the error/non-error, this is how the data look ##
####################################################################

## tree height vs elevation
plot_elevation_vs_height <-
  ggplot(data=crown_dataframe,aes(terrain_height_mean, max_z, col = error_by_crown)) + 
  geom_point(size = 0.1) + 
  scale_colour_discrete(name  ="Error by Crown",
                        labels=c("Error", "Non-error")) +
  geom_hline(yintercept=40 , linetype="dashed", color = "red") + 
  labs(title = "Tree height vs Elevation", x = "Elevation / m" , y = "Tree height / m") + 
  theme_bw()

## tree height vs slope
plot_slope_vs_height <-
  ggplot(data=crown_dataframe,aes(slope_mean, max_z, col = error_by_crown)) + 
  geom_point(size = 0.1) + 
  scale_colour_discrete(name  ="Error by Crown",
                        labels=c("Error", "Non-error")) +
  geom_hline(yintercept=40 , linetype="dashed", color = "red") + 
  labs(title = "Tree height vs Slope", x = "Slope / degree" , y = "Tree height / m") + 
  theme_bw()


## -> Honestly, I am not sure if this threshold is making sense
##    Hoever, since there is no article found and we just cannot spend
##    forever to think about it, I just will continue as it is for now.



## 4) Analysis of error data -------------------------------------------------------------------

## create field for 2 errors combined
crown_dataframe$errors <- ifelse(crown_dataframe$error_by_z == "Error"  | crown_dataframe$error_by_crown  == "Error", "Error", "Non-error")

############################################
## Histograms by grouping error/non-error ##
############################################

## First of all, visualize data grouped by errors by histogram
## To see if there is any different trend going on between error & non-error subclass

## Mean and SD for visualization
msd_elv = crown_dataframe %>% group_by(group=errors) %>% summarise(mean=mean(terrain_height_mean), sd=sd(terrain_height_mean))
msd_slop = crown_dataframe %>% group_by(group=errors) %>% summarise(mean=mean(as.numeric(slope_mean),na.rm=TRUE), sd=sd(as.numeric(slope_mean),na.rm=TRUE))
msd_hgt = crown_dataframe %>% group_by(group=errors) %>% summarise(mean=mean(max_z), sd=sd(max_z))

msd_elv$y = c(500,20000)
msd_slop$y = c(300,20000)
msd_hgt$y = c(300,45000)

## Histograms

hist_elevation_error <- 
  ggplot(crown_dataframe, aes(x = terrain_height_max, col=group)) +
  geom_histogram(aes(color = errors, fill = errors), 
                 position = "identity", bins = 30, alpha = 0.1)+
  labs(title = "Histogram of errors", x = "Elevation / m" , y = "Count") + 
  theme_bw() +
  geom_segment(data=msd_elv, aes(y=y,yend=y, x=mean - sd, xend=mean + sd), lty="solid", size =2) +
  geom_point(data=msd_elv, aes(y=y, x=mean), size = 3) +
  geom_segment(data=msd_elv, aes(x=mean-sd, xend=mean-sd, y=0, yend=y), alpha=1, lty="solid") +
  geom_segment(data=msd_elv, aes(x=mean+sd, xend=mean+sd, y=0, yend=y), alpha=1, lty="solid")

hist_slope_error <- 
  ggplot(crown_dataframe, aes(x = slope_mean, col = group)) +
  geom_histogram(aes(color = errors, fill = errors), 
                 position = "identity", bins = 30, alpha = 0.1)+
  labs(title = "Histogram of errors", x = "Slope / degree" , y = "Count") + 
  theme_bw() +
  geom_segment(data=msd_slop, aes(y=y,yend=y, x=mean - sd, xend=mean + sd), lty="solid", size =2) +
  geom_point(data=msd_slop, aes(y=y, x=mean), size = 3) +
  geom_segment(data=msd_slop, aes(x=mean-sd, xend=mean-sd, y=0, yend=y), alpha=1, lty="solid") +
  geom_segment(data=msd_slop, aes(x=mean+sd, xend=mean+sd, y=0, yend=y), alpha=1, lty="solid")


## -> Judging from these 2 histogram, elevation and slope did cause
##    errors a bit. Ex) the distribution of slope is slightly shifted to larger for errors.
##    This is easy to expect because steeper terrain had holes and that may cause distorsion.
##    For Elevation, the distribution for errors are shifter slighly smaller. 
##    That is difficult to imagine, but maybe LU(vegetation type) will probably explain this trend.
##    However, the diffrences (shifts) are very small

## Next, same vilsualization was made for tree height
hist_tree_height_error <- 
  ggplot(crown_dataframe, aes(x = max_z, col = group)) +
  geom_histogram(aes(color = errors, fill = errors), 
                 position = "identity", bins = 30, alpha = 0.1)+
  labs(title = "Histogram of errors", x = "Tree height / m" , y = "Count") + 
  theme_bw() +
  geom_segment(data=msd_hgt, aes(y=y,yend=y, x=mean - sd, xend=mean + sd), lty="solid", size =2) +
  geom_point(data=msd_hgt, aes(y=y, x=mean), size = 3) +
  geom_segment(data=msd_hgt, aes(x=mean-sd, xend=mean-sd, y=0, yend=y), alpha=1, lty="solid") +
  geom_segment(data=msd_hgt, aes(x=mean+sd, xend=mean+sd, y=0, yend=y), alpha=1, lty="solid")
## -> since I used Z > 40 as one of the thresholds, it makes sense that 
##    2 group have different distribution. However, when you ignore z>40,
##    there are still differences in peaks. Errors have a peak in larger height
##    That is probably because these errors have more points in 
##    general, that means there are higher chance to have greater value for one crown
##    than it actually is?? (Like smaller trees are detected as a part of larger tree >> 
##    so the errors are taller)




#############################
## Visualization of errors ##
#############################

## I decided to visualize some errors which are randomely selected

## choose sample data randomely (both error with height, and crown size)
error_samples_z <- sample_n(crown_dataframe[crown_dataframe$error_by_z == "Error",],2)
error_samples_crown <- sample_n(crown_dataframe[crown_dataframe$error_by_crown == "Error",],2)
error_samples <- rbind(error_samples_z,error_samples_crown)

## create extent 50x50 rectangle around these sample data
error_samples$ext_x_1 <- error_samples$min_x - 50
error_samples$ext_y_1 <- error_samples$min_y - 50
error_samples$ext_x_2 <- error_samples$min_x + 50
error_samples$ext_y_2 <- error_samples$min_y + 50


## -- SAMPLE-1 -- ##

## read laz file
p1_1<- c(error_samples$ext_x_1[1],error_samples$ext_y_1[1])
p2_1<- c(error_samples$ext_x_2[1],error_samples$ext_y_2[1])
las_1 <- lidR::readLAS(paste0(path_to_laz,"filtered_points_x_224000_y_3180000.laz"))
transect_1 <- lidR::clip_transect(las_1, p1_1, p2_1, width = 5)
sample_1 <- lidR::clip_rectangle(las_1, xleft = p1_1[1], ybottom = p1_1[2], xright = p2_1[1], ytop = p2_1[2])
## plot lider points
lidR::plot(sample_1, color = "Classification", axis = TRUE)
## plot transect
ggplot(transect_1@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal()+
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  theme_bw()
## clip polygons
crowns_1 <- raster::crop(crown_polygon,extent(p1_1[1],p2_1[1],p1_1[2],p2_1[2]))
crowns_1_data <- dplyr::left_join(crowns_1@data,crown_dataframe)
plot(crowns_1)
er_1 <- crowns_1_data$error_by_z == "Error"
plot(crowns_1[ er_1, ], col = "yellow", add = TRUE)

## -> local steep slope + hole

## -- SAMPLE-2 -- ##

## read laz file
p1_2<- c(error_samples$ext_x_1[2],error_samples$ext_y_1[2])
p2_2<- c(error_samples$ext_x_2[2],error_samples$ext_y_2[2])
las_2 <- lidR::readLAS(paste0(path_to_laz,"filtered_points_x_220000_y_3182000.laz"))
transect_2 <- lidR::clip_transect(las_2, p1_2, p2_2, width = 5)
sample_2 <- lidR::clip_rectangle(las_2, xleft = p1_2[1], ybottom = p1_2[2], xright = p2_2[1], ytop = p2_2[2])
## plot lider points
lidR::plot(sample_2, color = "Classification", axis = TRUE)
## plot transect
ggplot(transect_2@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal()+
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  theme_bw()
## clip polygons
crowns_2 <- raster::crop(crown_polygon,extent(p1_2[1],p2_2[1],p1_2[2],p2_2[2]))
crowns_2_data <- dplyr::left_join(crowns_2@data,crown_dataframe)
plot(crowns_2)
er_2 <- crowns_2_data$error_by_z == "Error"
plot(crowns_2[ er_2, ], col = "yellow", add = TRUE)

## -> the errors seem to be lacating at dense canopy area

## -- SAMPLE-3 -- ##

## read laz file
p1_3<- c(error_samples$ext_x_1[3],error_samples$ext_y_1[3])
p2_3<- c(error_samples$ext_x_2[3],error_samples$ext_y_2[3])
las_3 <- lidR::readLAS(paste0(path_to_laz,"filtered_points_x_220000_y_3181000.laz"))
transect_3 <- lidR::clip_transect(las_1, p1, p2, width = 5)
sample_3 <- lidR::clip_rectangle(las_3, xleft = p1_3[1], ybottom = p1_3[2], xright = p2_3[1], ytop = p2_3[2])
## plot lider points
lidR::plot(sample_3, color = "Classification", axis = TRUE)
## plot transect
ggplot(transect_3@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal()+
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  theme_bw()
## clip polygons and visualize
crowns_3 <- raster::crop(crown_polygon,extent(p1_3[1],p2_3[1],p1_3[2],p2_3[2]))
crowns_3_data <- dplyr::left_join(crowns_3@data,crown_dataframe)
plot(crowns_3)
er_3 <- crowns_3_data$error_by_crown == "Error"
plot(crowns_3[ er_3, ], col = "blue", add = TRUE)

## -> Kind of dense + slope


## -- SAMPLE-4 -- ##

## read laz file
p1_4<- c(error_samples$ext_x_1[4],error_samples$ext_y_1[4])
p2_4<- c(error_samples$ext_x_2[4],error_samples$ext_y_2[4])
las_4 <- lidR::readLAS(paste0(path_to_laz,"filtered_points_x_226000_y_3180000.laz"))
transect_4 <- lidR::clip_transect(las_4, p1_4, p2_4, width = 10)
sample_4 <- lidR::clip_rectangle(las_4, xleft = p1_4[1], ybottom = p1_4[2], xright = p2_4[1], ytop = p2_4[2])
## plot lider points
lidR::plot(sample_4, color = "Classification", axis = TRUE)
## plot transect
ggplot(transect_4@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal()+
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  theme_bw()
## clip polygons and visualize
crowns_4 <- raster::crop(crown_polygon,extent(p1_4[1],p2_4[1],p1_4[2],p2_4[2]))
crowns_4_data <- dplyr::left_join(crowns_4@data,crown_dataframe)
plot(crowns_4)
er_4 <- crowns_4_data$error_by_crown == "Error"
plot(crowns_4[ er_4, ], col = "blue", add = TRUE)

## Well, in this plot, smaller polygons look like an error

## -> Visual comparison will be updated as pdf file and I'll show it on Thursday




## 5) Summarize the charasteristics of the study area -------------------------------------------------------------------

## Before I start summarizing non-error data,
## I'll create an overview of the study area itself

########
## LU ##
########

## map LU
LU_raster <- raster::raster(path_to_LU_raster)
plot(LU_raster)

## calculate error rate on each LU
error_dataframe <- crown_dataframe %>% count(LU,errors)
for (i in 1:nrow(error_dataframe)){
  target_nrow <- 2*i
  num_errors <- error_dataframe$n[target_nrow - 1]
  num_all <- error_dataframe$n[target_nrow] + num_errors
  error_dataframe$error_rate[target_nrow] <- num_errors * 100 / num_all
  error_dataframe$error_rate[target_nrow-1] <- NA
}

## Error rate were highest with LU "Coniferous forest inside crater"
## The next was "Dense coniferous forest"
## Suprisingly, "broad-leaved forest" showed lower error rate even though
## they are quite dense and I thought it would cause bigger crowns.
## That is probably because broad-leaved forest are locating lower elevation,
## so terrain did affect less compared to coniferous forest???

## to use left join, dataframe was modified

error_dataframe_modified <- error_dataframe
for (i in 1:10) {
  error_dataframe_modified$n[2*i] <- error_dataframe_modified$n[2*i-1] + error_dataframe_modified$n[2*i]
}
error_dataframe_modified <- subset(error_dataframe_modified[c(2,4,6,8,10,12,14,16,18,20),])
error_dataframe_modified[3,1] <-  "Coniferous forest (dense)"
error_dataframe_modified[4,1] <- "Coniferous forest" 
error_dataframe_modified[5,1] <- "Coniferous forest (crater)"
error_dataframe_modified[9,1] <- "Sparsely vegetated areas (crater)"
error_dataframe_modified[7,1] <- "Sclerophyllous vegetation"


##  -> To discuss this more, the terrain information on each LU were prepared by QGIS
## read polygon that stores terrain info for each LU
LU_polygon <- readOGR(path_to_LU_polygon)
LU_info <- dplyr::left_join(LU_polygon@data,error_dataframe_modified)
## create new column for tree density (detected)
LU_info$density <- LU_info$n/LU_info$Shape_Area


ggplot(LU_info,aes(density,error_rate))+
  geom_point(aes(color = LU_modifie), size = 4)+
  theme_bw()

ggplot(LU_info,aes(terrain_me,error_rate))+
  geom_point(aes(color = LU_modifie), size = 4)+
  theme_bw()

ggplot(LU_info,aes(slope_mean,error_rate))+
  geom_point(aes(color = LU_modifie), size = 4)+
  theme_bw()

## So density is kind of correted with error rate
## Also, Slope is also sort of correlated with error rate

## 6) Analysis of non-error data -------------------------------------------------------------------

## Plot histograms of tree height of errors grouped by LU
hist_tree_height <-
  ggplot(crown_dataframe[crown_dataframe$errors=="Non-error",], aes(x = max_z)) +
    geom_histogram(aes(color = LU, fill = LU), 
                   position = "identity", bins = 30, alpha = 0.2)+
    labs(title = "Histogram of tree height", x = "Tree height / m" , y = "Count") + 
    theme_bw()

## Mean height were compared
## One-way Anova to compare the mean of tree height on each LU
height.LU.anova <- aov(max_z ~ LU, data = crown_dataframe[crown_dataframe$errors=="Non-error",])
summary(height.LU.anova)
TukeyHSD(height.LU.anova)
## -> all pairs exept Sclerophyllous vegetaion-Moors and heathland show P-value ≈ 0 <=> all paris having different means




## Reference ------------------------------------------------------------------------------


#################
## Refernce #1 ##
#################
## It is very interesting but Wikipedia on La Palma is described way more detailed in German than in English
## Anyway, according to Wikipedia (https://de.wikipedia.org/wiki/La_Palma#Flora_und_Vegetation),
## the vegetation is of course depending on elevation and they are categorized into 5 classes.
## 1) Küstenzone (bis 500 m), 2) Lorbeerwälder (500–1000 m), 3) Baumheide (1000–1500 m)
## 4) Kiefernwald (1500–2000 m), 5) Subalpine Hochgebirgsformen (ab 2000 m)
## Because our study area's elevation is 585 - 2338 m, we just need to think about class 2,3 and 4.
## And on Wikipedia, they say that Lorbeerwälder grows up to 30m, and Baumheide up to 20m.
## For Kiefernwald, the main spiece is Canary Pine, and again I asked 
## Wikipedia (https://en.wikipedia.org/wiki/Pinus_canariensis), and it says they grow up to 40m
