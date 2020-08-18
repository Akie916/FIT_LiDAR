## This is a trial for some statistical analysis
## I now only have dataframe and LAS (not crown polygon)
## So first, I'll just see the basic regression


## Read data --------------------------------------------------------------------------------------------------

segmented_points_metrics <- readRDS(file = "../../Data/output/segmentation/points_metrics_dataframe.rds")


## Regression analysis between tree height VS slope,aspect,and elevation -----------------------------------------------

## here, I just assume Z_max is the tree height and use _mean for other 3 variables 
## first of all, visualize by plotiing the relationship between variables
segmented_points_statictics <- dplyr::select(segmented_points_metrics,
                                             treeID,
                                             Z_max,
                                             terrain_height_mean,
                                             slope_mean,
                                             aspect_mean)
plot(segmented_points_statictics[,c(2:5)])
## Alright, I cannot do even this with this poor machine...
## So let's skip visually analyze the correlation but just calculate it

psych::corr.test(segmented_points_statictics[,c(2:5)],
                 use = "pairwise",
                 method = "pearson",
                 alpha = 0.5)

## It returns that there seems no correlation between those 4 variables

## Let's just check if there are any visual clue for any correlation between variables
plot(segmented_points_statictics[,c(2:3)])
plot(segmented_points_statictics[,c(2,4)])
plot(segmented_points_statictics[,c(2,5)])
## So, as a whole data, they really don't look correlated in any way at all.
## However, as expected, outliers of Z_max (something like taller than 40m) 
## looks kind of correlated with slope_mean 
## Therefore, I'll just do the regression analysis only for those data (Z_max > 35)
## not knowing if it is giving us any cool ideas for further work

segmented_points_outliers <- subset.data.frame(segmented_points_statictics, Z_max > 35)
plot(segmented_points_outliers[,c(2,4)])

LinearModel_Z_to_slope_outlier <- lm(Z_max ~ slope_mean, segmented_points_outliers)
summary(LinearModel_Z_to_slope_outlier)
## R^2 = 0.2051 so not really working with linear regression

NonLinearModel_Z_to_slope_outlier <- nls(Z_max ~ a ^ slope_mean + b, 
                                         data = segmented_points_outliers,
                                         start = list(a = 1.5, b = 40))

summary(NonLinearModel_Z_to_slope_outlier)
r_square <- 1-(sum({summary(NonLinearModel_Z_to_slope_outlier)$residuals }^2)/sum({segmented_points_outliers$Z_max -mean(segmented_points_outliers$Z_max)}^2))
## R^2 for this is 0.2329
NonLinearModel_Z_to_slope_outlier_plot <- ggplot ()+
  geom_point(aes(segmented_points_outliers$slope_mean , segmented_points_outliers$Z_max),size =0.1)+
  geom_line(aes(segmented_points_outliers$slope_mean , predict(NonLinearModel_Z_to_slope_outlier)),size =1)+
  xlab("slope")+
  ylab("Tree Height / m")+
  ggtitle("Nonlinear  model for outliers  by slope")+
  theme_bw()

## well, I could not find any good correlation info on nonlinear model either...
## for data with Z>35, however, other than this, I don't find any possibility
## Maybe crown size will also give some information

