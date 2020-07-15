install.packages("renv")
renv::init()

## Prepare Environment -----------------------------------

install.packages("lidR")
install.packages("rgl") ## Requires to install Xquartz on Mac OS
install.packages("../../RPackages/crownsegmentr_0.0.0.9000.tar.gz")

# renv::snapshot()
