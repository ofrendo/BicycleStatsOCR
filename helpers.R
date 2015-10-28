#library(ripa) # Image loading
library(jpeg)

library(raster) # Data structure for images

# For dbscan
library(fpc)
#library(dbscan)

rotateM <- function(m) t(m)[,nrow(m):1]
