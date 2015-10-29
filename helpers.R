#library(ripa) # Image loading
library(jpeg)

library(raster) # Data structure for images

# For dbscan
library(fpc)
#library(dbscan)

rotateM <- function(m) t(m)[,nrow(m):1]




#Probably not needed
getRed <- function(x) floor(x %% 0x1000000 / 0x10000)
getGreen <- function(x) floor(x %% 0x10000 / 0x100)
getBlue <- function(x) floor(x %% 0x100)
getRGB <- function(x) c(getRed(x), getGreen(x), getBlue(x))