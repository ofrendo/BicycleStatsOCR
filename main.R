source("helpers.R")
source("loadData.R")

# Algorithm idea:
# 0) PREPROCESSING
# Load image in pixels
# Convert to greyscale, leaving red ones as black and others as white
path <- "img/IMG_20151027_224017.jpg"
# bright pixel
#x <- 1737
#y <- 1373
# next to bright pixel
#x <- 1429
#y <- 1601

#path <- "img/rgbTest.jpg"
#img <- readJPEG(path) # using "jpeg"
#img.red <- raster(img[,,1]) #red
#img.green <- raster(img[,,2]) #green
#img.blue <- raster(img[,,3]) #blue

convertToBinary <- function(r,g,b) {
  minR <- 0.9
  minG <- 0.9
  minB <- 0.9
  result <- r
  result[] <- ifelse(as.vector(r >= minR) & as.vector(g >= minG) & as.vector(b >=minB), 1, 0)
  result
}
getBinary <- function() {
  img <- readJPEG(path) # using "jpeg"
  img.red <- raster(img[,,1]) #red
  img.green <- raster(img[,,2]) #green
  img.blue <- raster(img[,,3]) #blue
  result <- convertToBinary(img.red, img.green, img.blue)
  result
}
img.binary <- getBinary()

# plot channels
#plot(stack(img.red, img.green, img.blue, binary.img), main = c("red", "green", "blue", "binary"))
plot(stack(img.binary))  


# 1) TOKENIZATION
# With DBSCAN
getDBScan <- function(eps, MinPts) {
  # transform matrix of 
  # 0 0 1
  # 0 1 0
  # to points
  # 1 3
  # 2 2
  binary <- rotateM(as.matrix(img.binary))
  binaryPoints <- matrix(nrow=NROW(binary[binary==1]), ncol=2)
  counter <- 0
  for (i in 1:NROW(binary)) {
    for (j in 1:NCOL(binary)) {
      if (binary[i, j] == 1) {
        counter <- counter + 1
        binaryPoints[counter,] = c(i, j)
      }
    }
  }
  
  #binaryPoints <- 
  #binary <- as.vector(img.binary)
  result <- dbscan(binaryPoints, eps=eps, MinPts=MinPts, method="raw")
}

img.dbscan <- getDBScan(eps = 2, MinPts = 11)
img.clusters <- img.dbscan$cluster
plot(img.dbscan, binaryPoints)
plot(binaryPoints[img.clusters==1,])













#Probably not needed
getRed <- function(x) floor(x %% 0x1000000 / 0x10000)
getGreen <- function(x) floor(x %% 0x10000 / 0x100)
getBlue <- function(x) floor(x %% 0x100)
getRGB <- function(x) c(getRed(x), getGreen(x), getBlue(x))