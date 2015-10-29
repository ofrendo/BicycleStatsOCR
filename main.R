source("helpers.R")
source("loadData.R")

# Algorithm idea:
# 0) PREPROCESSING
# Load image in pixels
# Convert to greyscale, leaving red ones as black and others as white
path <- "img/IMG_20151027_224017.jpg" # low res pic
#path <- "img/IMG_20151028_133125.jpg" # day time pic
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
#plot(stack(img.red, img.green, img.blue, img.binary), main = c("red", "green", "blue", "binary"))
plot(stack(img.binary))  


# 1) TOKENIZATION
# With DBSCAN
getDBScan <- function(eps, MinPts) {
  binary <- rotateM(as.matrix(img.binary))
  binaryPoints <<- toPointsMatrix(binary)

  result <- dbscan(binaryPoints, eps=eps, MinPts=MinPts, method="raw")
}

img.dbscan <- getDBScan(eps = 2, MinPts = 11)
img.clusters <- img.dbscan$cluster
plot(img.dbscan, binaryPoints)
plot(binaryPoints[img.clusters==4,])

singleDigit <- binaryPoints[img.clusters==1,]
singleDigitPixels <- toPixelMatrix(singleDigit)




# transform matrix of 
# 0 0 1
# 0 1 0
# to points
# 1 3
# 2 2
toPointsMatrix <- function(m) {
  result <- matrix(nrow=NROW(m[m==1]), ncol=2)
  counter <- 0
  for (i in 1:NROW(m)) {
    for (j in 1:NCOL(m)) {
      if (m[i, j] == 1) {
        counter <- counter + 1
        result[counter,] <- c(i, j)
      }
    }
  }
  result
}

toPixelMatrix <- function(m) {
  result <- matrix(nrow=max(m[,2])-min(m[,2]), ncol=max(m[,1])-min(m[,1]))
  
  minX <- min(m[,1])
  maxX <- max(m[,1])
  dx <- maxX-minX
  minY <- min(m[,2])
  result[] <- 0
  for (i in 1:NROW(m)) {
    row <- m[i,2]-minY
    col <- m[i,1]-minX
    #print(row)
    #print(col)
    
    result[row,col] <- 1
  }
  result <- result[,NCOL(result):1]
  result
}



