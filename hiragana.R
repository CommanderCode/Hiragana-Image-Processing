#  READ THE ETL8B2C1 FILE AND CREATES THE DATA
#  MAKE SURE TO SAVE 'ETL8B2C1 in your working directory.
#  The file can be downloaded from this link
#  https://drive.google.com/open?id=1CSDC8Cc4INioYwpWg6SXomvpwAwkqy7H
#  To check your working directory, type getwd() in your R console

library(tidyverse)

#opens file connection
my.file <- file('ETL8B2C1', 'rb')

#create elements
labels <- c()
image <- list()

# reads the first 512 bites, which are buffer
readBin(my.file, raw(), n = 512L)

#reads 512 bytes at a time, 1 to 12000 entries  (su/zu 2875 - 3017, chi/ji 4019-4167)
for (i in 1:12000){
d <- readBin(my.file, raw(), n = 512L)

# read ascii character
labels[i] <- rawToChar(d[5:8])

# read image data
brev <- function(x) rev(rawToBits(x))
image[[i]] <- as.integer(paste(sapply(d[9:512],brev)))
}
close(my.file)

# Change character labels of characters with duplicate labels
labels[3201:3360] <- "SUZU"
labels[4481:4640] <- "CHIJI"
labels <- as.factor(labels)

# add all images into a large matrix
image <- matrix(unlist(image), ncol = 4032, byrow = TRUE)

# partition the data into train and test sets
#  x is the images, y is the labels
set.seed(1983)
index <- sample(12000, 1200)
y_train<-factor(labels[-index])
x_train <- image[-index,]
y_test<-factor(labels[index])
x_test <- image[index,]

rm(d,i,index,labels,image,my.file)

#  KNN model
library(caret)
colnames(x_train) <- 1:4032
colnames(x_test) <- colnames(x_train)

# dimension reduce by taking out columns with near zero variance
nzv <- nearZeroVar(x_train)
col_index <- setdiff(1:ncol(x_train), nzv)
col_means <- colMeans(x_train[,col_index])

# calculating the pca takes up to 10 minutes
pca <- prcomp(x_train[,col_index])


K <- 30
x_train_pca <- pca$x[,1:K]
fit <- knn3(x_train_pca, y_train)
#transform the test set
x_test_pca <- sweep(x_test[,col_index], 2, col_means) %*% pca$rotation
x_test_pca <- x_test_pca[,1:K]
# make predictions and see results
y_hat <- predict(fit, x_test_pca, type = "class")
results <- confusionMatrix(y_hat, factor(y_test))
results$overall
