# STA 141A - Final Project 
# Due December 5, 2017 @ 5:00 PM
# GROUP 52: Mitchell Layton, Caroline Mai, William Powell
#---------------------------------------------------------
library(tidyverse)
library(ggmosaic)
library(plotly)
library(scales)
library(car)
library(RColorBrewer)
library(rio)
library(readr)
library(ggmap)
library(ggrepel)
library(data.table)
library(lubridate)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggthemes)
library(grid)
library(sqldf)
library(reshape)
library(zoo)
library(bootstrap)    
library(boot)
library(class)
library(matlib)
#1)---------------------------------------------------------------------


read_digits = function(x) {
    txt = read.table(x)
}

data = read_digits("C:/Users/layto/Desktop/Davis/Data Science/Data Sets/train.txt")


#2)---------------------------------------------------------------------

view_digit = function(read_data, observation) {
    temp = melt(read_data[observation,])
    temp = temp$value[2:length(temp$value)]
    temp = matrix(temp, nrow = 16, ncol = 16, byrow = T)
        rotate <- function(x) {
            t(apply(x, 2, rev))
        }
    temp = rotate(temp)
    image(temp, axes = FALSE, col = grey(seq(0, 1, length = 256)))
    
}

view_digit(data,131)




#3)---------------------------------------------------------------------


# Display  graphically what each digit (0-9) looks like on average

avg_digit = function(read_data) {
        
        rotate <- function(x) {
            t(apply(x, 2, rev))
        }
    
                nums = seq(0,9,1)
                for (i in nums) {
                    # I use sqldf() package for SQL query like statements. Also to get sqldf to use my i in my for loop,
                    # I had to use sprintf and strwrap for wrapping my SQL statement string senctence to be able to
                    # use the C-style string formatting command within the sqldf() function.
                    temp = sqldf(strwrap(sprintf("SELECT * FROM read_data WHERE V1 = '%s'",i), simplify = T))
                    temp = as.data.table(sapply(temp,mean))[2:257]
                    temp = melt(temp, id=1)
                    temp = temp$V1
                    temp = matrix(temp, nrow = 16, ncol = 16, byrow = T)
                    temp = rotate(temp)
                    print(image(temp, axes = FALSE, col = grey(seq(0, 1, length = 256))))
                }
            
}
avg_digit(data)


#4)---------------------------------------------------------------------

predict_knn = function(predict, train, distance, k) {
    # matrix of our prediction point(s) that will be calculated distance from training points
    pred_matrix = as.matrix(predict, nrow = nrow(predict), ncol = ncol(predict), byrow = T)
    
    
    
}

#4)--William----------------------------------
predict_knn = function(points, train, k, metric) {
  #Set up a vector for the predicted classes
  predictions = c(rep(NA,nrow(points)))
  
  #A matrix of the prediction points, excluding the class values, to use for the distance function
  dist_mat = rbind(points,train[1:nrow(train),])[,2:257]
  
  #A matrix of all the distances
  one_dist = as.matrix(dist(dist_mat,method = metric))
  for (i in 1:nrow(points)){
    #frequencies of the nearest classes stored in "classes"
    classes = c(rep(0,10))
    #Extracting distances from the matrix
    distances = one_dist[(1+nrow(points)):nrow(one_dist),i]
    #Finding the closest k
    k_nearest = sort(distances)[1:k]
    for(j in 1:k){
      #incrementing frequencies of classes for the k-nearest neighbors
      classes[train[which(distances==k_nearest[j]),1] + 1] = classes[train[which(distances==k_nearest[j]),1] + 1] + 1
    }
    #print(classes)
    
    #Solving ties
    if (length(which(classes==max(classes))) > 1){
      #choose one of the max frequency classes at random (-1 to adjust from 1:10 to 0:9)
      predictions[i] = sample(which(classes==max(classes)),1) - 1
    }
    else{
      #choose the max frequency class
      predictions[i] = which(classes==max(classes)) - 1
    }
  }
  #output the vector of predictions
  predictions
}

#For testing the function, you can use: 
  # >predict_knn(data[1:5,],data,100,"euclidean")
#Or try it on the test data
  # >predict_knn(test_data,data,90,"minkowski")








