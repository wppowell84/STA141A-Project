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

data = read_digits("E:/Davis/STA 141A/DATA/test.txt")


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

view_digit(data,20)




#3)---------------------------------------------------------------------


# Display  graphically what each digit (0-9) looks like on average

avg_digit = function(read_data) {
        
        rotate <- function(x) {
            t(apply(x, 2, rev))
        }
    
                nums = seq(1,9,1)
                for (i in nums) {
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








