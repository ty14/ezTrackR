#' df  raw dataset
#' ROIX = distance in X of ROI
#' ROIY = distance in Y of ROI

get_coords <- function(df, ROIX = 200, ROIY = 100){

  library(tidyverse)
  library(stringi)
  library(jsonlite)

  dx.ROI <- df %>% select(ROI_coordinates)
  str1 <- (dx.ROI[1,]) # just takes the first row (since all rows are the same)
  str1<-stri_sub(str1,15, -2) # gets rid of the first part of the string and last )
  x1 <- gsub("'", '"', str1) # does something important with slashes
  dx.ROI <- data.frame( jsonlite::fromJSON(x1)) ### pulled out all x and y for each ROI and creates data frame.

  #get the coordinates for cup and box
  dx.flip <- data.frame(t(dx.ROI[])) %>%
    mutate(box = X1) %>%
    mutate(cup = X2) %>%
    select(box,cup) ### remember the first 4 columns are x coordinates and last 4 are y coordinates

  # pulls out x/y min and max for cup range
  cup.mean <- dx.flip %>%
    select(cup) %>%
    mutate(x.mean = mean(dx.flip[c(1:4),2])) %>%
    mutate(y.mean = mean(dx.flip [c(5:8),2]))%>%
    mutate(x_max = x.mean + ROIX) %>%
    mutate(x_min = x.mean -ROIX) %>%
    mutate(y_max = y.mean + ROIY) %>%
    mutate(y_min = y.mean - ROIY) %>%
    select(-cup,-x.mean,-y.mean)

  cup.range <- (cup.mean[1,])

  # pulls out x/y min and max for box range
  box.mean <- dx.flip %>%
    select(box) %>%
    mutate(left = mean(dx.flip[c(1:2),1])) %>%
    mutate(top = mean(dx.flip[c(5,8),1])) %>%
    mutate(right = mean(dx.flip[c(3:4),1])) %>%
    mutate(bottom = mean(dx.flip[c(6:7),1])) %>%
    select(left,top,right,bottom)

  box.range <- (box.mean[1,])

  ROI <-cbind(cup.range,box.range) ## I have no idea what I am doing, really I didn't even make a funcation yet.
  full <- cbind(df,ROI) %>%   select(-Other, -ROI_coordinates)

  full %>%
    mutate(plotX = ifelse(X < left, left, ifelse(X > right, right, X))) %>%
    mutate(plotY = ifelse(Y < top, top, ifelse(Y > bottom, bottom, Y))) -> full

  full %>% select(Frame,X,Y,plotX,plotY) -> full

  return(list('cup' = cup.range, 'box' = box.range, 'xy' = full))

}
