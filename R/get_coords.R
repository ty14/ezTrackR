#'Gets rescaled coordinates of Box and Cup ROI


#' @param df raw dataframe from python output
#' @param secs number of seconds to get data for from beginning
#' @param fr  frame rate of camera
#' @param burnin = number of frames to ignore
#' @return a list i) cup ROI coordinates ii) box coordinates iii) data frame including Frame,X and Y coordinates,plotX and plotY for graphing
#' @examples
#' get_coords_habit(df, secs=300, fr=30, burnin = 0)
#' get_coords_habit(data, secs=600, fr=60, burnin = 3)
#' @export

get_coords <- function(df, secs=300, fr= 30, burnin = 0){

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

  cup.mean.x <- mean(dx.flip[c(1:4),2])
  cup.mean.y <- mean(dx.flip [c(5:8),2])

  cup.range <- data.frame(cup.mean.x, cup.mean.y)


  # pulls out x/y min and max for box range
  box.range <- data.frame(
left = mean(dx.flip[c(1,4),1]),
top = mean(dx.flip[c(5,6),1]),
right = mean(dx.flip[c(2,3),1]),
bottom = mean(dx.flip[c(7,8),1]))


  ROI <-cbind(cup.range,box.range) #
  full <- cbind(df,ROI) %>%   select(-Other, -ROI_coordinates)

  full %>%
    mutate(plotX = ifelse(X < left, left, ifelse(X > right, right, X))) %>%
    mutate(plotY = ifelse(Y < top, top, ifelse(Y > bottom, bottom, Y))) -> full

  full %>% select(Frame,X,Y,plotX,plotY) -> full


  #rescale
  full$rescaleX <- ((full$plotX - box.range$left) / (box.range$right - box.range$left))*1000
  full$rescaleY <- ((full$plotY - box.range$top) / (box.range$bottom - box.range$top))*1000

  #rescale the cup means to the box range
  full$rescalecup.meanx<- ((cup.range$cup.mean.x - box.range$left) / (box.range$right - box.range$left))*1000
  full$rescalecup.meany <- ((cup.range$cup.mean.y - box.range$top) / (box.range$bottom - box.range$top))*1000



  # keep data based on time, frame-rate and burn-in
  nframes <- (secs * fr) + burnin

  # keep data based on time, frame-rate and burn-in
  full <- full %>% filter (Frame <= nframes)
  full <- full[(1+burnin):(nrow(full)+burnin),]



  return(list('cup_roi' = cup.range, 'box' = box.range, 'xy' = full))

}


