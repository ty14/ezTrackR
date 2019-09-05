#' Gets rescaled coordinates for box

#' @param df raw dataframe from python output during habituation
#' @param secs number of seconds to get data for from beginning
#' @param fr  frame rate of camera
#' @param burnin - number of frames to ignore
#' @return a list i) box coordinates ii) data frame including Frame,X and Y coordinates,plotX and plotY for graphing
#' @examples
#' get_coords_habit(df, secs=300, fr=30, burnin = 0)
#' get_coords_habit(data, secs=600, fr=60, burnin = 3)
#' @export

get_coords_habit <- function(df, secs=300, fr=30, burnin = 0){

  library(tidyverse)
  library(stringi)
  library(jsonlite)

  nframes <- (secs * fr) + burnin


  dx.ROI <- df %>% select(ROI_coordinates)
  str1 <- (dx.ROI[1,]) # just takes the first row (since all rows are the same)
  str1<-stri_sub(str1,15, -2) # gets rid of the first part of the string and last )
  x1 <- gsub("'", '"', str1) # does something important with slashes
  dx.ROI <- data.frame( jsonlite::fromJSON(x1)) ### pulled out all x and y for each ROI and creates data frame.

  #get the coordinates for cup and box
  dx.flip <- data.frame(t(dx.ROI[]))

  # pulls out x/y min and max for box range
  box.range <- data.frame(
    left = mean(dx.flip[c(1,4),1]),
    top = mean(dx.flip[c(5,6),1]),
    right = mean(dx.flip[c(2,3),1]),
    bottom = mean(dx.flip[c(7,8),1]))


  full <- cbind(df,box.range) %>%   select(-ROI_coordinates)

  full %>%
    mutate(plotX = ifelse(X < left, left, ifelse(X > right, right, X))) %>%
    mutate(plotY = ifelse(Y < top, top, ifelse(Y > bottom, bottom, Y))) -> full

  full %>% select(Frame,X,Y,plotX,plotY) -> full

#rescale
  full$rescaleX <- ((full$plotX - box.range$left) / (box.range$right - box.range$left))*1000
  full$rescaleY <- ((full$plotY - box.range$top) / (box.range$bottom - box.range$top))*1000

# keep data based on time, frame-rate and burn-in
  full <- full %>% filter (Frame <= nframes)
  full <- full[(1+burnin):(nrow(full)+burnin),]



  return(list('box' = box.range, 'xy' = full))

}


