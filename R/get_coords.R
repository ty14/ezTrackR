#' df  raw dataset
#' ROIX = distance in X of ROI
#' ROIY = distance in Y of ROI
#' @export

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

  cup.mean.x <- mean(dx.flip[c(1:4),2])
  cup.mean.y <- mean(dx.flip [c(5:8),2])

  cup.range <- data.frame(
    x_max = cup.mean.x + ROIX,
    x_min = cup.mean.x - ROIX,
    y_max = cup.mean.y + ROIY,
    y_min = cup.mean.y - ROIY
  )


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

  # keep data based on time, frame-rate and burn-in
  full <- full %>% filter (Frame <= nframes)
  full <- full[(1+burnin):(nrow(full)+burnin),]


  return(list('cup_roi' = cup.range, 'box' = box.range, 'xy' = full))

}


