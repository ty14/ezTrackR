#' Function to Get Data for X seconds from raw file.

#' @param df raw dataframe from python output
#' @param secs number of seconds to get data for from beginning
#' @param fr  frame rate of camera
#' @param burnin - number of frames to ignore
#' @return a data frame including Frame number, X and Y coordinates, distance traveled per frame, and total distance
#' @examples
#' get_data(df, secs=300, fr=30, burnin=0)
#' get_data(data, secs=600, fr=60, burnin=5)
#' @export


get_data <- function(df, secs=300, fr=30, burnin = 0){

  library(tidyverse)

  nframes <- (secs * fr) + burnin

  dx <- df %>% select(File, Frame, X, Y, ROI, ROI_coordinates)
  dx$Frame <- as.numeric(dx$Frame)
  dx.5 <- dx %>% filter (Frame <= nframes)

  # get distance traveled.. So much fun dis = sqrt((x1 - x2)^2+(y1 - y2)^2).

  dist <- dx.5 %>%
    select(Frame,X,Y) %>% ## gets distance for each frame need for later use
    mutate( X_prev = lag( X ), Y_prev = lag( Y )) %>% #get the previous x and y value
    filter( !is.na( X_prev ) ) %>%  #filter out rows without previous x value
    mutate( distance = sqrt( abs (X - X_prev )^2 + abs( Y - Y_prev )^2 )) %>% #calculate the distance
    mutate( total_distance = sum( distance )) %>% #summarise to get the total distance
    select(Frame,X,Y,distance,total_distance, X_prev)

  dist.5 <- dx.5 %>% full_join(dist) %>% select(Frame,X,Y,distance,total_distance)

  dist.5 <- dist.5[(1+burnin):(nrow(dist.5)+burnin),]

  return(dist.5)
}
