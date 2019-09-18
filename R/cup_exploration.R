#'  this is cup_exploration
#'
#' @param df output of get_coords_habit or get_coords
#' @param fr  frame rate of camera
#' @param ROIX = distance in X for cup ROI
#' @param ROIY = distance in Y for cup ROI
#' @return a data frame including Frame number, X and Y coordinates, distance per frame in the cup ROI,
#'         total distance in cup ROI,time spent in cup ROI
#' @examples
#' cup_exploration(df,df1)
#' cup_exploration(df,df_coords, fr=60)
#'@export


cup_exploration <- function(df, fr=30, ROIX= 200, ROIY= 100) {

  df1 <- df$xy
#distance in cup ROI
  c <- df1 %>%
    mutate(x_min = rescalecup.meanx - ROIX) %>%
    mutate(x_max = rescalecup.meanx + ROIX) %>%
    mutate(y_min = rescalecup.meany - ROIY) %>%
    mutate(y_max = rescalecup.meany + ROIY) %>%
    filter(X>x_min, X<x_max) %>%
    filter(Y>y_min, Y<y_max) %>%
    mutate( X_prev = lag( X ), Y_prev = lag( Y )) %>% #get the previous x and y value
    filter( !is.na( X_prev ) ) %>%  #filter out rows without previous x value
    mutate( cupdistance = sqrt( abs (X - X_prev )^2 + abs( Y - Y_prev )^2 )) %>% #calculate the distance
    mutate( total_cupdistance = sum( cupdistance )) %>% #summarise to get the total distance
    select(Frame,X,Y,cupdistance,total_cupdistance, X_prev)
#time in cup ROI
cup_row <- nrow(c)
  time_c<- c %>%
    mutate(time_cup = cup_row/fr)
#join everything together
  cup_exp <- c %>%
    full_join(time_c)

  return(cup_exp %>% select(Frame,X,Y,cupdistance,total_cupdistance, time_cup))
}
