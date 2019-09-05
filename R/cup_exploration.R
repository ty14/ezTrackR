#'  this is cup_exploration
#'


cup_exploration <- function(df, df1, fr=30) {

  cup_roi <- df1[[1]]
#distance in cup ROI
  c <- data.frame(df,cup_roi) %>%
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
