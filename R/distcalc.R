#' Calculate Distance Traveled
#'
#' @param df dataframe from get_coords_habit or get_coords
#' @return Calculates distance from rescaleX and rescaleY values
#' @examples
#' distcalc(df)
#' @export

distcalc <- function(df){
# get distance traveled.. So much fun dis = sqrt((x1 - x2)^2+(y1 - y2)^2).

df %>%
  mutate( X_prev = lag( rescaleX ), Y_prev = lag( rescaleY )) %>% #get the previous x and y value
  filter( !is.na( X_prev ) ) %>%  #filter out rows without previous x value
  mutate( distance = sqrt( abs (rescaleX - X_prev )^2 + abs( rescaleY - Y_prev )^2 )) %>% #calculate the distance
  mutate( total_distance = sum( distance )) %>% #summarise to get the total distance
  select(Frame,rescaleX,rescaleY,distance,total_distance)

}

