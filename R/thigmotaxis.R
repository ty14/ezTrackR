#' thigmotaxis
#'
#' @param  df  dataframe from get_coords or get_coords habit
#' @param xleft adding x units from the left to calculate thigmotaxis
#' @param xright subtracting x units from the right to calculate thigmotaxis
#' @param ytop adding y units from the top to calculate thigmotaxis
#' @param xleft subtracting y units from the bottom to calculate thigmotaxis
#' @return a data frame including Frame, X and Y coordinates, c_dist (center distance), t_dist (thigmotaxis distance),  c_time, t_time
#' @examples
#' thigmotaxis(df,xleft=100, xright= -100, ytop =100, ybottom=-100, fr=30)
#' thigmotaxis(get_coords_habit(df),xleft=50, xright= -50, ytop =50, ybottom=-50, fr=60)
#' @export
#'

thigmotaxis <- function(df, xleft=100, xright= -100, ytop =100, ybottom=-100, fr=30) {

  DF <- df$xy
  DF.dist <- distcalc(DF)

  #getting distance
  ct<- DF.dist %>%
    mutate(t_left = 0 + xleft) %>%
    mutate(t_right = 1000 +xright) %>%
    mutate(t_bottom = 1000 + ybottom) %>%
    mutate(t_top = 0 + ytop) %>%
    filter(rescaleX> t_left, rescaleX<t_right) %>%
    filter(rescaleY<t_bottom, rescaleY> t_top) %>%
    mutate(c_dist= sum(distance)) %>%
    mutate(t_dist = total_distance - c_dist)

  #getting time
  total_row<-nrow(DF.dist)
  center_row<-nrow(ct)

  time <- ct %>%
    mutate(c_time = center_row/fr) %>%
    mutate(t_time = (total_row/fr)-c_time)

  #combine data
  all<- ct %>%
    full_join(time)


  return(all %>% select(Frame,rescaleX,rescaleY,c_dist,t_dist, c_time, t_time))

}
