#' thigmotaxis
#'
#' @param  df  dataframe from get_data function
#' @param df1 output of get_coords
#' @param xleft adding x units from the left to calculate thigmotaxis
#' @param xright subtracting x units from the right to calculate thigmotaxis
#' @param ytop adding y units from the top to calculate thigmotaxis
#' @param xleft subtracting y units from the bottom to calculate thigmotaxis
#' @return a data frame including Frame, X and Y coordinates, c_dist (center distance), t_dist (thigmotaxis distance),  c_time, t_time
#' @examples
#' thigmotaxis(df,df1,xleft=100, xright= -100, ytop =100, ybottom=-100, fr=30)
#' thigmotaxis(df,get_coords_habit(df),xleft=50, xright= -50, ytop =50, ybottom=-50, fr=60)
#' @export
#'

thigmotaxis <- function(df, df1, xleft=100, xright= -100, ytop =100, ybottom=-100, fr=30) {

  box <- df1[[2]]
#getting distance
  ct<- data.frame(df,box) %>%
    mutate(t_left = left + xleft) %>%
    mutate(t_right = right +xright) %>%
    mutate(t_bottom = bottom + ybottom) %>%
    mutate(t_top = top + ytop) %>%
    filter(X> t_left, X<t_right) %>%
    filter(Y<t_bottom, Y> t_top) %>%
    mutate(c_dist= sum(distance)) %>%
    mutate(t_dist = total_distance - c_dist)
#getting time
  total_row<-nrow(df)
  center_row<-nrow(ct)

  time <- ct %>%
    mutate(c_time = center_row/fr) %>%
    mutate(t_time = (total_row/fr)-c_time)

#combine data
  all<- ct %>%
    full_join(time)


  return(t <- all %>% select(Frame,X,Y,c_dist,t_dist, c_time, t_time))

}



