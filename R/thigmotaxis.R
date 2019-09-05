#' thigmotaxis - center area vs non-center area. (use raw data)
#' use raw data in dist.5 for thigmotaxis has distance in it already
#' @export

thigmotaxis <- function(df, df1, xleft=100, xright= -100, ytop =100, ybottom=-100, fr=30) {

  box <- df1[[1]]
#getting distance
  ct<- data.frame(df,box) %>%
    mutate(t_left = box$left + xleft) %>%
    mutate(t_right = box$right +xright) %>%
    mutate(t_bottom = box$bottom + ybottom) %>%
    mutate(t_top = box$top + ytop) %>%
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


  return(all %>% select(Frame,X,Y,c_dist,t_dist, c_time, t_time))

}


