#' thigmotaxis - center area vs non-center area. (use raw data)
#' use raw data in dist.5 for thigmotaxis has distance in it already
#' @export

thigmotaxis <- function(df, left=100, right= -100, top =100, bottom=-100, FPS=30) {

  c <- df %>%
    mutate(max_x= max(X)) %>%
    mutate(min_x = min(X)) %>%
    mutate(max_y = max(Y)) %>%
    mutate(min_y = min(Y)) %>%
    mutate(t_left = min_x + left) %>%
    mutate(t_right = max_x +right) %>%
    mutate(t_bottom = max_y + bottom) %>%
    mutate(t_top = min_y + top) %>%
    filter(X> t_left, X<t_right) %>%
    filter(Y<t_bottom, Y> t_top) %>%
    mutate(c_dist= sum(distance)) %>%
    mutate(t_dist = total_distance - c_dist)


  return(c %>% select(Frame,X,Y,c_dist,t_dist))

}
