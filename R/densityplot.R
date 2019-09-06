#' Getting a density plot during habituation and social approach
#' @param df output of get_coords
#' @return a plot split into a 1000 by 1000 grid of squares and then color the points by the estimated density in each square.
#' @export


densityplot <- function(df){

  df<- df[[3]]
df$density <- get_density(df$rescaleX, df$rescaleY, n = 1000)

library(gridExtra)

p <- ggplot(df, aes(rescaleX, rescaleY, color = density)) +
    geom_point(alpha = 0.7, show.legend = FALSE)  +
    scale_color_viridis()


return(p)

}
