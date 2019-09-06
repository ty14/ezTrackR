#' Plot density graph
#' @param df output of get_coords_habit or get_coords
#'
#'
#'
#'



library(viridis)
densityplot <- function(df) {
  dens <- MASS::kde2d(df$X, Y, ...)
  ix <- findInterval(df$X, dens$X)
  iy <- findInterval(df$Y, dens$Y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])



library(viridis)
df$density <- get_density(df$X, df$Y, n = 1000)

p <- ggplot(df, aes(X, Y, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()

}


densityplot(df)
