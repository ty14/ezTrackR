#' Getting a density plot in 5 min bins during habituation and social approach
#'
#' @param df output of get_coords
#' @return a 5 plot in 1 min bins split into a 1000 by 1000 grid of squares and then color the points by the estimated density in each square.
#' @export
densityplot5 <- function(df){

df<- df[[3]]
df$density <- get_density(df$rescaleX, df$rescaleY, n = 1000)

library(viridis)
df1 <- df %>% filter(Frame < 1800)
df2 <- df %>% filter(Frame %in% (1800:3600))
df3 <- df %>% filter(Frame  %in% (3600:5400))
df4 <- df %>% filter(Frame  %in% (5400:7200))
df5 <- df %>% filter(Frame  %in% (7200:9000))

library(gridExtra)

p1 <- ggplot(df1, aes(rescaleX, rescaleY, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()
p2 <- ggplot(df2, aes(rescaleX, rescaleY, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()
p3 <- ggplot(df3, aes(rescaleX, rescaleY, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()
p4 <- ggplot(df4, aes(rescaleX, rescaleY, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()
p5 <- ggplot(df5, aes(rescaleX, rescaleY, color = density)) +
  geom_point(alpha = 0.7, show.legend = FALSE)  +
  scale_color_viridis()

grid <- grid.arrange(p1, p2,p3,p4,p5, ncol=2)

return(grid)

}
