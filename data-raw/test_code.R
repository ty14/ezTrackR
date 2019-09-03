## TESTS

library(ezTrackR)

head(habit)

## try first function
df <- get_data(habit)
head(df)
distplot(df)


eg <- get_coords(cup)

df1 <- eg$xy

head(df1)


df1$newX <- ((df1$plotX - eg$box$left) / (eg$box$right - eg$box$left))*1000
df1$newY <- ((df1$plotY - eg$box$top) / (eg$box$bottom - eg$box$top))*1000

head(df1)

ggplot(df1, aes(x=newX,y=newY)) + geom_point(alpha=.3)

thigmotaxis(df)

