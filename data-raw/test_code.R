## TESTS

library(ezTrackR)

head(habit)

## try first function
df <- get_data(habit)
head(df)
distplot(df)



### James new stuff
hdata <- get_coords_habit(habit) #gets rescaled raw data and box coordinates
hdata$xy #rescaled data
hdata$box #box coordinates

distcalc(hdata$xy) #gets distance calculated
distplot(distcalc(hdata$xy)) #distance plot
quickplot(hdata$xy)

#get immobility
distdf <- distcalc(hdata$xy)

immobility(distdf)

immobility(distdf, X=1, N=3) # 3 seconds at less than 1 unit traveled
immobility(distdf, X=2, N=1.5) # 1.5 seconds at less than 2 unit traveled


##############3


eg <- get_coords(cup)

df1 <- eg$xy

head(df1)


df1$newX <- ((df1$plotX - eg$box$left) / (eg$box$right - eg$box$left))*1000
df1$newY <- ((df1$plotY - eg$box$top) / (eg$box$bottom - eg$box$top))*1000

head(df1)

ggplot(df1, aes(x=newX,y=newY)) + geom_point(alpha=.3)

thigmotaxis(df)

