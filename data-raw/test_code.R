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





##############

#note get_coords_habit also works with cup data
# can use as a quick way to calculate distances/immobility

cdata <- get_coords_habit(cup) #gets rescaled raw data and box coordinates
cdata$xy #rescaled data
cdata$box #box coordinates

distcalc(cdata$xy) #gets distance calculated
distplot(distcalc(cdata$xy)) #distance plot
quickplot(cdata$xy)

#get immobility
distdf <- distcalc(cdata$xy)
immobility(distdf)


##############
#get center and thigmotaxis distance/time use habit data and get_coord_habit
df <- get_data(habit)
df1 <- get_coords_habit(habit)


#Actually function
thigmotaxis(df,df1)


#############
# getting cup_exploration
df <- cup
df1 <- get_coords(cup)

cup_exploration(df,df1)






