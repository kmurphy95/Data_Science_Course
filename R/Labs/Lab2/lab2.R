#Author: Kathy Murphy
#Date: September 11, 2020
#Purpose: EDA and Visualization
#Breakout Group #6

rm()

#Load ggplot
library(ggplot2)
ggplot(data = mpg)

#Relationship between engine size and fuel efficiency
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))


#Exercise 1 (Part 1)
# 11 columns, 234 rows, drv describes whether or not the vehicle
# is a front or backwheel drive,

#Make a scatterplot of hwy vs cyl (MPG for vehicles at various cylinder counts)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

#Make a scatterplot of class vs drv
ggplot(data = mpg)+
  geom_point(mapping = aes(x = class, y = drv))
#Not useful because Drv and class are categorical variables and would be better
#represented by a different visualization (a bar graph)
#It is only showing whether or not two categories overlap at some point in the
#data


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
#Use the color in the legend to determine which class a vehicle belongs to on
#the previous graphic for hwy v displ

#Exercise 2 (Part 3)
#Categorical variables/columns: manufacturer, model, year, cyl, trans, drv, fl
#                               class
#Continuous variables/columns: displ, cty, hwy


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

#Mapping continuous variables to color gives a gradiant of colors that represents
#lower numbers with one tone and higher numbers with another
#For size, there is a legend of different point sizes, and for shape,
#an error is returned. Shape cannot use continuous variables.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cty, color = cty))

#Mapping the same variable to multiple aesthetics creates two visual indicators
#for that variable. The points are larger for larger values, and they are also
#brighter. It combines the effects of the aesthetics but permits you to use the
#same variable for two different aesthetic effects.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

#Mapping the aesthetic qualities to non-variables is permitted. After x >= 5
#the color of the observations in the visualizations change.

#Exercise 3 (Part 4)

#Create subplots of hwy vs displ for each class of vehicle
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

#Create faceted plots based on two variables in a grid
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

#What happens if you facet on a continuous variable?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ cty, nrow = 2)
#It creates a bunch of plots for each unique observation

#What do the empty cells in plot with facet_grid(drv ~ cyl) mean?
#It means that no observations fell into that intersection of categories.
#There are no rear drive vehicles in the 4 or 5 size category, or 4wd vehicles
#in the 5 category.

#How do they relate to this graph?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))
#Where there are no black points in this plot's intersections, there are blank
#plots in the grid visualization. They represent a place in the data where there
#are no observations that fit all of the criteria.


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)
#Advantage: Data for each class is separated so it is easy to see how the vehicle
#classes affected the hwy mpg in relation to the size of the engine.

#Disadvantage: Certain plots in the faceted view have very few observations
#and take up disproportionate amounts of space compared to their color-coded
#counterparts.
