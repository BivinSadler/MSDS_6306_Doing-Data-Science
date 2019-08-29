#UNIT 2: Visualization | ggplot2, plotly (Video)
#THINGS TO REMEMBER
    #  %>% means "PIPE" 
    # aes means "AESTHETICS"
  # take mpg dataset, use ggplot, utilize aesthetics for mapping on x and y axis...
  # the + sign because ggplot was invented before the "pipe" so + serves the same purpose as pipe...
  # + says "hey add a geom_point to it, so we add geom_point then add a title
library(tidyverse)
mpg%>% ggplot(aes(x = hwy, y = cty)) + geom_point() + ggtitle("City MPG v. Highway MPG")
library(tidyverse)
plot(mpg$hwy,mpg$cty,pch = 15, main = "City MPG v. Highway MPG", ylab = "City MPG", xlab = "Highways MPG")

#HISTOGRAM
library(tidyverse)
hist(mpg$cty,col = "blue")
library(tidyverse)
mpg%>% ggplot(aes(x=cty)) + geom_histogram()
# For histogram we dont need a Y we only need one continuos variable that is X
# the plus sign is similar to pipe since ggplot2 was created before the pipe

#BOXPLOTS
library(tidyverse)
boxplot(cty~class, data = mpg, main = "Boxplot: City MPG v. Class") #base boxplot
mpg%>%ggplot(aes(x = class, y = cty)) + geom_boxplot()
# dataset, pipe then ggplot, aesthetics. the x here is a factor which gives us the ability to have diff boxplots on the bottom...
# x is a factor y is a continuous variable. We will add more to it later

#BARCHARTS
library(tidyverse) #Base Barchart
mpg$classFact = as.factor(mpg$class)
barplot(summary(mpg$classFact))
library(tidyverse)
mpg%>%ggplot(aes(x = class)) + geom_bar() + ggtitle("Distribution of Class")
#mpg, pipe, dataset ggplot, categorical variable + the count @ geom_bar

#GGPLOT TEMPLATE
ggplot(data = mpg),(mapping = aes(<MAPPINGS>)) + #this is where we map x and y, colors or fills
<GEOM_FUNCTION> #replace this with geom function or collection of mappings. Lets add MAPPING component here
#You will use the above template & keep modifying or extending it to make different types
ggplot(data = mpg, aes(x=cty)) + geom_histogram()
#the first argument in ggplot is data. geom function, geom point, ggplot we can explicity write the dataset like above...
#or we can sue pipe, say ggplot - either two ways
#What is a Template?
ggplot(data = <DATA>, mapping = aes(<MAPPINGS>))
<GEOM_FUNCTION>()+
<LABEL_FUNCTION>()+
<FACET_FUNCTION>()+
<COORD_FUNCTION>()+
<THEME_FUNCTION>()+
  
#2.4 MAPPING | AESTHETICS aes()
  #options: x and y, color, size, alpha, shape#
  
#`````````````` X and Y``````````````````
  library(tidyverse) #scatter plot
  mpg%>%
  ggplot(mapping=aes(x=hwy,y=cty))+ #Scatter Plot requires two continuous variables
  geom_point()
  # in above there is linear rship b/w hwy mpg and city mpg
  
  library(tidyverse) #histogram
  mpg%>%
  ggplot(mapping=aes(x=hwy))+ #Histogram does not require two continuous variables
  geom_histogram()

library(tidyverse) #barplot
mpg%>%
ggplot(mapping=aes(x=class))+ #Barplot does not require two continuous variables
geom_bar()
  
library(tidyverse) #Barplot
mpg%>%
ggplot(mapping=aes(x=class, y=..prop..,group=1))+ #Barplot does not require two continuous variables
geom_bar()

# ````````````````Color```````````````````
#In mapping we are mapping color to a variable below using color = mpg$drv
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) +
<GEOM_FUNCTION>()
library(tidyverse) #scatter plot
mpg%>%ggplot(aes(x=hwy,y = cty, color = mpg$drv))+ #Scatter Plot requires two continuous variables
geom_point()


# ```````````````Setting Color Manually - Scatter Plot``````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) +
  <GEOM_FUNCTION>(aes)()
library(tidyverse) #scatter plot
mpg%>%ggplot(aes(x=hwy, y = cty, color = "blue"))+ #Scatter Plot requires two continuous variables
  geom_point()
# Add (aes)() after <GEOM_FUNCTION> and set color manually to "blue" under color
#ERROR: notice the error, it colors it all red and not blue because "blue" is not a column in our data set


# `````````Correct Below```````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) +
  <GEOM_FUNCTION>(aes)()
library(tidyverse) #scatter plot
mpg%>%ggplot(aes(x = hwy, y = cty)) + geom_point(color="blue") #Scatter Plot requires two continuous variables

# `````````````````aes | color Histogram`````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) +
<GEOM_FUNCTION>()
library(tidyverse) #Histogram
mpg[mpg$class == "compact",] %>% ggplot(aes(x=cty, fill = drv))+ 
#here we are saying 'hey lets just look at the compact cars'. Just give me the mpg rows that are compact cars...
#and then lets say ggplot... fill means we are gonna fill the bar. for compact cars they only had forward and 4 wheel no rear
geom_histogram(color = "black") #this is color of the outline of a bar
# note we are now "mapping" fill to a variable (drv)
?mpg

# ```````````````Setting Color Manually - Histogram``````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) + #template
<GEOM_FUNCTION>() #template
library(tidyverse) #histogram
mpg[mpg$class == "compact",] %>%ggplot(aes(x=cty))+ #Histogram does not require two continuous variables
geom_histogram(fill = "blue", color = "black")
#Note that fill and color are not in an 'aes' funciton we are setitng them manually
#We are not "mapping" variables to colors... rather just setting the color"
#When we add a color manually we do it at geom_function, not mapping


# `````````````````aes | color Boxplot`````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) + #template
<GEOM_FUNCTION>() #template
library(tidyverse)
boxplot(cty~class, data = mpg, main = "Boxplot: City MPG v. Class") #base boxplot
mpg%>%ggplot(aes(x = class, y = cty, fill = drv)) + geom_boxplot(color = "blue")
#Note above that we are now "mapping" fill to a variable (drv)


# ```````````````Setting Color Manually - Boxplot``````````````````
mpg%>%ggplot(aes(x = class, y = cty)) + geom_boxplot(color = "blue", fill = "black")
# Please note above that fill and color are not in an aes funciton we are setting them manually
# We are not "mapping" variables to colors... rather just setting the color


#``````````````````` SIZE ````````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) + #template
<GEOM_FUNCTION>() #template
mpg%>%ggplot(aes(x = hwy, y = cty, size = mpg$displ)) + geom_point()

#``````````````````` ALPHA ````````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) + #template
<GEOM_FUNCTION>() #template
mpg%>%ggplot(aes(x = hwy, y = cty, alpha = mpg$displ)) + geom_point()

#``````````````````` SHAPE ````````````````````
ggplot(data = <DATA>, mapping = aes(<MAPPINGS)) + #template
<GEOM_FUNCTION>() #template
mpg%>%ggplot(aes(x = hwy, y = cty, shape = mpg$drv)) + geom_point()

#Quiz 2.4
mpg %>% ggplot(aes(x = displ, y = hwy, color = drv)) + geom_point()


# *******************************GEOMETRIC OBJECTS | GEOMS ********************************************
# So far we have learned about these 4 ---> geom_point() , geom_boxplot, geom_histogram, and geom_barplot()
# HERE WE ARE GOING TO LEARN ABOUT geom_smooth()
library(tidyverse) #scatter plot GEOM POINT
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point()

library(tidyverse) #scatter plot GEOM SMOOTH low S curve
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth()

library(tidyverse) #scatter plot GEOM SMOOTH fit 3 low S curve one for each type of drive
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv)) + geom_smooth()
#we want three lines because we have 3 levels of drive - f, r and 4
#Every geom function in ggplot2 takes a mapping argument. However, not every aesthetic works with every geom....
#You could set the shape of a point, but you couldn't set the "shape" of a line...
#On the other hand, you 'could' set the linetype of a line. geom_smooth() will draw a different line, with a different linetype...
#for each unique value of the variable that you map to linetype

#  ~~~~~~~~~~ ******* YOU TRY IT!  ******** ~~~~~~~~~~~
# Analyze city vs highway mpg
library(tidyverse)
mpg%>% ggplot(aes(x = hwy, y = cty)) + geom_point() + ggtitle("City MPG v. Highway MPG")

#Assess if there is evidence that the relationship between city and highway mpg is...
#different based on the drive of the car (4 wheel, front or rear)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, linetype = drv)) + geom_smooth()
# Th data shows an uphill pattern as you move from left to right, this indicates 
#a positive relationship between city and highway mpg. As the X-values 
#increase (move right), the Y-values tend to increase (move up).

#Scatter plot with just the points
mpg%>% ggplot(aes(x = hwy, y = cty)) + geom_point() + ggtitle("City MPG v. Highway MPG")

#Plot with the smoothed line
mpg%>% ggplot(aes(x = hwy, y = cty)) + geom_smooth() + ggtitle("City MPG v. Highway MPG")

#Plot that has a different line for each level of drive
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, linetype = drv)) + geom_smooth()

#Plot that has the smoothed line and the points and designate the type of 
#drive by color.. in both the line and the points
library(tidyverse)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv)) + 
geom_smooth()

#Create a plot that 









# CREATING A GGPLOT (textbook Hadley)
library(tidyverse)
ggplot2::mpg
?mpg
ggplot(data = mpg) + #this creates an empty graph 
  geom_point(mapping = aes(x = displ, y = hwy)) #geom_point adds a layer of points to your plot which creates scatter plot
#each geom function in ggplot2 takes a 'mapping' argument so you put mapping
#mapping defines how variables in a dataset are mapped to visual properties. mapping argument is always paired with 'aes()'
#the x and y arguments of aes() specify which variables to map to the x and y axis
#I confirm the hypotheses of fuel efficiency and engine size because the data shows a downhill pattern as you move...
#from left to right, this indicates a negative relationship between X and Y...
#As the X-values increase (move right) the Y-values tend to decrease (move down)

#A GRAPHING TEMPLATE - MAPPING COMPONENT
ggplot(data = <DATA>) + #replace this with dataset 
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>)) #replace this with geom function or collection of mappings. Lets add MAPPING component here
#You will use the above template & keep modifying or extending it to make different types of graphs

#Exercise 1
ggplot(data = mpg)

#Exercise 2
ggplot(mtcars) 

#Exercise 3
?mpg
#drv
f = front-wheel drive, r = rear wheel drive, 4 = 4wd

#Exercise 4 Make a scatterplot of hwy  vs cyl
library(tidyverse)
ggplot2::mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))
#In above scatter plot a linear relationship between X and Y exists because the pattern of X– and Y-values...
#resembles a line going downhill (which is a negative slope), vs uphill (with a positive slope)

#Exercise 5 What happens if you make a scatterplot of class vs drv? why is the plot not useful?
library(tidyverse)
ggplot2::mpg
ggplot(data = mpg) + 
geom_point(mapping = aes(x = class, y = drv))
#The plot is not useful because  the data don’t seem to resemble any kind of pattern (even a vague one),...
#therefore no relationship exists between X and Y.

