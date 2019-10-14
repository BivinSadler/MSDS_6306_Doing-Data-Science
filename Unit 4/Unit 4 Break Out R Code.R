#Live Session 4 For Live Session Web Scraping Code

library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Breakout 1 Restaurants!  

#using xml ... what is the problem?
data <-getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
zipcodes <- xpathSApply(doc,"//zipcode",xmlValue)
councildistrict <- xpathSApply(doc,"//councildistrict",xmlValue)
rests = data.frame(names,zipcodes,councildistrict)
dim(rests)
restsDTown = rests[which(rests$councildistrict == "11"),]
grep("Sushi",rests$names,ignore.case = T)


#Using rvest

hp<-read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
hp_name2 <- html_nodes(hp,"name")
hp_zipcode2 <- html_nodes(hp,"zipcode")
hp_councildistrict2 <- html_nodes(hp,"councildistrict")

hp_name2 = stri_sub(hp_name2,7,-8)
hp_zipcode2 = stri_sub(hp_zipcode2,10,-11)
hp_councildistrict2 = stri_sub(hp_councildistrict2,18,-19)

hp_zipcode2 = as.numeric(hp_zipcode2)
hp_councildistrict2 = as.numeric(hp_councildistrict2)



#How many restaurants total 
#restByDist = hist(hp_councildistrict2)
#barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants")
#barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Number of Restaurants",ylab = "Council District", horiz = TRUE)

RestaurantDF = data.frame(Name = hp_name2, Zip = hp_zipcode2, District = hp_councildistrict2)
RestaurantDF %>% ggplot(aes(x = District, fill = factor(District))) + geom_bar(stat = "count")

#How many Sushi Restaurants?
restsDTown = RestaurantDF %>% filter(District == "11")
grep("Sushi",restsDTown$Name,ignore.case = T)
grep("Sushi",restsDTown$Name,ignore.case = T)

