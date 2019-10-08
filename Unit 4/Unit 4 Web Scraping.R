#Live Session 4 For Live Session Web Scraping Code

library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML

# Method 1: XML

data <-getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
length(grep("covered",bfasts$description))
grepl("covered",bfasts$description)
sum(grepl("covered",bfasts$description))
which(grepl("covered",bfasts$description))


# Method 2: rvest

hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_nameR <- html_nodes(hp,"name")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
bfast = data.frame(hp_name,hp_price,hp_desc)
grep("toast", bfast$hp_desc)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))