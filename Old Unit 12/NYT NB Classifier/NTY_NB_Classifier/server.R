#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
 # output$bdateText <- renderText({
    
 #   paste("Beginning Date is: ", str_replace_all(as.character(input$bdate),"[^[:alnum:] ]", ""))

    
 # })
  
 # output$edateText <- renderText({
    
    
  #  paste("Ending Date is: ", str_replace_all(as.character(input$edate),"[^[:alnum:] ]", ""))
    
  #})
  
  output$ConfusionMatrix = renderPrint({
    
    b = str_replace_all(as.character(input$bdate),"[^[:alnum:] ]", "")
    e = str_replace_all(as.character(input$edate),"[^[:alnum:] ]", "")
    
    library(tm) #text mining library provides the stopwords() function
    library(tidyr)
    library(plyr)
    library(jsonlite)
    library(dplyr)
    library(tidyverse)
    library(stringr)
    library(caret)
    
    NYTIMES_KEY = "OG89fUubcS8FXofVrLA4dmIOHh5omiFa" #Your Key Here … get from NTY API website
    
    # Let's set some parameters
    term <- "Central+Park+Jogger" # Need to use + to string together separate words
    begin_date <- as.character(b)
    end_date <- as.character(e)
    
    baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                      "&begin_date=",begin_date,"&end_date=",end_date,
                      "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")
    
    baseurl
    
    initialQuery <- jsonlite::fromJSON(baseurl)
    maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)
    maxPages
    
    pages <- list()
    for(i in 0:maxPages){
      nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
      message("Retrieving page ", i)
      pages[[i+1]] <- nytSearch 
      Sys.sleep(7) 
    }
    
    allNYTSearch <- rbind_pages(pages)
    
    
    #Segmentation
    
    # Visualize coverage by section
    allNYTSearch %>% 
      group_by(response.docs.type_of_material) %>%
      dplyr::summarize(count=n()) %>%
      mutate(percent = (count / sum(count))*100) %>%
      ggplot() +
      geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()
    
    #Make another column of News versus Other ... The labels
    
    allNYTSearch$NewsOrOther = ifelse(allNYTSearch$response.docs.type_of_material == "News","News","Other")
    #There is an NA in NewsOrOther
    
    # Visualize coverage of News or Other
    allNYTSearch[!is.na(allNYTSearch$NewsOrOther),] %>% 
      group_by(NewsOrOther) %>%
      dplyr::summarize(count=n()) %>%
      mutate(percent = (count / sum(count))*100) %>%
      ggplot() +
      geom_bar(aes(y=percent, x=NewsOrOther, fill=NewsOrOther), stat = "identity") + coord_flip()
    
    
    
    #Train and Test Split 70%/30%
    
    set.seed(2)
    trainInd = sample(seq(1,dim(allNYTSearch)[1],1),round(.7*dim(allNYTSearch)[1]))
    allNYTSearchTrain = allNYTSearch[trainInd,]
    allNYTSearchTest = allNYTSearch[-trainInd,]
    
    
    #This function returns P(News | Keyword) 
    #P(News|KW) = P(KW|News)* P(News) / P(KW)
    Pnews_word = function(key_word, trainingSet, alphaLaplace = 1, betaLaplace = 1) # alpha and beta are for laplace smoothing
    {
      trainingSet$response.docs.headline.main = unlist(str_replace_all(trainingSet$response.docs.headline.main,"[^[:alnum:] ]", "")) #Take out all but alpha numeric characters from training headlines
      
      #print(key_word)
      NewsGroup = trainingSet[trainingSet$NewsOrOther == "News",]
      OtherGroup = trainingSet[trainingSet$NewsOrOther == "Other",]
      
      pNews = dim(NewsGroup)[1] / (dim(NewsGroup)[1] + dim(OtherGroup)[1])
      pOther = 1 - pNews
      
      pKWGivenNews = (length(str_which(NewsGroup$response.docs.headline.main,regex(str_c("\\b",key_word,"\\b",sep=""),ignore.case = TRUE)))+alphaLaplace)/(dim(NewsGroup)[1]+betaLaplace)
      pKWGivenOther = (length(str_which(OtherGroup$response.docs.headline.main,regex(str_c("\\b",key_word,"\\b",sep=""),ignore.case = TRUE)))+alphaLaplace)/(dim(OtherGroup)[1]+betaLaplace)
      
      pKW = length(str_which(trainingSet$response.docs.headline.main,regex(str_c("\\b",key_word,"\\b",sep=""),ignore.case = TRUE)))/dim(trainingSet)[1]
      
      pNewsGivenKW = pKWGivenNews*pNews/pKW
      pOtherGivenKW = pKWGivenOther*pOther/pKW
      
      return(pNewsGivenKW)
    }
    
    theScoreHolderNews = c()
    theScoreHolderOther = c()
    articleScoreNews = 0;
    articleScoreOther = 0;
    
    
    for (i in 1 : dim(allNYTSearchTest)[1])  #This loop iterates over the articles in the Test Set
    {
      
      articleScoreNews = 1; 
      articleScoreOther = 1;
      
      #The [^[:alnum:] ] replaces all non alphanumeric characters with nulls.  
      theText = unlist(str_split(str_replace_all(allNYTSearchTest[i,]$response.docs.headline.main,"[^[:alnum:] ]", ""), stringr::boundary("word"))) #Take out all but alpha numeric characters from search string ... theText holds each word in the headline as its own word.  
      
      # stopwords() #from package tm
      wordsToTakeOut = stopwords()
      
      # put word boundaries stopwords so that we don't detect partial words later
      wordsToTakeOut = str_c(wordsToTakeOut,collapse = "\\b|\\b") 
      wordsToTakeOut = str_c("\\b",wordsToTakeOut,"\\b")
      #wordsToTakeOut
      
      importantWords = theText[!str_detect(theText,regex(wordsToTakeOut,ignore_case = TRUE))]
      
      #importantWords
      
      for(j in 1 : length(importantWords))  #This loop iterates over the important words in the headline
      {
        articleScoreNews = articleScoreNews * Pnews_word(importantWords[j],allNYTSearchTrain)
        articleScoreOther = articleScoreOther * (1 - Pnews_word(importantWords[j],allNYTSearchTrain))
      }
      theScoreHolderNews[i] = articleScoreNews
      theScoreHolderOther[i] = articleScoreOther
    }
    
    # Classify the aricle as News or Other based on a given piece of information from the article.
    allNYTSearchTest$Classified = ifelse(theScoreHolderNews > theScoreHolderOther,"News","Other")
    
    #Confusion Matrix
    confusionMatrix(factor(allNYTSearchTest$Classified),factor(allNYTSearchTest$NewsOrOther)) #Actual in Columns
  
    
  })
})
