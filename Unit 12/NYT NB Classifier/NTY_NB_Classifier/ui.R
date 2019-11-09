#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NYT Article Classifier!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       dateInput("bdate", label = h3("Begin Date"), value = "2014/01/01"),
       
       dateInput("edate", label = h3("End Date"), value = "2015/01/01")
       
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
    
      column(12,
      verbatimTextOutput("bdateText"),
      verbatimTextOutput("edateText"),
      verbatimTextOutput("ConfusionMatrix")
      )
    )
  )
))
