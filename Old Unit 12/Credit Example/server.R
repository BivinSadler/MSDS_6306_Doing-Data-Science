# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    credit <- read.csv(input$file1$datapath,
                   header = TRUE)
    
    #make resposnse a factor rather than 0,1
    credit$default.payment.next.month = factor(credit$default.payment.next.month,labels = c("NoDefault","Default"))
    summary(credit)
    #plot the data
    credit %>% ggplot(aes(x = AGE, y = LIMIT_BAL,color = default.payment.next.month)) + geom_point()
    
    #Create standardized variables for later. 
    credit$Z_Lim = (credit$LIMIT_BAL-mean(credit$LIMIT_BAL))/sd(credit$LIMIT_BAL)
    credit$Z_AGE = (credit$AGE-mean(credit$AGE))/sd(credit$AGE)
    
    #create training and test sets
    trainInd = sample(seq(1,30000,1), .8*30000)
    train = credit[trainInd,]
    test = credit[-trainInd,]
    
    #Raw Limit and AGE
    classifications = knn(train[,c(2,6)],test[,c(2,6)],train$default.payment.next.month,prob = TRUE, k = input$k)
    confusionMatrix(table(classifications,test$default.payment.next.month))
    
    #Standardized
    classifications = knn(train[,c(15,16)],test[,c(15,16)],train$default.payment.next.month,prob = TRUE, k = input$k)
    CT = confusionMatrix(table(classifications,test$default.payment.next.month))
    
    dfOutput = data.frame(Stat = names(CT$byClass), Value = CT$byClass)

  
  })
  
  output$kplot <- renderPlot({
    
    #tune k (hyperparameter)
    
    iterations = 10
    numks = 20
    
    masterAcc = matrix(nrow = iterations, ncol = numks)
    
    for(j in 1:iterations)
    {
      trainInd = sample(seq(1,30000,1), .8*30000)
      train = credit[trainInd,]
      test = credit[-trainInd,]
      
      for(i in 1:numks)
      {
        classifications = knn(train[,c(2,6)],test[,c(2,6)],train$default.payment.next.month,prob = TRUE, k = i)
        CM = confusionMatrix(table(classifications,test$default.payment.next.month))
        masterAcc[j,i] = CM$overall[1]
      }
      
    }
  
    MeanAcc = colMeans(masterAcc)
    dfPlot = data.frame(x = seq(1,numks,1), y = MeanAcc)
    #return(MeanAcc)
    #return(plot(seq(1,numks,1),MeanAcc, type = "l"))
    #hist(rnorm(1000,0,1))
    dfPlot %>% ggplot(aes(x = x, y = y)) + geom_line() +ggtitle("Optimal K")
    
  })
  
}
