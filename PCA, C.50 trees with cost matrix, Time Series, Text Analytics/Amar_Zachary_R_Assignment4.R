#PART 1

library(readxl)
baseball <- read_excel("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/baseball.xlsx")

baseballnum = baseball[,c(3,5:19)]

  # standardize  variables
  norm.values <- preProcess(baseballnum[,1:16], method=c("center","scale"))
  baseballnum[,1:16] <- predict(norm.values, baseballnum[,1:16])
  
  # create training (90%) and testing (10%) datasets to validate pca later
  set.seed(2)
  training=sample(row.names(baseballnum), dim(baseballnum)[1]*0.90)
  validation=setdiff(row.names(baseballnum), training)
  train.bsball <- baseballnum[training,]
  test.bsball <- baseballnum[validation,]

  #Analyse Variables
  library(psych)
  
  #Descriptive Stats
  describe(train.bsball)
    #When looking at min, max, mean, stdev, skew and kurtosis there seems to be no concern
  
  #Correlation Stats
  as.dist((round(cor(train.bsball), 3)))
    #All variables except age are generally correlated between each other
  
  # Generate principal component analysis
  
  # PCA with all 16 predictors as factors
  pca.bsball <- principal(train.bsball,nfactors=16,rotate="none")
  
  #How many components should be extracted?
    # The Proportion of Variance Explained Criterion:
    pca.bsball$loadings  
    # The Eigenvalue Criterion:
    pca.bsball$values        
    # The Minimum Communality Criterion
    pca.bsball$loadings      
    # The Screen Plot Criterion:
    plot(pca.bsball$values,type="b",main = "Screen Plot for Baseball Data")
  
  #Validate Hypothesis
  pca.bsball2 <- principal(test.bsball,nfactors=4,rotate="none")
  pca.bsball2$loadings     
  

 
  
  #5. What is the increase/decrease in revenue when the cost matrix is incorporated?
  c50cost <- C5.0(loans_train[,3:5], loans_train[,6], costs = cost_matrix_adjusted_norm, control = C5.0Control(CF = .1))
  model.c50cost.pred <- predict(object = c50cost, newdata = loans_test)
  
  #See accuracy of model & resulting matrix
  accuracy.cost <- confusionMatrix(loans_test[,6], model.c50cost.pred)
  accuracy.cost
  accuracy.cost$table
  
  #Calculate the total cost
  total_cost_with_matrix = (accuracy.cost$table * cost_matrix_adjusted)
  total_cost_with_matrix
  sum(total_cost_with_matrix)
  
  summary(c50cost)
  
  sum(total_cost) 
  sum(total_cost_with_matrix)
  
  #Calculate cost difference
  cost_savings = (-sum(total_cost) + sum(total_cost_with_matrix))
  cost_savings
  #Cost savings of 162,792,627  
  
  
#PART 2

library(readr)
Loans_Training <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/Loans_Training_3.csv")
Loans_Test <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/Loans_Test_3.csv")

  Loans_Test$Approval3 = as.factor(Loans_Test$Approval3)
  levels(Loans_Test$Approval3)
  levels(Loans_Test$Approval3)[1]<-"1 - Denied"

  #Standardize interest column
  library(caret)
  norm.values <- preProcess(Loans_Training[,c(2)],method=c("center","scale"))
  Loans_Training[,c(2)] <- predict(norm.values, Loans_Training[,c(2)])
  norm.values2 <- preProcess(Loans_Test[,c(2)],method=c("center","scale"))
  Loans_Test[,c(2)] <- predict(norm.values2, Loans_Test[,c(2)])

  #No Cost model
    library("C50")
    nocostmodel <- C5.0(Loans_Training[,3:5],as.factor(Loans_Training$Approval3))
   
    # Predict with test data
    nocostmodel.pred <- predict(nocostmodel, newdata=Loans_Test[,3:5])
    
    # Obtain the confusion matrix with cutoff = 0.5 
    Loans_Test$Approval3 # Actual categories
    nocostmodel.pred   # Predicted categories
    confusionMatrix(nocostmodel.pred, Loans_Test$Approval3)
  

  #Cost model (See Excel file for development of it)
  cost.matrix = matrix(c(0,2.22,6.44,
                         5.44,0,3.22,
                         6.44,1,0),
                       3,3)
  rownames(cost.matrix) = colnames(cost.matrix) = levels(as.factor(Loans_Training$Approval3))
  cost.matrix
  
  c50cost<-C5.0(x=Loans_Training[,3:5],as.factor(Loans_Training$Approval3), costs=cost.matrix)
  
  c50cost.pred = predict(c50cost, newdata=Loans_Test[,3:5])
  
  confusionMatrix(c50cost.pred, Loans_Test$Approval3)
  
  
#PART 3

library(readxl)
SouvenirSales <- read_excel("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/SouvenirSales.xls", 
                              col_types = c("date", "numeric", "skip", 
                                            "skip"))
  #Well Fitted plot of the series
  library(forecast)
  dept.sales.ts <- ts(SouvenirSales$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12) 
  plot(dept.sales.ts, main = "Time Series of Souvenir Sales", xlab = "Time", ylab = "Sales")

  #Log the varibales
  SouvenirSales$Saleslog = log(SouvenirSales$Sales)
  log.dept.sales.ts <- ts(SouvenirSales$Saleslog, start = c(1995, 1), end = c(2001, 12), freq = 12) 
  
  log.dept.sales.lm <- tslm(log.dept.sales.ts ~ trend)
  plot(log.dept.sales.ts, main = "Time Series of Souvenir Sales (Logged)", xlab = "Time", ylab = "Sales (Logged)")
  lines(log.dept.sales.lm$fitted, lwd=2)
  
  #Data partitionning
    #Check if there is seasonality
      dept.sales.components <- decompose(dept.sales.ts) 
      plot(dept.sales.components)
        #Observe an exponential trend and seasonlity

#PART 4

library(readxl)
farm_ads <- read_excel("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/Farm-ads.xlsx", sheet = "Sheet1", col_names = TRUE)

  names(farm_ads)[1] <- "Relevance"
  names(farm_ads)[2] <- "ads_keywords"
  
  # Develop the Term Document Matrix
  library(tm)
  farm_ads$ads_keywords = gsub('-', ' ', farm_ads$ads_keywords)
  # define a vector of sentences ("docs")
  text <- farm_ads$ads_keywords
  
  #Create corpus
  corpus.add <- Corpus(VectorSource(text))
  
  #Tokenisation, Stopwords removal, Stemming
  corpus.add <- tm_map(corpus.add, stripWhitespace)
  corpus.add <- tm_map(corpus.add, removePunctuation)
  corpus.add <- tm_map(corpus.add, removeNumbers)
  corpus.add <- tm_map(corpus.add, removeWords, stopwords("english"))
  corpus.add<-tm_map(corpus.add,stemDocument)
  
  #Term document matrix
  tdm <- TermDocumentMatrix(corpus.add)
  tfidf<-weightTfIdf(tdm)
  inspect(tdm)
  
  #Extract 20 concepts
  library(lsa)
  lsa.tfidf<-lsa(tfidf,dim=20)
  
  #Logistic preditions
  
  label<-c(rep(0,1933),rep(1,2210))
  
  # convert to data frame
  words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
  
  # sample 60% training data
  set.seed(1)
  training<-sample(c(1:4143), 0.6*4143)
  
  # run logistic model on training
  trainData=cbind(label=label[training],words.df[training,])
  set.seed(1)
  reg<-glm(label~.,data=trainData,family='binomial')
  summary(reg)
  
  # compute accuracy on validation set
  validData=cbind(label=label[-training],words.df[-training,])
  pred<-predict(reg,newdata=validData,type="response")
  
  # produce the confusion matrix
  library(caret)
  confusionMatrix(ifelse(pred>0.5,1,0),label[-training])
  