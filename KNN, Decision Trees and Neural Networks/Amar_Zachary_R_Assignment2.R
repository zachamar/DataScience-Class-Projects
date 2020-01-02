#######
#PART 1
#######

library(readr)
UniversalBank <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/UniversalBank.csv")
attach(UniversalBank)

#Split Data
train1 = UniversalBank[c(1:3000),]
test1 = UniversalBank[c(3001:5000),]

#Split out the Education Data 
train1$Education1<-ifelse(train1$Education==1,1,0)
train1$Education2<-ifelse(train1$Education==2,1,0)
train1$Education3<-ifelse(train1$Education==3,1,0)

# Find the k-nearest neighbor for Record Given using k=1, Euclidean distance, 
    #target variable = Personal loan

  # Normalize the observations
  train1.norm = train1
  train1.norm$Age <- (train1.norm$Age - min(train1.norm$Age))/(max(train1.norm$Age) - min(train1.norm$Age))
  train1.norm$Experience <- (train1.norm$Experience - min(train1.norm$Experience))/(max(train1.norm$Experience) - min(train1.norm$Experience))
  train1.norm$Income <- (train1.norm$Income - min(train1.norm$Income))/(max(train1.norm$Income) - min(train1.norm$Income))
  train1.norm$Family <- (train1.norm$Family - min(train1.norm$Family))/(max(train1.norm$Family) - min(train1.norm$Family))
  train1.norm$CCAvg <- (train1.norm$CCAvg - min(train1.norm$CCAvg))/(max(train1.norm$CCAvg) - min(train1.norm$CCAvg))
  train1.norm = train1.norm[,-c(1,5,8)]
  
  #Create new record
  new.record = data.frame(Age=40,Experience=10,Income=84,Family=2,CCAvg=2, Mortgage=0,Securities.Account=0, CD.Account=0,Online=1, CreditCard=1,Education_1=0,Education_2=1, Education_3=0)
  new.record.norm = new.record
  new.record.norm$Age <- (new.record.norm$Age - min(train1 $Age))/(max(train1$Age) - min(train1$Age))
  new.record.norm$Experience <- (new.record.norm$Experience - min(train1$Experience))/(max(train1$Experience) - min(train1$Experience))
  new.record.norm$Income <- (new.record.norm$Income - min(train1$Income))/(max(train1$Income) - min(train1$Income))
  new.record.norm$Family <- (new.record.norm$Family - min(train1$Family))/(max(train1$Family) - min(train1$Family))
  new.record.norm$CCAvg <- (new.record.norm$CCAvg - min(train1$CCAvg))/(max(train1$CCAvg) - min(train1$CCAvg))
  
  #Classify it
  library(class)
  knn.record1 = knn(train=train1.norm[,c(1:6,8:14)], test=new.record.norm, cl=train1.norm$`Personal Loan`, k=1)
  knn.record1
  
#Find the optimal value of k
  
  #Split out the Education Data in test
  test1$Education1<-ifelse(test1$Education==1,1,0)
  test1$Education2<-ifelse(test1$Education==2,1,0)
  test1$Education3<-ifelse(test1$Education==3,1,0)

  #Normalize the test data
  test1.norm = test1
  test1.norm$Age <- (test1.norm$Age - min(test1.norm$Age))/(max(test1.norm$Age) - min(test1.norm$Age))
  test1.norm$Experience <- (test1.norm$Experience - min(test1.norm$Experience))/(max(test1.norm$Experience) - min(test1.norm$Experience))
  test1.norm$Income <- (test1.norm$Income - min(test1.norm$Income))/(max(test1.norm$Income) - min(test1.norm$Income))
  test1.norm$Family <- (test1.norm$Family - min(test1.norm$Family))/(max(test1.norm$Family) - min(test1.norm$Family))
  test1.norm$CCAvg <- (test1.norm$CCAvg - min(test1.norm$CCAvg))/(max(test1.norm$CCAvg) - min(test1.norm$CCAvg))
  test1.norm = test1.norm[,-c(1,5,8)]
  
  #Run loop to determine best k
  library(Metrics)
  #rule of thumb = sqrt(n); sqrt(5000) = 71 so i will test from k = 1 to k = 100
  knn.rmse <- data.frame(k = seq(1,100,1), rmse = rep(0,100))
  
  for(i in 1:100) {
    knn.pred <- class::knn(train = train1.norm[,c(1:6,8:14)],test = test1.norm[,c(1:6,8:14)],
                           cl = train1.norm$`Personal Loan`, k = i)
    knn.rmse[i,2] <- rmse(test1.norm[,7],as.numeric(as.character(knn.pred)))  
  }
  knn.rmse
    #When oppenning the dataset order the rmse column from min to max and determine best k
    # In this case it is 3
  
  knn.record1_k3 = knn(train=train1.norm[,c(1:6,8:14)], test=new.record.norm, cl=train1.norm$`Personal Loan`, k=3)
  knn.record1_k3
  
  
#######
#PART 2
#######
library(readr)
FlightDelay <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/FlightDelays.csv", 
                        col_types = cols(`Flight Status` = col_factor(levels = c("ontime", 
                                                                                 "delayed"))))
    #Question 4 (With Weather)
    FlightDelay2 = FlightDelay[,-c(3,6,7,12)]
    
    library("rpart")
    library("rpart.plot")
    
    #Set Variables As Factor
    
    FlightDelay2$Weather = as.factor(FlightDelay2$Weather)
    FlightDelay2$DAY_OF_MONTH = as.factor(FlightDelay2$DAY_OF_MONTH)
    FlightDelay2$DAY_WEEK = as.factor(FlightDelay2$DAY_WEEK)
    FlightDelay2$CARRIER = as.factor(FlightDelay2$CARRIER)
    FlightDelay2$DEST = as.factor(FlightDelay2$DEST)
    FlightDelay2$ORIGIN = as.factor(FlightDelay2$ORIGIN)
    
    #Build Tree
    mytree = rpart(FlightDelay2$`Flight Status`~., data=FlightDelay2, method="class")
    rpart.plot (mytree, extra = 101, digits = -4, main = "Classification Tree")
    
    #Question 5 (Without Weather)
    FlightDelay3 = FlightDelay2[,-c(6)]
    
    mytree_noW = rpart(FlightDelay3$`Flight Status`~., data=FlightDelay3, method="class")
    rpart.plot (mytree_noW, extra = 101, digits = -4, main = "Classification Tree")
    
#######
#PART 3
#######
    
library(readxl)
EastWestAirlines = read_excel(path = "~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/EastWestAirlines.xlsx", sheet = "data", col_names = TRUE)
attach(EastWestAirlines)

  #Normalize the data
  library(caret)
  norm.values <- preProcess(EastWestAirlines[,c(3,4,8,9,10,11,12)],method="range")
  EastWestAirlines[,c(3,4,8,9,10,11,12)] <- predict(norm.values, EastWestAirlines[,c(3,4,8,9,10,11,12)])
  
  #Preapre dataset
  EastWestAirlines2 =EastWestAirlines[,c(-1)]
  library(plyr)
  EastWestAirlines3 = rename(EastWestAirlines2, c("cc1_miles?"="cc1_miles", "cc2_miles?"="cc2_miles", "cc3_miles?"="cc3_miles"))  
  
  # split the data into training and validation 
  set.seed(100)
  train.index <- sample(row.names(EastWestAirlines), .6*dim(EastWestAirlines))
  valid.index <- setdiff(row.names(EastWestAirlines), train.index)
  train.EWA <- EastWestAirlines[train.index,]
  valid.EWA <- EastWestAirlines[valid.index,]
  
  #Run Neural Network (5 nodes)
  library(neuralnet)
  set.seed(1)
  EWA.nn <- neuralnet(Phone_sale ~ Topflight + Balance + Qual_miles + `cc1_miles?` + `cc2_miles?` + `cc3_miles?` + 
                      Bonus_miles + Bonus_trans + Flight_miles_12mo + Flight_trans_12 + Online_12 + Email + 
                      Club_member + Any_cc_miles_12mo, data = train.EWA, linear.output = F, 
                      hidden = 5,lifesign = "full")
  ## Plot the graph
  plot(EWA.nn, rep="best")  
  
  ## Evaluate the performance of the model
  training.prediction <- compute(EWA.nn, train.EWA[,-c(1,16)])
  training.prediction = ifelse((training.prediction$net.result>0.5),1,0)
  confusionMatrix(training.prediction, train.EWA$Phone_sale)
  
  validation.prediction <- compute(EWA.nn, valid.EWA[,-c(1,16)])
  validation.prediction = ifelse((validation.prediction$net.result>0.5),1,0)
  confusionMatrix(validation.prediction, valid.EWA$Phone_sale)
  
  
   #Run Neural Network (1 node1)
  set.seed(1)
  EWA.nn_1 <- neuralnet(Phone_sale ~ Topflight + Balance + Qual_miles + `cc1_miles?` + `cc2_miles?` + `cc3_miles?` + 
                        Bonus_miles + Bonus_trans + Flight_miles_12mo + Flight_trans_12 + Online_12 + Email + 
                        Club_member + Any_cc_miles_12mo, data = train.EWA, linear.output = F, 
                      hidden = 1,lifesign = "full")
  
  ## Evaluate the performance of the model
  training.prediction2 <- compute(EWA.nn_1, train.EWA[,-c(1,16)])
  training.prediction2 = ifelse((training.prediction2$net.result>0.5),1,0)
  confusionMatrix(training.prediction2, train.EWA$Phone_sale)
  
  validation.prediction2 <- compute(EWA.nn_1, valid.EWA[,-c(1,16)])
  validation.prediction2 = ifelse((validation.prediction2$net.result>0.5),1,0)
  confusionMatrix(validation.prediction2, valid.EWA$Phone_sale)

  
  
  
  