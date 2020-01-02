#PART 1
library(readr)
cereals <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/cereals.CSV")

#Question1

  #Clean Data Set
  cereals2 = na.omit(cereals[,-c(1,16)])
  
  #Dummify factor variables
  cereals3 = cereals2
  manuf.matrix <- model.matrix(~ Manuf - 1, data = cereals3)
  type.matrix <- model.matrix(~ Type - 1, data = cereals3)
  cereals3 <- data.frame(cereals2[,-c(1,2)],manuf.matrix,type.matrix) 

  #Normalize the Data
  cereals3.norm <- sapply(cereals3, scale)
  row.names(cereals3.norm) <- row.names(cereals3) 

  #Run the kmeans algorithm with k=2
  set.seed(1)
  km.cereals <- kmeans(cereals3.norm, 2)
  km.cereals  
  
#Question 2
  library(factoextra)
  fviz_nbclust(cereals3.norm, kmeans, method = "wss")
    #pick 7 clusters
  
#Question 3
  
  #Run the kmeans algorithm with k=7
  set.seed(1)
  km.cereals7 <- kmeans(cereals3.norm, 7)
  km.cereals7 
  
  cereals4 = na.omit(cereals[,-c(1)])
  cereals4 = data.frame(cereals4, km.cereals7$cluster)
  cereals4$km.cereals7.cluster = as.factor(cereals4$km.cereals7.cluster)
  reg = lm(Rating ~ cereals4$km.cereals7.cluster, data = cereals4)
  summary(reg)
  
#PART 2
library(readr)
adult<-read.csv(file="~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/adult.txt", stringsAsFactors=TRUE)
  
  #Remove Education char column as we have the lvl
  adult2 = adult[,-c(4,15)]

  #Merge the many Levels
  levels(adult2$marital.status)
  levels(adult2$marital.status)[2:4]<-"Married"
  
  levels(adult2$workclass)
  levels(adult2$workclass)[c(2,3,8)]<-"Gov"
  levels(adult2$workclass)[c(5,6)]<-"Self"
  
  levels(adult2$occupation)
  table(adult2$occupation)
  levels(adult2$occupation)[c(3,4,6,10,12,15)]<-"HandWork"
  levels(adult2$occupation)[c(1,7)]<-"OtherOccupation"
  levels(adult2$occupation)[c(2,4:9)]<-"Profession"
  
  levels(adult2$relationship)
  table(adult2$relationship)
  levels(adult2$relationship)[c(3,4,5)]<-"OtherRelashionship"
  
  levels(adult2$race)
  levels(adult2$race)[c(1,2,4)]<-"OtherRace"
  
  levels(adult2$native.country)
  levels(adult2$native.country)[c(3,6,7,9,14,15,24,27,34,39,40)]<-"NorthAmerica"
  levels(adult2$native.country)[c(5,6,12,21,23)]<-"SouthAmerica"
  levels(adult2$native.country)[c(6:10,12,15,16,21,22,23,28)]<-"Europe"
  levels(adult2$native.country)[c(2,4,7,8,9,10,11,13,15,16,17)]<-"Asia"
  levels(adult2$native.country)[c(1,6,7)]<-"OtherCountry"
  
  #Split out factor columns
    
    #Marital Status
  adult2$Divorced<-ifelse(adult2$marital.status=="Divorced",1,0)
  adult2$Married<-ifelse(adult2$marital.status=="Married",1,0)
  adult2$NeverMarried<-ifelse(adult2$marital.status=="Never-married",1,0)
  adult2$Separated<-ifelse(adult2$marital.status=="Separated",1,0)
  adult2$Widowed<-ifelse(adult2$marital.status=="Widowed",1,0)

    #Workclass
  adult2$OtherWorkclass<-ifelse(adult2$workclass=="?",1,0)
  adult2$Gov<-ifelse(adult2$workclass=="Gov",1,0)
  adult2$NeverWored<-ifelse(adult2$workclass=="Never-worked",1,0)
  adult2$Private<-ifelse(adult2$workclass=="Private",1,0)
  adult2$WithoutPay<-ifelse(adult2$workclass=="Without-pay",1,0)
  
    #Occupation  
  adult2$Profession<-ifelse(adult2$occupation=="Profession",1,0)
  adult2$HandWork<-ifelse(adult2$occupation=="HandWork",1,0)
  adult2$OtherOccupation<-ifelse(adult2$occupation=="OtherOccupation",1,0)
  
    #Relashionship
  adult2$Husband<-ifelse(adult2$relationship=="Husband",1,0)
  adult2$Notinfamily<-ifelse(adult2$relationship=="Not-in-family",1,0)
  adult2$Wife<-ifelse(adult2$relationship=="Wife",1,0)
  adult2$OtherRelashionship<-ifelse(adult2$relationship=="OtherRelashionship",1,0)
  
    #Race
  adult2$Black<-ifelse(adult2$race=="Black",1,0)
  adult2$White<-ifelse(adult2$race=="White",1,0)
  adult2$OtherRace<-ifelse(adult2$race=="OtherRace",1,0)
  
    #Sex
  adult2$Male<-ifelse(adult2$race=="Male",1,0)
  adult2$Female<-ifelse(adult2$race=="Female",1,0)
  
    #Country
  adult2$Asia<-ifelse(adult2$native.country=="Asia",1,0)
  adult2$NorthAmerica<-ifelse(adult2$native.country=="NorthAmerica",1,0)
  adult2$SouthAmerica<-ifelse(adult2$native.country=="SouthAmerica",1,0)
  adult2$Europe<-ifelse(adult2$native.country=="Europe",1,0)
  adult2$OtherCountry<-ifelse(adult2$native.country=="OtherCountry",1,0)

  #Remove factored columns
  adult3 = adult2[,-c(2,5:9,13)]
  
  #Normalize the Data
  library(caret)
  norm.values <- preProcess(adult3[,c(1:6)],method="range")
  adult3[,c(1:6)] <- predict(norm.values, adult3[,c(1:6)])
  
  # run the Kohonen clustering algorithm with 3x3 topology
  install.packages("kohonen")
  library(kohonen)
  
  # Kohonen Clustering
  som.adult <- som(as.matrix(adult3),
                   grid=somgrid(3,3),
                   rlen=170,alpha=c(0.3,0.00),radius=2)
  
  #Cluster Characteristics 
  plot(som.adult,type="codes",codeRendering = "segments")
  
  # Cluster Count
  plot(som.adult,type=c("counts"),palette.name=rainbow,main="Cluster Counts")
  
#Question 6
  
  # Income Percentage by Cluster
  c.table<-table(adult$income,som.adult$unit.classif)
  round(prop.table(c.table,2)*100,2)

#PART 3
library(readr)
Loans_Training <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/Loans_Training_2.csv")
Loans_Test <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Datasets/Loans_Test_2.csv")

#Question 7 

  #Clean Data Set
  Loans_Training2 = na.omit(Loans_Training)
  Loans_Training2 = Loans_Training2[,-c(5)]

  #Normalize the Data
  library(caret)
  Loans_Training2.norm <- preProcess(Loans_Training2[,c(2:4)],method="range")
  Loans_Training2[,c(2:4)] <- predict(Loans_Training2.norm, Loans_Training2[,c(2:4)])
  Loans_Training3 = Loans_Training2[,-c(1)]
  
  #Run the kmeans algorithm with k=3
  km.loanTr3 <- kmeans(Loans_Training3, 3)
  km.loanTr3 
  
  #Run the kmeans algorithm with k=4
  km.loanTr4 <- kmeans(Loans_Training3, 4)
  km.loanTr4 
  
#Question 8
  
  # Merge cluster membership to the data
  clusters3 = as.data.frame(km.loanTr3$cluster)
  clusters4 = as.data.frame(km.loanTr4$cluster)
  Loans_Training4 = data.frame(Loans_Training3, clusters3)
  Loans_Training5 = data.frame(Loans_Training3, clusters4)
  Loans_Training6 = data.frame(Loans_Training3, clusters3, clusters4)
  
  #Get silhouette information
  dist1<-dist(Loans_Training4[,1:4], method="euclidean")
  library(cluster)
  sil1<-silhouette(km.loanTr3$cluster,dist1)
  sil1viz = fviz_silhouette(sil1)
  
  dist2<-dist(Loans_Training5[,1:4],method="euclidean")
  sil2<-silhouette(km.loanTr4$cluster,dist2)
  sil2vix = fviz_silhouette(sil2)
  
  #Plot information
  library(gridExtra)
  grid.arrange(sil1viz, sil2vix, ncol = 2)
  
  #Checking silhouette value
  fviz_nbclust(Loans_Training6, kmeans, k.max = 5, method = "silhouette")

  #Question 9
      pseudoF = function(X, k, ns = 25){
        nk = length(k)
        n = nrow(X)
        T = sum(scale(X,scale=F)^2)
        W = rep(T, nk)
        for (i in 1:nk){
          cli = kmeans(X, k[i], nstart=ns)
          W[i] = sum(cli$withinss)
        }
        pF = ((T-W)/(k-1))/(W/(n-k))
        return(list(k=k, W=W, pF=pF))
      }

      pseudoF(Loans_Training3, 1:5)

        
#Question 10

      #For k = 3
        test<-Loans_Test[,-1]
        set.seed(1)
        kmtest<-kmeans(test,3)
        
        clust.sum<-matrix(0.0,ncol=3,nrow=4)
        colnames(clust.sum)<-c("Cluster 1","Cluster 2","Cluster 3")
        rownames(clust.sum)<-c("Test Data Mean","Train Data Mean","Test Data Std Dev","Train Data Atd Dev")
        clust.sum[1,]<-tapply(test$`Debt-to-Income Ratio`,kmtest$cluster,mean)
        clust.sum[2,]<-tapply(Loans_Training$`Debt-to-Income Ratio`, km.loanTr3$cluster,mean)
        clust.sum[3,]<-tapply(test$`Debt-to-Income Ratio`,kmtest$cluster,sd)
        clust.sum[4,]<-tapply(Loans_Training$`Debt-to-Income Ratio`, km.loanTr3$cluster,sd)
        
        clust.sum
      