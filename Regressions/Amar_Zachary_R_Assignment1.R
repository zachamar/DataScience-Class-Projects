library(readxl)
library(dplyr)
library(stargazer)
library(readr)
library(lmtest)
library(rms)
library(leaps)
library(olsrr)


#Part 1
baseball <- read_excel("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Individual Assignment 1/Dataset/baseball.xlsx")
attach(baseball)
baseball_2 = dplyr::mutate(baseball, ln_homeruns = (log(baseball$homeruns+1)))

baseball_100 = dplyr::filter(baseball_2, at_bats>100)
lr = lm(baseball_100$ln_homeruns ~ baseball_100$bat_ave)
stargazer(lr, type = "html")
summary(lr)

plot(baseball_100$ln_homeruns, baseball_100$bat_ave)

#Part 2
nutrition <- read.csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Individual Assignment 1/Dataset/nutrition.csv")
attach(nutrition)

# Forward Selection
# plot correlations among variables
pairs(nutrition$CALORIES ~ nutrition$WT_GRAMS + nutrition$PC_WATER + nutrition$PROTEIN + nutrition$FAT + nutrition$SAT_FAT + nutrition$MONUNSAT + nutrition$POLUNSAT + nutrition$CHOLEST
      + nutrition$CARBO + nutrition$CALCIUM + nutrition$PHOSPHOR + nutrition$IRON + nutrition$POTASS+  nutrition$SODIUM + nutrition$VIT_A_IU
      + nutrition$VIT_A_RE + nutrition$THIAMIN + nutrition$RIBOFLAV + nutrition$NIACIN + nutrition$ASCORBIC + nutrition$CAL_GRAM + nutrition$IRN_GRAM 
      + nutrition$PRO_GRAM + nutrition$FAT_GRAM, labels=names(nutrition), cex.labels=1)

nutrition2 = nutrition[,-c(1)]

cor(nutrition2)   # Carbo has highest correlation of 0.88085862 with CALORIES

# TO PREVENT OVERFITTING: remove variables that have a correlation inferior to 0.33 as seen in class with CALORIES: PC_WATER, VIT_A_IU, VIT_A_RE, ASCORBIC, IRN_GRAM, PRO_GRAM, FAT_GRAM, CAL_GRAM

nutrition3=nutrition[-c(3,17,18,22,23,24,25,26)]

msfwd.empty <- lm(CALORIES ~ 1,data=nutrition3)  # start by declaring an empty model
stepF1 <- add1(msfwd.empty,scope=nutrition3[], test="F", trace=TRUE)
stepF1    # examine the output: proceed to step 2 and add CARBO, the variable with the highest correlation with CALORIES 

msfwd.empty2 <- lm(CALORIES ~ CARBO,data=nutrition3)
stepF2 <- add1(msfwd.empty2,scope=nutrition3[], test="F", trace=TRUE) 
stepF2    # examine the output: proceed to step3 and add FAT,the var with the highest significant F-statistic 

msfwd.empty3 <- lm(CALORIES ~ CARBO + FAT,data=nutrition3)
stepF3 <- add1(msfwd.empty3,scope=nutrition3[], test="F", trace=TRUE) 
stepF3   #PROTEIN

msfwd.empty4 <- lm(CALORIES ~ CARBO + FAT + PROTEIN,data=nutrition3)
stepF4 <- add1(msfwd.empty4,scope=nutrition3[], test="F", trace=TRUE) 
stepF4 #POTASS

msfwd.empty5 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS ,data=nutrition3)
stepF5 <- add1(msfwd.empty5,scope=nutrition3[], test="F", trace=TRUE) 
stepF5 #WT GRAMS

msfwd.empty6 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS ,data=nutrition3)
stepF6 <- add1(msfwd.empty6,scope=nutrition3[], test="F", trace=TRUE) 
stepF6 #THIAMIN

msfwd.empty7 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN ,data=nutrition3)
stepF7 <- add1(msfwd.empty7,scope=nutrition3[], test="F", trace=TRUE) 
stepF7 #IRON

msfwd.empty8 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN + IRON ,data=nutrition3)
stepF8 <- add1(msfwd.empty8,scope=nutrition3[], test="F", trace=TRUE) 
stepF8 #Phosphor

msfwd.empty9 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN + IRON + PHOSPHOR ,data=nutrition3)
stepF9 <- add1(msfwd.empty9,scope=nutrition3[], test="F", trace=TRUE) 
stepF9 #riboflav

msfwd.empty10 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN + IRON + PHOSPHOR + RIBOFLAV ,data=nutrition3)
stepF10 <- add1(msfwd.empty10,scope=nutrition3[], test="F", trace=TRUE) 
stepF10 #calcium

msfwd.empty11 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN + IRON + PHOSPHOR + RIBOFLAV + CALCIUM ,data=nutrition3)
stepF11 <- add1(msfwd.empty11,scope=nutrition3[], test="F", trace=TRUE) 
stepF11 #Polunsat

msfwd.empty12 <- lm(CALORIES ~ CARBO + FAT + PROTEIN + POTASS + WT_GRAMS + THIAMIN + IRON + PHOSPHOR + RIBOFLAV + CALCIUM + POLUNSAT,data=nutrition3)
stepF12 <- add1(msfwd.empty12,scope=nutrition3[], test="F", trace=TRUE) 
stepF12 
# we stop here because there are not significant variables anymore

summary(msfwd.empty12)
## preferred model by forward selection method: 

#Backward selection

nutrition2[ , c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)] <- nutrition2[ , c(3,1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)]
colnames(nutrition2) <- c("CALORIES", "WT_GRAMS", "PC_WATER", "PROTEIN", "FAT", "SAT_FAT", "MONUNSAT", "POLUNSAT", "CHOLEST", "CARBO", "CALCIUM", "PHOSPHOR", "IRON", "POTASS", "SODIUM", "VIT_A_IU", "VIT_A_RE", "THIAMIN", "RIBOFLAV","NIACIN","ASCORBIC","CAL_GRAM", "IRN_GRAM", "PRO_GRAM", "FAT_GRAM")
msbkwd.full <- lm(CALORIES ~ WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST
                  + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + VIT_A_IU
                  + VIT_A_RE + THIAMIN + RIBOFLAV + NIACIN + ASCORBIC + CAL_GRAM + IRN_GRAM 
                  + PRO_GRAM + FAT_GRAM, data=nutrition2)

# start by declaring a full model
stepB1 <- drop1(msbkwd.full,scope=nutrition2[], test="F", trace=TRUE) 
stepB1 # examine the output and proceed to step2 without RIBOFLAV, the variable with smallest non-significant F-value

msbkwd.full2 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + VIT_A_IU+ VIT_A_RE + THIAMIN + NIACIN + ASCORBIC + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB2 <- drop1(msbkwd.full2,scope=nutrition2[,-19], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB2  # examine the output and proceed to step3 without VIT A RE, the variable with smallest non-significant F-value

msbkwd.full3 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + VIT_A_IU+ THIAMIN + NIACIN + ASCORBIC + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB3 <- drop1(msbkwd.full3,scope=nutrition2[,-c(17,19)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB3  #VIT A IU 

msbkwd.full4 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + NIACIN + ASCORBIC + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB4 <- drop1(msbkwd.full4,scope=nutrition2[,-c(16,17,19)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB4  #NIACIN 

msbkwd.full5 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + ASCORBIC + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB5 <- drop1(msbkwd.full5,scope=nutrition2[,-c(16,17,19,20)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB5  #CHOLEST 

msbkwd.full6 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + ASCORBIC + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB6 <- drop1(msbkwd.full6,scope=nutrition2[,-c(9,16,17,19,20)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB6 #ASCORBIC

msbkwd.full7 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB7 <- drop1(msbkwd.full7,scope=nutrition2[,-c(9,16,17,19,20,21)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB7 #POLUNSAT

msbkwd.full8 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB8 <- drop1(msbkwd.full8,scope=nutrition2[,-c(8,9,16,17,19,20,21)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB8 # MONUNSAT

msbkwd.full9 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB9 <- drop1(msbkwd.full9,scope=nutrition2[,-c(7,8,9,16,17,19,20,21)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB9 # SAT FAT 

msbkwd.full10 <- lm(CALORIES ~  WT_GRAMS + PC_WATER + PROTEIN + FAT + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + CAL_GRAM + IRN_GRAM + PRO_GRAM + FAT_GRAM, data=nutrition2)  
stepB10 <- drop1(msbkwd.full10,scope=nutrition2[,-c(6,7,8,9,16,17,19,20,21)], test="F", trace=TRUE) # note that the scope of predictors must include only those specified in current model
stepB10 # nothing insignificant

summary(msbkwd.full10)
# preferred model by backward selection method: y-hat = 

#STEPWISE

mssw <- ols(CALORIES ~ WT_GRAMS+ PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST
            + CARBO + PHOSPHOR + IRON + POTASS+  SODIUM + THIAMIN + RIBOFLAV +CALCIUM + NIACIN, data=nutrition) # drop PC_WATER, VIT_A_IU, VIT_A_RE, ASCORBIC, IRN_GRAM, PRO_GRAM, FAT_GRAM, CAL_GRAM because correlation negligible with CALORIES
stepS <- fastbw(mssw, rule='p') 
stepS  # examine the output for factors in the final model #WT_GRAMS PROTEIN  FAT CARBO PHOSPHOR IRON POTASS SODIUM  THIAMIN RIBOFLAV CALCIUM 

best.model3 <- lm(CALORIES ~ WT_GRAMS+PROTEIN+FAT+CARBO+PHOSPHOR+IRON+POTASS+SODIUM+THIAMIN+RIBOFLAV+CALCIUM,data=nutrition)  # best model from fwd,stepwise, and best subset selections
summary(best.model3)

#BEST SUBSETS

stepBS <- regsubsets(x=CALORIES ~ WT_GRAMS + PC_WATER + PROTEIN + FAT + SAT_FAT + MONUNSAT + POLUNSAT + CHOLEST
                     + CARBO + CALCIUM + PHOSPHOR + IRON + POTASS+  SODIUM + VIT_A_IU
                     + VIT_A_RE + THIAMIN + RIBOFLAV + NIACIN + ASCORBIC + CAL_GRAM + IRN_GRAM 
                     + PRO_GRAM + FAT_GRAM, data=nutrition,nbest = 2)
sum.stepBS <- summary(stepBS)
names(sum.stepBS) # examine the names of values in the output
sum.stepBS$which  # examine the matrix containing which variables are in each model
sum.stepBS$rsq    # examine the r-squared for each model
sum.stepBS$cp     # examine Mallow's cp for each model: cp is the statistic to compare models with different parameters. 
# smaller cp means the model is more precise 
cbind(data.frame(sum.stepBS$which),sum.stepBS$rsq,sum.stepBS$cp) # examine all the outputs together: 
# observe that the smallest cp is  141.0818 and it corresponds to model 15 with FAT GRAM, CAL GRAM, WT GRAM, PC WATER, PROTEIN, FAT, CARBO, POTASS

#preferred model from best subsets selection because they all have small p value but that model has the least amount of variables
# d.Interpret the results of the best model you found
best.model4 <- lm(CALORIES ~ FAT_GRAM + CAL_GRAM + WT_GRAMS + PC_WATER + PROTEIN + FAT + CARBO + POTASS,data=nutrition)  # best model from fwd,stepwise, and best subset selections
summary(best.model4)
stargazer(best.model4, type = "html")

#Colinearity
mreg=lm(CALORIES~., data=nutrition2)
ols_coll_diag(mreg)
ols_coll_diag(best.model4)


#Part 3

BREAST_CANCER <- read_csv("~/Dropbox/Zach/McGill/Year 2 Semester 2/INSY 446/Individual Assignment 1/Dataset/BREAST_CANCER.csv")

BREAST_CANCER$bare_nuclei = as.numeric(as.character(BREAST_CANCER$bare_nuclei))
lrg=glm(class~., data=BREAST_CANCER, family=binomial)
stargazer(lrg, type = "html")
summary(lrg)
lrtest(lrg1)

