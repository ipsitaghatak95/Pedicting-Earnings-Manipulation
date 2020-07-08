#Predicting earnings manipulators using case study "Predicting earning manipulation by Indian firms using machine
#learning" from Harvard Business Publishing Education

library(tidyverse)
library(readxl)

##lets create an m score for every company by beneish formula to verify whether the data
##set we have can be worked upon with beneish mode

abc <- read_excel("IMB579-XLS-ENG.xlsx",sheet=2) #import data 
summary(abc) 
View(abc) 

#Using beneish model
abc$mscore <- -4.84+0.92*abc$DSRI+0.528*abc$GMI+0.404*abc$AQI+0.892*abc$SGI+0.115*abc$DEPI-0.172*abc$SGAI+4.679*abc$ACCR-0.327*abc$LEVI

misclasssified <- abc[abc$mscore<-1.78,]  

nrow(misclasssified) 
# 1 misclassified as non-manipulator.So we can use beneish model

a <- read_excel("IMB579-XLS-ENG.xlsx",sheet=5) 
table(a$`C-MANIPULATOR`) 

#Here we have a class imbalance problem 
table(a$Manipulator) ## so 39 *2 

#Data change

names(a)[1] <- "ID" 
a$`C-MANIPULATOR`<- NULL 
a$ID <- NULL 
a$Manipulator<- factor(a$Manipulator) 
a 
View(a)

#Applying an undersampling technique to balance data

library(ROSE) #for undersampling  (ovun.sample)
under <- ovun.sample(Manipulator~.,data=a,method="under",N=78)$data 
table(under$Manipulator) ### we have balanced data set 
View(under) 
str(under)

#Stepwise logistic regression that can be used by MCA Technologies Private Limited
#for predicting probability of earnings manipulation

set.seed(1234) 
ind <- sample(2, nrow(under), replace = T, prob = c(0.65, 0.35)) 
train <- under[ind==1,] 
test <- under[ind==2,] 
full<- glm(Manipulator ~ ., data = train, family = 'binomial') 
null <- glm(Manipulator ~ 1, data = train, family = 'binomial') 
step(full,scope =list(lower=null,upper=full),direction ="both") 
logit <- glm(Manipulator ~  DSRI + GMI + AQI + SGI + DEPI + ACCR ,data = train, family = "binomial") 
summary(logit)

#EVALUATION 

library(lattice)
library(caret) 
p <- predict(logit, test, type = 'response') 
p 
pred <- ifelse(p>0.5, "Yes", "No") 
tab<- table(Predicted = pred,Actual = test$Manipulator) 
tab 
cm <- confusionMatrix(as.factor(pred),test$Manipulator, positive = "Yes") 
cm  #output for undersampled logistic regression on sample data


#Evaluating the model further with the help of an ROC curve
#ROC curve for logistic regression 

library(pROC) 
a <- roc(test$Manipulator,p,plot=TRUE,legacy.axes=TRUE) 
df <- data.frame(tpp=a$sensitivities*100,fpp=(1-a$specificities)*100,thresholds=a$thresholds) 
head(df) 
df 


#Now we will choose the best threshold for sensititivity around 80 percent and choose the threshold   :  0.715118697 

library(caret) 
p <- predict(logit, test, type = 'response') 
p 
pred <- ifelse(p> 0.715118697, "Yes", "No") 
tab<- table(Predicted = pred,Actual = test$Manipulator) 
tab 
cm <- confusionMatrix(as.factor(pred),test$Manipulator, positive = "Yes") 
cm

#Classification and regression tree (CART) model

library(tidyverse) 
library(readxl) 
a <- read_excel("IMB579-XLS-ENG.xlsx",sheet=4) 
names(a)[1] <- "ID" 
a$`C-MANIPULATOR`<- NULL 
a$ID <- NULL 
a$Manipulater<- factor(a$Manipulater) 
table(a$Manipulater) 
set.seed(1234) 
ind <- sample(2, nrow(a), replace = T, prob = c(0.65, 0.35)) 
train <- a[ind==1,] 
test <- a[ind==2,] 

#Smote on train data

library(UBL) 
smote <- SmoteClassif(Manipulater~.,as.data.frame(train), "balance") #SmoteClassif balances the number of "Yes"and "No" in train$Manipulater
prop.table(table(smote$Manipulater)) 
table(smote$Manipulater) 
library(rpart) 

library(rpart.plot) 
tree <- rpart(Manipulater~.,data = smote,control=rpart.control(mincriterion=0.95,maxdepth = 6)) 
rpart.plot(tree,extra = 1) 

predictcart <- predict(tree,test,type = "class") 
confusionMatrix(predictcart,test$Manipulater,positive = "Yes") 
library(pROC) 
predictroc <- predict(tree,test,type = "prob") 
roc(test$Manipulater,predictroc[,2],plot=TRUE,legacy.axes=TRUE) 

#Pruning with cp

opt <- which.min(tree$cptable[,"xerror"])  
cp <- tree$cptable[opt, "CP"] 
tree_prune <- prune(tree, cp = cp)
rpart.plot(tree_prune,extra = 1) 
predictcart <- predict(tree_prune,test,type = "class") 
confusionMatrix(predictcart,test$Manipulater,positive = "Yes") 


#Logistic regression model using the complete data set 
#(1200 non-manipulators and 39 manipulators)

full<- glm(Manipulater ~ ., data = smote, family = 'binomial') 
null <- glm(Manipulater ~ 1, data = smote, family = 'binomial') 
fulllog <- step(null,scope =list(lower=null,upper=full),direction ="both") 
summary(fulllog) 

#EVALUATION 

p <- predict(fulllog, test, type = 'response') 
p 
pred <- ifelse(p>0.5, "Yes", "No") 
tab<- table(Predicted = pred,Actual = test$Manipulater) 
tab 
cm <- confusionMatrix(as.factor(pred),test$Manipulater, positive = "Yes") 
cm 

#Random Forest

library(tidyverse) 
library(readxl) 
a <- read_excel("IMB579-XLS-ENG.xlsx",sheet=4) 
names(a)[1] <- "ID" 
a$`C-MANIPULATOR`<- NULL 
a$ID <- NULL 
a$Manipulater<- factor(a$Manipulater) 
table(a$Manipulater) 
set.seed(1234) 
ind <- sample(2, nrow(a), replace = T, prob = c(0.65, 0.35)) 
train <- a[ind==1,] test <- a[ind==2,] 

#Smote on train data

library(UBL) 
smote <- SmoteClassif(Manipulater~.,as.data.frame(train), "balance") 
prop.table(table(smote$Manipulater)) 
table(smote$Manipulater) 
library(randomForest) 
set.seed(1234) 
smoterf <- randomForest(Manipulater~.,data = smote) 
print(smoterf)

library(caret) 
p1 <- predict(smoterf,test) 
head(p1) 
head(test$Manipulater) 
confusionMatrix(p1,test$Manipulater,positive = "Yes") 
plot(smoterf) 


#we will choose 300 trees  

t <- tuneRF(smote[,-9],smote[,9],stepFactor = 0.5,plot=TRUE,ntreeTry = 300,trace = TRUE,improve = 0.05) 

## mtry value = 2 so rebuild rf 
smoterf2 <- randomForest(Manipulater~.,data = smote,ntree = 500,mtry=sqrt(ncol(a)-1),importance = TRUE,proximity=TRUE) 

#Adaboost

library("adabag")
adaboost <- boosting(Manipulater ~ .,data = smote,mfinal = 100,control = rpart.control(maxdepth = 1))
summary(adaboost)
pred_boost = predict.boosting(adaboost, newdata = as.data.frame(test))
summary(pred_boost)
print("Confusion Matrix using Adaboost")
print(pred_boost$confusion)

#The final recommendation would be to use logistic regression with smote sampled data set 
#the major predictors for these are : AQI SGI ACCR