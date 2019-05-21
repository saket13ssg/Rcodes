library(readr)
train_and_test2 <- read_csv("C:/Users/Prajakta/Desktop/R_practice/train_and_test2.csv")
View(train_and_test2)


head(train_and_test2,5)
##rename the column name for last column
colnames(train_and_test2)[9] <- "survived"
colnames(train_and_test2)


head(train_and_test2,5)
summary(train_and_test2$survived)

length(train_and_test2$Passengerid)
##1309 values
##make it 60-40 but stratified sampling
table(train_and_test2$survived)
##0 = 967, 1 = 342

##data of 0s
data_0 <- train_and_test2[train_and_test2$survived==0,]
head(data_0)
length(data_0$survived)

##data of 1s
data_1 <- train_and_test2[train_and_test2$survived==1,]
head(data_1)
length(data_1$survived)


##now create training and test datasets
##training needs
round(0.6*length(data_0$survived))
round(0.6*length(data_1$survived))

data_train <- rbind(data_0[0:580,], data_1[0:205,])
head(data_train)
length(data_train$survived)

data_test <- rbind(data_0[581:966,], data_1[206:341,])
head(data_test)
length(data_test$survived)


table(data_train$survived)
table(data_test$survived)

prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

##so now we have a stratified sample

head(data_train)
data_train1 <- data_train[-1]
##removing the row number column
head(data_train1)

##now building a logistic model
##before that we need to see some correlations and patterns

##treat the missing data
sapply(data_train1,function(x) sum(is.na(x)))
data_train1$Embarked[is.na(data_train1$Embarked)] <- mean(data_train1$Embarked,na.rm = T) 
sapply(data_train1,function(x) sum(is.na(x)))


##actually running the partition tree model on data_train1
attach(data_train1)
model <- glm(survived~.,family = binomial(link = "logit"),data = data_train1)
summary(model)


##running the logistic regression model
model <- glm(survived~Age + Sex + sibsp + Pclass,family = binomial(link = "logit"),data = data_train1)
summary(model)


##now we run ANOVA
anova(model, test="Chisq")

##McFadden R squuare
library(pscl)
pR2(model)

head(data_train1)
head(data_test)
data_test1 <- data_test[-1]
head(data_test1)
data_test2 <- data_test1[-8]
head(data_test2)

##prediction using predict function
fitted.results <- predict(model,newdata=data_test2, type = "response")
head(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)
data_test1$new_val <- fitted.results
head(data_test1)

table(data_test1$survived, data_test1$new_val)
prop.table(table(data_test1$survived, data_test1$new_val))
##this gives misclassification matrix to see the misclassification %
##then we compare decision tree and logistic and choose final model.

