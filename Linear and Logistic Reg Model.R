##Linear Regression Model
##use usedcars.xlsx and homeappliances.xlsx for working on this code
library(readxl)
UsedCars <- read_excel("C:/Users/Prajakta/Downloads/Saket/Books/OPIM 5603/Assignment/UsedCars.xlsx")
View(UsedCars)

colnames(UsedCars)
colnames(UsedCars)[] = c("Observation","Price","Age","Mileage","MPG","KBB","CRScore")
colnames(UsedCars)

head(UsedCars)
UC <- UsedCars[-1]
head(UC)

UCReg <- lm(UC$Price~.,data = UC)
summary(UCReg)


#normality, homoscedasticity, and collinearity (compute VIFs for each independent variable)

plot(UCReg)

w = residuals(UCReg)
w
mean(w)
median(w)
library(moments)
skewness(w)
kurtosis(w)

##Homoscedasticity 

head(UC[2:6])
length(UC$Price)
length(w)


H = data.frame(UC[2:6],residuals=w)
View(H)
pairs(H)
corr(H)
##no clear cut pattern between errors and other vaariables individually

##Collinearity of the model
View(cor(UC[2:6]))
head(H)
H1 <- H[-6]
##Now calcualting VIFs
##First create a model among independent variables with one of them as a dependent variable
##Lets call KBB as dependent here
reg1 <- lm(H1$KBB~.,data = H1)
rs1 <- summary(reg1)
rs1$r.square
##VIFs
1/(1-rs1$r.square)

##Check with dot and without dot for Rsqaured changes.


reg2 <- lm(H1$Age~.,data = H1)
rs2 <- summary(reg2)
rs2$r.squared
1/(1-rs2$r.square)

reg3 <- lm(H1$Mileage~.,data = H1)
rs3 <- summary(reg3)
rs3$r.squared
1/(1-rs3$r.square)

reg4 <- lm(H1$MPG~.,data = H1)
rs4 <- summary(reg4)
rs4$r.squared
1/(1-rs4$r.square)

reg5 <- lm(H1$CRScore~.,data = H1)
rs5 <- summary(reg5)
rs5$r.squared
1/(1-rs5$r.square)



### Logistic regression model
####Problem Statement 2
HomeAppliances <- read_excel("C:/Users/Prajakta/Downloads/Saket/Books/OPIM 5603/Assignment/HomeAppliances.xlsx",
sheet = "Training Data")
View(HomeAppliances)

colnames(HomeAppliances)
colnames(HomeAppliances)[] = c("Customer","BoughtProduct" ,"Gender","AnnualIncome","Age","HouseAge" )
colnames(HomeAppliances)
head(HomeAppliances)


HA <- HomeAppliances[-1]
head(HA)

##Lets make purchased as 1 and not purchased as 0 i.e. Yes = 1 and No = 0
HA1 <- data.frame(HA[-1],BroughtProdFlag = ifelse(HA$BoughtProduct=="Yes",1,0))
HA1
class(HA1$BroughtProdFlag)

regHA1 <- glm(HA1$BroughtProdFlag~.,data = HA1,family = binomial(link = "logit"))
summary(regHA1)
##with important variables
regHA2 <- glm(HA1$BroughtProdFlag~ HA1$Gender+HA1$AnnualIncome+HA1$HouseAge,data = HA1,
              family = binomial(link = "logit"))
summary(regHA2)


##R square
##McFadden R-Square

1-48.076/133.750
##0.6405533

##So this is our final model using Logit function
##Now use this model to predict the traning data's brought product flag.

##get coefficients
V = coef(regHA2)
V
V[1]
head(HA1)
##Convert Gender = M to 1 and F to 0
HA2 <- data.frame(HA1[-1],Gender1 = ifelse(HA1$Gender=="M",1,0))
head(HA2)
L = V[1] + HA2$Gender1*V[2] + HA2$AnnualIncome*V[3] + HA2$HouseAge*V[4]
L

##make exponent of L
PR <- exp(L)/(1+exp(L))
PR

HA3_final <- data.frame(HA2,BroughtProdFlagPredicted = PR)
head(HA3_final)
HA3_final$newpredict <- ifelse(HA3_final$BroughtProdFlagPredicted> 0.5,"Yes","No")
##length(which(HA3_final$BroughtProdFlag==1))
##length(which(HA3_final$newpredict=="Yes"))

View(HA3_final)
HA3_final$BroughtProd1 <- ifelse(HA3_final$BroughtProdFlag==1,"Yes","No")
length(which(HA3_final$newpredict!=HA3_final$BroughtProd1))
##11/100 = 11% mismatch
##calculate the misclassification,  find false positives and true negatives


HA3_final$newpredict <- ifelse(HA3_final$BroughtProdFlagPredicted> 0.4,"Yes","No")
length(which(HA3_final$newpredict!=HA3_final$BroughtProd1))
##13/100 = 13% mismatch

HA3_final$newpredict <- ifelse(HA3_final$BroughtProdFlagPredicted> 0.45,"Yes","No")
length(which(HA3_final$newpredict!=HA3_final$BroughtProd1))
##12/100 = 12% mismatch


HA3_final$newpredict <- ifelse(HA3_final$BroughtProdFlagPredicted> 0.55,"Yes","No")
length(which(HA3_final$newpredict!=HA3_final$BroughtProd1))
##11/100 = 11% mismatch

HA3_final$newpredict <- ifelse(HA3_final$BroughtProdFlagPredicted> 0.60,"Yes","No")
length(which(HA3_final$newpredict!=HA3_final$BroughtProd1))
##9/100 = 9% mismatch


View(HA3_final)



##Now use this accuracy of 60% to determine or forcast the buying probability for test dataset

HomeAppliancesPred <- read_excel("C:/Users/Prajakta/Downloads/Saket/Books/OPIM 5603/Assignment/HomeAppliances.xlsx",
sheet = "Predictions")
View(HomeAppliancesPred)
##selecting only the required columns for the prediction
head(HomeAppliancesPred)
HAP <- HomeAppliancesPred[c(-1,-4)]
HAP
HAP$Gender <- ifelse(HAP$Gender=="M",1,0)
HAP
L1 = V[1] + HAP$Gender*V[2] + HAP$`Annual income`*V[3] + HAP$`House Age`*V[4]
L1

PR1 <- exp(L1)/(1+exp(L1))
PR1

HAP_final <- data.frame(HAP,BroughtProdFlagPredicted = PR1)
HAP_final
HAP_final$newpredict <- ifelse(HAP_final$BroughtProdFlagPredicted> 0.6,"Yes","No")
View(HAP_final)
###only 4 customers who are female would want to buy!!
