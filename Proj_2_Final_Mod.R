library(janitor)
library(foreign)
library(caret)
library(pROC)
library(zoo)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(stargazer)
library(readr)
library(boot)
library(caTools)
library(e1071)
library(glmnet)
library(kernlab)
library(ROCR)
library(rpart)
library(corrplot)
library(rattle)
library(forcats)
library(ggpubr)
library(tidyverse)
library(ggthemes)
library(arules)
library(arulesViz)


##############   load the data   #################################################
#7043 customers total in both train and test data

data = read.csv(file="Train_Churn_binary.csv", header=TRUE, sep=",")
#View(data) #5282 rows

data_test = read.csv(file="Test_Churn.csv", header=TRUE, sep=",")
#View(data_test) #1761 rows
dim(data_test)



#percentage of churn in train data
perc_churn = tabyl(data$Churn)
perc_churn



##############   preprocess the train data   ##################################
#change senior citizen to a factor
data$SeniorCitizen = factor(data$SeniorCitizen)
data$SeniorCitizen = factor(data$SeniorCitizen,
                            levels = c(0,1),
                            labels = c("No", "Yes"))

#change churn to a factor
data$Churn = factor(data$Churn)
data$Churn = factor(data$Churn,
                    levels = c(0,1),
                    labels = c("No", "Yes"))



#change nas to 0
data = data %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges), 0, TotalCharges))
anyNA(data)


#combine levels
levels(data$StreamingTV) = gsub("No internet service", "No", levels(data$StreamingTV))
#levels(data$StreamingTV)

levels(data$MultipleLines) = gsub("No phone service", "No", levels(data$MultipleLines))
#levels(data$MultipleLines)

levels(data$OnlineSecurity) = gsub("No internet service", "No", levels(data$OnlineSecurity))
#levels(data$OnlineSecurity)

levels(data$OnlineBackup) = gsub("No internet service", "No", levels(data$OnlineBackup))
#levels(data$OnlineBackup)

levels(data$DeviceProtection) = gsub("No internet service", "No", levels(data$DeviceProtection))
#levels(data$DeviceProtection)

levels(data$TechSupport) = gsub("No internet service", "No", levels(data$TechSupport))
#levels(data$TechSupport)

levels(data$StreamingMovies) = gsub("No internet service", "No", levels(data$StreamingMovies))
#levels(data$StreamingMovies)

#drop customer number column
data = data[ -c(1) ] #drop customer number column



#standardize totalcharges
data$TotalCharges = scale(data$TotalCharges)
str(data$TotalCharges)


#standardize monthlycharges
data$MonthlyCharges = scale(data$MonthlyCharges)
str(data$TotalCharges)


###bin tenure/make discrete
#min(data$tenure) #1
#max(data$tenure) #72
binned_tenure = function(tenure){
  if (tenure >= 0 & tenure <=12) {
    return ("0-12 Months")
  }else if (tenure > 12 & tenure <= 24){
    return ("12-24 Months")
  }else if (tenure > 24 & tenure <= 36){
    return ("24-36 Months")
  }else if (tenure > 36 & tenure <= 48){
    return ("36-48 Months")
  }else if (tenure > 48 & tenure <= 60){
    return ("48-60 Months")
  }else if (tenure > 60 & tenure <= 72){
    return ("60-72 Months")
  }
}
data$binned = sapply(data$tenure,binned_tenure)
data$binned = as.factor(data$binned)
#delete original tenure column
data$tenure = NULL
str(data$binned)



#see if numerical charges variables correlated
num_dat = sapply(data, is.numeric)
corr_num = cor(data[,num_dat])
corrplot(corr_num,method="number") #main="Monthly and Total Charges Correlation", 
#.65 medium correlation
#https://www.rdocumentation.org/packages/corrplot/versions/0.84
#https://www.bing.com/videos/search?q=is+.65+correlation+in+predictor+variables&view=detail&mid=CB8A6E169FA25245BCB6CB8A6E169FA25245BCB6&FORM=VIRE
#https://www.bing.com/videos/search?q=is+.65+correlation+in+predictor+variables&view=detail&mid=A1C4D8F309AB4D11F6E4A1C4D8F309AB4D11F6E4&FORM=VIRE
plot(data$MonthlyCharges, data$TotalCharges)

#remove one of the correlated columns
data$MonthlyCharges = NULL


###############   preprocess the test data   ########################################
#change senior citizen to a factor
data_test$SeniorCitizen = factor(data_test$SeniorCitizen)
data_test$SeniorCitizen = factor(data_test$SeniorCitizen,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))

#change churn to a factor
data_test$Churn = factor(data_test$Churn)
data_test$Churn = factor(data_test$Churn,
                         levels = c(0,1),
                         labels = c("No", "Yes"))

#impute nas to 0
data_test = data_test %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges), 0, TotalCharges))

anyNA(data_test)


#remove churn column from test data
data_test$Churn = NULL


#combine levels
levels(data_test$StreamingTV) = gsub("No internet service", "No", levels(data_test$StreamingTV))

levels(data_test$MultipleLines) = gsub("No phone service", "No", levels(data_test$MultipleLines))

levels(data_test$OnlineSecurity) = gsub("No internet service", "No", levels(data_test$OnlineSecurity))

levels(data_test$OnlineBackup) = gsub("No internet service", "No", levels(data_test$OnlineBackup))

levels(data_test$DeviceProtection) = gsub("No internet service", "No", levels(data_test$DeviceProtection))

levels(data_test$TechSupport) = gsub("No internet service", "No", levels(data_test$TechSupport))

levels(data_test$StreamingMovies) = gsub("No internet service", "No", levels(data_test$StreamingMovies))

#drop customer number column
data_test = data_test[ -c(1) ] #drop customer number column



#standardize monthlycharges and totalcharges
data_test$TotalCharges = scale(data_test$TotalCharges)
data_test$MonthlyCharges = scale(data_test$MonthlyCharges)



#bin tenure
#min(data_test$tenure) #0
#max(data_test$tenure) #72
binned_test_tenure = function(tenure){
  if (tenure >= 0 & tenure <=12) {
    return ("0-12 Months")
  }else if (tenure > 12 & tenure <= 24){
    return ("12-24 Months")
  }else if (tenure > 24 & tenure <= 36){
    return ("24-36 Months")
  }else if (tenure > 36 & tenure <= 48){
    return ("36-48 Months")
  }else if (tenure > 48 & tenure <= 60){
    return ("48-60 Months")
  }else if (tenure > 60 & tenure <= 72){
    return ("60-72 Months")
  }
}
data_test$binned = sapply(data_test$tenure,binned_test_tenure)
data_test$binned = as.factor(data_test$binned)
#remove tenure column
data_test$tenure = NULL


#remove MonthlyCharges due to its correlation to totalcharges
data_test$MonthlyCharges = NULL



###############   association rule mining   #########################################

#https://towardsdatascience.com/association-rule-mining-in-r-ddf2d044ae50
#selection_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="Churn")))
#shows vars are related to churning and will generate rules that lead to churning

churn_rules = apriori(data, parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(churn_rules, by = "confidence"), 3)) #get top 3 rules

select_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="Churn=Yes"))
inspect(head(sort(select_rules, by = "confidence"), 3))
#PhoneService=No,                                                                    
#DeviceProtection=Yes,                                                               
#PaymentMethod=Electronic check,                                                     
#binned=0-12 Months}             => {Churn=Yes} 0.001893222          1 3.789096    10

#SeniorCitizen=Yes,                                                                  
#PhoneService=No,                                                                    
#OnlineBackup=No,                                                                    
#PaymentMethod=Electronic check,                                                     
#binned=0-12 Months}             => {Churn=Yes} 0.002461189          1 3.789096    13

#gender=Female,                                                                      
#SeniorCitizen=Yes,                                                                  
#PhoneService=No,                                                                    
#PaperlessBilling=No,                                                                
#binned=0-12 Months}             => {Churn=Yes} 0.001135933          1 3.789096     6


###########   visualization   ############################################
aggregate(data$Churn, by=list(data$SeniorCitizen), FUN=length)
aggregate(data$Churn, by=list(data$SeniorCitizen,data$binned), FUN=length)
aggregate(data$Churn, by=list(data$SeniorCitizen,data$PhoneService), FUN=length)

#totalcharges boxplot
boxplot(data$TotalCharges,data=data, main="TotalCharges") #scaled totalcharges, right skewed, median closer to -1, 75% closer to -1

#binned tenure histogram
tenure = ggplot(data, aes(x = binned))
tenure + geom_histogram(aes(fill = Churn), stat="count",
                        alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

tenure = ggplot(data, aes(x = data$tenure))
tenure + geom_histogram(aes(fill = Churn), stat="count",
                        alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))



##############   divide train data: nested holdout and cv   ################################
#note: preprocessed data before splitting
set.seed(123)
train_split = sample(1:nrow(data), nrow(data)*0.8) #take 80% sample
train = data[train_split, ]
#dim(train)
test_without_y = subset(data[-train_split, ])#, select= -Churn)
test_with_y = data[-train_split, ]$Churn

train_control = trainControl(method = "cv", number = 5)


##############   variable/feature selection   ######################################################
#step glm
mod = step(glm(Churn ~., data=data, family=binomial(link="logit")), direction="both")
summary(mod) #AIC: 4407.6
#SeniorCitizenYes                      **  
#PhoneServiceYes                       **
#MultipleLinesYes                      *** 
#InternetServiceFiber optic            ***
#InternetServiceNo                     ***
#OnlineSecurityYes                     ***  
#OnlineBackupYes                       *  
#TechSupportYes                        *** 
#StreamingTVYes                        ***
#StreamingMoviesYes                    **
#ContractOne year                      ***
#ContractTwo year                      ***
#PaperlessBillingYes                   ***
#PaymentMethodElectronic check         ** 
#TotalCharges                          *
#binned12-24 Months                    ***
#binned24-36 Months                    ***
#binned36-48 Months                    ***
#binned48-60 Months                    ***
#binned60-72 Months                    **


library(MASS)
step = stepAIC(mod, trace=FALSE)
step$anova #aic 4407.6
#Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models for the data, AIC estimates the quality of each model, relative to each of the other models. Thus, AIC provides a means for model selection.
#Final Model:
#Churn ~ SeniorCitizen + PhoneService + MultipleLines + InternetService + 
  #OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + 
  #StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
  #TotalCharges + binned


#lasso
x = model.matrix(Churn~.,data)[,-1] #sets up x with all vars except for target survived
colnames(x) #no churn
View(x)
y = data$Churn #sets up y with target var only

train1=sample(1:nrow(x), nrow(x)/2)
View(train1)
test=(-train1)
y.test=y[test]

cv.out_lasso = cv.glmnet(x[train1,],y[train1],alpha=1, family="binomial")
#alpha 1 is lasso
cv.out_lasso
#model output tells you min lambda using mse (mean squared error)
summary(cv.out_lasso)
plot(cv.out_lasso)
#the best model is under lambda = lambda.min
bestlam2 = cv.out_lasso$lambda.min
bestlam2
#0.001 best lambda for lasso
out2 = glmnet(x,y, alpha=1, family = "binomial")
plot(out2, xvar = "lambda", label = TRUE)
lasso.coef = predict(out2,type="coefficients",s=bestlam2)[1:26,] #26 is how many columns
lasso.coef
#take out 0 coefficients for lasso, 0 vars did not explain anything in model. weight/coef doesn't matter much the smaller the number
lasso.coef[lasso.coef!=0]
#selected variables with coefficients not 0:
#genderMale 
# -0.004683997 
#SeniorCitizenYes 
#0.232654005 
#PartnerYes 
#  -0.064675174
#DependentsYes 
#-0.086875641 
#PhoneServiceYes 
# -0.365213935
#MultipleLinesYes 
#  0.276604792 
#InternetServiceFiber optic 
# 0.952562762  ###########
#InternetServiceNo 
#-0.917323347  ##########
#OnlineSecurityYes 
#-0.432490811 
#OnlineBackupYes 
# -0.179811306 
#TechSupportYes 
# -0.347179541
#StreamingTVYes 
# 0.327467436
#StreamingMoviesYes 
#0.282863182
#ContractOne year 
#-0.769861401
#ContractTwo year 
#-1.459498365    ###########
#PaperlessBillingYes 
# 0.274033842
#PaymentMethodCredit card (automatic) 
# -0.102438224 
#PaymentMethodElectronic check             
#0.336699401                          
#PaymentMethodMailed check 
#-0.008508070 
#binned12-24 Months  
#-0.654101146 
#binned24-36 Months                    
#-0.991063326      ###########                   
#binned48-60 Months 
#-0.731431264 
#binned36-48 Months 
#-0.69581624
#TotalCharges                    
#-0.455297724


#look at sig vars using the p-value
logist_mod_full = train(Churn ~ .,
                   data = data,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_mod_full)
logist_mod_full$results
#accuracy=.80, aic=4413
#sig vars
#SeniorCitizenYes                       *  
#PhoneServiceYes                        ** 
#MultipleLinesYes                       ** 
#`InternetServiceFiber optic`           *** ###########
#InternetServiceNo                      ***
#OnlineSecurityYes                      ***
#OnlineBackupYes                        *  
#TechSupportYes                         ***
#StreamingTVYes                         ***
#StreamingMoviesYes                     ** 
#`ContractOne year`                     ***
#`ContractTwo year`                     *** ############
#PaperlessBillingYes                    ** 
#`PaymentMethodElectronic check`        ** 
#TotalCharges                           *  
#`binned12-24 Months`                   *** #############
#`binned24-36 Months`                   *** #############
#`binned36-48 Months`                   ***
#`binned48-60 Months`                   ***
#`binned60-72 Months`                   *  



#########   final model ##################################################

logist_final = train(Churn ~ SeniorCitizen + PhoneService + MultipleLines + 
                        InternetService +OnlineSecurity + 
                        OnlineBackup + TechSupport + StreamingTV + 
                        StreamingMovies + Contract + 
                        PaperlessBilling + PaymentMethod + 
                        TotalCharges + binned,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(logist_final)
logist_final$results
#aic 3521.1

#auc 
pred = predict(logist_final, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8447

#plot auc
plot(performance(out, "tpr", "fpr"), colorize=TRUE)
abline(0,1)
#false positive rate metric corresponds to the proportion of negative 
#data points that are mistakenly considered as positive, with respect 
#to all negative data points. In other words, the higher FPR
#the more negative data points we will missclassified

#dashed line in the diagonal is the ROC curve of a random 
#predictor: it has an AUROC of 0.5. The random predictor is commonly 
#used as a baseline to see whether the model is useful

#AUC of a classifier is equal to the probability that the classifier 
#will rank a randomly chosen positive example higher than a randomly 
#chosen negative example
#In other words, the area under the curve is the probability that a 
#random positive sample will have a higher score than a random negative 
#sample






#glm()
log_mod = glm(Churn ~ SeniorCitizen + PhoneService + MultipleLines + 
                       InternetService +OnlineSecurity + 
                       OnlineBackup + TechSupport + StreamingTV + 
                       StreamingMovies + Contract + 
                       PaperlessBilling + PaymentMethod + 
                       TotalCharges + binned,data = train, family=binomial)

summary(log_mod)

#scatterplot of the residuals, against predicted values (the score actually)
#https://stats.stackexchange.com/questions/121490/interpretation-of-plot-glm-model
plot(log_mod)
#The Residuals vs Fitted plot can help you see, for example, if there are curvilinear trends that you missed. But the fit of a logistic regression is curvilinear by nature, so you can have odd looking trends in the residuals with nothing amiss. 
#The Normal Q-Q plot helps you detect if your residuals are normally distributed. But the deviance residuals don't have to be normally distributed for the model to be valid, so the normality / non-normality of the residuals doesn't necessarily tell you anything. 
#The Scale-Location plot can help you identify heteroscedasticity. But logistic regression models are pretty much heteroscedastic by nature. 
#The Residuals vs Leverage can help you identify possible outliers. But outliers in logistic regression don't necessarily manifest in the same way as in linear regression, so this plot may or may not be helpful in identifying them. 



#ground truth percentage of churn in test subset data
#count    percent
#No  769  0.7275307
#Yes 288  0.2724693
#1057 total

library(car)
vif(log_mod)
vif_values = vif(log_mod) #create vector of values
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


#cm
predcm = predict(logist_final, test_without_y, type="raw")
confusionMatrix(predcm, test_with_y)
            #Reference
#Prediction   No  Yes       
        #No   703 141
        #Yes  66  147

#Accuracy : 0.8042  tp[147] + tn[703] / p[288] + n[769] aka correct # of decisions/total # of decisions
#95% CI : (0.7789, 0.8277)

#Sensitivity : 0.9142   tn[703] / (tn[703] + fp[66]) true negative rate   
#Specificity : 0.5104   tp[147] / (tp[147] +fn[141])  true positive rate, also called recall
#precision   : 0.69     tp[147] / (tp[147] + fp[66])


#specificity metric corresponds to the proportion of positive data points
#that are correctly considered as positive, with respect to all positive 
#data points. 
#The higher TPR, the fewer positive data points we will miss.




#estimated (fake business scenario) cost benefit analysis to keep estimates???
b1= sum(data$TotalCharges, na.rm=TRUE)    
b1
b2= sum(data_test$TotalCharges, na.rm=TRUE)    
b2
t = (11908562 + 4147607)
t
worth = (16056169/7043) #7043 customers
worth
#2279.734 per customer total charges on average (mean)
summary(data$TotalCharges)

b3= sum(data$MonthlyCharges, na.rm=TRUE)    
b3
b4= sum(data_test$MonthlyCharges, na.rm=TRUE)    
b4
t2 = (340712.6 + 115404)
t2
worth2 = (456116.6/7043)
worth2
#64 per customer per month charges on average (mean)

summary(data$tenure)
#mean is 32
summary(data$MonthlyCharges)
#mean is 64
worth3 = (32*64)
worth3
#2048 per customer with 32 months of tenure and 64 dollars monthly charges


#probabilities of each decision outcome based on our cm
#p(Y, p) = 147/1057 = .14
#p(Y, n) = 66/1057  = .06
#p(N, p) = 141/1057 = .13
#p(N, n) = 703/1057 = .67

