#Kimberly Glock
#2/11/2020
#Project 2, Imagination at Work
#Dr. Mingle MSDS 5223
#Lipscomb University


library(janitor)
#https://www.rdocumentation.org/packages/janitor/versions/1.2.0
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
data = read.csv(file="Train_Churn_binary.csv", header=TRUE, sep=",")
#View(data) #5282 rows

data_test = read.csv(file="Test_Churn.csv", header=TRUE, sep=",")
#View(data_test) #1761 rows

data_cust_num = read.csv(file="Test_Churn.csv", header=TRUE, sep=",")
#to get train data customer number



#percentage of churn
perc_churn = tabyl(data$Churn)
perc_churn


###############   check for nas   ###################################################
anyNA(data) #TRUE
sum(is.na(data)) #7nas

anyNA(data_test) #TRUE
sum(is.na(data_test)) #Churn column removed, 4nas



###############   view and examine data, descriptive stats   #######################
str(data)
sapply(data, function(x) sum(is.na(x)))
summary(data) #7nas in totalcharges

str(data_test)
sapply(data_test, function(x) sum(is.na(x)))
summary(data_test) #4nas in totalcharges



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



#delete nas
sum(is.na(data))
#data = data %>% 
  #filter (!is.na(TotalCharges))
data = data %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges), 0, TotalCharges))
anyNA(data)
str(data$TotalCharges)
#0r delete nas
#data = na.omit(data)


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


#check for skewdness
#https://rpubs.com/marvinlemos/log-transformation?fbclid=IwAR3GPfrMUfvBz_MPb7OqpLFsFUeYSvulV3z-IB7JCGfy-501jy4RpE7knIQ
skewness(data$TotalCharges) #.98 so highly skewed
#skewness of the predictor variable is 0, the data is perfectly symmetrical,
#less than -1 or greater than +1, the data is highly skewed,
#is between -1 and -0.5 or between +1 and +0.5 then the data is moderately skewed,
#is -0.5 and +0.5, the data is approximately symmetric.




#take log of total charges 
data$TotalCharges = log(data$TotalCharges)
#data$TotalCharges = log1p(data$TotalCharges)
data$TotalCharges = format(round(data$TotalCharges, 2), nsmall = 2) #limit decimal places to 2
data$TotalCharges = as.numeric(as.character(data$TotalCharges))
str(data$TotalCharges)

#merge three columns to make one
#total/monthly = tenure
#data$merg_tenure = data$TotalCharges / data$MonthlyCharges
#View(data)
#data$tenure = NULL
#data$TotalCharges = NULL
#data$MonthlyCharges = NULL

#standardize
#data$TotalCharges = (data$TotalCharges - mean(data$TotalCharges)) / sd(data$TotalCharges)
#or
data$TotalCharges = scale(data$TotalCharges)
#View(data)

#standardize
#data$MonthlyCharges = (data$MonthlyCharges - mean(data$MonthlyCharges)) / sd(data$MonthlyCharges)
data$MonthlyCharges = scale(data$MonthlyCharges)

data$MonthlyCharges = log(data$MonthlyCharges)
data$MonthlyCharges = format(round(data$MonthlyCharges, 2), nsmall = 2) #limit decimal places to 2
data$MonthlyCharges = as.numeric(as.character(data$MonthlyCharges))
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
#View(data)
#str(data)
data$tenure = NULL
str(data$binned)

#bin merg_tenure
#min(data$merg_tenure) #0
#max(data$merg_tenure) #80
#binned_tenure = function(tenure){
 # if (tenure >= 0 & tenure <=12) {
  #  return ("0-12 Months")
  #}else if (tenure > 12 & tenure <= 24){
   # return ("12-24 Months")
  #}else if (tenure > 24 & tenure <= 36){
   # return ("24-36 Months")
  #}else if (tenure > 36 & tenure <= 48){
   # return ("36-48 Months")
  #}else if (tenure > 48 & tenure <= 60){
   # return ("48-60 Months")
  #}else if (tenure > 60 & tenure <= 80){
   # return ("60-80 Months")
  #}
#}
#data$binned = sapply(data$merg_tenure,binned_tenure)
#data$binned = as.factor(data$binned)
#View(data)
#data$merg_tenure = NULL

#down sample
#https://www.rdocumentation.org/packages/caret/versions/6.0-85/topics/downSample
samp = downSample(data, data$Churn, list = FALSE, yname = "Churn")
class(samp)
#merge(one_hot, samp, all=TRUE)
#https://datascienceplus.com/combining-data-in-r-the-skill-of-merging-joining-and-stacking/
library(Stack)
stk = Stack(data, samp)
stk
summary(stk)

perc_churn = tabyl(stk$Churn)
perc_churn
#aprx 34% churn


#see if numerical charges variables correlated
num_dat = sapply(data, is.numeric)
corr_num = cor(data[,num_dat])
corrplot(corr_num, main="Monthly and Total Charges Correlation", method="number")
#.65 medium correlation
#https://www.rdocumentation.org/packages/corrplot/versions/0.84
#https://www.bing.com/videos/search?q=is+.65+correlation+in+predictor+variables&view=detail&mid=CB8A6E169FA25245BCB6CB8A6E169FA25245BCB6&FORM=VIRE
#https://www.bing.com/videos/search?q=is+.65+correlation+in+predictor+variables&view=detail&mid=A1C4D8F309AB4D11F6E4A1C4D8F309AB4D11F6E4&FORM=VIRE
plot(data$MonthlyCharges, data$TotalCharges)
#remove one

#data$TotalCharges = NULL
#data$MonthlyCharges = NULL


#concatenate columns 
data$bin12_phonservno = as.factor(paste(data$binned,data$PhoneService))
#one_hot$bin12_phonservno = paste(one_hot$binned,one_hot$PhoneService)

data$internetserv_onlinesec = as.factor(paste(data$InternetService,data$OnlineSecurity))
#one_hot$internetserv_onlinesec = paste(one_hot$InternetService,one_hot$OnlineSecurity)

data$internetserv_onlinebac = as.factor(paste(data$InternetService,data$OnlineBackup))
#one_hot$internetserv_onlinebac= paste(one_hot$InternetService,one_hot$OnlineBackup)

data$internetserv_streamov = as.factor(paste(data$InternetService,data$StreamingMovies))
#one_hot$internetserv_streammov = paste(one_hot$InternetService,one_hot$StreamingMovies)

data$internetserv_techsupp = as.factor(paste(data$InternetService,data$TechSupport))
#one_hot$internetserv_techsupp = paste(one_hot$InternetService,one_hot$TechSupport)

data$sencityes_phonservno = as.factor(paste(data$SeniorCitizen,data$PhoneService))
#one_hot$sencityes_phonservno = paste(one_hot$SeniorCitizen,one_hot$PhoneService)

data$part_deps = as.factor(paste(data$Partner,data$Dependents))
#one_hot$part_deps = paste(one_hot$Partner,one_hot$Dependents)

data$part_sencitz = as.factor(paste(data$Partner,data$SeniorCitizen))
#one_hot$part_sencitz = paste(one_hot$Partner,one_hot$SeniorCitizen)

data$multipline_deps = as.factor(paste(data$MultipleLines,data$Dependents))
#one_hot$multipline_deps = paste(one_hot$MultipleLines,one_hot$Dependents)

data$part_deps_multpline = as.factor(paste(data$Partner,data$Dependents,data$MultipleLines))
#one_hot$part_deps_multpline = paste(one_hot$Partner,one_hot$Dependents,one_hot$MultipleLines)

data$techsupp_pymtmailcheck = as.factor(paste(data$TechSupport,data$PaymentMethod))
#one_hot$techsupp_pymtmailcheck = paste(one_hot$TechSupport,one_hot$PaymentMethod)

data$dep_multpli_intserv = as.factor(paste(data$Dependents,data$MultipleLines,data$InternetService))
#one_hot$dep_multpli_intserv = paste(one_hot$Dependents,one_hot$MultipleLines,one_hot$InternetService)

#one_hot$part_streamtv = paste(one_hot$Partner,one_hot$StreamingTV)

data$paplsbilling_sencit_phnservno = as.factor(paste(data$PaperlessBilling,data$SeniorCitizen,data$PhoneService))
#one_hot$paplsbilling_sencit_phnservno = paste(one_hot$PaperlessBilling,one_hot$SeniorCitizen,one_hot$PhoneService)

data$onlinsec_intserv = as.factor(paste(data$OnlineSecurity,data$InternetService))
#one_hot$onlinsec_intserv = paste(one_hot$OnlineSecurity,one_hot$InternetService)

View(data)
str(data)
View(one_hot)
#str(one_hot)

one_hot$part_streamtv = as.factor(one_hot$part_streamtv)
View(one_hot$part_streamtv)
levels(one_hot$part_streamtv)
one_hot$bin12_phonservno = as.factor(one_hot$bin12_phonservno)
one_hot$sencityes_phonservno = as.factor(one_hot$sencityes_phonservno)
one_hot$part_deps = as.factor(one_hot$part_deps)
one_hot$bin12_phonservno = as.factor(one_hot$bin12_phonservno)
one_hot$part_sencitz = as.factor(one_hot$part_sencitz)
one_hot$multipline_deps = as.factor(one_hot$multipline_deps)
one_hot$part_deps_multpline = as.factor(one_hot$part_deps_multpline)
one_hot$techsupp_pymtmailcheck = as.factor(one_hot$techsupp_pymtmailcheck)
one_hot$bin12_phonservno = as.factor(one_hot$bin12_phonservno)
one_hot$dep_multpli_intserv = as.factor(one_hot$dep_multpli_intserv)
one_hot$part_streamtv = as.factor(one_hot$part_streamtv)
one_hot$onlinsec_intserv = as.factor(one_hot$onlinsec_intserv)


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

#impute nas
data_test = data_test %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges), 0, TotalCharges))

data_test$Churn = NULL

anyNA(data_test)


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

#merge total/monthly = tenure
#data_test$merg_tenure = data_test$TotalCharges / data_test$MonthlyCharges
#data_test$tenure = NULL
#data_test$TotalCharges = NULL
#data_test$MonthlyCharges = NULL

#log of total charges
data_test$TotalCharges = log(data_test$TotalCharges)
#data_test$TotalCharges = log1p(data_test$TotalCharges)
data_test$TotalCharges = format(round(data_test$TotalCharges, 2), nsmall = 2) #limit decimal places to 2
data_test$TotalCharges = as.numeric(as.character(data_test$TotalCharges))
str(data_test)

#standardize
#data_test$TotalCharges = (data_test$TotalCharges - mean(data_test$TotalCharges)) / sd(data_test$TotalCharges)
data_test$TotalCharges = scale(data_test$TotalCharges)
#data_test$MonthlyCharges = (data_test$MonthlyCharges - mean(data_test$MonthlyCharges)) / sd(data_test$MonthlyCharges)
data_test$MonthlyCharges = scale(data_test$MonthlyCharges)

data_test$MonthlyCharges = log(data_test$MonthlyCharges)
data_test$MonthlyCharges = format(round(data_test$MonthlyCharges, 2), nsmall = 2) #limit decimal places to 2
data_test$MonthlyCharges = as.numeric(as.character(data_test$MonthlyCharges))



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
#View(data_test)
data_test$tenure = NULL

#bin merg_tenure
#min(data_test$merg_tenure) #0
#max(data_test$merg_tenure) #77
#binned_test_tenure = function(tenure){
#  if (tenure >= 0 & tenure <=12) {
 #   return ("0-12 Months")
  #}else if (tenure > 12 & tenure <= 24){
   # return ("12-24 Months")
  #}else if (tenure > 24 & tenure <= 36){
   # return ("24-36 Months")
  #}else if (tenure > 36 & tenure <= 48){
   # return ("36-48 Months")
  #}else if (tenure > 48 & tenure <= 60){
   # return ("48-60 Months")
  #}else if (tenure > 60 & tenure <= 77){
   # return ("60-77 Months")
  #}
#}
#data_test$binned = sapply(data_test$merg_tenure,binned_test_tenure)
#data_test$binned = as.factor(data_test$binned)
#View(data_test)
#str(data_test$binned)
#data_test$tenure = NULL





#remove TotalCharges or MonthlyCharges
#data_test$TotalCharges = NULL
#data_test$MonthlyCharges = NULL

#concatenate columns in test data
data_test$bin12_phonservno = as.factor(paste(data_test$binned,data_test$PhoneService))

data_test$internetserv_onlinesec = as.factor(paste(data_test$InternetService,data_test$OnlineSecurity))

data_test$internetserv_onlinebac = as.factor(paste(data_test$InternetService,data_test$OnlineBackup))

data_test$internetserv_streamov = as.factor(paste(data_test$InternetService,data_test$StreamingMovies))

data_test$internetserv_techsupp = as.factor(paste(data_test$InternetService,data_test$TechSupport))

data_test$sencityes_phonservno = as.factor(paste(data_test$SeniorCitizen,data_test$PhoneService))

data_test$part_deps = as.factor(paste(data_test$Partner,data_test$Dependents))

data_test$part_sencitz = as.factor(paste(data_test$Partner,data_test$SeniorCitizen))

data_test$multipline_deps = as.factor(paste(data_test$MultipleLines,data_test$Dependents))

data_test$part_deps_multpline = as.factor(paste(data_test$Partner,data_test$Dependents,data_test$MultipleLines))

data_test$techsupp_pymtmailcheck = as.factor(paste(data_test$TechSupport,data_test$PaymentMethod))

data_test$dep_multpli_intserv = as.factor(paste(data_test$Dependents,data_test$MultipleLines,data_test$InternetService))

#data_test$part_streamtv = as.factor(paste(data_test$Partner,data_test$StreamingTV))
#data_test$part_streamtv = NULL

data_test$paplsbilling_sencit_phnservno = as.factor(paste(data_test$PaperlessBilling,data_test$SeniorCitizen,data_test$PhoneService))

data_test$onlinsec_intserv = as.factor(paste(data_test$OnlineSecurity,data_test$InternetService))
#View(data_test)

#A matrix that does not have "full rank" is said to be "rank deficient". A matrix is said to have full rank if its rank is either equal to its number of columns or to its number of rows (or to both). 


###############   association rule mining   #########################################

#https://towardsdatascience.com/association-rule-mining-in-r-ddf2d044ae50
#data$MonthlyCharges = NULL
#data$TotalCharges = NULL
#trans = lapply(data, function(x){as.factor(x)})
#transaction_data = as(data, "transactions") #covert to transactional dataset
#for feature selection
#class(data)
#inspect(head(data, 2))
#selection_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="Churn")))
#shows vars are realated to churning and will generate rules that lead to churning

churn_rules = apriori(data, parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(churn_rules, by = "confidence"), 25)) #get top 5 rules
selection_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="PaymentMethod=Electronic check"))
inspect(head(sort(selection_rules, by = "confidence"), 5))
#gender=Male,                                                                                      
#SeniorCitizen=Yes,                                                                                
#PhoneService=No,                                                                                  
#DeviceProtection=Yes,                                                                             
#Churn=Yes} => {PaymentMethod=Electronic check} 
# support 0.001135933    confidence 1      lift 2.964085     
select_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="Churn=Yes"))
inspect(head(sort(select_rules, by = "confidence"), 10))
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

#gender=Female,                                                                      
#SeniorCitizen=Yes,                                                                  
#Partner=No,                                                                         
#PhoneService=No,                                                                    
#binned=0-12 Months}             => {Churn=Yes} 0.001893222          1 3.789096    10

#gender=Female,                                                                      
#SeniorCitizen=Yes,                                                                  
#PhoneService=No,                                                                    
#OnlineBackup=No,                                                                    
#binned=0-12 Months}             => {Churn=Yes} 0.001893222          1 3.789096    10

#gender=Female,                                                                      
#SeniorCitizen=Yes,                                                                  
#Partner=No,                                                                         
#PhoneService=No,                                                                    
#PaperlessBilling=No}            => {Churn=Yes} 0.001135933          1 3.789096     6

#SeniorCitizen=Yes,                                                                  
#Partner=No,                                                                         
#PhoneService=No,                                                                    
#DeviceProtection=No,                                                                
#PaperlessBilling=No}            => {Churn=Yes} 0.001135933          1 3.789096     6

#PhoneService=No,                                                                    
#InternetService=DSL,                                                                
#DeviceProtection=Yes,                                                               
#PaymentMethod=Electronic check,                                                     
#binned=0-12 Months}             => {Churn=Yes} 0.001893222          1 3.789096    10

#PhoneService=No,                                                                    
#DeviceProtection=Yes,                                                               
#StreamingMovies=Yes,                                                                
#PaymentMethod=Electronic check,                                                     
#binned=0-12 Months}             => {Churn=Yes} 0.001325256          1 3.789096     7

#gender=Male,                                                                        
#PhoneService=No,                                                                    
#DeviceProtection=Yes,                                                               
#PaymentMethod=Electronic check,                                                     
#binned=0-12 Months}             => {Churn=Yes} 0.001135933          1 3.789096     6

techsupp_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="TechSupport=Yes"))
inspect(head(sort(techsupp_rules, by = "confidence"), 10))

intserv_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="InternetService=Fiber optic"))
inspect(head(sort(intserv_rules, by = "confidence"), 10))

paperlessbil_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="PaperlessBilling=Yes"))
inspect(head(sort(paperlessbil_rules, by = "confidence"), 10))

onlinesec_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="OnlineSecurity=No"))
inspect(head(sort(onlinesec_rules, by = "confidence"), 10))

streamtv_rules = apriori(data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="StreamingTV=Yes"))
inspect(head(sort(streamtv_rules, by = "confidence"), 10))


##############   visualization   #####################################################
#groupby
aggregate(data$TotalCharges, by=list(data$SeniorCitizen,data$PhoneService), FUN=mean)
aggregate(data$TotalCharges, by=list(data$SeniorCitizen,data$Partner), FUN=mean)
aggregate(data$TotalCharges, by=list(data$PaymentMethod), FUN=mean)
aggregate(data$TotalCharges, by=list(data$InternetService), FUN=mean)
aggregate(data$Churn, by=list(data$SeniorCitizen), FUN=length)
aggregate(data$Churn, by=list(data$SeniorCitizen,data$binned), FUN=length)
aggregate(data$Churn, by=list(data$SeniorCitizen,data$PhoneService), FUN=length)



sen_cit= ggplot(data, aes(x = SeniorCitizen))
sen_cit + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("yellow", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

partn= ggplot(data, aes(x = Partner))
partn + geom_histogram(aes(fill = Churn), stat="count",
                         alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("pink", "yellow")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

depn = ggplot(data, aes(x = Dependents))
depn + geom_histogram(aes(fill = Churn), stat="count",
                       alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("purple", "blue")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

phn_serv = ggplot(data, aes(x = PhoneService))
phn_serv + geom_histogram(aes(fill = Churn), stat="count",
                      alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("brown", "blue")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

multp_lin = ggplot(data, aes(x = MultipleLines))
multp_lin + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("brown", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

tech = ggplot(data, aes(x = TechSupport))
tech + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("yellow", "purple")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

int_serv = ggplot(data, aes(x = InternetService))
int_serv + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

on_lin_sec = ggplot(data, aes(x = OnlineSecurity))
on_lin_sec + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

billing = ggplot(data, aes(x = PaperlessBilling))
billing + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

stream_tv = ggplot(data, aes(x = StreamingTV))
stream_tv + geom_histogram(aes(fill = Churn), stat="count",
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

tenure = ggplot(data, aes(x = binned))
tenure + geom_histogram(aes(fill = Churn), stat="count",
                           alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("orange", "red")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))



#plot(data$binned_tenure, xlim=range(data$binned_tenure)
bin_ten = ggplot(data, aes(binned)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()    
bin_ten


boxplot(data$MonthlyCharges,data=data, main="Monthly Charges")
#qplot(data$MonthlyCharges, geom="histogram")
#qplot(MonthlyCharges, data = data, geom = "histogram",
      #fill = Churn)

a = ggplot(data, aes(x = MonthlyCharges))
#a + geom_dotplot(aes(fill = Churn), stat="count", binwidth = 1) +
  #scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(data, aes(x = factor(1), y = MonthlyCharges)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Churn, shape = Churn), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label

a + geom_area(aes(fill = Churn), color = "white", 
              stat ="bin", bins = 30) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

#http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/


##############   divide train data: nested holdout and cv   ################################
set.seed(123)
train_split = sample(1:nrow(data), nrow(data)*0.8) #take 80% sample
train = data[train_split, ]
#View(train)
#dim(train)
#anyNA(train)
test_without_y = subset(data[-train_split, ], select= -Churn)
test_with_y = data[-train_split, ]$Churn


train_control = trainControl(method = "cv", number = 5)

#######   create and train models using cv
#View(data)
#View(stk)

logist_mod = train(Churn ~ SeniorCitizen + MultipleLines + InternetService +OnlineSecurity + 
                     TechSupport + StreamingTV + StreamingMovies + Contract + 
                     PaperlessBilling + PaymentMethod + 
                     MonthlyCharges + TotalCharges + binned,
                   data = train,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_mod)
logist_mod$results
#nas when using log of total and monthly
#accuracy .80

#auc train
pred = predict(logist_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8589


log_mod = glm(Churn ~ .,family=binomial(link="logit"),data=train)
print(summary(log_mod))

library(MASS)
step = stepAIC(log_mod, trace=FALSE)
step$anova
#Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models for the data, AIC estimates the quality of each model, relative to each of the other models. Thus, AIC provides a means for model selection.
#Final Model:
#Churn ~ SeniorCitizen + MultipleLines + InternetService +OnlineSecurity + 
#TechSupport + StreamingTV + StreamingMovies + Contract + 
#PaperlessBilling + PaymentMethod + 
#MonthlyCharges + TotalCharges + binned


anova(log_mod, test="Chisq")
#difference between the null deviance and the residual deviance shows how the model is doing against the null model (a model with only the intercept)
#the wider this gap, the better
#analyzing the table look at the drop in residual deviance when adding each variable one at a time
#look at what significantly reduces the residual deviance
#some variables will improve the model less even though they may have a low p-value
#large p-value here indicates that the model without the variable explains more or less the same amount of variation
#ultimately what we would like to see is a significant drop in deviance and the AIC
#https://stats.stackexchange.com/questions/59879/logistic-regression-anova-chi-square-test-vs-significance-of-coefficients-ano
                  #Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#NULL                              4224     4858.0              
#gender            1     0.06      4223     4857.9 0.8104937    
#SeniorCitizen     1    80.72      4222     4777.2 < 2.2e-16 ***
#Partner           1   104.26      4221     4672.9 < 2.2e-16 ***
#Dependents        1    26.60      4220     4646.3 2.508e-07 ***
#PhoneService      1     0.88      4219     4645.4 0.3474045    
#MultipleLines     1     4.16      4218     4641.3 0.0412870 *  
#InternetService   2   401.85      4216     4239.4 < 2.2e-16 ***
#OnlineSecurity    1   144.06      4215     4095.4 < 2.2e-16 ***
#OnlineBackup      1    76.90      4214     4018.5 < 2.2e-16 ***
#DeviceProtection  1    45.47      4213     3973.0 1.548e-11 ***
#TechSupport       1    87.50      4212     3885.5 < 2.2e-16 ***
#StreamingTV       1     5.50      4211     3880.0 0.0190246 *  
#StreamingMovies   1     0.18      4210     3879.8 0.6682700    
#Contract          2   232.18      4208     3647.6 < 2.2e-16 ***
#PaperlessBilling  1    14.24      4207     3633.4 0.0001606 ***
#PaymentMethod     3    29.56      4204     3603.8 1.705e-06 ***
#MonthlyCharges    1     1.65      4203     3602.2 0.1995248    
#TotalCharges      1    77.84      4202     3524.3 < 2.2e-16 ***
#binned            5    51.86      4197     3472.5 5.773e-10 ***





#final test
pred1 = predict(logist_mod, data_test, type="prob")[,2]
pred1

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred1 = as.data.frame(pred1)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin1 = cbind(cus, pred1)
fin1

colnames(fin1)
names(fin1)[1] = "Customer Number"
names(fin1)[2] = "Churn"
#View(fin1)


#export to a csv
write.csv(fin1,'log_regr_1_kmg.csv', row.names = FALSE)
#.84

#confusion matrix
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
testing$Churn <- as.character(testing$Churn) #test_without_y
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)




#chosen vars
Churn ~ SeniorCitizen + Dependents + binned + MultipleLines + 
  InternetService + OnlineSecurity + TechSupport + StreamingTV + 
  StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
  MonthlyCharges



#model 18
#final test
pred18 = predict(logist_mod, data_test, type="prob")[,2]
pred18

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred18 = as.data.frame(pred18)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin18 = cbind(cus, pred18)
fin18

colnames(fin18)
names(fin18)[1] = "Customer Number"
names(fin18)[2] = "Churn"
#View(fin1)


#export to a csv
write.csv(fin18,'log_regr_18_kmg.csv', row.names = FALSE)
#.84

#model 19 
data19 = data[ -c(1,3:4,9:10) ]
View(data19)
data19_test = data_test[ -c(1,3:4,9:10) ]
set.seed(123)
train_split = sample(1:nrow(data19), nrow(data19)*0.8) #take 80% sample
train = data19[train_split, ]
#dim(train)
test_without_y = subset(data19[-train_split, ], select= -Churn)
test_with_y = data19[-train_split, ]$Churn

logist_mod19 = train(Churn ~ .,
                   data = data19,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_mod19)
logist_mod19$results
#accuracy .80

#auc train
pred = predict(logist_mod19, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8479

#final test
pred19 = predict(logist_mod19, data19_test, type="prob")[,2]
pred19

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred19 = as.data.frame(pred19)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin19 = cbind(cus, pred19)
fin19

colnames(fin19)
names(fin19)[1] = "Customer Number"
names(fin19)[2] = "Churn"
#View(fin1)


#export to a csv
write.csv(fin19,'log_regr_19_kmg.csv', row.names = FALSE)
#.84








#binned tenure, deleted monthly charges, no one hot, model #13
logist_13mod = train(Churn ~ .,
                   data = train,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_13mod)
logist_13mod$results
#accuracy .80

#auc train
pred = predict(logist_13mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8589

#final test
pred13 = predict(logist_13mod, data_test, type="prob")[,2]
pred13

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred13 = as.data.frame(pred13)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin13 = cbind(cus, pred13)
#fin13

colnames(fin13)
names(fin13)[1] = "Customer Number"
names(fin13)[2] = "Churn"
#View(fin13)


#export to a csv
write.csv(fin13,'log_regr_13_binned_delmonthchar_noonehot_kmg.csv', row.names = FALSE)
#.84


#merged total/monthly = tenure no binned no sig vars model #14
logist_14mod = train(Churn ~ .,
                     data = train,
                     trControl = train_control,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_14mod)
logist_14mod$results
#accuracy .80

#auc train
pred = predict(logist_14mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8589

#merged total/monthly = tenure no binned, sig vars model #15
logist_15mod = train(Churn ~ SeniorCitizen + PhoneService + 
                       MultipleLines + InternetService +
                       OnlineSecurity + TechSupport +
                       StreamingTV + StreamingMovies + Contract + 
                       PaperlessBilling + 
                       PaymentMethod+ merg_tenure,
                     data = train,
                     trControl = train_control,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_15mod)
logist_15mod$results
#accuracy .80

#auc train
pred = predict(logist_15mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8438

#final test
pred15 = predict(logist_15mod, data_test, type="prob")[,2]
#pred15

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred15 = as.data.frame(pred15)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin15 = cbind(cus, pred15)
#fin15

colnames(fin15)
names(fin15)[1] = "Customer Number"
names(fin15)[2] = "Churn"
#View(fin2)


#export to a csv
write.csv(fin15,'log_regr_15_merge_sigvars_kmg.csv', row.names = FALSE)
#.84

#merged total/monthly = tenure binned, sig vars model #16
logist_16mod = train(Churn ~ SeniorCitizen + PhoneService + 
                       MultipleLines + InternetService +
                       OnlineSecurity + TechSupport +
                       StreamingTV + StreamingMovies + Contract + 
                       PaperlessBilling + 
                       PaymentMethod+ merg_tenure,
                     data = train,
                     trControl = train_control,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_16mod)
logist_16mod$results
#accuracy .80

#auc train
pred = predict(logist_16mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8438


#model #17 one hot, merged, binned
View(one_hot)
train_split = sample(1:nrow(one_hot), nrow(one_hot)*0.8) #take 80% sample
train = one_hot[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn
logist_17mod = train(Churn ~ SeniorCitizen.Yes + PhoneService.Yes + 
                       MultipleLines.Yes + InternetService.Fiber.optic +
                       InternetService.No + OnlineSecurity.Yes + TechSupport.Yes +
                       StreamingTV.Yes + StreamingMovies.Yes + Contract.One.year + 
                       Contract.Two.year + PaperlessBilling.Yes + 
                       PaymentMethod.Electronic.check + binned.12.24.Months +
                       binned.24.36.Months + binned.36.48.Months,
                     data = one_hot,
                     trControl = train_control,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_17mod)
logist_17mod$results
#accuracy .80

#auc train
pred = predict(logist_17mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.83



#no binned tenure, no one hot, standardized monthly and total, ran vars model #11
logist_11mod = train(Churn ~ SeniorCitizen + tenure + 
                       MonthlyCharges + TotalCharges,
                   data = train,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_11mod)
logist_11mod$results
#accuracy .80

#auc train
pred = predict(logist_11mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8589


#model #21
logist_21mod = train(Churn ~ .,
                     data = train,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_21mod)
logist_21mod$results
#accuracy .80

#auc train
pred = predict(logist_21mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8438

#final test
pred21 = predict(logist_21mod, data_test, type="prob")[,2]
#pred15

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred21 = as.data.frame(pred21)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin21 = cbind(cus, pred21)
#fin15

colnames(fin21)
names(fin21)[1] = "Customer Number"
names(fin21)[2] = "Churn"
#View(fin2)


#export to a csv
write.csv(fin21,'log_CONCAT_21_kmg.csv', row.names = FALSE)


#model #22
logist_22mod = train(Churn ~ TotalCharges + InternetService +
                       OnlineSecurity + TechSupport +
                       StreamingTV + Contract + 
                       PaperlessBilling + techsupp_pymtmailcheck +
                       dep_multpli_intserv + paplsbilling_sencit_phnservno,
                     data = train,
                     trControl = train_control,
                     method = "glm",
                     family=binomial(link="logit"))
summary(logist_22mod)
logist_22mod$results
#accuracy .80

#auc train
pred = predict(logist_22mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8438

#model 23
logist_mod = train(Churn ~ .,
                   data = train,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_mod)
logist_mod$results
#accuracy .80

#auc train
pred = predict(logist_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.8589




##############   variable selection   ######################################################
#step glm
mod = step(glm(Churn ~., data=train, family=binomial(link="logit")), direction="both")
summary(mod)
  #SeniorCitizenYes                     0.025601 *  
  #MultipleLinesYes                     0.001508 ** 
  #InternetServiceFiber optic           2.12e-09 ***
  #InternetServiceNo                    2.51e-11 ***
  #OnlineSecurityYes                    0.014527 *  
  #TechSupportYes                       0.005828 ** 
  #StreamingTVYes                       2.26e-06 ***
  #StreamingMoviesYes                   5.41e-05 ***
  #ContractOne year                     9.01e-08 ***
  #ContractTwo year                     1.14e-12 ***
  #PaperlessBillingYes                  0.000209 ***
  #PaymentMethodElectronic check        0.032552 * 
  #MonthlyCharges                       0.000541 ***
  #binned12-24 Months                   7.78e-12 ***
  #binned24-36 Months                    < 2e-16 ***
  #binned36-48 Months                   3.01e-13 ***
  #binned48-60 Months                    < 2e-16 ***
  #binned60-72 Months                   2.32e-15 ***

#train the model 
mod2 = train(Churn ~  + MultipleLines + InternetService + 
               OnlineSecurity + TechSupport + StreamingTV + StreamingMovies + 
               Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + 
               binned, data = train,
               trControl = train_control,
               method = "glm",
               family=binomial())


summary(mod2)
mod2$results
#79% accuarcy

mod2b = train(Churn ~ MultipleLines + InternetService + 
               TechSupport + StreamingTV + StreamingMovies + 
               Contract + PaperlessBilling + MonthlyCharges + 
               binned, data = train,
             trControl = train_control,
             method = "glm",
             family=binomial())


summary(mod2b)
mod2b$results
#79% accuarcy

#auc train
pred = predict(mod2, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.84

#final test
pred2 = predict(mod2, data_test, type="prob")[,2]
pred2

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred2 = as.data.frame(pred2)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin2 = cbind(cus, pred2)
fin2

colnames(fin2)
names(fin2)[1] = "Customer Number"
names(fin2)[2] = "Churn"
#View(fin2)


#export to a csv
write.csv(fin2,'log_regr_2_kmg.csv', row.names = FALSE)
#.84


#lasso
#for use in lasso
x = model.matrix(Churn~.,data)[,-1] #sets up x with all vars except for target survived
head(x)
colnames(x) #no churn

y = data$Churn #sets up y with target var only


train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.out_lasso = cv.glmnet(x[train,],y[train],alpha=1, family="binomial")
#alpha 1 is lasso
cv.out_lasso
#model output tells you min lambda using mse (mean squared error)
summary(cv.out_lasso)
plot(cv.out_lasso)
#the best model is under lambda = lambda.min
bestlam2 = cv.out_lasso$lambda.min
bestlam2
#0.0005 best lambda for lasso
out2 = glmnet(x,y, alpha=1, family = "binomial")
plot(out2, xvar = "lambda", label = TRUE)
lasso.coef = predict(out2,type="coefficients",s=bestlam2)[1:20,] #24 is how many columns
lasso.coef
#take out 0 coefficients for lasso, 0 vars did not explain anything in model. weight/coef doesn't matter much the smaller the number
lasso.coef[lasso.coef!=0]
#selected variables with coefficients not 0:
#genderMale 
#-0.003662635 
#SeniorCitizenYes 
#0.223715613 
#PartnerYes 
#-0.042506439 
#DependentsYes 
#-0.113007263 
#tenure 
#-0.050274055 
#PhoneServiceYes 
#-0.562246231 
#MultipleLinesYes 
#0.241867166 
#InternetServiceFiber optic 
#0.761239503 
#InternetServiceNo 
#-0.826784151 
#OnlineSecurityYes 
#-0.469136794 
#OnlineBackupYes 
#-0.202553072 
#DeviceProtectionYes 
#-0.050192477 
#TechSupportYes 
#-0.425985192 
#StreamingTVYes 
#0.236641865 
#StreamingMoviesYes 
#0.182401628 
#ContractOne year 
#-0.640221657 
#ContractTwo year 
#-1.053880762 
#PaperlessBillingYes 
#0.301972081 
#PaymentMethodCredit card (automatic) 
#-0.138336952 


#fwd and bkwd variable selection 
#forward and Backward Stepwise Selection
library(leaps)
regfit.fwd=regsubsets(Churn~.,data=data,nvmax=20,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Churn~.,data=data,nvmax=20,method="backward")
summary(regfit.bwd)

coef(regfit.fwd,15)

coef(regfit.bwd,15)

#choosing vars
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(data),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Churn~.,data=data[train,],nvmax=20)
test.mat=model.matrix(Churn~.,data=data[test,])
val.errors=rep(NA,20)
for(i in 1:20){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data$Churn[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,15)
#SeniorCitizenYes 
#0.051072957 
#tenure                         MultipleLinesYes 
#-0.002213441                   0.044111938 
#InternetServiceFiber optic     InternetServiceNo 
#0.152653487                    -0.191460442 
#OnlineSecurityYes              OnlineBackupYes 
#-0.081892949                   -0.037028418 
#TechSupportYes                 StreamingTVYes 
#-0.053942001                   0.085300255 
#StreamingMoviesYes             ContractOne year 
#0.040088258                    -0.107636799 
#ContractTwo year               PaperlessBillingYes 
#-0.060683559                   0.028349936 
#PaymentMethodElectronic check  TotalCharges 
#0.093990170                    -0.099862692 



predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Churn~.,data=data,nvmax=20)
coef(regfit.best,10)
#PhoneServiceYes 
#-0.13818976 
#OnlineSecurityYes                OnlineBackupYes 
#-0.09263979                      -0.05888742 
#TechSupportYes                   ContractOne year 
#-0.09754852                      -0.12969314 
#ContractTwo year                 PaperlessBillingYes 
#-0.09638347                      0.04306435 
#PaymentMethodElectronic check    MonthlyCharges 
#0.08033032                       0.00684376 
#TotalCharges 
#-0.14897207 



#models using variables selected/no binned tenure/standardized totalcharges/got rid of monthlycharges column
#final vars in proj 1
#tenure, Contract.Two.year, InternetService.No, InternetService.Fiber.optic
#TotalCharges, Contract.One.year, PhoneService.Yes, TechSupport.Yes, OnlineSecurity.Yes
#PaperlessBilling.Yes, PaymentMethod.Electronic.check, StreamingTV.Yes, SeniorCitizen.Yes
#OnlineBackup.Yes and MultipleLines.Yes



set.seed(123)
View(data)
sel_vars = data[c(2,5,7:8,12:16,19)] #19 is churn
train_split = sample(1:nrow(sel_vars), nrow(sel_vars)*0.8) #take 80% sample
train = sel_vars[train_split, ]
#dim(train)
test_without_y = subset(sel_vars[-train_split, ], select= -Churn)
test_with_y = sel_vars[-train_split, ]$Churn


train_control = trainControl(method = "cv", number = 5)

sel_vars_mod = train(Churn ~ .,
                   data = train,
                   trControl = train_control,
                   method = "glm",
                   family=binomial(link="logit"))
summary(sel_vars_mod)
sel_vars_mod$results
#accuracy .79

#auc train
pred = predict(sel_vars_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.83
#not submitted




##############   one hot encoded models   ####################################################
#one-hot encode categoricals to make numerical for machine learning
View(data)
dummies = dummyVars(~ ., data = data[, -19], fullRank = TRUE)#19 is churn
one_hot = data.frame(predict(dummies, newdata = data))
one_hot$Churn = data$Churn
View(one_hot)
#one_hot$Churn.Yes = NULL
#103 columns with concatenated columns added

dummies_test = dummyVars(~ ., data = data_test, fullRank = TRUE)
one_hot_test = data.frame(predict(dummies_test, newdata = data_test))
View(one_hot_test)

#split train data into nested holdout 
sel_one_hot = one_hot[c(2,5,6:11,13:14,16:18,20,22:23)] #23 is churn
sel_one_hot_test = one_hot_test[c(2,5,6:11,13:14,16:18,20,22:22)] 

str(one_hot)
summary(one_hot)

train_split = sample(1:nrow(lass), nrow(one_hot)*0.8) #take 80% sample
train = one_hot[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn

#down sample
#https://www.rdocumentation.org/packages/caret/versions/6.0-85/topics/downSample
samp = downSample(one_hot, one_hot$Churn, list = FALSE, yname = "Churn")
class(samp)
#merge(one_hot, samp, all=TRUE)
#https://datascienceplus.com/combining-data-in-r-the-skill-of-merging-joining-and-stacking/
library(Stack)
stk = Stack(one_hot, samp)
stk
#8870 obs


one_hot[c(1)] = list(NULL)
View(one_hot)

one_hot_test[c(15)] = list(NULL)
View(one_hot_test)

train_control = trainControl(method = "cv", number = 5)
one_mod = train(Churn ~ .,
                     data = one_hot,
                     trControl = train_control,
                     method = "glm",
                     family=binomial)
summary(one_mod)
one_mod$results


mod = glm(Churn ~ ., data=one_hot, family=binomial)
summary(mod)

#AIC
step = stepAIC(mod, trace=FALSE)
step$anova



#lasso one_hot
#for use in lasso
x = model.matrix(Churn~.,one_hot)[,-1] #sets up x with all vars except for target survived
head(x)
colnames(x) #no churn

y = one_hot$Churn #sets up y with target var only


train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.out_lasso = cv.glmnet(x[train,],y[train],alpha=1, family="binomial")
#alpha 1 is lasso
cv.out_lasso
#model output tells you min lambda using mse (mean squared error)
summary(cv.out_lasso)
plot(cv.out_lasso)
#the best model is under lambda = lambda.min
bestlam2 = cv.out_lasso$lambda.min
bestlam2
#0.0005 best lambda for lasso
out2 = glmnet(x,y, alpha=1, family = "binomial")
plot(out2, xvar = "lambda", label = TRUE)
lasso.coef = predict(out2,type="coefficients",s=bestlam2)[1:89,] #89 is how many columns
lasso.coef
#take out 0 coefficients for lasso, 0 vars did not explain anything in model. weight/coef doesn't matter much the smaller the number
lasso.coef[lasso.coef!=0]
#"StreamingTV.Yes ", "Contract.One.year", "Contract.Two.year", "PaperlessBilling.Yes",
#"PaymentMethod.Electronic.check", "TotalCharges", "binned.24.36.Months",
#"bin12_phonservno.0.12.Months.Yes", "bin12_phonservno.24.36.Months.No",
#"internetserv_onlinesec.DSL.Yes", 
#"internetserv_onlinesec.Fiber.optic.No", "internetserv_onlinesec.No.No",
#"internetserv_onlinebac.DSL.Yes",
#"internetserv_onlinebac.Fiber.optic.No", 
#"internetserv_onlinebac.No.No", 
#"internetserv_streamov.Fiber.optic.Yes",
#"internetserv_streamov.No.No", 
#"internetserv_techsupp.DSL.Yes",
#"internetserv_techsupp.Fiber.optic.No", 
#"internetserv_techsupp.No.No",
#"sencityes_phonservno.No.Yes", 
#"sencityes_phonservno.Yes.No",
#"part_sencitz.No.Yes", 
#"part_deps_multpline.Yes.Yes.No",
#"techsupp_pymtmailcheck.No.Credit.card..automatic.", 
#"techsupp_pymtmailcheck.No.Electronic.check",
#"techsupp_pymtmailcheck.Yes.Bank.transfer..automatic.", 
#"dep_multpli_intserv.No.No.Fiber.optic", 
#"dep_multpli_intserv.No.Yes.Fiber.optic", 
#"dep_multpli_intserv.Yes.No.DSL", 
#"dep_multpli_intserv.Yes.No.Fiber.optic", 
#"dep_multpli_intserv.Yes.No.No",
#"dep_multpli_intserv.Yes.Yes.Fiber.optic", 
#"onlinsec_intserv.No.Fiber.optic", 
#"paplsbilling_sencit_phnservno.No.No.Yes", 
#"paplsbilling_sencit_phnservno.No.Yes.No", 
#"paplsbilling_sencit_phnservno.Yes.No.No")

#one_hot_mod results
#accuracy .80
#MultipleLines.Yes               0.232219   0.103619   2.241  0.02502 * 
#OnlineBackup.Yes               -0.223281   0.100532  -2.221  0.02635 * 


#auc train
pred = predict(one_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.85

#final test
pred25 = predict(one_mod, one_hot_test, type="prob")[,2]


#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred25 = as.data.frame(pred25)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin25 = cbind(cus, pred25)
fin25

colnames(fin25)
names(fin25)[1] = "Customer Number"
names(fin25)[2] = "Churn"
#View(fin2)


#export to a csv
write.csv(fin25,'log_regr_25B_conc_one_hot_kmg.csv', row.names = FALSE)
#.84

#use anova selected vars
View(one_hot)
anov_one_hot = one_hot[c(2,5,7:8,10:11,13:14,16,18:19,23:24)] #24 is churn
train_split = sample(1:nrow(anov_one_hot), nrow(anov_one_hot)*0.8) #take 80% sample
train = anov_one_hot[train_split, ]
#dim(train)
test_without_y = subset(anov_one_hot[-train_split, ], select= -Churn)
test_with_y = anov_one_hot[-train_split, ]$Churn

anov_one_hot_test = one_hot_test[c(2,5,7:8,10:11,13:14,16,18:19,23)] 

train_control = trainControl(method = "cv", number = 5)

anov_one_hot_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(anov_one_hot_mod)
anov_one_hot_mod$results
#accuracy .79


#auc train
pred = predict(anov_one_hot_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.84

#final test
pred12 = predict(anov_one_hot_mod, anov_one_hot_test, type="prob")[,2]
pred12

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred12 = as.data.frame(pred12)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin12 = cbind(cus, pred12)
#fin12

#colnames(fin12)
names(fin12)[1] = "Customer Number"
names(fin12)[2] = "Churn"
#View(fin12)


#export to a csv
write.csv(fin12,'log_regr_12_anov_one_hot_no_binned_kmg.csv', row.names = FALSE)
#.82




#use all one_hot vars
train_split = sample(1:nrow(one_hot), nrow(one_hot)*0.8) #take 80% sample
train = one_hot[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn

train_control = trainControl(method = "cv", number = 5)

one_hot_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(one_hot_mod)
one_hot_mod$results
#accuracy .80
#MultipleLines.Yes               0.232219   0.103619   2.241  0.02502 * 
#OnlineBackup.Yes               -0.223281   0.100532  -2.221  0.02635 * 


#auc train
pred = predict(one_hot_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.82


#sel_one_hot_mod minus 10 (online backup)
sel_one_hot = one_hot[c(2,5,6:9,11,13:14,16:18,20,22:23)] #23 is churn
train_split = sample(1:nrow(sel_one_hot), nrow(sel_one_hot)*0.8) #take 80% sample
train = sel_one_hot[train_split, ]
#dim(train)
test_without_y = subset(sel_one_hot[-train_split, ], select= -Churn)
test_with_y = sel_one_hot[-train_split, ]$Churn

sel_one_hot_test = one_hot_test[c(2,5,6:9,11,13:14,16:18,20,22:22)] 

train_control = trainControl(method = "cv", number = 5)
sel_one_hot_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(sel_one_hot_mod)
sel_one_hot_mod$results
#accuracy .80


#auc train
pred = predict(sel_one_hot_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#.84

#final test
pred6 = predict(sel_one_hot_mod, sel_one_hot_test, type="prob")[,2]
pred6

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred6 = as.data.frame(pred6)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin6 = cbind(cus, pred6)
fin6

colnames(fin6)
names(fin6)[1] = "Customer Number"
names(fin6)[2] = "Churn"
View(fin6)


#export to a csv
write.csv(fin6,'log_regr_6_sel_one_hot_no_10backup_kmg.csv', row.names = FALSE)
#.84

#one_hot_ecoded and tenure_binned
train_split = sample(1:nrow(one_hot), nrow(one_hot)*0.8) #take 80% sample
train = one_hot[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn


train_control = trainControl(method = "cv", number = 5)
one_hot_bin_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(one_hot_bin_mod)
one_hot_bin_mod$results
#accuracy .80


#auc train
pred = predict(one_hot_bin_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#.81

#significant vars one hot binned
train_split = sample(1:nrow(one_hot), nrow(one_hot)*0.8) #take 80% sample
train = one_hot[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn


train_control = trainControl(method = "cv", number = 5)
one_hot_bin_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(one_hot_bin_mod)
one_hot_bin_mod$results
#accuracy .80


#auc train
pred = predict(one_hot_bin_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#.81


#significant vars one hot binned
View(one_hot)
sel_one_hot_bin = one_hot[c(2,5:10,12:17,19,22:27)] #27 is churn
#sel_one_hot_bin = one_hot[c(2,5:9,12:13,15:17,22:25,27)] #27 is churn
train_split = sample(1:nrow(sel_one_hot_bin), nrow(sel_one_hot_bin)*0.8) #take 80% sample
train = sel_one_hot_bin[train_split, ]
#dim(train)
test_without_y = subset(sel_one_hot_bin[-train_split, ], select= -Churn)
test_with_y = sel_one_hot_bin[-train_split, ]$Churn

#View(one_hot_test)
sel_one_hot_bin_test = one_hot_test[c(2,5:10,12:17,19,22:26)]

train_control = trainControl(method = "cv", number = 5)
sel_one_hot_bin_mod = train(Churn ~ .,
                        data = sel_one_hot_bin,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(sel_one_hot_bin_mod)
sel_one_hot_bin_mod$results
#accuracy .80


#auc train
pred = predict(sel_one_hot_bin_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#.84


#final test
pred8 = predict(sel_one_hot_bin_mod, sel_one_hot_bin_test, type="prob")[,2]
#pred8

#get probs and ids to a dataframe
#https://www.datacamp.com/community/tutorials/merging-datasets-r
pred8 = as.data.frame(pred8)

cust_num = data_cust_num$Customer.Number
cust_num = as.character(cust_num)
#str(cust_num)
cus = as.data.frame(cust_num)
#str(cus)
#View(cus)

fin8 = cbind(cus, pred8)
fin8

colnames(fin8)
names(fin8)[1] = "Customer Number"
names(fin8)[2] = "Churn"
View(fin8)


#export to a csv
write.csv(fin8,'log_regr_8_sel_one_hot_sigvars_bin_kmg.csv', row.names = FALSE)
#.83


#random one hot model #10
ran_one_hot = one_hot[c(7,19,21:28)] #28 is churn
train_split = sample(1:nrow(ran_one_hot), nrow(ran_one_hot)*0.8) #take 80% sample
train = ran_one_hot[train_split, ]
#dim(train)
test_without_y = subset(ran_one_hot[-train_split, ], select= -Churn)
test_with_y = ran_one_hot[-train_split, ]$Churn

ran_one_hot_test = one_hot_test[c(7,19,21:27)] 

train_control = trainControl(method = "cv", number = 5)
ran_one_hot_mod = train(Churn ~ .,
                        data = train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial(link="logit"))
summary(ran_one_hot_mod)
ran_one_hot_mod$results
#accuracy .79


#auc train
pred = predict(ran_one_hot_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.80


#one_hot concatenated cols one_hot, no motnhly charges, tenure binned, totalcharges log
set.seed(123)
train_split = sample(1:nrow(one_hot), nrow(one_hot)*0.8) #take 80% sample
train = one_hot_red[train_split, ]
#dim(train)
test_without_y = subset(one_hot[-train_split, ], select= -Churn)
test_with_y = one_hot[-train_split, ]$Churn


train_control = trainControl(method = "cv", number = 5)



logist_mod = train(Churn ~ .,
                   data = train,
                   method = "glm",
                   family=binomial(link="logit"))
summary(logist_mod)
logist_mod$results
#accuracy .80

#auc train
pred = predict(logist_mod, test_without_y, type="prob")[,2]
#pred
out = prediction(pred, test_with_y)
auc = performance(out, measure="auc")@y.values[[1]]
auc
#0.847




#https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4
#https://medium.com/@rohitlal/customer-churn-prediction-model-using-logistic-regression-490525a78074
#cm using glm
test$Churn = as.character(test$Churn)
test$Churn[test$Churn=="No"] <- "0"
test$Churn[test$Churn=="Yes"] <- "1"
fitted.results = predict(glmLogModel,newdata=test,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)
misClasificError = mean(fitted.results != test$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(test$Churn, fitted.results > 0.5)


#cm for caret
pred_rf_new = predict(rfModel_new, test)
caret::confusionMatrix(pred_rf_new, test$Churn)
