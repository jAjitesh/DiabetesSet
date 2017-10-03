
#Imputation of missing values
#Pima Indians Dataset - Insulin has many values with 0
#so does SkinThickness
#assume 0 means missing values
#
#assuming random
#using mice to handle the missing values
#
#import dataset
library(mice)

#data <-read.csv(file="diabetes.csv",stringsAsFactors=FALSE,header=TRUE)
data <-read.csv(file="C:\\Users\\Anjani Reddy\\Google Drive\\KDD\\KDD_Project\\diabetes.csv",stringsAsFactors=FALSE,header=TRUE)

#replace missing values in cols 4 and 5 with NA
data[, 4:5][data[, 4:5] == 0] <- NA


View(data)

#check for % of missing data using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}

#shows columns and percent missing
apply(data,2,pMiss)

#row wise
apply(data,1,pMiss)




#skinthickness is 29.6% and insulin is 48.7%
md.pattern(data)

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#In a box plot red shows distribution of pregnancies with insulin missing
marginplot(data[c(1,5)])
#In this box plot red shows distribution of pregnancies with skin thickness missing

marginplot(data[c(1,4)])

#In this box plot red shows distribution of Insulin with skin thickness missing
marginplot(data[c(4,5)])

#m is number of multiple imputations default is 5, maxit is number of iterations
#with default equal to 5, seed is for offsetting the random number generator
tempData <- mice(data,m=5,maxit=5,meth='norm.predict',seed=500)
summary(tempData)

#It will check the imputed data
tempData$imp$Insulin
tempData$imp$SkinThickness

#now combining data back together
diabetes <- complete(tempData,1)
View(diabetes)

#frequency of a value in each column
ldply(diabetes, function(c)
  sum(c=="0"))

# Recreate the missing value table
diabetes[, 2][diabetes[, 2] == 0] <- NA #Glucose
diabetes[, 3][diabetes[, 3] == 0] <- NA #BP
diabetes[, 6][diabetes[, 6] == 0] <- NA #BMI

# Replace missing values in Glucose with mean
diabetes[, 2][is.na(diabetes[, 2])] <-
  mean(na.omit(diabetes$Glucose))


# Replace missing values in BP with mode
BP_mode_table <- table(diabetes$BloodPressure)
BP_mode <- names(BP_mode_table)[BP_mode_table ==
                               max(BP_mode_table)]

diabetes[, 3][is.na(diabetes[, 3])] <- BP_mode


#replace each of the missing BMI values with random BMI values
diabetes$BMI[is.na(diabetes$BMI)] <- sample(diabetes$BMI, size=sum(is.na(diabetes$BMI)), replace=F)


library(lattice)
#magenta is imputed
#blue is observed
densityplot(tempData)

#stripplot shows one dimensional scatter plot for individual data points
stripplot(tempData, pch = 20, cex = 1.2)


#Outliers detection section
library(plyr)



count(boxplot.stats(diabetes$BloodPressure)$out)
#Total number of Outliers for BloodPressure :14

count(boxplot.stats(diabetes$Pregnancies)$out)
#Total number of Outliers for Pregnancies 4

count(boxplot.stats(diabetes$Glucose)$out)
#Total number of Outliers for Glucose: 0

count(boxplot.stats(diabetes$SkinThickness)$out)
#Total number of Outliers for SkinThickness 4

count(boxplot.stats(diabetes$Insulin)$out)
#Total number of Outliers for Insulin 24

count(boxplot.stats(diabetes$BMI)$out)
#Total number of Outliers for BMI 8

count(boxplot.stats(diabetes$DiabetesPedigreeFunction)$out)
#Total number of Outliers for DiabetesPedigreeFunction 28

count(boxplot.stats(diabetes$Age)$out)
#Total number of Outliers for Age 9


#univariate analysis


#preliminary analysis
str(diabetes)
diabetes$Outcome <- as.factor(diabetes$Outcome)
str(diabetes)
summary(diabetes$Outcome)
summary(diabetes$Pregnancies) #Dividing based on Median value 
PregnanciesLessThan3 <- subset(diabetes, Pregnancies <= 3)
PregnanciesGreaterThan3 <- subset(diabetes, Pregnancies > 6)
str(PregnanciesLessThan3)
str(PregnanciesGreaterThan6)
by(diabetes$Outcome,diabetes$Pregnancies,summary)
diabetes$BloodPressure<-as.numeric(diabetes$BloodPressure)
library(ggplot2)
qplot(data= diabetes, x= Pregnancies)
qplot(data=PregnanciesLessThan3, x= Outcome)
qplot(data=PregnanciesGreaterThan6, x = Outcome)  
qplot(data=diabetes, x = diabetes$Glucose,fill = diabetes$Outcome ) 

barplot(table( factor(diabetes$Outcome),diabetes$Glucose))

qplot(data=diabetes, x = diabetes$Glucose, bins = 5,fill = factor(diabetes$Outcome) )

qplot(data=diabetes, x = diabetes$BMI, bins = 5,fill = factor(diabetes$Outcome) )

diabetes$BloodPressure<-as.numeric(diabetes$BloodPressure)
qplot(data=diabetes, x = diabetes$BloodPressure, fill = factor(diabetes$Outcome) )

barplot(table( factor(diabetes$Outcome),diabetes$Glucose))




#  women are more likely to get diabetes with no of pregnancies > 6


summary(diabetes$Glucose)
qplot(data = diabetes, x = Glucose) #Has outlier values (below 50) - 6 values
GlucoseGreaterThan50 <- subset(diabetes, Glucose > 50)
str(GlucoseGreaterThan50)
qplot(data = GlucoseGreaterThan50, x = Glucose)        
GlucoseBetween75and150 <- subset(diabetes, Glucose >75 & Glucose<=150)
str(GlucoseBetween75and150) # around 600 members in this range
summary(GlucoseBetween75and150$Outcome)
prop.table(table(GlucoseBetween75and150$Outcome))*100
GlucoseGreaterThan150 <- subset(diabetes,  Glucose>150)
summary(GlucoseGreaterThan150$Outcome)
prop.table(table(GlucoseGreaterThan150$Outcome))*100 
#women with glucose levels > 150 have 75% chance of getting diabetes


summary(diabetes$BloodPressure)
qplot(data = diabetes, x = BloodPressure)  
boxplot(diabetes$BloodPressure) #Has outliers below 40 and above 110
BPbetween40To100 <- subset(diabetes, BloodPressure > 40 & BloodPressure <= 100)
qplot(data = BPbetween40To100, x = BloodPressure) 
summary(BPbetween40To100$Outcome)
prop.table(table(BPbetween40To100$Outcome))*100
BPgreaterThan80 <- subset(diabetes, BloodPressure > 80)
qplot(data = BPgreaterThan80 , x = BloodPressure) 
summary(BPgreaterThan80$Outcome)
prop.table(table(BPgreaterThan80$Outcome))*100
#Women with BP > 80 are 47% more likely to get diabetes
# MOre exploration has to be done

summary(diabetes$SkinThickness)
qplot(data = diabetes, x = SkinThickness)  
SkinThicknessLessThan25 <- subset(diabetes, SkinThickness <= 25)
summary(SkinThicknessLessThan25$Outcome)
prop.table(table(SkinThicknessLessThan25$Outcome))*100
SkinThicknessLessThan50 <- subset(diabetes, SkinThickness >= 25 & SkinThickness<=50 )
summary(SkinThicknessLessThan50$Outcome)
prop.table(table(SkinThicknessLessThan50$Outcome))*100
# Women with skinthickness greater than 25, a women is 43.5% likely to get diabetic. 



qplot(data= diabetes, x= Age)
qplot(data= diabetes, x= diabetes$Glucose,fill = diabetes$Outcome)
AgeLessthan30 <- subset(diabetes, Age <= 30 &(Glucose >=100 & Glucose <=150))
summary(AgeLessthan30$Outcome)
prop.table(table(AgeLessthan30$Outcome)) *100
AgeGreaterthan30 <- subset(diabetes, Age > 30 &(Glucose >=100 & Glucose <=150))
summary(AgeGreaterthan30$Outcome)
prop.table(table(AgeGreaterthan30$Outcome)) *100
#WOMEN WITH AGE GREATER THAN 30 YRS AND ABOVE CONDITIONS HAS APPROXIMATELY 50% CHANCE TO GET DIABETES

by(GlucoseGreaterThan150$Outcome,GlucoseGreaterThan150$Pregnancies, summary)
diabetesWith1 <- subset(diabetes, Outcome = 1)
qplot(data = diabetesWith1, x= Pregnancies, y= BloodPressure)

numericData <- subset(diabetes, select = -Outcome)
numericData <- subset(numericData, select = -BloodPressure)
str(numericData)
cor(numericData) #None of them are correlated variables 

ggplot(diabetes, aes(x= Pregnancies, y=Glucose, color=Outcome)) + geom_point()
# 1. If glucose levels is greater than 150 then the number of pregnancies wont matter. You are always prone to diabetes.
# 2. Even though a women has greater 5 pregnancies, she can still not be diabetic if her glucose levels is less than 100

ggplot(diabetes, aes(x= BloodPressure, y= Glucose , color= Outcome)) + geom_point()
# It is evident that for high BP and High glucose, chances of getting diabtetes is also high
ggplot(diabetes, aes(x= BloodPressure, y= Glucose , color= Outcome)) + geom_point()+
scale_x_continuous(limits = c(40, 100))+
  scale_y_continuous(limits = c(100, 150))

#This portion seems to be equally scattered. Analysing further,

GlucoseBetween100and150 <- subset(diabetes, Glucose >=100 & Glucose <=150)
GlucoseBP <- subset(diabetes, (Glucose >=100 & Glucose <=150) & (BloodPressure > 75 & BloodPressure <= 100))
summary(GlucoseBP$Outcome)
prop.table(table(GlucoseBP$Outcome))*100
# 1.In the above range a women is 40% more likely to get diabetes, which is comparatively high even with medium levels of glucose


ggplot(diabetes, aes(x=factor(Pregnancies) , y= Glucose)) + geom_boxplot(aes(fill = factor(Outcome)))
# For any number of pregnancies, Higher Glucose indicates higher chances of diabetes


ggplot(diabetes, aes(x=factor(Pregnancies) , y= BMI)) + geom_boxplot(aes(fill = factor(Outcome)))
# For any number of pregnancies, Higher BMI indicates higher chances of diabetes

ggplot(diabetes, aes(x=diabetes$Glucose , y= diabetes$Insulin)) + geom_point()

ggplot(diabetes, aes(x=diabetes$BMI , y= diabetes$Insulin)) + geom_point()

ggplot(diabetes, aes(x=diabetes$Glucose , y= diabetes$Outcome)) + geom_point()


ggplot(diabetes, aes(x=AgeBin , y= BloodPressure)) + geom_boxplot(aes(fill = factor(Outcome)))
#Women of age group 60-80 and high BP are highly prone to diabetes


ggplot(diabetes, aes(x=diabetes$Glucose)) + geom_boxplot(aes(fill = factor(Outcome)))


#-----------------------------------------  Data preparation Phase -------------------------------------------------------------------

#Z-score standardization
diabetes$Glucose <- (diabetes$Glucose - mean(diabetes$Glucose))/sd(diabetes$Glucose)
diabetes$BloodPressure <- (diabetes$BloodPressure - mean(diabetes$BloodPressure))/sd(diabetes$BloodPressure)
diabetes$SkinThickness <- (diabetes$SkinThickness - mean(diabetes$SkinThickness))/sd(diabetes$SkinThickness)
diabetes$Insulin <- (diabetes$Insulin - mean(diabetes$Insulin))/sd(diabetes$Insulin)
diabetes$BMI <- (diabetes$BMI - mean(diabetes$BMI))/sd(diabetes$BMI)
diabetes$DiabetesPedigreeFunction <- (diabetes$DiabetesPedigreeFunction - mean(diabetes$DiabetesPedigreeFunction))/sd(diabetes$DiabetesPedigreeFunction)

##rounding off the attributes to 2 digits
diabetes$Glucose
str(diabetes)
diabetes[,2:7] <- as.data.frame(sapply(diabetes[,2:7], as.numeric)) #<- sapply is here
str(diabetes)


#Outlier detection after standardization
Glucose_outliers <- subset(diabetes, diabetes$Glucose > 3 & diabetes$Glucose < -3)
nrow(Glucose_outliers)
#Number of Outliers after standardization : 0
BloodPressure_outliers <- subset(diabetes, diabetes$BloodPressure > 3 & diabetes$BloodPressure < -3)
nrow(BloodPressure_outliers)
#Number of Outliers after standardization : 0
SkinThickness_outliers <- subset(diabetes, diabetes$SkinThickness > 3 & diabetes$SkinThickness < -3)
nrow(SkinThickness_outliers)
#Number of Outliers after standardization : 0
Insulin_outliers <- subset(diabetes, diabetes$Insulin > 3 & diabetes$GlInsulinucose < -3)
nrow(Insulin_outliers)
#Number of Outliers after standardization : 0
BMI_outliers <- subset(diabetes, diabetes$BMI > 3 & diabetes$BMI < -3)
nrow(BMI_outliers)
#Number of Outliers after standardization : 0
DiabetesPedigreeFunction_outliers <- subset(diabetes, diabetes$DiabetesPedigreeFunction > 3 & diabetes$DiabetesPedigreeFunction < -3)
nrow(DiabetesPedigreeFunction_outliers)
#Number of Outliers after standardization : 0


str(diabetes)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
#

diabetes_n<-diabetes
diabetes_n[1:8] <- as.data.frame(lapply(diabetes[1:8], normalize))
diabetes_n

library(caTools)
set.seed(132)
diabetes$index<-sample.split(diabetes$Outcome, SplitRatio=0.7)
training=subset(diabetes,diabetes$index==TRUE)
testing=subset(diabetes, diabetes$index==FALSE)

#remove index from all three
diabetes<-diabetes[,-10]
training<-training[,-10]
testing<-testing[,-10]

#creating training labels and
#test labels
str(diabetes)
diabetes_train_labels <- training[, 9]
diabetes_test_labels <- testing[, 9]


#remove target variable from testing and training dataset: Outcome (9th column)
training<-training[,-9]
testing<-testing[,-9]





# One sample Proportion Test and Confidence Interval
# diabetes$Outcome <- as.factor(diabetes$Outcome)
# # Show possible levels of Churn variable
# levels(diabetes$Outcome)
# # Find how many women are diabetic
# num.diabetic <- sum(diabetes_train_labels == 1)
# # Find sample size, Calculate p, Z_data
# sample.size <- dim(training)[1]
# p <- num.diabetic/sample.size
# Z_data <- (p - 0.15) / sqrt((0.15*(1-0.15))/sample.size)
# # Find confidence interval, p-value of Z_data
# error <- qnorm(0.975, mean = 0, sd = 1)*
#   sqrt((p*(1-p))/sample.size)
# lower.bound <- p - error
# upper.bound <- p + error
# p.value <- 2*pnorm(Z_data, mean = 0, sd = 1)
# Z_data; p.value
# lower.bound; upper.bound




# Two-Sample Z-Test for Difference in Proportions

x1 <- sum(diabetes_train_labels == 1)
x2 <- sum(diabetes_test_labels == 1)
n1 <- dim(training)[1]
n2 <- dim(testing)[1]
p1 <- x1 / n1
p2 <- x2 / n2
ppooled <- (x1+x2) / (n1+n2)
# Calculate test statistic
zdata <- (p1-p2) / sqrt(ppooled*(1-ppooled)*((1/n1)+(1/n2)))
# Find the p-value
pvalue <- 2*pnorm(abs(zdata), lower.tail = FALSE)

pvalue  # 0.9656672

#There is no evidence that the proportion of outcomes differs between the training and test data sets. 
#For this variable, the partition is valid




#Check validilty of partition for Pregnancies:

# testing_age<-table(testing$BMI)
# training_age<-table(training$BMI)
# 
# table_age <- as.table(rbind(training_age,
#                                  testing_age))
# 
# Xsq_data <- chisq.test(table_age)
# # Show the test statistic,
# # p-value, expected frequencies
# Xsq_data$statistic  #1.013271
# Xsq_data$p.value #0.3141205
# Xsq_data$expected
# 
# #as per the chi square test , training data set represents actual dataset'


#-----------------------------------------  Data Modelling Phase -------------------------------------------------------------------

#Logistic reg model


m1=glm(diabetes$Outcome~., data=diabetes, family='binomial')
summary(m1)
# Call:
#   glm(formula = diabetes$Outcome ~ ., family = "binomial", data = diabetes)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6357  -0.7218  -0.3928   0.7060   2.3867  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -1.838434   0.309666  -5.937 2.91e-09 ***
#   Pregnancies               0.118195   0.032515   3.635 0.000278 ***
#   Glucose                   1.194970   0.137861   8.668  < 2e-16 ***
#   BloodPressure            -0.127143   0.103920  -1.223 0.221153    
# SkinThickness             0.069011   0.127119   0.543 0.587208    
# Insulin                  -0.132144   0.122506  -1.079 0.280733    
# BMI                       0.645951   0.133465   4.840 1.30e-06 ***
#   DiabetesPedigreeFunction  0.296211   0.099021   2.991 0.002777 ** 
#   Age                       0.015521   0.009669   1.605 0.108442    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 993.48  on 767  degrees of freedom
# Residual deviance: 709.95  on 759  degrees of freedom
# AIC: 727.95
# 
# Number of Fisher Scoring iterations: 5


#From the above output it is evident that the significant variables are  Pregnancies,Glucose,BMI and DiabetesPedigreeFunction



#Decision Trees

#Binning for Pregnancy attribute
diabetes$PregnancyBin<-cut(diabetes$Pregnancies, c(0,5,10,15,20), include.lowest=TRUE)
diabetes$AgeBin<-cut(diabetes$Age, c(0,20,40,60,80,100), include.lowest=TRUE)

library("rpart"); 
library("rpart.plot"); 
library("C50")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose + AgeBin  + diabetes$BloodPressure +
                   diabetes$BMI ,
                 data = diabetes,
                 method = "class")

my_prediction <- predict(diabfit, diabetes , type = "class")

my_solution <- data.frame(Outcome = diabetes$Outcome, Diabetic =  my_prediction)

fancyRpartPlot(diabfit)

# n= 768 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510416667 0.3489583333)  
# 2) diabetes$Glucose< 0.1909990468 485  94 0 (0.8061855670 0.1938144330) *
#   3) diabetes$Glucose>=0.1909990468 283 109 1 (0.3851590106 0.6148409894)  
# 6) diabetes$BMI< -0.3604221538 76  24 0 (0.6842105263 0.3157894737)  
# 12) diabetes$Glucose< 0.7824049556 41   6 0 (0.8536585366 0.1463414634) *
#   13) diabetes$Glucose>=0.7824049556 35  17 1 (0.4857142857 0.5142857143)  
# 26) AgeBin=(20,40],(60,80] 25   9 0 (0.6400000000 0.3600000000) *
#   27) AgeBin=(40,60] 10   1 1 (0.1000000000 0.9000000000) *
#   7) diabetes$BMI>=-0.3604221538 207  57 1 (0.2753623188 0.7246376812) *



diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose+diabetes$BloodPressure+diabetes$BMI+diabetes$SkinThickness,
                 data = diabetes,
                 method = "class")

rpart.plot(diabfit)
print(diabfit)
fancyRpartPlot(diabfit)

# n= 768 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510416667 0.3489583333)  
# 2) diabetes$Glucose< 0.1909990468 485  94 0 (0.8061855670 0.1938144330) *
#   3) diabetes$Glucose>=0.1909990468 283 109 1 (0.3851590106 0.6148409894)  
# 6) diabetes$BMI< -0.3604221538 76  24 0 (0.6842105263 0.3157894737)  
# 12) diabetes$Glucose< 0.7824049556 41   6 0 (0.8536585366 0.1463414634) *
#   13) diabetes$Glucose>=0.7824049556 35  17 1 (0.4857142857 0.5142857143)  
# 26) diabetes$SkinThickness>=-0.2653926752 15   5 0 (0.6666666667 0.3333333333) *
#   27) diabetes$SkinThickness< -0.2653926752 20   7 1 (0.3500000000 0.6500000000) *
#   7) diabetes$BMI>=-0.3604221538 207  57 1 (0.2753623188 0.7246376812) *



diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose+diabetes$BMI,
                 data = diabetes,
                 method = "class")

rpart.plot(diabfit)
print(diabfit)
fancyRpartPlot(diabfit)

#n= 768 

# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510417 0.3489583)  
# 2) diabetes$Glucose< 0.190999 485  94 0 (0.8061856 0.1938144) *
#   3) diabetes$Glucose>=0.190999 283 109 1 (0.3851590 0.6148410)  
# 6) diabetes$BMI< -0.3604222 76  24 0 (0.6842105 0.3157895) *
#   7) diabetes$BMI>=-0.3604222 207  57 1 (0.2753623 0.7246377) *

#





plot(diabfit)
text(diabfit)



fancyRpartPlot(diabfit)

# Put the predictors into 'x', the response into 'y'
names(diabetes)
x <- diabetes[,c(2,3,4,5,6,7,8)]
y <- diabetes$Outcome
c50fit <- C5.0(x, y)
summary(c50fit)

# Call:
#   C5.0.default(x = x, y = y)
# 
# 
# C5.0 [Release 2.07 GPL Edition]  	Sun Apr 23 19:53:52 2017
# -------------------------------
#   
#   Class specified by attribute `outcome'
# 
# Read 768 cases (8 attributes) from undefined.data
# 
# Decision tree:
# 
# Glucose <= 0.1745711:
# :...BMI <= -0.8726218: 0 (124/1)
# :   BMI > -0.8726218:
# :   :...Age <= 28: 0 (186/22)
# :       Age > 28:
# :       :...Glucose > -0.7453936:
# :           :...DiabetesPedigreeFunction <= -0.820564: 0 (21/4)
# :           :   DiabetesPedigreeFunction > -0.820564: 1 (101/42)
# :           Glucose <= -0.7453936:
# :           :...DiabetesPedigreeFunction <= 0.9510912: 0 (45/4)
# :               DiabetesPedigreeFunction > 0.9510912:
# :               :...SkinThickness <= 0.6776816: 1 (5/1)
# :                   SkinThickness > 0.6776816: 0 (3)
# Glucose > 0.1745711:
# :...BMI > -0.3676362:
# :...Glucose > 1.160248: 1 (92/12)
# :   Glucose <= 1.160248:
# :   :...Age > 30:
# :       :...DiabetesPedigreeFunction > -0.1354435: 1 (37/5)
# :       :   DiabetesPedigreeFunction <= -0.1354435:
# :       :   :...BMI <= 1.883156: 0 (23/10)
# :       :       BMI > 1.883156: 1 (5)
# :       Age <= 30:
# :       :...BloodPressure <= -0.02441388:
# :           :...BloodPressure <= -0.9329975: 1 (5)
# :           :   BloodPressure > -0.9329975:
# :           :   :...BloodPressure <= -0.6852019: 0 (4)
# :           :       BloodPressure > -0.6852019: 1 (17/4)
# :           BloodPressure > -0.02441388:
# :           :...BloodPressure <= 1.049367: 0 (17/1)
# :               BloodPressure > 1.049367:
# :               :...BMI <= 1.277174: 0 (3)
# :                   BMI > 1.277174: 1 (4)
# BMI <= -0.3676362:
# :...Glucose <= 0.765977: 0 (41/6)
# Glucose > 0.765977:
# :...Age <= 25: 0 (4)
# Age > 25:
# :...Age > 61: 0 (4)
# Age <= 61:
# :...BMI <= -0.7716247: 1 (12/1)
# BMI > -0.7716247:
# :...BloodPressure > 0.8015712: 0 (4)
# BloodPressure <= 0.8015712:
# :...DiabetesPedigreeFunction <= -0.2290062: 1 (8/1)
# DiabetesPedigreeFunction > -0.2290062: 0 (3)
# 
# 
# Evaluation on training data (768 cases):
# 
# Decision Tree   
# ----------------  
# Size      Errors  
# 
# 24  114(14.8%)   <<
# 
# 
# (a)   (b)    <-classified as
# ----  ----
# 434    66    (a): class 0
# 48   220    (b): class 1
# 
# 
# Attribute usage:
# 
# 100.00%	Glucose
# 100.00%	BMI
# 66.54%	Age
# 32.68%	DiabetesPedigreeFunction
# 8.46%	BloodPressure
# 1.04%	SkinThickness
# 
# 
# Time: 0.0 secs






p1=predict(m1, diabetes, type='response')
p1=round(p1)
table(p1, diabetes$Outcome)



barplot(table( factor(diabetes$Outcome),diabetes$Pregnancies))

#KNN 


#install.packages("class")
library(class)

#use knn to train a  model on the data




#note the target variable has not
#been included in the training and
#test data sets
str(training)
diabetes_test_pred <- knn(train = training, 
                          test = testing,
                          cl = diabetes_train_labels,
                          k=10)


#-----------------------------------------  Data Evaluation Phase -------------------------------------------------------------------


library(gmodels)
CrossTable(x=diabetes_test_labels,
           y=diabetes_test_pred,prop.chisq=FALSE)


#######################################
