---
date: 2024-08-21
output: html_document
title: bank
---

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

# The data-set

The data is related with direct marketing campaigns (phone calls) of a
Portuguese banking institution. The classification goal is to predict if
the client will subscribe a term deposit (variable y).he data is related
with direct marketing campaigns of a Portuguese banking institution. The
marketing campaigns were based on phone calls. Often, more than one
contact to the same client was required, in order to access if the
product (bank term deposit) would be ('yes') or not ('no') subscribed.

``` {r,comment=na,echo=false}
library("rmarkdown")
bank=read.csv("C:\\Users\\HP\\OneDrive\\Documents\\figure\\bank-additional-full.csv",sep=";")
paged_table(bank)
```

# Variable Information

• 1 - age (numeric)

• 2 - job : type of job (categorical:
"admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
"blue-collar","self-employed","retired","technician","services")

• 3 - marital : marital status (categorical:
"married","divorced","single"; note: "divorced" means divorced or
widowed)

• 4 - education (categorical:
"unknown","secondary","primary","tertiary")

• 5 - default: has credit in default? (binary: "yes","no")

• 6 - balance: average yearly balance, in euros (numeric)

• 7 - housing: has housing loan? (binary: "yes","no")

• 8 - loan: has personal loan? (binary: "yes","no")

• 9 - contact: contact communication type (categorical:
"unknown","telephone","cellular")

• 10 - day: last contact day of the month (numeric)

• 11 - month: last contact month of year (categorical: "jan", "feb",
"mar", ..., "nov", "dec")

• 12 - duration: last contact duration, in seconds (numeric)

• 13 - campaign: number of contacts performed during this campaign and
for this client (numeric, includes last contact)

• 14 - pdays: number of days that passed by after the client was last
contacted from a previous campaign (numeric, -1 means client was not
previously contacted)

• 15 - previous: number of contacts performed before this campaign and
for this client (numeric)

• 16 - poutcome: outcome of the previous marketing campaign
(categorical: "unknown","other","failure","success")

• 17 - y - has the client subscribed a term deposit? (binary:
"yes","no")

``` {r,comment=na,echo=false}
str(bank)
```

# Dimension of the data-set

``` {r,comment=na,echo=false}
dim(bank)
index1=which(bank$job=="unknown")
index2=which(bank$marital=="unknown")
index3=which(bank$education=="unknown")
index4=which(bank$default=="unknown")
index5=which(bank$housing=="unknown")
index6=which(bank$loan=="unknown")
bank=bank[-c(index1,index2,index3,index4,index5,index6),]
dim(bank)
```

*We have some 'unknown' values in the columns: job, marital, education,
default, housing, and loan. Therefore, we removed the rows containing
'unknown' values. After this, we have 30,488 rows and 21 columns.*

# Column Names

``` {r,comment=na,echo=false}
colnames(bank)
```

# Summary of the data-set

``` {r,comment=na,echo=false}
summary(bank)
```

# Checking for the missing values

``` {r,comment=na,echo=false}
sum(is.na(bank))
```

*• There is no missing values in the data-set*

# Transform character variables into factor variables.

*There are 11 character variables in the data-set*

``` {r,comment=na,echo=false}
bank$job=as.factor(bank$job)
bank$marital=as.factor(bank$marital)
bank$education=as.factor(bank$education)
bank$default=as.factor(bank$default)
bank$housing=as.factor(bank$housing)
bank$loan=as.factor(bank$loan)
bank$contact=as.factor(bank$contact)
bank$month=as.factor(bank$month)
bank$day_of_week=as.factor(bank$day_of_week)
bank$poutcome=as.factor(bank$poutcome)
bank$y=as.factor(bank$y)
str(bank)
```

# Plot the 'int' and 'num' variables using histogram and their corresponding density curve

``` {r,comment=na,echo=false}
library("ggplot2")
ggplot(data=bank,aes(x=age))+geom_histogram(aes(y=..density..),bis = 24,colour="black",fill="#CC0000")+geom_density()
ggplot(data=bank,aes(x=duration))+geom_histogram(aes(y=..density..),bins =24 ,colour="black",fill="#993399")+geom_density()
ggplot(data=bank,aes(x=campaign))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#3333CC")+geom_density()
ggplot(data=bank,aes(x=pdays))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#99CCFF")+geom_density()
ggplot(data=bank,aes(x=previous))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#FFCC99")+geom_density()
ggplot(data=bank,aes(x=emp.var.rate))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#FFFF66")+geom_density()
ggplot(data=bank,aes(x=cons.price.idx))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#CCCC99")+geom_density()
ggplot(data=bank,aes(x=cons.conf.idx))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#339966")+geom_density()
ggplot(data=bank,aes(x=euribor3m))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#006633")+geom_density()
ggplot(data=bank,aes(x=nr.employed))+geom_histogram(aes(y=..density..),bins = 24,colour="black",fill="#333300")+geom_density()
```

# Plot the factor variables

## Bar chart,column chart,Donut chart

``` {r,connent=na,echo=false}
#job
data=data.frame("Category"=c("admin.","housemaid","services","blue-collar","retired","technician","unemployed","student","self-employed","management","entrepreneur"),"Values"=c(sum(bank$job=="admin."),sum(bank$job=="housemaid"),sum(bank$job=="services"),sum(bank$job=="blue-collar"),sum(bank$job=="retired"),sum(bank$job=="technician"),sum(bank$job=="unemployed"),sum(bank$job=="student"),sum(bank$job=="self-employed"),sum(bank$job=="management"),sum(bank$job=="entrepreneur")))
ggplot(data=data,aes(x=Category,y=Values,fill=Category))+geom_bar(stat="identity")+labs(title="Job")+geom_text(aes(label=Values))+theme(axis.text.x=element_text(angle=90))
#marital
ggplot(data=bank,aes(x=marital,fill=marital))+geom_bar()+labs(title="Marital")+theme(axis.text.x=element_text(angle=90))+coord_flip()
#education
ggplot(data=bank,aes(x=education,fill=education))+geom_bar()+labs(title="Education")+theme(axis.text.x=element_text(angle=90))+coord_flip()
#housing
data1=data.frame("cat1"=c("no","yes"),"val1"=c(sum(bank$housing=="no"),sum(bank$housing=="yes")))
slices1=c(sum(bank$housing=="no"),sum(bank$housing=="yes"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("no","yes"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title="Housing")+scale_fill_manual(values=c("yellow","purple"))
#loan
data1=data.frame("cat1"=c("no","yes"),"val1"=c(sum(bank$loan=="no"),sum(bank$loan=="yes")))
slices1=c(sum(bank$loan=="no"),sum(bank$loan=="yes"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("no","yes"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title="Loan")+scale_fill_manual(values=c("#FFFF99","#CC0066"))
#contact
data1=data.frame("cat1"=c("cellular","telephone"),"val1"=c(sum(bank$contact=="cellular"),sum(bank$contact=="telephone")))
slices1=c(sum(bank$contact=="cellular"),sum(bank$contact=="telephone"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("cellular","telephone"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title="Contact")+scale_fill_manual(values=c("#9999FF","#996633"))
#month
ggplot(data=bank,aes(x=month,fill=month))+geom_bar()+labs(title="Month")+theme(axis.text.x=element_text(angle=90))
#day_of_week
ggplot(data=bank,aes(x=day_of_week,fill=day_of_week))+geom_bar()+labs(title="day_of_week")
#y
data1=data.frame("cat1"=c("no","yes"),"val1"=c(sum(bank$y=="no"),sum(bank$y=="yes")))
slices1=c(sum(bank$y=="no"),sum(bank$y=="yes"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("no","yes"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title="y")+scale_fill_manual(values=c("#66CC66","#006699"))
```

# Boxplot

``` {r,echo=false,echo=false}
boxplot(bank[,1:7],col="blue")
boxplot(bank[,8:11],col="orange")
boxplot(bank[,12:16],col="green")
boxplot(bank[,17:21],col="red")
```

# Detection of outlier

An outlier is a data point that differs significantly from other
observations. An outlier may be due to a variability in the measurement,
an indication of novel data, or it may be the result of experimental
error; the latter are sometimes excluded from the data set. An outlier
can be an indication of exciting possibility, but can also cause serious
problems in statistical analyses.

The interquartile range (IQR) is a measure of statistical dispersion,
which is the spread of the data. The IQR may also be called the mid
spread, middle 50%, fourth spread, or H‑spread. It is defined as the
difference between the 75th and 25th percentiles of the data. To
calculate the IQR, the data set is divided into quartiles, or four
rank-ordered even parts via linear interpolation. These quartiles are
denoted by Q1 (also called the lower quartile), Q2 (the median), and Q3
(also called the upper quartile). The lower quartile corresponds with
the 25th percentile and the upper quartile corresponds with the 75th
percentile, so IQR = Q3 − Q1.

``` {r,comment=na,echo=false}
quantiles=quantile(bank$age,prob=c(.25,.75),na.rm=FALSE)
iqr=IQR(bank$age)
lower=quantiles[1]-1.5*iqr
upper=quantiles[2]+1.5*iqr
bank=subset(bank,bank$age>lower & bank$age < upper)

quantiles=quantile(bank$duration,prob=c(.25,.75),na.rm=FALSE)
iqr=IQR(bank$duration)
lower=quantiles[1]-1.5*iqr
upper=quantiles[2]+1.5*iqr
bank=subset(bank,bank$duration>lower & bank$duration < upper)
```

# Boxplot after removing the outliers from the dataset

``` {r,echo=false,comment=na}
boxplot(bank[,1:7],col="blue")
boxplot(bank[,8:11],col="orange")
boxplot(bank[,12:16],col="green")
boxplot(bank[,17:21],col="red")
```

# Correlation Matrix

*We select the numeric variables and then calculate their correlations*

``` {r,comment=na,echo=false}
num=bank[,c(1,11,12,13,14,16,17,18,19,20)]
num=cor(num)
library("corrplot")
corrplot(num,method="number")
bank=bank[,-c(16,19,20)]
```

*We can see that euribor3m and emp.var.rate are highly correlated, with
a correlation of 0.97. Additionally, euribor3m and nr.employed are
highly correlated, with a correlation of 0.95, and nr.employed and
emp.var.rate are also highly correlated, with a correlation of 0.90.We
removed the variables euribor3m, nr.employed, and emp.var.rate from the
dataset because they have high correlations.*

### **Dividing the data-set into training data and test data**

Training data is a subset of a dataset used to train a machine learning
model.

Test data refers to a subset of a dataset that is used to evaluate the
performance of a machine learning model after it has been trained.

*We randomly divide 90% of the dataset into training data and 10% into
test data.*

``` {r,echo=false,comment=na}
sample = sample(c(TRUE,FALSE), nrow(bank),replace=TRUE, prob=c(0.9,0.1)) 
train_bank = bank[sample, ] 
test_bank = bank[!sample, ] 
paged_table(train_bank)
paged_table(test_bank)
```

# Decision tree classifier

``` {r,echo=false,comment=na}
library("rpart")

library("rpart.plot")
d_tree=rpart(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+pdays+previous+poutcome+cons.price.idx+cons.conf.idx,data=train_bank)
rpart.plot(d_tree)
```

**Root Node (Top Node):**

pdays \>= 513 is the first decision point in the tree. If this condition
is not met (no branch), the predicted outcome is no, with a probability
of 0.09 (or 9%) and a sample distribution of 100%. If this condition is
met (yes branch), the tree moves to the next level.

**Left Subtree (pdays \>= 513 and month):**

For the yes branch of pdays \>= 513, the next decision is based on the
month. If the month is April, August, July, June, May, or November, the
predicted outcome is no, with a probability of 0.07 (7%) and a sample
distribution of 96%. If the month is not one of these, the tree further
splits based on duration.

**Further Splits:**

For the branches where the month condition is met, if duration \< 184,
the model predicts no with a probability of 0.39 (39%) and a sample
distribution of 4%. If duration \>= 184, the prediction is no with a
probability of 0.19 (19%) and a sample distribution of 2%. If duration
\< 184, the prediction is yes with a probability of 0.60 (60%) and a
sample distribution of 2%.

**Right Subtree (pdays \< 513 and duration):**

If pdays \>= 513 is yes but the month condition is not met, the decision
moves to duration \< 165. If duration \< 165, the predicted outcome is
no with a probability of 0.26 (26%) and a sample distribution of 1%. If
duration \>= 165, the prediction is yes with a probability of 0.74 (74%)
and a sample distribution of 3%.

# Actual vs Predicted Value of the variable y

``` {r,echo=false,comment=na}
library("caret")
df=data.frame(value=test_bank$y,pred=predict(d_tree,test_bank,type="class"))
paged_table(df)
```

# Confusion Matrix

``` {r,echo=false,comment=na}
confusionMatrix(as.factor(df$value),as.factor(df$pred))
```

True Positives (TP): 2437 (Predicted "no" and actually "no")

False Positives (FP): 205 (Predicted "yes" but actually "no")

False Negatives (FN): 121 (Predicted "no" but actually "yes")

True Negatives (TN): 174 (Predicted "yes" and actually "yes")

The model correctly predicts the outcome 88.9% of the time. This is the
proportion of true results (both true positives and true negatives)
among the total number of cases examined. This range, from 0.8771 to
0.9001, indicates the interval in which the true accuracy lies with 95%
confidence.

Also known as recall, sensitivity is the ability of the model to
correctly identify positive instances (those who are actually "no").
Here, 92.24% of actual "no" cases are correctly identified.

Specificity is the ability of the model to correctly identify negative
instances (those who are actually "yes"). Here, 58.98% of actual "yes"
cases are correctly identified. This is relatively low, indicating that
the model struggles more with identifying the "yes" cases correctly.

The model performs well in predicting the majority class ("no"), it
struggles with the minority class ("yes"), leading to an imbalance in
prediction accuracy.
