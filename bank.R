bank=read.csv("C:\\Users\\HP\\OneDrive\\Documents\\figure\\bank-additional-full.csv",sep=";")
str(bank)
dim(bank)
index1=which(bank$job=="unknown")
index2=which(bank$marital=="unknown")
index3=which(bank$education=="unknown")
index4=which(bank$default=="unknown")
index5=which(bank$housing=="unknown")
index6=which(bank$loan=="unknown")
bank=bank[-c(index1,index2,index3,index4,index5,index6),]
dim(bank)
colnames(bank)
summary(bank)
sum(is.na(bank))
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


# Plot the factor variables

## Bar chart,column chart,Donut chart


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

# Boxplot

boxplot(bank[,1:7],col="blue")
boxplot(bank[,8:11],col="orange")
boxplot(bank[,12:16],col="green")
boxplot(bank[,17:21],col="red")

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

boxplot(bank[,1:7],col="blue")
boxplot(bank[,8:11],col="orange")
boxplot(bank[,12:16],col="green")
boxplot(bank[,17:21],col="red")

num=bank[,c(1,11,12,13,14,16,17,18,19,20)]
num=cor(num)
library("corrplot")
corrplot(num,method="number")
bank=bank[,-c(16,19,20)]

sample = sample(c(TRUE,FALSE), nrow(bank),replace=TRUE, prob=c(0.9,0.1)) 
train_bank = bank[sample, ] 
test_bank = bank[!sample, ] 



library("rpart")

library("rpart.plot")
d_tree=rpart(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+pdays+previous+poutcome+cons.price.idx+cons.conf.idx,data=train_bank)
rpart.plot(d_tree)

library("caret")
df=data.frame(value=test_bank$y,pred=predict(d_tree,test_bank,type="class"))
confusionMatrix(as.factor(df$value),as.factor(df$pred))

