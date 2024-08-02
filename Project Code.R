library(dplyr)
library(mltools)
library(data.table)
library(infotheo)
library(Metrics)
library(caret)
library(tidyverse)
library(devtools)
library(ggplot2)

data_science=read.csv(file.choose(),header=TRUE)  #importing data from excel                           
colnames(data_science)<-c("x","wy","ds","exp","es","sa","el","cl","cs","wr")  #changing the column names
head(data_science) #displaying first six rows of the dataframe named "data_science"
data<-subset(data_science,select=-x)  #excluding the serial number column from "data_science"
head(data) #displaying first 6 rows of the new dataframe "data"
dim(data)
sapply(data,function(x) sum(is.na(x)))
str(data)
sapply(data,function(x) n_distinct(x))
summary(data)
object.size(data)
salary<-gsub(",","",data$sa)  #Extracting "sa" (salary column) from "data", deleting all "," from each of 607 figures
sal<-as.numeric(salary)       #Converting the datatype of "sa" from "char"(character) to "num"(numeric)
data[]<-lapply(data,factor)   #Coverting all the non-numeric column variables of dataframe "data" to factor variables
data_new<-data.frame(cbind(sal,subset(data,select=-sa))) #Creating new dataframe by combining sal_sd and columns of "data" excluding "sa"
dat_new<-data.frame(lapply(data_new,as.numeric))#Converting the datatype of each column of "data_new" to 'num'(numeric)for the correlation matrix
round(cor(dat_new[c('wy','ds','exp','es','el','cl','cs','wr')]),2) #Displaying the correlation matrix

library(EnvStats)
epdPlot(sal,epdf.col="red")



apply((subset(data,select=-sa)),2,table)

freq_table<-apply((subset(data,select=-sa),2,table)
freq_table
exp_data<-subset(data,select=-sa)

ggplot(data_new,aes(x=wy,y=sal,fill=wy))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Working Year vs Data Science Salary")
ggplot(data_new,aes(x=ds,y=sal,fill=ds))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Designation vs Data Science Salary")
ggplot(data_new,aes(x=exp,y=sal,fill=exp))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Experience_Level vs Data Science Salary")
ggplot(data_new,aes(x=es,y=sal,fill=es))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Employment_Status Vs Data Science Salary")
ggplot(data_new,aes(x=el,y=sal,fill=el))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Employment_Location Vs Data Science Salary")
ggplot(data_new,aes(x=cl,y=sal,fill=cl))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Company_Location Vs Data Science Salary")
ggplot(data_new,aes(x=cs,y=sal,fill=cs))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Company_Size Vs Data Science Salary")
ggplot(data_new,aes(x=wr,y=sal,fill=wr))+geom_bar(stat="identity")+ggtitle("Barplot Diagram For Remote Working Ratio Vs Data Science Salary")



ggplot(data_new,aes(x="",y=sal_sd,fill=wy))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal_sd,fill=ds))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=exp))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=es))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=el))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=cl))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=cs))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)
ggplot(data_new,aes(x="",y=sal,fill=wr))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)

ggplot(data=data_new,aes(x=wy,y=sal,fill=wy))+geom_boxplot()+ggtitle("Working Year VS Salary")
ggplot(data=data_new,aes(x=ds,y=sal,fill=ds))+geom_boxplot()+ggtitle("Designation VS Salary")
ggplot(data=data_new,aes(x=exp,y=sal,fill=exp))+geom_boxplot()+ggtitle("Experience VS Salary")
ggplot(data=data_new,aes(x=es,y=sal,fill=es))+geom_boxplot()+ggtitle("Employment Status VS Salary")
ggplot(data=data_new,aes(x=el,y=sal,fill=el))+geom_boxplot()+ggtitle("Employment Location VS Salary")
ggplot(data=data_new,aes(x=cl,y=sal,fill=cl))+geom_boxplot()+ggtitle("Company Location VS Salary")
ggplot(data=data_new,aes(x=cs,y=sal,fill=cs))+geom_boxplot()+ggtitle("Company Size VS Salary")
ggplot(data=data_new,aes(x=wr,y=sal,fill=wr))+geom_boxplot()+ggtitle("Remote Working Ratio VS Salary")

boxplot<-boxplot(sal,col="cyan",ylab="Boxplot Diagram For Salary Data",horizontal=TRUE)
boxplot$stats
Quartile<-c(boxplot$stats[2,],boxplot$stats[3,],boxplot$stats[4,])
Quartile

threshold<-(Quartile[3]-Quartile[1])*1.5
vec<-c()                        #Empty Vector declared
for(x in 1:length(sal)){
if(sal[x]>Quartile[3]+threshold|sal[x]<Quartile[1]-threshold){ #Trimmimg Condition
vec<-append(vec,x)              #Outlier Index Vecror Formed       
}
}
length(vec)                     #Number of outlier values
subdat<-data[-vec,]             #Trimmed Dataset
sal_trim<-sal[-vec]             #Trimmed Salary Vector

boxplot(sal_trim,col="darkorchid2",ylab="Salary",horizontal=TRUE)
sal_sd<--as.numeric(scale(sal_trim,center=TRUE,scale=TRUE)) 

datam=read.csv(file.choose(),header=TRUE)
colnames(datam)<-c("x","wy","ds","exp","es","sa","el","cl","cs","wr")
dat<-data.frame(table(datam$wy,datam$ds))
names(dat)<-c("Year","Designation","Count")
ggplot(dat,aes(x=Year,y=Count,fill=Designation))+geom_bar(stat="identity")

ggplot(data=data_new,aes(x=wy,y=sal_sd,color=ds))+geom_point()

dat_new<-data.frame(lapply(data_new,as.numeric))
round(cor(dat_new[c('wy','ds','exp','es','el','cl','cs','wr')]),2)
ggplot(data=data_new,aes(x=sal_sd))+geom_histogram(fill="steelblue",color="black")+ggtitle("Histogram of Salary Values")
sapply(data,function(x) n_distinct(x))


df<-subset(subdat,select=-sa)
newdf<-one_hot(as.data.table(df))
datan<-data.frame(cbind(sal_sd,newdf))
datan[1:3,2:4]
sample<-sample(c(TRUE,FALSE),nrow(datan),replace=TRUE,prob=c(0.8,0.2))
train<-datan[sample,]
test<-datan[!sample,]
model<-lm(sal_sd~.,data=train)
summary(model)$sigma
coeff<-model$coefficients    #extracting the model coefficients
mulc<-c()                    #empty vector declared
for(x in 1:length(coeff)){
if(is.na(coeff[x])==TRUE){   #condition checking for NA coefficients
mulc<-append(mulc,x)         #vector of indices for NA coefficients
}
}
train_dat<-subset(train,select=-mulc)  #new train data
test_dat<-subset(test,select=-mulc)    #new test data
model2<-lm(sal_sd~.,data=train_dat)     #Regression model for the new train data
coeff2<-model2$coefficients            #Coefficient extraction from the new regression model
any(is.na(coeff2))                     #Checking for NA coefficients for the new regression model
rsa<-summary(model)$sigma
r2<-summary(model)$r.squared

mut<-c()         #Empty vector declaraed for storing the mutual informations.
num_bins<-10
for(x in 2:length(train_dat)){
a<-cut(train_dat[,1],breaks=num_bins)
b<-cut(train_dat[,x],breaks=num_bins)
mutual<-mutinformation(a,b,method="emp")
mut<-append(mut,mutual)
}
sort<-sort(mut,decreasing=T)  #Sorting the mut vector
sortmut<-sort[c(1:50)]        #Extracting the first 50 values of the sorted vector
nam<-colnames(train_dat)      #Creating the vector of column names of train_dat dataframe
name<-nam[-1]                 #Deleting the name "sal_sd" of Salary column.
names<-c()  #Empty Vector declared for storing the names of the most important explanatory variables
for(x in 1:length(sortmut)){
for(y in 1:length(mut)){
if(sortmut[x]==mut[y]){       #Condition Checking  
names<-append(names,name[y])  #names for which (x,y) values satisfy the above condition are satisfied are added 
}
}
}
length(names)
newnames<-unique(names)
length(newnames)
train_new<-subset(train_dat,select=c("sal_sd",newnames))
test_new<-subset(test_dat,select=c("sal_sd",newnames))
reg_model<-lm(sal_sd~.,data=train_new)
summary(reg_model)

predict<-predict(model2,newdata=test_dat,interval='confidence')
predictions<-predict(model2,newdata=test_dat)
rmse<-rmse(test_dat$sal_sd,predictions)
mae<-mae(test_dat$sal_sd,predictions)
mse<-mse(test_dat$sal_sd,predictions)
r2<-R2(test_dat$sal_sd,predictions)
model_metrics_lm<-cbind(rmse,mse,mae,r2)
row.names(model_metrics_lm)<-"Linear Regression"
model_metrics_lm

predict<-predict(reg_model,newdata=test_new,interval='confidence')
predictions<-predict(reg_model,newdata=test_new)
rmse<-rmse(test_new$sal_sd,predictions)
mae<-mae(test_new$sal_sd,predictions)
mse<-mse(test_new$sal_sd,predictions)
r2<-R2(test_new$sal_sd,predictions)
model_metrics_lm<-cbind(rmse,mse,mae,r2)
row.names(model_metrics_lm)<-"Linear Regression"
model_metrics_lm
predictions<-factor(predictions,levels=levels(test_check$sal_new))
accuracy<-confusionMatrix(data=as.factor(predictions),reference=as.factor(test_new$salsd))

vif<-vif(model2)
flag<-c()
for(x in 1:length(vif)){
if(vif[x]>5){
flag<-append(flag,x+1)
}
}
train_check<-subset(train_dat,select=-flag)
test_check<-subset(test_dat,select=-flag)
mod<-lm(salsd~.,data=train_check)
rel_weights<-calc.relimp(model2,type="lmg")


