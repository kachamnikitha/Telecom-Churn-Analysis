#Data Exploration

library(readr)
TelcoCustomerChurn <- read.csv("C:\\Users\\Nikitha\\Desktop\\GMU\\Fall - 2017\\DAEN 690\\project\\TelcoCustomerChurn (2).csv")
View(TelcoCustomerChurn)
churn=subset(TelcoCustomerChurn,select = c("gender","SeniorCitizen","Partner","Dependents","tenure","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges","TotalCharges","Churn"))
View(churn)
names(churn)
nrow(churn)
ncol(churn)
table(is.na(churn))
sum(is.na(churn))


# Consider the all the variable gender
table(is.na(churn$gender))
is.numeric(churn$gender)
class(churn$gender)
table(churn$gender)
Gender <- factor(churn$gender, levels = c("Female", "Male"), labels = c(0,1))
is.factor(Gender)
table(Gender)
churn$gender<-Gender

# Consider the all the variable SeniorCitizen
is.numeric(churn$SeniorCitizen)
class(churn$SeniorCitizen)
table(churn$SeniorCitizen)
sum(is.na(churn$SeniorCitizen))
range(churn$SeniorCitizen)

# Consider the all the variable Partner
is.numeric(churn$Partner)
class(churn$Partner)
is.integer(churn$Partner)
table(is.na(churn$Partner))
table(churn$Partner)
partner<-factor(churn$Partner,levels=c("Yes","No"),labels = c(1,0))
churn$Partner<-partner

# Consider the all the variable Dependents
is.numeric(churn$Dependents)
class(churn$Dependents)
is.integer(churn$Dependents)
table(is.na(churn$Dependents))
table(churn$Dependents)
dependents<-factor(churn$Dependents,levels=c("Yes","No"),labels=c(1,0))
churn$Dependents<-dependents

# Consider the all the variable tenure
is.integer(churn$tenure)
class(churn$tenure)
range(churn$tenure)
table(is.na(churn$tenure))

# Consider the all the variable PhoneService
is.numeric(churn$PhoneService)
class(churn$PhoneService)
is.factor(churn$PhoneService)
table(is.na(churn$PhoneService))
table(churn$PhoneService)
phoneservice<-factor(churn$PhoneService,levels=c("Yes","No"),labels = c(1,0))
churn$PhoneService<-phoneservice

# Consider the all the variable MultipleLines
is.integer(churn$MultipleLines)
class(churn$MultipleLines)
table(is.na(churn$MultipleLines))
table(churn$MultipleLines)
multiplelines<-factor(churn$MultipleLines,levels = c("Yes","No","No phone service"),labels = c(1,0,2))
churn$MultipleLines<-multiplelines

# Consider the all the variable InternetService
is.numeric(churn$InternetService)
class(churn$InternetService)
table(is.na(churn$InternetService))
table(churn$InternetService)
internetservice<-factor(churn$InternetService,levels=c("DSL","Fiber optic","No"),labels=c(1,2,0))
churn$InternetService<-internetservice

# Consider the all the variable OnlineSecurity
is.integer(churn$OnlineSecurity)
class(churn$OnlineSecurity)
table(is.na(churn$OnlineSecurity))
table(churn$OnlineSecurity)
onlinesecurity<-factor(churn$OnlineSecurity,levels=c("Yes","No","No internet service"),labels = c(1,0,2))
churn$OnlineSecurity<-onlinesecurity

# Consider the all the variable OnlineBackup
is.numeric(churn$OnlineBackup)
class(churn$OnlineBackup)
table(is.na(churn$OnlineBackup))
table(churn$OnlineBackup)
onlinebackup<-factor(churn$OnlineBackup,levels=c("Yes","No","No internet service"),labels = c(1,0,2))
churn$OnlineBackup<-onlinebackup

# Consider the all the variable DeviceProtection
is.integer(churn$DeviceProtection)
class(churn$DeviceProtection)
table(is.na(churn$DeviceProtection))
table(churn$DeviceProtection)
deviceprotection<-factor(churn$DeviceProtection,levels = c("Yes","No internet service","No"),labels = c(1,2,0))
churn$DeviceProtection<-deviceprotection

# Consider the all the variable TechSupport
is.integer(churn$TechSupport)
class(churn$TechSupport)
table(is.na(churn$TechSupport))
table(churn$TechSupport)
techsupport<-factor(churn$TechSupport,levels = c("Yes","No","No internet service"),labels = c(1,0,2))
churn$TechSupport<-techsupport

# Consider the all the variable StreamingTV
is.integer(churn$StreamingTV)
class(churn$StreamingTV)
table(is.na(churn$StreamingTV))
table(churn$StreamingTV)
streamingtv<-factor(churn$StreamingTV,levels = c("Yes","No","No internet service"),labels = c(1,0,2))
churn$StreamingTV<-streamingtv

# Consider the all the variable StreamingMovies
is.numeric(churn$StreamingMovies)
class(churn$StreamingMovies)
sum(is.na(churn$StreamingMovies))
table(churn$StreamingMovies)
streamingmovies<-factor(churn$StreamingMovies,levels = c("Yes","No","No internet service"),labels = c(1,0,2))
churn$StreamingMovies<-streamingmovies

# Consider the all the variable Contract
is.integer(churn$Contract)
class(churn$Contract)
table(is.na(churn$Contract))
table(churn$Contract)
contract<-factor(churn$Contract,levels=c("Month-to-month","One year","Two year"),labels=c(1,2,3))
churn$Contract<-contract

# Consider the all the variable PaperlessBilling
is.integer(churn$PaperlessBilling)
class(churn$PaperlessBilling)
sum(is.na(churn$PaperlessBilling))
table(churn$PaperlessBilling)
paperlessbilling<-factor(churn$PaperlessBilling,levels=c("Yes","No"),labels = c(1,0))
churn$PaperlessBilling<-paperlessbilling

# Consider the all the variable PaymentMethod
is.integer(churn$PaymentMethod)
class(churn$PaymentMethod)
table(is.na(churn$PaymentMethod))
table(churn$PaymentMethod)
paymentmethod<-factor(churn$PaymentMethod,levels=c("Bank transfer (automatic)","Credit card (automatic)","Electronic check","Mailed check"),labels=c(1,2,3,4))
churn$PaymentMethod<-paymentmethod

# Consider the all the variable MonthlyCharges
is.integer(churn$MonthlyCharges)
class(churn$MonthlyCharges)
range(churn$MonthlyCharges)
table(is.na(churn$MonthlyCharges))

# Consider the all the variable TotalCharges
is.integer(churn$TotalCharges)
class(churn$TotalCharges)
range(churn$TotalCharges)
table(is.na(churn$TotalCharges))
mean(churn$TotalCharges,na.rm = TRUE)
churn$TotalCharges[which(is.na(churn$TotalCharges))]<-mean(churn$TotalCharges,na.rm = TRUE)
sum(is.na(churn$TotalCharges))

#hotdeck
library(hot.deck)
library(HotDeckImputation)
df<-data.frame(hot.deck(churn))

# Consider the all the variable Churn
is.integer(churn$Churn)
class(churn$Churn)
table(is.na(churn$Churn))
table(churn$Churn)
Churn<-factor(churn$Churn,levels=c("Yes","No"),labels = c(1,0))
churn$Churn<-Churn

# Changing of Data type of the variabels
# By clicking on each coloum and select the numeric type where ever its require in the data frame 
fix(churn)

#Check for correlation
library(corrplot)
corrplot(cor(churn), order = 'hclust', tl.col='black', tl.cex=.80) 

###############################Statistical Analysis#################################

library(ResourceSelection)
library(fmsb)
library(ROCR)
library(rpart)
set.seed(100)
a=sample(nrow(churn),19720)
train=churn[a,]
test=churn[-a,]
View(train)
View(test)
nrow(train)
nrow(test)
# logistic model with enter method
logmo=glm(Churn~.,data=train,family=binomial)
summary(logmo)
# logistic model with forward method
logmof=step(logmo,direction="forward")
summary(logmof)
# logistic method with backward method
logmob=step(logmo,direction="backward")
summary(logmob)
# Final model of logistic regression
loomodsig=glm(Churn~-gender+SeniorCitizen-Partner-Dependents+tenure-PhoneService+MultipleLines+InternetService+OnlineSecurity-OnlineBackup-DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling-PaymentMethod+MonthlyCharges+TotalCharges,data=train,family=binomial)
logmobsig2=step(loomodsig,direction="backward")
summary(logmobsig2)
test$score=predict(logmobsig2,newdata=subset(test,select=c(1:19)),type='response')
prbsig2 <- prediction(test$score, test$Churn)
prfbsig2 <- performance(prbsig2, measure = "tpr", x.measure = "fpr")
plot(prfbsig2)
abline(0,1, lty = 8, col = 'red')
auc=performance(prbsig2,measure="auc")
auc=auc@y.values[[1]]
auc
#Confusion matrix
library(caret)
y_pred <- ifelse(test$score > 0.5, 1 ,0)
cm <- table(test$Churn, y_pred)
cm
confusionMatrix( table(test$Churn, y_pred))

#Efficency
Efficency <- sum(diag(cm))/sum(cm)
Efficency



######################################Naive Bayes Classification################################


### Partitioning the data
churn2<-subset(TelcoCustomerChurn,select=c("gender","SeniorCitizen","Partner","Dependents","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","Churn"))
View(churn2)
set.seed(100)
a=sample(nrow(churn2),19720)
train=churn2[a,]
test=churn2[-a,]
### Applying the Agorithm
library(e1071)
model<-naiveBayes(Churn~., data=train)
summary(model)
print(model)
##conditional probobility for given "Churn"
tbl_list <- sapply(test[-17], table, test[ , 17])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })
cond_probs <- lapply(cond_probs, t)

print(cond_probs)

#Confusion matrix
preds=predict(model,newdata=test)
cm <- table(preds, test$Churn)
cm
#Efficency
Efficency <- sum(diag(cm))/sum(cm)
Efficency

#####Visualization#######
churn1=subset(TelcoCustomerChurn,select = c("gender","SeniorCitizen","Partner","Dependents","tenure","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges","TotalCharges","Churn"))
View(churn1)
# Consider the Churn Variable
library(plotrix)
barplot(table(churn1$Churn),xlab = "Churn",ylab = "Frequency",col = c("yellow","green"),main="Barplot of Churn")
####
Churn <-table(churn1$Churn) 
lbls <- c("Yes", "No")
pct <- round(Churn/sum(Churn)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(Churn,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Churn")
#Consider the variable gender
table(churn1$gender)
barplot(table(churn1$gender),xlab = "gender",ylab = "Frequency",col = c("red","green"),main="Barplot of gender")
####
gender <-table(churn1$gender) 
lbls <- c("Female", "Male")
pct <- round(gender/sum(gender)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(gender,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of gender")
#Consider the variable senior Citizen
table(churn1$SeniorCitizen)

barplot(table(churn1$SeniorCitizen),xlab = "Senior Citizen",ylab = "Frequency",col = c("blue","orange"),main="barplot of seniorCitizen")
###
Seniorcitizen <-table(churn1$SeniorCitizen) 
lbls <- c("0", "1")
pct <- round(Seniorcitizen/sum(Seniorcitizen)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(Seniorcitizen,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of SeniorCitizen")
#consider the variable partner
table(churn1$Partner)
barplot(table(churn1$Partner),xlab = "gender",ylab = "Frequency",col = c("skyblue","pink"),main="barplot of Partner")
###
partner <-table(churn1$Partner) 
lbls <- c("No", "Yes")
pct <- round(partner/sum(partner)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(partner,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of partner")

# Consider the variable Dependents
table(churn1$Dependents)
barplot(table(churn1$Dependents),xlab = "Dependents",ylab = "Frequency",col = c("orange","skyblue"),main="barplot of Dependents")
###
Dependents <-table(churn1$Dependents) 
lbls <- c("No", "Yes")
pct <- round(Dependents/sum(Dependents)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(Dependents,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Dependents")
# consider the variable tenure
hist(churn1$tenure,xlab = "tenure", ylab = "frequency",col=c("blue","green","skyblue","pink","yellow"))
#consider the variable MultipleLines
barplot(table(churn1$MultipleLines),xlab = "MultipleLines",ylab = "Frequency",col = c("yellow","green","skyblue"),main="barplot of MultipleLines")
###
MultipleLines <-table(churn1$MultipleLines) 
lbls <- c("No", "No phone service","Yes")
pct <- round(MultipleLines/sum(MultipleLines)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(MultipleLines,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of MultipleLines")
# Consider the variable Internet Service
barplot(table(churn1$InternetService),xlab = "Internet service",ylab = "Frequency",col = c("pink","yellow","skyblue"),main="barplot of InternetService")
###
Internetservice <-table(churn1$InternetService) 
lbls <- c("DSL", "Fiber optic","No")
pct <- round(Internetservice/sum(Internetservice)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(Internetservice,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Internet Service")
# consider the variable Contract
barplot(table(churn1$Contract),xlab = "Contract",ylab = "Frequency",col = c("pink","yellow","skyblue"),main="barplot of Contract")
###
Contract <-table(churn1$Contract) 
lbls <- c("Month-to-month", "One year","Two year")
pct <- round(Contract/sum(Contract)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(Contract,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Contract")
# consider the variable PaymentMethod
barplot(table(churn1$PaymentMethod),xlab = "PaymentMethod",ylab = "Frequency",col = c("pink","yellow","skyblue"),main="barplot of PaymentMethod")
###
paymentmethod <-table(churn1$PaymentMethod) 
lbls <- c("Bank transfer", "Credit card","Electrinic check","Mailed check")
pct <- round(paymentmethod/sum(paymentmethod)*100)
lbls <- paste(lbls, pct)                                   
lbls <- paste(lbls,"%",sep="")
pie3D(paymentmethod,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Payment Method")


#...........VISUALIZATIONS............
library(readr)
TelcoCustomerChurn <- read.csv("C:\\Users\\Nikitha\\Desktop\\GMU\\Fall - 2017\\DAEN 690\\project\\TelcoCustomerChurn (2).csv")
View(TelcoCustomerChurn)
churn=subset(TelcoCustomerChurn,select = c("gender","SeniorCitizen","Partner","Dependents","tenure","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges","TotalCharges","Churn"))
View(churn)

# Counts plot
theme_set(theme_bw())  # pre-set the bw theme.
library(ggplot2)
g <- ggplot(churn, aes(TotalCharges, MonthlyCharges))
countp<-g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="churn: TotalCharges vs MonthlyCharges", 
       y="MonthlyCharges", 
       x="TotalCharges", 
       title="Counts Plot")
countp

#Density Plot
theme_set(theme_classic())
g <- ggplot(churn, aes(tenure))
denplt<-g + geom_density(aes(fill=factor(Contract)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="tenure Grouped by Number of contract",
       caption="Source: churn",
       x="tenure",
       fill="# Contract")
denplt

g <- ggplot(churn, aes(Churn, MonthlyCharges))
g + geom_boxplot(aes(fill=factor(PaymentMethod))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="MonthlyCharges grouped by Churn",
       caption="Source: churn",
       x="Churn",
       y="MonthlyCharges")

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(churn, aes(tenure)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=Churn), 
                   binwidth = 2.0, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="tenure across Churn classes")  

# Scatterplot with Overlapping points
g <- ggplot(churn, aes(tenure, TotalCharges))
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="churn: tenure vs TotalCharges", 
       y="tenure", 
       x="TotalCharges", 
       title="Scatterplot with overlapping points", 
       caption="Source: churn")

#Corrplot
library(corrplot)
corrplot(cor(churn), order = "hclust", tl.col='black', tl.cex=.80) 

#Binary fitted line plot
library(readr)
churn1<-read.csv("C:\\Users\\Nikitha\\Desktop\\GMU\\Fall - 2017\\DAEN 690\\project\\churn.csv")
View(churn1)
fit = glm(Churn ~tenure, data=churn1, family=binomial)
newdat <- data.frame(tenure=seq(min(churn1$tenure), max(churn1$tenure),len=100))
newdat$Churn = predict(fit, newdata=newdat, type="response")
plot(Churn~tenure, data=churn1, col="red4")
lines(Churn ~ tenure, newdat, col="green4", lwd=2)

#correlogram
library(ggcorrplot)
# Correlation matrix
data<-read.csv("C:\\Users\\Nikitha\\Desktop\\GMU\\Fall - 2017\\DAEN 690\\project\\churn.csv")
View(data)
corr <- round(cor(data), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of churn", 
           ggtheme=theme_bw)







