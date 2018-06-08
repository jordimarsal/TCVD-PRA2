## ----load_libraries, include=FALSE---------------------------------------
library(arules)
library(arulesViz)
library(plyr)
## --- model libraries
library(C50)
library(rpart)
## -- plot libraries
library(corrplot)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(tcltk, pos=19)
library(aplpack, pos=19)

## ---- echo=TRUE----------------------------------------------------------
# read data
gender_submission <- read.csv("../dataset/gender_submission.csv")
test <- read.csv("../dataset/test.csv")
train <- read.csv("../dataset/train.csv")

## ---- echo=TRUE----------------------------------------------------------
# cleaning columns
t <- train_r
t$Embarked <- as.character( t$Embarked )
t$Embarked [t$Embarked==""] <- "S"
t$Embarked <- as.factor( t$Embarked )
table ( t$Embarked )
train_r <- t
str(train_r)

## ---- echo=TRUE----------------------------------------------------------
# Select columns
train_r <- train [,c("Survived","Pclass","Sex","Age","SibSp","Parch","Embarked")]
test_r <- test [,c("Pclass","Sex","Age","SibSp","Parch","Embarked")]

## ---- echo=TRUE----------------------------------------------------------
# factorize train data
data <- train_r

data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
data$Name <- as.character(data$Name)
data$Ticket <- as.character( data$Ticket )
data$Cabin <- as.character( data$Cabin )

edat <- discretize(data$Age,categories=5)
edat <- factor(edat, labels=c("nen","jove","adult","madur","vell"))
data$Age <- edat

train_f <- data

# factorize test data for to compare with train
data <- test

data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
data$Name <- as.character(data$Name)
data$Ticket <- as.character( data$Ticket )
data$Cabin <- as.character( data$Cabin )

edat <- discretize(data$Age,categories=5)
edat <- factor(edat, labels=c("nen","jove","adult","madur","vell"))
# mean by group:
(0.42+16.34)/2  = 8.38
(16.34+32.25)/2 = 24.3 
(32.25+48.17)/2 = 40.21
(48.17+64.08)/2 = 56.13
(64.08+80)/2    = 72.04
data$Age <- edat

test_f <- data

## ---- echo=TRUE----------------------------------------------------------
# Quality of data
sapply( train , function(x) (sum(is.na(x))))
sapply( train , function(x) {sum(x=="")})
# sample filtering
idx <- sample(1:nrow( train ),5)
train [idx,] # filter 5 rows of data


## ---- echo=TRUE----------------------------------------------------------

# factorized columns
train_f <- data [,c("Survived","Pclass","Sex","Age","SibSp")]
test_f <- data [,c("Pclass","Sex","Age","SibSp")]

# frecuencies
surv = subset(data,Survived=="1")
notsurv = subset(data,Survived=="0")
table( surv$Age )
table( notsurv$Age )
table( surv$Sex )
table( notsurv$Sex )
table( surv$Pclass )
table( notsurv$Pclass )
table( surv$Embarked )
table( notsurv$Embarked )
table( surv$SibSp )
table( notsurv$SibSp )
table( surv$Parch )
table( notsurv$Parch )

# Predictive Modeling C5.0
mod <- C5.0(Survived~.,data = train_f , rules=TRUE)
summary(mod)
pred <- predict( mod, test_f)
taula <- table(pred,gender_submission$Survived)
taula
percent <- 100*sum(diag(taula)) / sum(taula)
percent

## ---- echo=TRUE----------------------------------------------------------
# plots
# tiges i fulles:
library(tcltk, pos=19)
library(aplpack, pos=19)
with(train, stem.leaf(Fare, na.rm=TRUE))
# punts (Outliers)
with(train, discretePlot(Fare, scale="frequency"))

## ---- echo=TRUE----------------------------------------------------------
# save csv
write.csv(train_f, "D:\\Dropbox\\UOC-MA1\\TCVD_PRAC2\\dataset\\train_f.csv", row.names = FALSE)
write.csv(test_f, "D:\\Dropbox\\UOC-MA1\\TCVD_PRAC2\\dataset\\test_f.csv", row.names = FALSE)

## ---- echo=TRUE----------------------------------------------------------
# agrupacio per estadistiques
# menors d'edat / majors d'edat
train.men <- train[ train[,6] < 18 ,]
train.maj <- train [ train[,6] >= 18

## ---- echo=TRUE----------------------------------------------------------
# no supervivents de primera i de tercera classe
train.no1 <- train[ train[,3] == 1 & train[,2]==0 ,]
train.no3 <- train [ train[,3] == 3 & train[,2]==0 ,]

## ---- echo=TRUE----------------------------------------------------------
# variance analysys - Estadistics - Variancies - Test de Levene
with(train, tapply(Survived, Sex, var, na.rm=TRUE))
leveneTest(Survived ~ Sex, data=train, center="median")
## ---- echo=TRUE----------------------------------------------------------
# normality
p_val <- shapiro.test( train$Survived )$p.value
p_val <- shapiro.test( train$PassengerId )$p.value
p_val <- shapiro.test( train$Age )$p.value
p_val <- shapiro.test( train$SibSp )$p.value
p_val <- shapiro.test( train$Parch )$p.value
p_val <- shapiro.test( train$Fare )$p.value

## ---- echo=TRUE----------------------------------------------------------
# contraste de hipotesis
with(train, (t.test(Age, alternative='greater', mu=17.0, conf.level=.95)))

## ---- echo=TRUE----------------------------------------------------------
# correlation
vsur <- as.vector( train$Survived )
vpclass <- as.vector( train$Pclass )
vage <- as.vector( train$Age )
vsib <- as.vector( train$SibSp )
vfare <- as.vector( train$Fare )

st1 = cor.test( vsur , vpclass , method = "spearman", exact=FALSE)
st2 = cor.test( vsur , vage , method = "spearman", exact=FALSE)
st3 = cor.test( vsur , vsib , method = "spearman", exact=FALSE)
st4 = cor.test( vsur , vfare , method = "spearman", exact=FALSE)

## ---- echo=TRUE----------------------------------------------------------
# plot statistics

train_num <- train [,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare")]
train_num$Sex <- as.integer(train_num$Sex)
train_num$Age [is.na(train_num$Age)] <- 29.7
write.csv(train_num, "D:\\Dropbox\\UOC-MA1\\TCVD_PRAC2\\dataset\\train_num.csv", row.names = FALSE)

install.packages("corrplot")
library(corrplot)
M <- cor(train_num)
corrplot(M, method="circle")
corrplot(M, method="color")

table(train_f$Age)
nen  jove adult madur  vell 
139   139   150   138   148 

mean(train$Age, na.rm=TRUE)
[1] 29.69912
train_num$Age [is.na(train_num$Age)] <- 29.7
mean(train_num$Age)
[1] 29.69929

M <- cor(train_num)
corrplot(M, method="color")

tcor <- rbind(c(st1$estimate,st2$estimate,st3$estimate,st4$estimate))
colnames(tcor) <- c("Pclass","Age","SibSp","Fare")
rownames(tcor) <- c("Survived")
tcor
			Pclass        Age      SibSp      Fare
Survived -0.3396679 -0.0525653 0.08887948 0.3237361
corrplot(tcor, method="pie")

## ---- echo=TRUE----------------------------------------------------------
# altres plots
plot(train$Pclass, train$Fare, main="Fare x Pclass",  xlab="Classe", ylab="Preu bitllet", pch=19) 

pairs(~train_num$Survived+train_num$SibSp+train_num$Parch+train_num$Fare+train_num$Pclass+train_num$Sex+train_num$Age,data=train_num,      main="Titanic",col=blues9)

## ---- echo=TRUE----------------------------------------------------------
# Predictive Modeling rpart
# + plots del model

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp,
               data=train,
	 method="class")
plot(fit)
text(fit)

library(rpart.plot)
library(rattle)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "D:\\Dropbox\\UOC-MA1\\TCVD_PRAC2\\dataset\\rpart_prediction.csv", row.names = FALSE)

taula <- table(submit$Survived, gender_submission$Survived)
taula
percent <- 100*sum(diag(taula)) / sum(taula)
percent
[1] 97.12919

