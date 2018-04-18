


setwd("C:\\Users\\Administrator\\Desktop\\sunm\\")

dt <- read.csv("jdetl.csv")


# https://blog.csdn.net/qq_16365849/article/details/52734139
# id  <- sample(2, nrow(dt), replace=T, prob=c(0.8,0.2))
# 
# train <- dt[id==1,]
# test <- dt[id==2,]


dt1 <- def_rename(dt,1) %>% 
  filter(y==1 | y==2 | y==3)
library(magrittr)
library(dplyr)
dt2 <- select(dt1,
              y,
              x5,
              x7,
              x9,
              x10,
              x11,
              x13,
              x22,
              x23,
              x24,
              x25,
              x28,
              x29,
              x31,
              x35,
              x36,
              x42,
              x44,
              x53,
              x55,
              x54,
              x56,
              x58
)

dt2$y <- (dt2$y) %>% as.factor()
# dt$专业 <-  as.factor(dt$专业)
# dt$民族 <-  as.factor(dt$民族)
# dt$户口性质 <-  as.factor(dt$户口性质)
# dt$户籍 <-  as.factor(dt$户籍)
# dt$性别 <- (dt$性别+1) %>% as.factor()

library(dplyr)


data <- split_dt(dt2,0.9)
train <- data$train
test <- data$test


formula <- formula(y ~ .)

library(nnet)
mult <- multinom(formula,train)

# library(mlogit)
# mlog <- mlogit(formula,train)
library(e1071)
svm <- svm(formula,train)
## Naive Bayes
nb <- naiveBayes(formula,train)
## Random Forest
library(randomForest)
rf <- randomForest(formula,train)
## Decision Tree
library(tree)
tree <- tree(formula,train)


# library(arules)
# apr <- apriori(formula,train)
## Gradient Boosted Tree
library(gbm)
gbm <- gbm(formula,data=train)

## Neural Network



res  <- predict(svm,test[,2:ncol(test)],type ="class")
ver_ratio(res,test[,1]) # svm
res1  <- predict(nb,test[,2:ncol(test)],type ="class")
ver_ratio(res1,test[,1]) # Naive Bayes
res2  <- predict(rf,test[,2:ncol(test)],type ="class")
ver_ratio(res2,test[,1]) # Random Forest
# res3  <- predict(gbm,test[,2:ncol(test)],type ="class")
# ver_ratio(res3,test[,1])
res4  <- predict(tree,test[,2:ncol(test)],type ="class")
ver_ratio(res4,test[,1]) #  Decision Tree


library(psych)
dt.cor <- cor(dt[,-1])   #必须是数值类型的因变量
fa.parallel(df[,-1], fa = "pc", n.iter = 58,
            show.legend = F, main = "Scree plot with parallel analysis")





test1 <- test[,2:ncol(test)] %>% 
  select("x28", "x58", "x54" ,"x25" ,"x56" ,"x53" ,"x24", "x22" ,"x55" ,"x31")
svm <- svm(formula=y ~ x28+x58+x54+x25+x56+x53+x24+x22+x55+x31,
           data=train)
res  <- predict(svm,test1,type ="class")
ver_ratio(res,test[,1]) # svm

library(randomForest)
rf <- randomForest(formula=y ~ x28+x58+x54+x25+x56+x53+x24+x22+x55+x31,
                   data=train)
res2  <- predict(rf,test1,type ="class")
ver_ratio(res2,test[,1]) # Random Forest




df <- read.csv("jdetl.csv") %>% 
  def_rename(1) %>% 
  filter(y==1 | y==2 | y==3) %>% 
  select(-c(27:28,48:51)) 
df$y <-  df$y %>% as.factor()
library(mlbench)  
library(caret) 
findCorrelation(cor(df[,2:ncol(df)]), cutoff=0.5)  



control <- rfeControl(functions=rfFuncs, method="cv", number=10)  
results <- rfe(df[,2:ncol(df)], df[,1],sizes=c(2:ncol(df)), rfeControl=control)


# summarize the results  
print(results)  
# list the chosen features  
predictors(results)  
# plot the results  
plot(results, type=c("g", "o")) 

formula <- paste(predictors(results),collapse = "+") %>% paste0("y ~ ",.)
library(e1071)
formula <- formula(y ~ .)
svm <- svm(formula=formula,data=train)
res  <- predict(svm,test,type ="class")
ver_ratio(res,test[,1]) # svm

library(randomForest)
rf <- randomForest(formula=formula,data=train)
res2  <- predict(rf,test,type ="class")
ver_ratio(res2,test[,1]) 













