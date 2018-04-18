source("function.R")
df <- read.csv("jdetl.csv") %>% 
  def_rename(1) %>% 
  filter(y==1 | y==2 | y==3) %>% 
  select(-c(27:28,48:51)) 



library(lars) #lars函数值用于矩阵型数据
laa <- lars(df[,2:ncol(df)] %>% as.matrix(),df[,1] %>% as.matrix(),
            type="lasso")
# plot(laa)
la <- summary(laa)
nn <- sapply(laa$actions, names)
nn1 <- nn[1:which.min(la$Cp)] #选择Cp值最小的，表示回归模型越精确

data <- split_dt(df,0.9)
train <- data$train
test <- data$test
train$y <-  train$y %>% as.factor()
test$y <-  test$y %>% as.factor()


formula <-  paste0(nn1,collapse = "+") %>% 
  paste0("y ~ ",.) %>% 
  as.formula()

## Random Forest1

ress <- filter_factor(train[,2:ncol(train)],train[,1])
res0  <- predict(ress,test[,2:ncol(test)],type ="class")
ver_ratio(res0$pred,test[,1]) 

## Random Forest
library(randomForest)
rf <- randomForest(formula,train)
res1  <- predict(rf,test[,2:ncol(test)],type ="class")
ver_ratio(res1,test[,1]) 
##  svm
library(e1071)
svm <- svm(formula,train)
res2  <- predict(svm,test[,2:ncol(test)],type ="class")
ver_ratio(res2,test[,1]) 

## Naive Bayes
nb <- naiveBayes(formula,train)
res3  <- predict(nb,test[,2:ncol(test)],type ="class")
ver_ratio(res3,test[,1]) # nb

## Decision Tree1
library(RWeka)
tree <- J48(formula,train)
res4  <- predict(tree,test[,2:ncol(test)],type ="class")
ver_ratio(res4,test[,1]) #  Decision Tree
## Decision Tree2
library(tree)
tree2 <- tree::tree(formula,train)
res5  <- predict(tree2,test[,2:ncol(test)],type ="class")
ver_ratio(res5,test[,1]) 
## Decision Tree3
library(C50)
tree3 <- C5.0(formula,train)
res6  <- predict(tree3,test[,2:ncol(test)],type ="class")
ver_ratio(res6,test[,1]) 

predict(model,type='response')

#使用multinom做多类别logistic回归
library(nnet)
mult <- multinom(formula,train)
res6  <- predict(mult,test[,2:ncol(test)],type ="class")
ver_ratio(res6,test[,1]) 

anes1=glm(formula,family=binomial(link='logit'),data=train)
res7=predict(anes1,test[,2:ncol(test)],type='response')
ver_ratio(res7,test[,1]) 







model <- glm(Species ~.,family=binomial(link='logit'),data=iris)

predict(model,type='response')
###########################################################################
# https://www.r-bloggers.com/lang/chinese/1033
# 降维中的特征选择，降维大致有两大类别：
# 1、是从原始维度中提取新的维度，例如主成分分析或因子分析，
# 再或者是奇异值分解或是多维标度分析。        PCA  principal component analysis 

# 2、是从原始维度中选择一些子集，即称为特征选择（Feature Selection），
# 或者叫作最佳子集选择。                      lasso

# 避免过度拟合，改进预测性能
# 使学习器运行更快，效能更高
# 剔除不相关的特征使模型更为简单，容易解释

filter_factor <- function(x,y){
  require(caret)
  require(magrittr)
  temp  <- nearZeroVar(x) 
  x1  <- x[,-temp] # 先删去近似于常量的变量
  
  temp2 <- cor(x1) %>% 
        findCorrelation(., 0.90)
  x2 <- x1[,-temp2] # 再删去相关度过高的自变量
  
  x3 <- x2 %>% 
        preProcess %>% 
        predict(x2) # 数据预处理步骤（标准化，缺失值处理）
  
  # 用sbf函数实施过滤方法，这里是用随机森林来评价变量的重要性
  temp3 <- sbf(x3,y,sbfControl = sbfControl(functions=rfSBF,
                                             verbose=F,
                                             method='cv')) 
  temp4 <- x3[temp3$optVariables] 
  xn <- rfe(temp4,y,sizes = c(10,20,30,50,60),rfeControl = 
              rfeControl(functions=rfFuncs,method='cv'))
  return(xn)
}
ress <- filter_factor(mdrrDescr,mdrrClass)
plot(ress,type=c('o','g'))





# 加载扩展包和数据集mdrr，得到自变量集合mdrrDescr和因变量mdrrClass
library(caret)
data(mdrr)
# 先删去近似于常量的变量
zerovar <- nearZeroVar(mdrrDescr)
newdata1 <- mdrrDescr[,-zerovar]
# 再删去相关度过高的自变量
descrCorr <- cor(newdata1)
highCorr <- findCorrelation(descrCorr, 0.90)
newdata2 <- newdata1[, -highCorr]
# 数据预处理步骤（标准化，缺失值处理）
Process <- preProcess(newdata2)
newdata3 <- predict(Process, newdata2)
# 用sbf函数实施过滤方法，这里是用随机森林来评价变量的重要性
data.filter <- sbf(newdata3,mdrrClass,
                   sbfControl = sbfControl(functions=rfSBF,
                                           verbose=F,
                                           method='cv'))
# 根据上面的过滤器筛选出67个变量
x <- newdata3[data.filter$optVariables]
# 再用rfe函数实施封装方法，建立的模型仍是随机森林
profile <- rfe(x,mdrrClass,
               sizes = c(10,20,30,50,60),
               rfeControl = rfeControl(functions=rfFuncs
                                       ,method='cv'))
# 将结果绘图，发现20-30个变量的模型精度最高
plot(profile,type=c('o','g'))




###########################################################################
# 用R建立岭回归和lasso回归
cement <- data.frame(X1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10), 
                     X2 = c(26,29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68), 
                     X3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
                     X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26,34, 12, 12),
                     Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1,115.9, 83.8, 113.3, 109.4)
                     )

lm.sol <- lm(Y ~ ., data = cement)
summary(lm.sol)
# 从结果看，截距和自变量的相关系数均不显著。
# 利用car包中的vif（）函数查看各自变量间的共线情况
library(car)
vif(lm.sol)
# 从结果看，各自变量的VIF值都超过10，存在多重共线性，其中，X2与X4的VIF值均超过200.
plot(X2 ~ X4, col = "red", data = cement)
# 接下来，利用MASS包中的函数lm.ridge()来实现岭回归。
# 下面的计算试了151个lambda值，最后选取了使得广义交叉验证GCV最小的那个。
library(MASS)
ridge.sol <- lm.ridge(Y ~ ., lambda = seq(0, 150, length = 151), data = cement, 
                     model = TRUE)
names(ridge.sol)  # 变量名字
ridge.sol$lambda[which.min(ridge.sol$GCV)]  ##找到GCV最小时的lambdaGCV
ridge.sol$coef[which.min(ridge.sol$GCV)]  ##找到GCV最小时对应的系数
par(mfrow = c(1, 2))
# 画出图形，并作出lambdaGCV取最小值时的那条竖直线
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
# 下面的语句绘出lambda同GCV之间关系的图形
plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(beta))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

par(mfrow = c(1, 1))
# 从上图看，lambda的选择并不是那么重要，只要不离lambda=0太近就没有多大差别。
# 下面利用ridge包中的linearRidge()函数进行自动选择岭回归参数
library(ridge)
mod <- linearRidge(Y ~ ., data = cement)
summary(mod)
# 从模型运行结果看，测岭回归参数值为0.0147，各自变量的系数显著想明显提高（除了X3仍不显著）
# 最后，利用Lasso回归解决共线性问题

library(lars)
## Loaded lars 1.2
x = as.matrix(cement[, 1:4])
y = as.matrix(cement[, 5])
(laa = lars(x, y, type = "lar"))  #lars函数值用于矩阵型数据
## 
## Call:
## lars(x = x, y = y, type = "lar")
## R-squared: 0.982 
## Sequence of LAR moves:
##      X4 X1 X2 X3
## Var   4  1  2  3
## Step  1  2  3  4
# 由此可见，LASSO的变量选择依次是X4，X1，X2，X3
plot(laa)  #绘出图
summary(laa)  #给出Cp值
# 根据课上对Cp含义的解释（衡量多重共线性，其值越小越好），我们取到第3步，
# 使得Cp值最小，也就是选择X4，X1，X2这三个变量。





###########################################################################
# https://blog.csdn.net/a345017062/article/details/52593817


# IV、信息增益、Gini增益主要用于单特征重要性评估。 
# Lasso主要用于超大规模特征的降维筛选。

# //PCA方案1：用SVD实现
pca1<-prcomp(df[,2:ncol(df)], scale = TRUE)
# //PCA方案2：采用线性代数中的实对称均值的对角化实现
pca2<-princomp(df[,2:ncol(df)],cor=T)
summary(pca1)
summary(pca2)
# 上面三行分别为标准差，方差贡献率，累计方差贡献率。 
# 根据上面的数据，至PC3时，累计方差贡献率已达0.95664，因此只取前三个特征已经足够。

# 特征选择
# 特征选择主要有Filter、Wrapper、Embedded等几种不同的思路。这里主要写写Filter。
# 
# 卡方检验

library(vcd)
# 准备进行卡检验所需的数据，提取治疗方式与治疗效果
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
# 对mytable进行卡方检验
chisq.test(mytable)
# p < 0.01，可以判断患者接受的治疗方式对治疗效果有明显影响。


# 使用卡方检验判断患者的性别对治疗效果的影响

library(vcd)
# 准备进行卡检验所需的数据，提取患者性别与治疗效果
mytable<-xtabs(~Improved+Sex,data=Arthritis)
# 对mytable进行卡方检验
chisq.test(mytable)


# p > 0.05，可以判断患者的性别对治疗效果无明显影响。

# 上面的实验中，p值表示不同列之间的相互独立的概率。 
# 在1中，由于p值很小，所以拒绝了治疗方式与治疗效果之间相互独立的假设。 
# 在2中，由于p值不够小，所以无法拒绝性别与治疗效果之间相互独立的假设。

# WOE、IV
# 预测目标变量所需的信息总量蕴含在所有的特征中，
# 某个特征所蕴含信息量（IV值）越大，则越重要。 
# IV值的计算以WOE为基础。 


library(woe)
# 计算数据集mtcars中，cyl这一列对目标变量am的woe值和iv值。
woe(Data=mtcars,"cyl",FALSE,"am",10,Bad=0,Good=1)
# 计算数据集mtcars中，mpg这一列对目标变量am的woe值和iv值。
woe(Data=mtcars,"mpg",TRUE,"am",10,Bad=0,Good=1)



# Lasso的基本思想是在回归系数的绝对值之和小于一个常数的约束条件下，
# 使残差平方和最小化，从而能够产生某些严格等于0的回归系数，
# 达到特征选择的目的。
library(lars)
data(diabetes)
# 使用lasso进行特征选择
lars(diabetes$x ,diabetes$y,type="lasso")













