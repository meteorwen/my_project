# 拟合线性模型常用的函数
# summary()：展示拟合模型的详细结果；
# 
# coefficients()：列出拟合模型的模型参数（截距项和斜率）；
# 
# confint()：提供模型参数的置信区间（默认95%）；
# 
# fitted()：列出拟合模型的预测值；
# 
# residuals()：列出拟合模型的残差值；
# 
# anova()：生成一个拟合模型的方差分析表，或者比较两个及以上拟合模型的方差分析表；
# 
# vcov()：列出模型参数的协方差矩阵；
# 
# AIC()：输出赤池信息统计量；
# 
# plot()：生成评价拟合模型的诊断图；
# 
# predict()：用拟合模型对新的数据集预测响应变量值。

# 数据拆分成训练集和测试集，返回list类型
split_dt <- function(dt,proportion){
  index  <- sample(2, nrow(dt), replace=T, 
                   prob=c(proportion,(1-proportion)))
  train <- dt[index==1,]
  test <- dt[index==2,]
  data <- list(train=train,test=test)
  return(data)
}

#验证两个向量的异同，最后得出相同的比率
# x <- c(1,1,1,1,2,2,3,3,4,4)
# y <- c(1,1,1,1,2,1,5,3,4,4)

ver_ratio <- function(x,y){
  require(magrittr)
  table(x,y) %>% 
    prop.table %>% 
    diag %>% 
    sum %>% 
    return()
}
# ver_ratio(x,y)
# ver_ratio(y,x)
# 重名了列名，dt 是df 类型，y是指定自变量（单列），剩余因变量
def_rename <- function(dt,y){
  require(magrittr)
  colnames(dt)[y] <- "y"
  colnames(dt)[-y] <- rep("x",(ncol(dt)-1)) %>% 
    paste0(c(1:(ncol(dt)-1))) 
  return(dt)
}