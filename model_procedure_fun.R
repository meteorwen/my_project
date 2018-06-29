split_dt <- function(dt,proportion){
  index  <- sample(2, nrow(dt), replace=T, 
                   prob=c(proportion,(1-proportion)))
  train <- dt[index==1,] %>% as.data.frame()
  test <- dt[index==2,] %>% as.data.frame()
  data <- list(train=train,test=test)
  return(data)
}
# y是指在 dt列所在位置角标
def_rename <- function(dt,y){
  require(magrittr)
  colnames(dt)[y] <- "y"
  colnames(dt)[-y] <- rep("x",(ncol(dt)-1)) %>% 
    paste0(c(1:(ncol(dt)-1))) 
  return(dt)
}

ver_ratio <- function(x,y){
  require(magrittr)
  table(x,y) %>% 
    prop.table %>% 
    diag %>% 
    sum %>% 
    return()
}

ver_r <- function(a,b){
   c <- vector()
   for(i in 1:length(a)) {
   if(a[i]==b[i])
   {c[i]=1} 
   else 
   {c[i]=2}
   }
   return(table(c)[1]/sum(table(c)))
}

# 缺失值 填补（#缺失值填补函数 df列数=v的长度）
na_insert <- function(df,v){
  for(n in 1:length(v)){
    df[is.na(df[,n]),n] <- as.numeric(v[n])
  }
  return(df)
}











