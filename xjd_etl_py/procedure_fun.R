##   procedure function
# 将中文汉字 转换成 list结构的多拼音组合
etl_func <- function(x){
  require(magrittr)
  require(dplyr)
  require(stringr)
  require(pinyin)
  str_split(x,"") %>% 
  sapply(function(x){paste(x,collapse = "&")}) %>% 
  paste(collapse = "%") %>% 
  pinyin(method="toneless",sep = " ",multi = TRUE) %>% 
  str_split("%") %>% 
  unlist %>% 
  as.list() %>% 
  lapply(function(x){
  str_split(x,"&") %>% 
    unlist %>% 
    as.list()}) %>% 
  lapply(function(x){
    gsub("\\[|\\]","",x) %>% 
    as.list %>% 
    str_split(" ") %>% 
    lapply(function(x){x[x!=""]}) %>% 
    lapply(function(x){unique(x)})}) %>% 
  lapply(function(x){do.call(expand.grid,x)}) %>% 
  return()
}


  
# test <- names_etl[sample(1:length(names_etl),10,replace = T)] 

judge_fun <- function(t){
  require(magrittr)
  require(Hmisc)
  if(ncol(t) == 2){
    a <- t[,1] %>% as.character() %>% capitalize()
    b <- t[,2] %>% as.character() %>% capitalize()
    res <- paste(a,b,sep=",")
    
  } else if(ncol(t) == 3) {
    a <- t[,1] %>% as.character() %>% capitalize()
    b <- t[,2] %>% as.character() %>% capitalize()
    res1 <- paste(a,b,sep=",")
    c <- t[,3] %>% as.character()
    res <- paste0(res1,c)
  } else {
    a <- t[,1] %>% as.character() %>% capitalize()
    b <- t[,2] %>% as.character() %>% capitalize()
    res1 <- paste(a,b,sep=",")
    c <- t[,3] %>% as.character()
    res2 <- paste0(res1,c)
    d <- t[,4] %>% as.character()
    res <- paste0(res2,d)
  }
  return(res)
}



# 将list行元素，不一致情况转换成一维data.frame格式存储
## df <- cbind(vector,list)  第一列必须是名称，第二列存储元素个数不同的list
list2df <- function(df){
  require(magrittr)
  vec <- unlist(df[,1])
  list <- df[,2]
  names(list) <- vec
  data.frame(names=rep(names(list),sapply(list,length)),
             pinyin=unlist(list)) %>% 
    return()
}

# 缺失值填补函数 df列数=v的长度
na_insert <- function(df,v){
  for(n in 1:length(v)){
    df[is.na(df[,n]),n] <- as.numeric(v[n])
  }
  return(df)
}












  

  
  
  
  
  
  
  
  
  



