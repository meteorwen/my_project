#！/usr/bin/Rscript
library(RMySQL)
conn1 <- dbConnect(MySQL(),
                   dbname = "exchange", username="root",
                   password="SUN@xjtu2018", host="10.49.10.11", port=3306,
                   client.flag= CLIENT_MULTI_STATEMENTS)
#client.flag设置这样支持批量查询
dbSendQuery(conn1,'SET NAMES utf8')
u_s <- dbReadTable(conn1, "undergraduate_student")
user <- dbReadTable(conn1, "users_sgdac")
g_u <- dbReadTable(conn1, "graduate_fosters")
dbDisconnect(conn1)

library(dplyr)
s1 <- select(u_s,stu_no, stu_name)
t <- select(user,no, trueusername) 
s2 <- select(g_u,student_code, student_name) 
colnames(s1) =colnames(s2) =colnames(t)  = c('id','nn')
namess <- rbind(s1,s2,t) %>% arrange(nn)
chi_sta <- which(namess$nn=='丁一')[1] #获取汉字起始位置，“丁一”
eng_name <- namess[1:(chi_sta-1),]
namesc <- namess[chi_sta:nrow(namess),] 
namesc_etl <- gsub("\\*|\\.|\\?| ||\\|·|_|[a-z]|[A-Z]|　|","",namesc$nn)
# 过滤非法字符
#  去重名字
uniq_names <- unique(namesc_etl)
# 提取名字2-4个字
na2 <- uniq_names[nchar(uniq_names)==2]
na3 <- uniq_names[nchar(uniq_names)==3]
na4 <- uniq_names[nchar(uniq_names)==4]
na  <- c(na2,na3,na4)

source("/home/apple/Rspace/xjd_paper_names/procedure_fun.R")
names_py <- etl_func(na) %>% 
  sapply(judge_fun) 
df <- cbind(names=na,pinyin=names_py)  
res <- list2df(df)

library(RMySQL)
con <- dbConnect(MySQL(),
                 dbname = "my_testing", username="root",
                 password="SUN@xjtu2018", host="10.49.10.11", port=3306,
                 client.flag= CLIENT_MULTI_STATEMENTS)
dbGetQuery(con,"truncate table names_dic")
dbWriteTable(con,"names_dic",res,row.names=F,append = TRUE) 
#覆盖：overwrite = TRUE   追加append = TRUE
dbDisconnect(con)
rm(list = ls())
gc()



  


# library(readr)
# write_csv(res,"xjd_paper_names/xjd_authors.csv",col_names = F)
#############                 how use RMySQL
# # 建表
# dbGetQuery(con,"create TABLE `names_dic`(
#            `id` int(11) primary key not null auto_increment,
#            `names` varchar(255),
#            `pinyin` varchar(255),
#            `createtime` datetime DEFAULT CURRENT_TIMESTAMP,  
#            `updatetime` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, 
#            `timestamp` timestamp NOT NULL DEFAULT NOW()
# )")
# # 清空表
# dbGetQuery(con,"truncate table names_dic")
# # 删除表
# dbRemoveTable(con,"names_dic1")

#获取连接信息，查看database下所有表，以及删除testname表 
# summary(conn1)  
# dbGetInfo(conn1)  
# dbListTables(con)        #显示该库下的所有表名

# dbSendQuery(con,'SET NAMES utf8')
# dbGetQuery(con, "SELECT * FROM names_dic limit 5") 
# 
# res1 <- dbSendQuery(con, "SELECT *FROM names_dic")  
# data1 <- dbFetch(res1, n=1) 
#第一次取前2条数据(分批次接着上一次取)，n=-1时是获取所有数据  
# dbClearResult(res1)  



# sql
# create TABLE `names_dic`(
#   `id` int(11) primary key not null auto_increment,
#   `names` varchar(255) NOT NULL,
#   `pinyin` varchar(255) NOT NULL,
#   `timestamp` timestamp NOT NULL DEFAULT NOW()
# )











# library(RMySQL)
# conn1 <- dbConnect(MySQL(),
#                    dbname = "exchange", username="root",
#                    password="SUN@xjtu2018", host="10.49.10.11", port=3306)
# dbSendQuery(conn1,'SET NAMES utf8')
# u_s <- dbReadTable(conn1, "undergraduate_student")
# user <- dbReadTable(conn1, "users_sgdac")
# g_u <- dbReadTable(conn1, "graduate_fosters")
# dbDisconnect(conn1)
# 
# # library(devtools)
# # devtools::install_github("pzhaonet/pinyin")
# 
# library(dplyr)
# s1 <- select(u_s,stu_no, stu_name)
# t <- select(user,no, trueusername) 
# s2 <- select(g_u,student_code, student_name) 
# colnames(s1) =colnames(s2) =colnames(t)  = c('id','nn')
# namess <- rbind(s1,s2,t) %>% arrange(nn)
# chi_sta <- which(namess$nn=='丁一')[1] #获取汉字起始位置，“丁一”
# eng_name <- namess[1:(chi_sta-1),]
# namesc <- namess[chi_sta:nrow(namess),] 
# namesc$nn <- gsub("\\*|\\.|\\?| |","",namesc$nn)
# 
# #  去重名字
# 
# uniq_names <- unique(namesc$nn)
# 
# library(pinyin)
# library(stringr)
# pyfun <- function(x){
#   pinyin(x,method="toneless",sep = " ",multi = TRUE)
# } #,multi = TRUE
# fun1 <- function(x){
#     str_split(x,"\\]|\\[") %>% 
#     unlist %>% 
#     # gsub(" ","",.) %>% 
#     .[.!=""] %>%
#     .[.!=" "] %>%   
#     return()
# }
# 
# fun2 <- function(x){
#   as.list(x) %>% 
#     str_split(" ") %>% 
#     lapply(function(x){x[x!=""]}) %>% 
#     lapply(function(x){unique(x)}) %>% 
#     do.call(expand.grid,.) %>% 
#     return()
# }
# 
# test <- pyfun(paste(uniq_names[50001:50005],collapse = "%")) %>% 
#   str_split("%") %>%  
#   unlist %>% 
#   lapply(fun1) 
#   
#   str_split("]| ") %>% 
#   lapply(function(x){x[x!=""]}) %>% 
#   lapply(fun1) %>% 
#   lapply(fun2)
# 
# 
# fun2(test[[4]])
# 
# 
# do.call(expand.grid, one)   # expand.grid() 多集合进行和组合
# 
# 
# 
# 
# 
# uniq_dt <- cbind(uniq_names,py=test) %>% data.frame()
# 
# 
# #获取4字以上的id起始号：
# pyfun1 <- function(x){pinyin(x,method="toneless",sep = " ")}
# te <- pyfun1(paste(uniq_names,collapse = "%")) %>% 
#   str_split("%") %>%  
#   unlist %>% 
#   str_split(" ") %>% 
#   sapply(function(x){x[x!=""]}) %>% 
#   sapply(., length) %>%
#   sort(decreasing=T)   
# st <- which(te == 4)[1] #616 开始
# 
# # 过滤掉4字以上的名称
# id <- sapply(test, length) %>% 
#   order(decreasing=T) %>% 
#   .[st:length(.)]
# namesc1 <- uniq_dt[id,]
# namesc1$nchar <- nchar(namesc1$uniq_names)
# 
# dt1 <- filter(namesc1,nchar==2)
# pro_names<- function(x){
#   library(Hmisc)
#   a <- capitalize(x[1])
#   b <- capitalize(x[2])
#   n <- paste(a,b,sep = ",")
#   return(n)
# }
# n2 <- sapply(dt1$py,pro_names)
# dt1$last <- n2
# dt2 <- filter(namesc1,nchar==3)
# pro_names1<- function(x){
#   library(Hmisc)
#   a <- capitalize(x[1])
#   b <- capitalize(x[2])
#   n <- paste(a,b,sep = ",")
#   c <- tolower(x[3])
#   n1 <- paste(n,c,sep = "")
#   return(n1)
# }
# n3 <- sapply(dt2$py,pro_names1)
# dt2$last <- n3
# dt3 <- filter(namesc1,nchar==4)
# 
# pro_names2<- function(x){
#   library(Hmisc)
#   a <- capitalize(x[1])
#   b <- capitalize(x[2])
#   n <- paste(a,b,sep = ",")
#   c <- tolower(x[3])
#   d <- tolower(x[4])
#   n2 <- paste(n,c,d,sep = "")
#   return(n2)
# }
# n4 <- sapply(dt3$py,pro_names2)
# dt3$last <- n4
# dtt <- rbind(dt1,dt2,dt3)
# dtt$uniq_names <- unlist(dtt$uniq_names)
# dtt1 <- dtt[,-2]
# write.csv(dtt1,"names.csv",row.names = F)
# 
# 
# 
# 
# 
# library(devtools)
# devtools::install_github("pzhaonet/pinyin",force = TRUE)
# library(pinyin)
# pinyin("春眠不觉晓,处处闻啼鸟",method="toneless",sep = " ",multi = TRUE)
# pinyin("万",method="toneless",sep = " ",multi = TRUE)
# sapply(c('羌笛何须怨杨柳', '春风不度玉门关'),
#        function(x){pinyin(x,method="toneless",sep = " ")})



