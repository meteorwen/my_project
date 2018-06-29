#！/usr/bin/Rscript
library(RMySQL)
conn1 <- dbConnect(MySQL(),
                   dbname = "exchange", username="root",
                   password="SUN@xjtu2018", host="10.49.10.11", port=3306,
                   client.flag= CLIENT_MULTI_STATEMENTS)
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

uniq_names <- unique(namesc_etl)

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
dbDisconnect(con)
rm(list = ls())
gc()



