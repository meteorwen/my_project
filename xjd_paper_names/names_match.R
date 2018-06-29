#！/usr/bin/Rscript
# match names to wos_id
library(RMySQL)
library(magrittr)
library(dplyr)
conn <- dbConnect(MySQL(),
                   dbname = "my_testing", username="root",
                   password="SUN@xjtu2018", host="10.49.10.11", port=3306,
                   client.flag= CLIENT_MULTI_STATEMENTS)
dbSendQuery(conn,'SET NAMES utf8')
u_s <- dbGetQuery(conn, "select stu_no,stu_name from undergraduate_student;")  #本科生
teacher <- dbGetQuery(conn, "select no,trueusername from users_sgdac;") #教职工
g_s <- dbGetQuery(conn, "select student_code,student_name from 
                  exchange.graduate_fosters where year(start_time) >2012;") #研究生2012后入校
dic <- dbGetQuery(conn, "SELECT names,pinyin from my_testing.names_dic;") #全名称字典
# author <- dbGetQuery(conn,"SELECT a.accession_number as accession_number ,b.full_name as full_name, b.wos_standard_name as wos_standard_name, b.email as email 
# from subject.wos_thesis_thesis as a LEFT JOIN (
#                      SELECT	accession_number,full_name,wos_standard_name,email from subject.wos_thesis_author where full_name != '') as b
#                      on a.accession_number=b.accession_number
#                      where a.publish_year=2018;") #2018年论文
author1 <- dbGetQuery(conn,"SELECT accession_number,full_name,wos_standard_name,email 
FROM subject.wos_thesis_author
                      WHERE accession_number IN 
                      (SELECT accession_number FROM subject.wos_thesis_organization WHERE 
                      organization_enhanced_name = \"Xi'an Jiaotong University;\" 
                      GROUP BY accession_number);")
author1$full_name <- gsub(" ","",author1$full_name) 
# author$full_name <- gsub(" ","",author$full_name) 

temp <- merge(dic,author1,by.x="pinyin",by.y="full_name")
teacher_list <- merge(teacher[(teacher$trueusername %in% temp$names),],
               temp[(temp$names %in% teacher$trueusername),],
               by.x="trueusername",by.y="names") %>% na.omit()
undergraduate_list <- merge(u_s[(u_s$stu_name %in% temp$names),],
                            temp[(temp$names %in% u_s$stu_name),],
                            by.x="stu_name",by.y="names") %>% na.omit()
graduate_list <-merge(g_s[(g_s$student_name %in% temp$names),],
                      temp[(temp$names %in% g_s$student_name),],
                      by.x="student_name",by.y="names") %>% na.omit()
names(teacher_list) = names(graduate_list) = names(undergraduate_list) = c("Chinese_name","University_id",
                                                               "pinyin_name","accession_number",
                                                               "wos_standard_name","email")
dbGetQuery(conn,"truncate table my_testing.teacher_list")
dbGetQuery(conn,"truncate table my_testing.graduate_list")
dbGetQuery(conn,"truncate table my_testing.undergraduate_list")
dbWriteTable(conn,"teacher_list",teacher_list,
             row.names=F,append = T) # append=T追加 
dbWriteTable(conn,"graduate_list",graduate_list,
             row.names=F,append = T)  # overwrite=T 覆盖(主键缺失，需要行号row.names=T)
dbWriteTable(conn,"undergraduate_list",undergraduate_list,
             row.names=F,append = T) 
dbDisconnect(conn)
rm(list=ls())
gc()


# create TABLE `graduate_list`(
#   `id` int(11) primary key not null auto_increment,
#   `Chinese_name` varchar(255) NOT NULL,
#   `University_id` varchar(255) NOT NULL,
#   `pinyin_name` varchar(255) NOT NULL,
#   `accession_number` varchar(255) NOT NULL,
#   `wos_standard_name` varchar(255) NOT NULL,
#   `email` varchar(255) NOT NULL,
#   creat_time timestamp NOT NULL DEFAULT NOW(),
#   update_time  timestamp NOT NULL DEFAULT NOW(),
#   `timestamp` timestamp NOT NULL DEFAULT NOW()
# )







