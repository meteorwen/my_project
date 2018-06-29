library(RMySQL)
conn <- dbConnect(MySQL(),
                  dbname = "subject", username="root",
                  password="SUN@xjtu2018", host="10.49.10.11", port=3306)

auth <- dbReadTable(conn, "wos_thesis_author")
thesis <- dbReadTable(conn, "wos_thesis_thesis")

dbDisconnect(conn)




# shell :
  # # 查看物理CPU的个数
  # cat /proc/cpuinfo |grep "physical id"|sort |uniq|wc -l
  # # 查看逻辑CPU的个数
  # cat /proc/cpuinfo |grep "processor"|wc -l 
  # # 查看CPU是几核
  # cat /proc/cpuinfo |grep "cores"|uniq  
library(doParallel)
cores <- detectCores(logical=F)   # Real physical cores in the computer
cl <- makeCluster(cores)
registerDoParallel(cl, cores=cores)


library(foreach)
system.time(
  stu_no <- foreach(x=u_s$stu_name[1:10],
                    .combine=c)            %dopar%
    pyfun
)
stopCluster(cl)
stopImplicitCluster()



###  etl dt

library(dplyr)
reprint <- filter(auth,reprint=='1')  
tem <- merge(thesis,reprint,by='accession_number',all.x = T)
temp <- filter(tem,reprint=='1')
dt <- select(temp,accession_number,abstract,full_name)
dt1 <- filter(dt,abstract != '' & full_name != '') #去除空的摘要和空人名条数

id <- table(dt1$full_name) %>% sort(decreasing =T) %>% head(50) %>% names()
dt2 <- filter(dt1,full_name %in% id) #挑选50个作者列表集合

dt2$auth <- dt2$full_name %>%  
  gsub('\\(|\\)|;|\\ ','',.) %>% 
  as.factor() %>% #改变成数字类型（因子类型的）
  as.integer() # %>%   as.factor()     # y最后必须是factor 类型


dt2$abstract <- as.factor(dt2$abstract)

library(RTextTools)
doc_matrix<-create_matrix(dt2$abstract,language = "english",
                          removeNumbers = TRUE,
                          stemWords = TRUE,
                          removeSparseTerms = .998) #删除稀疏比率90%


container <- create_container(doc_matrix,dt2$auth[1:(nrow(doc_matrix))],
                              trainSize = 1:(round(nrow(doc_matrix)*0.8)),
                              testSize = (round(nrow(doc_matrix)*0.8+1)):(nrow(doc_matrix)),
                              virgin = FALSE)


SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
source("/home/apple/Rspace/procedure_fun.R")
ver_r(dt2$auth[1538:1921],SVM_CLASSIFY$SVM_LABEL)

RF <- train_model(container,"RF")
RF_CLASSIFY <- classify_model(container, RF)
ver_r(dt2$auth[1538:1921],RF_CLASSIFY$FORESTS_LABEL)

BOOSTING <- train_model(container,"BOOSTING")
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
ver_r(dt2$auth[1538:1921],BOOSTING_CLASSIFY$LOGITBOOST_LABEL)

test_lab <- dt$auth[(round(nrow(doc_matrix)*0.8+1)):(nrow(doc_matrix))]
ver_r(test_lab,RF_CLASSIFY$FORESTS_LABEL)
ver_r(test_lab,SVM_CLASSIFY$SVM_LABEL)
analytics <- create_analytics(container,BOOSTING_CLASSIFY)
summary(analytics)





























