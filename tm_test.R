library(data.table)  #fread() 相当于 read 大幅度提升读取速度3-5倍
# fread("/data/iris1g.txt", stringsAsFactors=T, sep=",",header=T, encoding="UTF-8")
# do.call() 是告诉list一个函数，然后list里的所有元素来执行这个函数。
#####################      do.call  example   #######
# list <- list(matrix(1:25, ncol = 5),
#              matrix(4:28, ncol = 5), 
#              matrix(21:45, ncol=5)) 
# list.sum<-do.call(sum,list)
# list.sum<-do.call(cbind,list)  
library(dplyr)
library(RMySQL)
library(magrittr)
conn <- dbConnect(MySQL(),
                  dbname = "subject", username="root",
                  password="SUN@xjtu2018", host="10.49.10.11", port=3306)

author <- dbReadTable(conn, "wos_thesis_author") %>% as.data.table
thesiss <- dbReadTable(conn, "wos_thesis_thesis") %>% as.data.table

# organization <- dbReadTable(conn, "wos_thesis_organization") %>% as.data.table
# funding <- dbReadTable(conn, "wos_thesis_funding") %>% as.data.table
# enhanced <- dbReadTable(conn, "wos_organizations_enhanced") %>% as.data.table
# publication <- dbReadTable(conn, "wos_thesis_publication") %>% as.data.table
# reference <- dbReadTable(conn, "wos_thesis_reference") %>% as.data.table

dbDisconnect(conn)


# adt <- left_join(author,organization,by="accession_number") %>% 
#         left_join(.,funding,by="accession_number") %>% 
#         left_join(.,publication,by="accession_number") 
# %>%  left_join(.,reference,by="accession_number") 




datas <- filter(author,reprint=='1') %>% 
        merge(thesiss,.,by='accession_number',all.x = T) %>% 
        filter(reprint=='1') %>% 
        select(accession_number,abstract,full_name,
               author_keywords,keywords_plus,research_areas,wos_categories) %>% 
        filter(abstract != '' & full_name != '')
datas$full_name <- gsub('\\(|\\)|;|\\ ','',datas$full_name) 
id <- table(datas$full_name) %>%
  sort(decreasing =T) %>%
  head(50) %>%
  names()
sel_dt <- filter(datas,full_name %in% id) #挑选50个作者列表集合

sel_dt$auth <- sel_dt$full_name %>%  
  as.factor() %>% #改变成数字类型（因子类型的）
  as.integer() %>%   
  as.factor() 

# library(RTextTools)
# data(USCongress)

# 加权平均数：text、author_kw、kwplus、re_areas、categories：
#           (1 : 2 : 2 : 1.5 :1.5) / sum()
library(tm)
text <- VectorSource(sel_dt$abstract) %>% 
  Corpus() %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix(control = list(weighting =function(x)weightTfIdf(x,normalize = TRUE))) %>% 
  removeSparseTerms(0.997) %>% 
  as.matrix()
text1 <- text*0.125
author_kw <- VectorSource(sel_dt$author_keywords) %>% 
  Corpus() %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix(control = list(weighting =function(x)weightTfIdf(x,normalize = TRUE))) %>% 
  removeSparseTerms(0.997) %>% 
  as.matrix()
author_kw1 <-  author_kw * 0.25
kwplus <- VectorSource(sel_dt$keywords_plus) %>% 
  Corpus() %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix(control = list(weighting =function(x)weightTfIdf(x,normalize = TRUE))) %>% 
  removeSparseTerms(0.997) %>% 
  as.matrix()
kwplus1 <- kwplus * 0.25
re_areas <- VectorSource(sel_dt$research_areas) %>% 
  Corpus() %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix(control = list(weighting =function(x)weightTfIdf(x,normalize = TRUE))) %>% 
  removeSparseTerms(0.995) %>%
  as.matrix()
re_areas1 <- re_areas * 0.1875
categories <- VectorSource(sel_dt$wos_categories) %>% 
  Corpus() %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix(control = list(weighting =function(x)weightTfIdf(x,normalize = TRUE))) %>% 
  removeSparseTerms(0.995) %>%
  as.matrix()
categories1 <- categories * 0.1875 

dtt <- cbind(text1,author_kw1,kwplus1,re_areas1,categories1) %>%
  scale(.) %>% 
  cbind(sel_dt$auth,.) %>% 
  def_rename(1) %>% 
  split_dt(0.8)

train <- dtt$train 
test <- dtt$test
formula <- formula(y ~ .)
library(e1071)
train$y <- as.factor(train$y)
svm <- svm(formula,train)
res0  <- predict(svm,test[,2:ncol(test)],type ="class")
test_res0 <- ver_r(res0,test[,1]) 
















abs1 <- text %>% 
  tm_map(stripWhitespace) %>%  #去除多余空格
  tm_map(tolower) %>%          #转换为小写 
  tm_map(removeWords, stopwords("english")) %>% #去除指定文字，文字需要自定义，也可以使用自带函数stopwords()
  tm_map(removeNumbers) %>%    #去除所有数字
  tm_map(stemDocument) %>%     #提取单词词干
  tm_map(removePunctuation)    #去除所有标点符号
# inspect(abs1)


dtm1 <- DocumentTermMatrix(abs1)
# tdm <- TermDocumentMatrix(abs1)
dtm11 <- removeSparseTerms(dtm1, 0.995) %>% as.matrix()

dtm2<-DocumentTermMatrix(abs1,
                          control = list(removePunctuation = T,
                                         stopwords = T,
                  weighting =function(x)weightTfIdf(x,normalize = TRUE)))
dtm21 <- removeSparseTerms(dtm2, 0.999) %>% as.matrix()

# class(tdm)
# inspect(tdm[1:10, 1:3])
source("procedure_fun.R")
dt1 <- cbind(sel_dt$auth,dtm11) %>%
  def_rename(1) %>% 
  split_dt(0.8)
dt2 <- cbind(sel_dt$auth,dtm21) %>% 
  def_rename(1) %>% 
  split_dt(0.8)


#      word frequency
train <- dt1$train 
test <- dt1$test
formula <- formula(y ~ .)
library(e1071)
train$y <- as.factor(train$y)
svm <- svm(formula,train)
res  <- predict(svm,test[,2:ncol(test)],type ="class")
test_res1 <- ver_r(res,test[,1]) 

#      keyword 
train2 <- dt2$train 
train2$y <- as.factor(train2$y)
test2 <- dt2$test
svm <- svm(formula,train2)
res  <- predict(svm,test2[,2:ncol(test2)],type ="class")
test_res2 <- ver_r(res,test2[,1]) 






########    compare with RTextTools 

library(RTextTools)
doc_matrix<-create_matrix(sel_dt$abstract,language = "english",
                          removeNumbers = TRUE,
                          stemWords = TRUE,
                          removeSparseTerms = .998) #删除稀疏比率90%


container <- create_container(doc_matrix,sel_dt$auth[1:(nrow(doc_matrix))],
                              trainSize = 1:(round(nrow(doc_matrix)*0.8)),
                              testSize = (round(nrow(doc_matrix)*0.8+1)):(nrow(doc_matrix)),
                              virgin = FALSE)
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
test_res3 <- ver_r(SVM_CLASSIFY$SVM_LABEL %>% as.numeric(),
      sel_dt$auth[round(nrow(doc_matrix)*0.8+1):nrow(doc_matrix)] %>% as.numeric())


fa_res <- cbind(test_res1,test_res2,test_res3)








# freq <- rowSums(as.matrix(tdm))
# head(sort(freq, decreasing = T))
# head(table(freq),10)     #词频的分布
# dim(tdm)
# tdms <- removeSparseTerms(tdm, 0.95)
# #中的第二个参数是区间为(0,1)的小数，小数越小，保留的单词量越少
# dim(tdms)
# # 词汇的相关度和词频对语料库进行挖掘
# findFreqTerms(tdms, lowfreq = 100)
# findAssocs(tdms, "paper", corlimit = 0.4)
# 
# dtms <- t(as.matrix(tdms))
# dtms[,which(colnames(dtms) %in% c("method","propos","show"))]






























