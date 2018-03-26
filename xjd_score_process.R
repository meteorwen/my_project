path <- "dt/"
hz <-  "*.csv"
require(magrittr)
require(readr)
library(tidyr)
readff <- function(path,hz){
    list.files(path=path,pattern=hz) %>% 
    paste0(path,.) %>% 
    lapply(function(x) read_csv(x, col_names = F,na = c("\\N", "NA"))) %>% 
    do.call("rbind", .) %>% 
    return()
}

system.time(
  df <- readff(path,hz)
)
library(dplyr)
meta <- df %>% 
        select(X2,X6,X14,X16,X26) 
names(meta) <- c('s_id','frofession','q_id','course','score')
dt<-na.omit(meta)


#抽取同专业同学期的学生对应的成绩列表(208专业，27个学期)生成5616个list


frofession_id <- dt$frofession %>% unique() %>% sort()
q_id <- dt$q_id %>% unique() %>% sort()

# 数据整理成同专业 同学期的list
# length(frofession_id)
# length(q_id)
# 
# temp <- list()
# for (i in 1:2){
#   for(j in 1:3){
#     temp[[i*j]] <- dt %>% 
#       filter(frofession== frofession_id[i]& q_id==q_id[j]) %>% 
#       #.[,c(1,4,5)] %>% 
#       spread(course,score) #%>% 
#       # t
#   }
# }
# 
# tem <- list()
# for (i in 1:14){
#     tem[[i]] <- dt %>% 
#       filter(frofession== frofession_id[i]) %>% 
#       spread(course,score)
# }
# spread 用法
# tem[[14]] %>% group_by(course) %>% mutate(id=1:n()) %>% spread(course,score)
# 需要对转换的目标列进行分组加key——id，在进行转换 才可以 

tem <-  lapply(frofession_id,function(x){  #替代上面的循环
    filter(dt,frofession== x) %>% 
    group_by(course) %>% 
    mutate(id=1:n()) %>% 
    spread(course,score) 
})
#  去除那些专业考试不足30次记录的专业
dtt1  <- tem[!(sapply(tem,nrow) <30)]
dtt <-list()
for (i in 1:length(tem)){
  if( tem[[i]] %>% nrow() < 30){
    dtt[[i]] <- NA
  }else{
    dtt[[i]] <- tem[[i]]
  }
}
dtt1 <- dtt[!is.na(dtt)]

####       
# index <- dtt1[[1]][,5] %>% is.na() 
# dtt1[[1]][!index,] %>% 
#   t %>% 
#   .[!apply(.,1,function(x){all(is.na(x))}),] %>% 
#   t
# 按照所有专业，过滤同专业内多余课程
index <- list()
course_list <- list()
course_l <- list()
for (i in 1:length(dtt1)){
  for (j in 5:ncol(dtt1[[i]])){
    index[[j-4]] <- dtt1[[i]][,j] %>% is.na() %>% as.vector()
    course_list[[j-4]] <- dtt1[[i]][!index[[j-4]],] %>% 
      t %>% 
      .[!apply(.,1,function(x){all(is.na(x))}),] %>% 
      t
    course_l[[i]] <- course_list
  }
}
# 同专业，过滤掉少于2个课程的记录,且 少于30次考试记录的(非一个年级的)
course_last <- list()
for (i in 1:length(course_l)){
  course_last[[i]] <-  course_l[[i]] %>% 
    .[!(sapply(., ncol) <6)] %>% 
    .[!(sapply(., nrow) <30)] 
}

# 复合list合并为一个长list
course_last1 <- list()
for (i in 1:length(course_last)){
  for(j in 1:length(course_last[[i]])){
    course_last1[[i*j]] <-  course_last[[i]][[j]] %>% as.data.frame()
  }
}
# 将只有一个学期（同专业内）剔除掉。
course_last2 <- list()
for(i in 1:length(course_last1)){
  if (course_last1[[i]]$q_id %>% unique() %>% length() <2){
    course_last2[[i]] <- NA
  } else{
    course_last2[[i]] <- course_last1[[i]]
  }
}
course_last2 <- course_last2[!is.na(course_last2)]
#得到每个训练的集合
course_last2 %>% sapply(dim) %>% t

# 获得最后一个学期的 期号 xq_max
xq_max <- vector()
for (i in 1: length(course_last2)){
  xq_max[i] <- course_last2[[i]]$q_id  %>% 
    table() %>% 
    names %>% 
    as.integer() %>% 
    max()
}
# 得到是否是最后一个学期的逻辑向量cf_id
cf_id <- list()
for(i in 1:length(course_last2)){
  cf_id[[i]] <- course_last2[[i]]$q_id %in% xq_max[i]
}
# 将所有集合按照得到的逻辑向量拆分训练集与测试集training,testing
training <-list()
testing <- list()
for (i in 1:length(course_last2)){
  training[[i]] <- course_last2[[i]][!cf_id[[i]],]
  testing [[i]] <- course_last2[[i]][cf_id[[i]],]
}

testing1 <- list()
training1 <- list()
for (i in 1:length(testing)){
  if(testing[[i]] %>% na.omit %>% nrow ==0){
    testing1[[i]] = NA
    training1[[i]] = NA
  } else {
    testing1[[i]] = testing[[i]]
    training1[[i]] = training[[i]]
}
}
testing1 <- testing1[!is.na(testing1)]
training1 <- training1[!is.na(training1)]


training1 %>% sapply(dim) %>% t

aa <- testing1[[1]][,-c(1:4)]
names(aa) <- c("x1","y")
lm.sol<-lm(体育4~.,data=testing1[[1]])



df <- data.frame(a =sample(c(1:100),5), b = sample(c(10000:100000),5))
df <- data.frame(a =sample(c(1:100),5), 
                 b = c(sample(c(10000:100000),4),NA),
                 c = c(rep(NA,3),55,99))

df1 <- df %>% scale(F) %>% as.data.frame()

lm.sol<-lm(a~.,data=df1)













# course_list1 <- list()
# for (i in 1:length(course_list)){
#   if ((course_list[[i]] %>% ncol() < 6) && (course_list[[i]] %>% nrow() < 30)){
#     course_list1[[i]] <- NA
#   }else{
#     course_list1[[i]] <- course_list[[i]]
#   }
# }
course_list1 <- course_list1[!is.na(course_list1)]
course_list1[[1]]
# 同专业，过滤掉少于2个课程的记录,且 少于30次考试记录的(非一个年级的)
course_list2 <- course_list[!(sapply(course_list, ncol) <6)] %>% 
.[!(sapply(., nrow) <30)]
sapply(course_list2, dim)

course_list2[[13]]











  # complete.cases() 返回逻辑向量行号是否是有缺失值T没有F有缺失值
  # na.omit() 返回实际值，并去除掉了该行内含有（1orN）缺失的值

row_last <- row_100[-which(apply(row_100,1,function(x)all(is.na(x)))),] 
  
row_100[-which(apply(row_100,1,function(x)all(is.na(x)))),
        -which(apply(row_100,2,function(x)all(is.na(x))))]
  
row.names(row_100) =NULL
row_100 <- data.frame(row_100)
apply(row_100,1,function(x){all(is.na(x))})   
row_100[!apply(row_100,1,function(x){all(is.na(x))}),]
row_100[-which(apply(row_100,1,function(x)all(is.na(x)))),]
all(is.na(row_100[c(5),]))   
# 接受0，1 / F，T这样的逻辑向量，判断是否全为F/0，则返回F









# dt1 <- dt %>% group_by(frofession,q_id,course)
# dt2 <- dt %>% arrange(s_id,frofession,q_id,course)
# library(tidyr)
# dt_wide <- spread(dt,course,score)

# dt %>% filter(s_id == "06203007") %>% data.frame() %>% 
#   spread(course,score)
# head(dt,10000) %>%  spread(s_id,score)

# 得到所有专业的list值，长度就是专业个数,course_w转宽的课程list
fro<- dt$frofession %>% unique()
fro_list <- list()

for(i in 1:length(fro) ){
  fro_list[[i]] <- filter(dt,frofession ==fro[i]) %>% 
                          .[,-2] 
                 # spread(course,score)
}
# 过滤只有一个学期的专业
xq_list <- list()
for(i in 1:length(fro_list)){
  if(fro_list[[i]]$q_id %>% unique() %>% length() <2){
    xq_list[[i]] <- NA
  } else {
    xq_list[[i]] <-  fro_list[[i]] 
  }
}
xq_list <- xq_list[!is.na(xq_list)]

#各专业里面最后一个学期list,为训练集做准备
xq_last <- list()
for(i in 1 :length(xq_list)){
  xq_last[[i]] <- xq_list[[i]]$q_id %>% unique() %>% max()
}
# 过滤后剩余145个符合的专业，并将最后一个学期当做test
id <- list()
test <- list()
train <- list()
for(i in 1 :length(xq_list)){
  id[[i]] <-  xq_list[[i]]$q_id  %in% xq_last[[i]]
  train[[i]] <- xq_list[[i]][!id[[i]],] 
  test[[i]] <- xq_list[[i]][id[[i]],]
}
#




# 要预测课程df
# course_w[[i]] %>% .[,1:3] %>% na.omit

# course_w[[i]][!is.na(course_w[[i]][,3]),]
course_list <- list()
course_list[[1]] <- NA
course_list[[2]] <- NA
course_num <- vector()
# 在一个专业里,过滤该n个课程小于30次考试的记录的课程
i =1
for(j in 3:ncol(course_w[[i]])){
  if (course_w[[i]][!is.na(course_w[[i]][,j]),] %>% nrow() <30){
    course_list[[j]] <- NA
  } else{
    course_list[[j]] <- course_w[[i]][!is.na(course_w[[i]][,j]),]
  }
}

course_list1 <- course_list[!is.na(course_list)]
#  过滤只有一个学期的
course_list2 <- list()
for (i in 1:length(course_list1)){
  if(course_list1[[i]]$q_id %>%  unique() %>% length()<2){
    course_list2[[i]] <-  NA
  } else {
    course_list2[[i]] <- course_list1[[i]]
  }
}
course_list2 <- course_list2[!is.na(course_list2)]

course_list2[[1]] %>% t %>% na.omit %>% t() %>% dim



















# dsg_na <- function(df){apply(df,2,function(x){sum(is.na(x)) %>% 
#     round(2) %>% 
#     return()})} #count how many NA
# dsg_na(meta)
# 
# group_by(dt1,course,frofession) %>% 
#   summarise(count=n()) %>% 
#   arrange(desc(count))
# 
# group_by(dt1, frofession,q_id,course) %>% 
#   summarise(count=n()) %>% 
#   arrange(count)
# 体育4 体育2


dl <- dt1 %>% 
  group_by(frofession,q_id,course) %>% 
  filter(course=='电力电子技术',frofession=='0421')
t_index <- unique(dl$q_id) %>% sort %>% .[-1]

 # 许多科目不具备训练条件，历史训练集太少。


# for(i in fro[1:3]){
#     for(j in course){
#     temp[[j]] <- dt %>% 
#       filter(frofession== i & course==j)
#   }
#   tem[[i]] <- temp
#   print(i)
# }
rm(list = ls()[!ls()=='dt']) #除dt内存 不清空，其余都清除内存空间
# 按照两列（专业，课程）进行抽取学生成绩
#（同时满足以上条件小于30条的记录去除）
fro <- unique(dt$frofession)
course <- unique(dt$course)
tem <- list()
temp <- list()

system.time(
for(i in fro){
  for(j in course){
    if(filter(dt,frofession== i & course==j) %>% nrow < 30){
      temp[[j]] <- NULL
    } else{
      temp[[j]] <- dt %>% 
        filter(frofession== i & course==j)
    }
      }
  tem[[i]] <- temp
  print(i)
}
)
library(readr)
saveRDS(tem,"dt/train.rds")
dt <- readRDS("dt/train.rds")
# system.time(for(i in 1:208){
#   for(j in 1:1281){
#     j=0+j
#   }
#   i=i*j
#   print(i)
# }
# )
# dt1 <- c(tem[[1]],tem[[2]],tem[[3]])
# 将三维list转为2维list进行合并
dt2 <- list()
for (i in 1:208){
  dt2 <- c(dt2,dt[[i]])
}
# 获取list的内部df的行数排序索引
train_num <- vector()
for (i in 1:length(dt2)){
  train_num[i] <-  dt2[[i]] %>% nrow()
}
train_index <- order(train_num,decreasing =T) #T大至小返回的索引下标（sort返回数值）
#对list的进行排序
dt3 <-list()
for (i in 1:length(train_index)){
  dt3[[i]] <-dt2[[train_index[i]]]
} 

# 了解各学期，同专业、课程的成绩，得到学期数目
for (i in 1: length(dt3)){
  len_num[i] <- dt3[[i]]$q_id  %>% table() %>% length()
}
#去除掉只有1个学期的同专业、课程的成绩
dt4 <- list()
for (i in 1:length(len_num) ){
  if(len_num[i] ==1){
    dt4[[i]] = NA
  } else{
    dt4[[i]]=dt3[[i]]
  }
}
dt4 <- dt4[!is.na(dt4)]
# 获得最后一个学期的 期号 xq_max
for (i in 1: length(dt4)){
  xq_max[i] <- dt4[[i]]$q_id  %>% 
    table() %>% 
    names %>% 
    as.integer() %>% 
    max()
}
# 得到是否是最后一个学期的逻辑向量cf_id
cf_id <- list()
for(i in 1:length(dt4)){
  cf_id[[i]] <- dt4[[i]]$q_id %in% xq_max[i]
}

# 将所有集合按照得到的逻辑向量拆分训练集与测试集training,testing
training <-list()
testing <- list()
for (i in 1:length(dt4)){
  training[[i]] <- dt4[[i]][!cf_id[[i]],]
  testing [[i]] <- dt4[[i]][cf_id[[i]],]
}


# 查看训练集行数，去除行数<10,返回向量
train_row <- vector()
for (i in 1:length(training)){
  train_row[i] <-  training[[i]] %>% nrow()
}
# 获取训练集合小于10的逻辑向量 index
index <- vector()
for(i in 1:length(train_row)){
  if (train_row[i] < 10){
    index[i] <- FALSE
  }else{
    index[i] <- TRUE
  }
}
train <- training[index]
test <- testing[index]

xjd <- list(train=train,test=test)
saveRDS(xjd,"dt/xjd_score_predict.rds")







#############################     model    ####################################
dt <- readRDS("dt/xjd_score_predict.rds")

train <- dt$train[[1]]
test <- dt$test[[1]]


blslx_model <- lm()
































