library(readr)
data <- read_delim("dt/XJTU.txt",col_names = TRUE,delim = "~")


data1 <- read_delim("dt/infoFile.txt",
                    col_names = TRUE,delim = "~")

dtt <- rbind(data,data1)

index <- unique(dtt$Accession_Number) %>% 
          grep("WOS:",.)
dtt[index,] %>% 
  write.csv("dt/paper.csv",sep = "~", fileEncoding="GBK")
names(dtt) <-c('标题',
               '作者',
               '日刊',
               '卷数',
               '页数',
               'DOI',
               '出版年份',
               '关键词',
               '版权作者',
               '版权作者地址',
               '地址',
               '供资',
               '研究领域',
               'WOS类别',
               '文件类型',
               '语言',
               '加入号',
               'ISSN',
               'eISSN',
               'IDS号',
               '引用WOS中的参考文献',
               '大学代码')

ddt <- dtt[index,]
library(stringr)
library(magrittr)

author <- lapply(ddt$作者, function(x){
  x %>%  
    strsplit(split=";") %>% 
    unlist %>% 
    word(1,sep = fixed("["))   #用于从语句中提取单词(字符串)
})

          
o_author <- ddt$版权作者  %>% 
            strsplit(split=" \\((reprint author))") %>% # 正则表达式，R语言\\转义
            unlist()
paper_jd <- list(ddt,o_author=o_author,author=author)  
saveRDS(paper_jd,"dt/paper_jd.rds")
















