
library(RMySQL)
conn <- dbConnect(MySQL(),
                   dbname = "subject", username="root",
                   password="SUN@xjtu2018", host="10.49.10.11", port=3306,
                   client.flag= CLIENT_MULTI_STATEMENTS)
dbSendQuery(conn,'SET NAMES UTF8')
thesis <- dbReadTable(conn, "ei_thesis_thesis")
dbDisconnect(conn)
library(magrittr)
library(dplyr)
temp <- select(thesis,abstract,corresponding_author_email) %>%
  filter(corresponding_author_email !="")

names <- unique(temp$corresponding_author_email)
# 选取前50个作者最多的论文集合 dt
id <- table(temp$corresponding_author_email) %>%
  sort(decreasing = T) %>%
  head(50) %>% 
  names
dt <- temp[(temp$corresponding_author_email %in% id),] %>% 
  arrange(corresponding_author_email)
x <- dt$abstract
y <- dt$corresponding_author_email

library(keras)
use_condaenv("r-tensorflow")
py_discover_config("keras")
install_keras()
library(reticulate)
library(tensorflow)
install_tensorflow()
model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")





