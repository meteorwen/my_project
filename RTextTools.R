library(RTextTools)
data(USCongress)

# 创建一个文档-词项矩阵

doc_matrix <- create_matrix(USCongress$text,
                          language ="english",
                          removeNumbers= TRUE,
                          stemWords = TRUE,
                          removeSparseTerms = .998)
# 创建容器
container <- create_container(doc_matrix,USCongress$major,
                            trainSize = 1:4000,testSize = 4001:4449,
                            virgin = FALSE)
# 这里，virgin =参数的设置影响到后续模型结果的分析解读。
# virgin = FALSE意味着告诉R,我们的测试集是有真实的类别标签的。

# 创建好文档-词项矩阵以后，下一步要做的就是对矩阵进行训练集/测试集的划分了。
# RTextTools中的容器（Container）概念，使得人们不必两次读入数据，
# 而将训练集和测试集一并读入，在容器内做区分即可。
# 既然我们是有监督的分类算法实现，当然不能忘了指定因变量（即类别标签）。
# 在我们的测试数据集中，类别标签为USCongress$major。
# 
# 注意：类别标签一定要为数值型！




length(unique(USCongress$major))   # 看看类别个数(***)


# train_model(container, 
#             algorithm=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"),
#             method = "C-classification",
#             cross = 0, cost = 100, kernel = "radial", maxitboost = 100,
#             maxitglm = 10^5, size = 1, maxitnnet = 1000, MaxNWts = 10000,
#             rang = 0.1, decay = 5e-04, trace=FALSE, ntree = 200,
#             l1_regularizer = 0, l2_regularizer = 0, use_sgd = FALSE,
#             set_heldout = 0, verbose = FALSE,...)

## 训练模型
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

## 使用训练好的模型进行文本分类
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)

## 测试集的分类结果的四种解读：从标签出发；从算法对比出发；从角度文档出发；以及整体评价。
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
                                    RF_CLASSIFY, GLMNET_CLASSIFY, TREE_CLASSIFY,
                                    NNET_CLASSIFY,MAXENT_CLASSIFY))

summary(analytics)


##  交叉验证
SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")









