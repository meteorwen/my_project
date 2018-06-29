
# https://blog.csdn.net/qq_16365849/article/details/67633034

library(woe)
library(riv)


str(iris)

freqMat <- zerocv.getFreqByColNum(data,col1,col2)
rowSum <- rowSums(freqMat$mat)
colSum <- colSums(freqMat$mat)
IV <- matrix(0,nrow(freqMat$mat),1)
TGI <- matrix(0,nrow(freqMat$mat),1)
WOE <- matrix(0,nrow(freqMat$mat),1)
for(i in 1:nrow(freqMat$mat))
{
  tmp <- freqMat$mat[i,]/colSum
  IV[i] <- zerocv.calculateIV(tmp[1],tmp[2])
  WOE[i] <- zerocv.calculateIV(tmp[1],tmp[2])
  TGI[i] <- tmp[1]/(rowSum[i]/sum(rowSum))*100
}
IVsum <- sum(IV)


cutx2 = c(-Inf, 30,35, 40, 45, 50, 55, 60, 65, 70, 75, Inf)
cutx3 = c(-Inf, 0,1, 3, 5, Inf)
cutx5 <- c(-Inf,1000, 2000, 3000, 4000, 5000, 6000, 7500, 9500, 12000, Inf)
cutx7 <- c(-Inf,0, 1, 3, 5, 10,Inf)
cutx8 <- c(-Inf,0, 1, 3, 5, Inf)
cutx9 <- c(-Inf,0, 1, 3, 5, Inf)
cutx10 <- c(-Inf,0, 1, 2, 3, 5, Inf)


totalgood <-as.numeric(table(train$y))[1]

totalbad <-as.numeric(table(train$y))[2]

getWOE <-function(a, p, q){
  Good <- as.numeric(table(train$y[a>p& a<=q]))[1]
  Bad <- as.numeric(table(train$y[a>p& a<=q]))[2]
  WOE <-log((Bad/totalbad)/(Good/totalgood), base=exp(1))
return(WOE)
}




matr <- matrix(1:12,3,4)

matr*0.2










