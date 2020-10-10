install.packages("plyr")
install.packages("reshape2")

library(plyr)
library(reshape2)#数据整合和重塑
class_prob <- function(trainData, strClassName){
  length.train <- nrow(trainData)
  dTemp <- ddply(trainData, strClassName, "nrow")
  dTemp <- ddply(dTemp, strClassName, mutate, prob = nrow/length.train)
  dTemp[,-2]
}
feature_class_prob <- function(trainData, strClassName){
  data.melt <- melt(trainData,id=c(strClassName))
  aa <- ddply(data.melt, c(strClassName,"variable","value"), "nrow")
  bb <- ddply(aa, c(strClassName,"variable"), mutate, sum = sum(nrow), prob = nrow/sum)
  colnames(bb) <- c("class.name",
                    "feature.name",
                    "feature.value",
                    "feature.nrow",
                    "feature.sum",
                    "prob")
  bb[,c(1,2,3,6)]
}
pre_class <- function(oneObs, pc,pfc){
  colnames(oneObs) <- c("feature.name", "feature.value")
  colnames(pc) <- c("class.name","prob")
  colnames(pfc) <- c("class.name","feature.name","feature.value","prob")
  feature.all <- join(oneObs,pfc,by=c("feature.name","feature.value"),type="inner")
  feature.prob <- ddply(feature.all,.(class.name),summarize,prob_fea=prod(prob))  #prod为连乘函数
  class.all <- join(feature.prob,pc,by="class.name",type="inner")
  ddply(class.all,.(class.name),mutate,pre_prob=prob_fea*prob)[,c(1,4)]
}

trainData <-data.frame(
  color=c("green","black","black","white","black","white"),
  tuber=c("crouch","crouch","crouch","little","little","crouch"),
  lines=c("clear","clear","clear","Slightly","Slightly","blur"),
  taste=c("good","good","bad","bad","bad","bad")
  )
 length.train <- nrow(trainData)
head(trainData)
#待预测样本
oneObs<-data.frame(
  feature.name =c("color", "tuber", " lines"),
  feature.value =c("green","crouch","Slightly")
)

#预测分类
pc <- class_prob(trainData,"taste")
pfc <- feature_class_prob(trainData,"taste")
pre_class(oneObs, pc,pfc)