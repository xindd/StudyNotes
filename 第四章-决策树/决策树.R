#### -- ctree -- ####
#install.packages("party")
#ctree(formula, data) # 决策树语法结构
## 输入数据
# 导入决策树的包
library(party)
#head(readingSkills)
# 构建输入数据集
input.dat <- readingSkills[c(1:105),]

# 定义输出图片名字
#png(file = "decision_tree.png")

# 创建决策树
  output.tree <- ctree(
  nativeSpeaker ~ age + shoeSize + score, 
  data = input.dat)

# 画决策树
plot(output.tree)

# 储存图片
#dev.off()


#### -- rpart -- ####
# 导入数据C50包
install.packages("C50")
library(C50)
## 输入数据
data(churn)
churnTrain<-churnTrain[,!names(churnTrain)%in%c("state","area_code","account_length")] # 删除多余特征
# 划分训练集和测试集
set.seed(1234)
ind=sample(2,nrow(churnTrain),replace = T,prob = c(0.7,0.3))
traintset<-churnTrain[ind==1,]
testset<-churnTrain[ind==2,]

## 使用递归分割树建立分类模型
# 导入决策树包
#install.packages("e1071")
library(rpart)
library(caret)
library(e1071)
churn.rp<-rpart(churn~.,data = traintset)
# 检查复杂性参数
printcp(churn.rp)
# 绘制复杂性参数
plotcp(churn.rp)

# 递归分割树可视化
plot(churn.rp,margin = 0.1)
text(churn.rp,all = T,use.n = T)

plot(churn.rp,uniform = T,branch = 0.6,margin = 0.1)
text(churn.rp,all = T,use.n = T)

## 递归分割树剪枝
min(churn.rp$cptable[,"xerror"])
which.min(churn.rp$cptable[,"xerror"])
churn.cp<-churn.rp$cptable[7,"CP"]
prune.tree<-prune(churn.rp,cp=churn.cp)
plot(prune.tree,margin = 0.1)
text(prune.tree,all = T,use.n = T)

## 评估递归分类树的预后能力
predictions<-predict(prune.tree,testset,type = "class")
table(testset$churn,predictions)
confusionMatrix(table(testset$churn,predictions))




