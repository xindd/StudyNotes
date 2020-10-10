install.packages("e1071")
library(e1071)
##################################################################
##################################################################

#确定将要使用的核函数
######kernel :"linear","polynomial","radial","sigmoid"
##################################################################
data(iris)
#建立svm模型
#提取iris数据中除第5列以外的数据作为特征变量
x <- iris[,-5]
#提取iris数据中第5列数据作为结果变量
y <- iris[,5]
#建立svm模型
model <- svm(x,y,kernel = "sigmoid", gamma = if(is.vector(x)) 1 else 1/ncol(x))

#结果分析
summary(model)

#预测判别
#确认需要继续预测的样本特征矩阵
x <- iris[,1:4]
#根据模型model对x数据进行预测
pred <- predict(model, x)
#随机挑选8个预测结果进行展示
print(pred[sample(1:150, 8)])
#模型预测精度展示
table(pred, y)

####可视化
par(mfrow=c(1,1))
plot(cmdscale(dist(iris[,-5])),col=c("blue","green","red")[as.integer(iris[,5])],
pch=c("o","+")[1:150 %in% model$index + 1])
legend(2,-0.8,c("setosa","versicolor","virginica"),col = c("blue","green","red"),lty=1)

######图中"+"表示的是支持向量，“0”表示的是普通样本点

#对模型类别关于模型中任意两个特征向量的变动过程进行绘图
data(iris)
model <- svm(Species~., data = iris)
plot(model, iris, Petal.Width~Petal.Length,fill = FALSE,symbolPalette = c("blue","green","pink"),svSymbol = "+")
legend(1,2.5,c("setosa","versicolor","virginica"),col = c("blue","green","pink"),lty=1)
##由于类别setosa同其他两个类别相差很大，所以我们可以考虑降低类别setosa在模型中的额比重，而提高另外两个类别的比重，即适当牺牲类别setosa的精度来提高其他两个类别的精度

##优化模型
#确定模型各个类别的比重为1:1:1
wts <- c(1,1,1)
#确定各个比重对应的类别
names(wts) <- c("setosa","versicolor","virginica")
#建立模型
model1 <- svm(x,y,class.weights = wts)

#将这两种类别的比重扩大100倍
wts <- c(1,100,100)
names(wts) <- c("setosa","versicolor","virginica")
model2 <- svm(x,y,class.weights = wts)
pred2 <- predict(model2,x)
table(pred2,y)
plot(cmdscale(dist(iris[,-5])),col=c("blue","green","red")[as.integer(iris[,5])],
pch=c("o","+")[1:150 %in% model$index + 1])
legend(2,-0.8,c("setosa","versicolor","virginica"),col = c("blue","green","red"),lty=1)

##提高类别versicolor和virginica的比重能对模型的预测精度产生影响，并且能产生正向影响
wts <- c(1,500,500)
names(wts) <- c("setosa","versicolor","virginica")
model3 <- svm(x,y,class.weights = wts)
pred3 <- predict(model3,x)
table(pred3,y)
##通过对权重的调整之后，我们建立的支持向量机模型能够将所有样本全部预测正确




