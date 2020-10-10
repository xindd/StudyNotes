###例子

###简单线性回归
fit<-lm(weight ~ height, data = women)#通过身高来预测体重
summary(fit)#结果
coefficients(fit)#回归系数
plot(women$height, women$weight,
     xlab = 'Height(in inches)',
     ylab = 'Weight(in pounds)')#结果图
abline(fit)#回归线

###多元线性回归
states<-as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])#获取数据
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)#通过人口、文盲率、收入、结霜天数预测谋杀率
summary(fit)#结果
coefficients(fit)#回归系数

###广义线性模型
library(AER)
data("Affairs",package = "AER")
#将affairs转化为二值型因子ynaffair
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
Affairs$ynaffair<-factor(Affairs$ynaffair,
                         levels = c(0,1),
                         labels = c('NO','YES'))
table(Affairs$ynaffair)
#Logistic回归
fit.full<-glm(ynaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating,
              data = Affairs,
              family = binomial(link = 'logit'))
summary(fit.full)
coefficients(fit.full)

###线性判别分析
library(kknn)
data(miete)#获取数据
#数据预处理
library(sampling)
n = round(2/3*nrow(miete)/5)#训练集占总样本的2/3，测试集占总样本的1/3
sub_train = strata(miete,stratanames = "nmkat",size=rep(n,5),method="srswor")#以nmkat变量的5个等级划分层次，进行分层抽样
#获取如上ID_unit所对应的样本构成训练集，并删除变量1、3、12
data_train = getdata(miete[,c(-1,-3,-12)],sub_train$ID_unit)
data_test = getdata(miete[,c(-1,-3,-12)],-sub_train$ID_unit)
#线性判别分析
library(MASS)
fit_lda1 = lda(nmkat~., data_train) #以公式格式进行线性判别
