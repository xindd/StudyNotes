###����

###�����Իع�
fit<-lm(weight ~ height, data = women)#ͨ�������Ԥ������
summary(fit)#���
coefficients(fit)#�ع�ϵ��
plot(women$height, women$weight,
     xlab = 'Height(in inches)',
     ylab = 'Weight(in pounds)')#���ͼ
abline(fit)#�ع���

###��Ԫ���Իع�
states<-as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])#��ȡ����
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)#ͨ���˿ڡ���ä�ʡ����롢��˪����Ԥ��ıɱ��
summary(fit)#���
coefficients(fit)#�ع�ϵ��

###��������ģ��
library(AER)
data("Affairs",package = "AER")
#��affairsת��Ϊ��ֵ������ynaffair
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
Affairs$ynaffair<-factor(Affairs$ynaffair,
                         levels = c(0,1),
                         labels = c('NO','YES'))
table(Affairs$ynaffair)
#Logistic�ع�
fit.full<-glm(ynaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating,
              data = Affairs,
              family = binomial(link = 'logit'))
summary(fit.full)
coefficients(fit.full)

###�����б����
library(kknn)
data(miete)#��ȡ����
#����Ԥ����
library(sampling)
n = round(2/3*nrow(miete)/5)#ѵ����ռ��������2/3�����Լ�ռ��������1/3
sub_train = strata(miete,stratanames = "nmkat",size=rep(n,5),method="srswor")#��nmkat������5���ȼ����ֲ�Σ����зֲ����
#��ȡ����ID_unit����Ӧ����������ѵ��������ɾ������1��3��12
data_train = getdata(miete[,c(-1,-3,-12)],sub_train$ID_unit)
data_test = getdata(miete[,c(-1,-3,-12)],-sub_train$ID_unit)
#�����б����
library(MASS)
fit_lda1 = lda(nmkat~., data_train) #�Թ�ʽ��ʽ���������б�
