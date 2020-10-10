library(survival)
addicts<-read.table('C:/wx/exp.txt',sep=' ', header=TRUE)

g1 = addicts['ENSG00000104518',]
g2 = addicts['ENSG00000073605',]
g3 = addicts['ENSG00000137752',]

medianG1 = median(as.numeric(g1))
medianG2 = median(as.numeric(g2))
medianG3 = median(as.numeric(g3))
g1[which(g1<= medianG1)] = 1
g1[which(g1> medianG1)] = 0

g2[which(g2<= medianG2)] = 1
g2[which(g2> medianG2)] = 0

g3[which(g3<= medianG3)] = 1
g3[which(g3> medianG3)] = 0

coln1 = gsub('\\.','-', colnames(g1))
coln2 = gsub('\\.','-', colnames(g2))
coln3 = gsub('\\.','-', colnames(g3))

follow_up = read.table('c:/wx/follow_up.txt', sep=' ')
rownames(follow_up) = follow_up[,1]
follow_up[coln ,'label'] = as.numeric(g1)
y<-Surv(follow_up$V2,follow_up$V3)
kmfit2<-survfit(y~label, data=follow_up) #clinic为分组标签
plot(kmfit2) 
plot(kmfit2, main="Kaplan-Meier curves of two groups", xlab="Time(days)",  ylab="Overall survival", mark=3,mark.time=T,col=c('red','blue'))
#log-rank
r<-survdiff(formula = y~  label,  rho = 0,data=follow_up) 
p<-1 - pchisq(r$chisq, length(r$n) - 1)
#############
follow_up[coln ,'label'] = as.numeric(g2)
y<-Surv(follow_up$V2,follow_up$V3)
kmfit2<-survfit(y~label, data=follow_up) #clinic为分组标签
plot(kmfit2) 
plot(kmfit2, main="Kaplan-Meier curves of two groups", xlab="Time(days)",  ylab="Overall survival", mark=3,mark.time=T,col=c('red','blue'))
#log-rank
r<-survdiff(formula = y~  label,  rho = 0,data=follow_up) 
p<-1 - pchisq(r$chisq, length(r$n) - 1)
#############
follow_up[coln ,'label'] = as.numeric(g3)
y<-Surv(follow_up$V2,follow_up$V3)
kmfit2<-survfit(y~label, data=follow_up) #clinic为分组标签
plot(kmfit2) 
plot(kmfit2, main="Kaplan-Meier curves of two groups", xlab="Time(days)",  ylab="Overall survival", mark=3,mark.time=T,col=c('red','blue'))
#log-rank
r<-survdiff(formula = y~  label,  rho = 0,data=follow_up) 
p<-1 - pchisq(r$chisq, length(r$n) - 1)
