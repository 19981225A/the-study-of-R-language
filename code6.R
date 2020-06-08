#1. Logistic回归
marriage <- read.csv("marriage.csv",header=T,sep=",")
fit.full <- glm(yn~gender+age+yearsmarried+children+religiousness+education+occupation+rating,data=marriage,family=binomial())
summary(fit.full)
fit.reduced <- glm(yn~age+yearsmarried+religiousness+rating,data=marriage,family=binomial())
summary(fit.reduced)
AIC(fit.reduced,fit.full)
coef(fit.reduced)

testdata <- data.frame(rating=c(1,2,3,4,5),age=mean(marriage$age),yearsmarried=mean(marriage$yearsmarried),religiousness=mean(marriage$religiousness))
testdata
testdata$prob <- predict(fit.reduced,newdata=testdata,type="response")
testdata

testdata <- data.frame(rating=mean(marriage$rating),age=seq(17,57,10),yearsmarried=mean(marriage$yearsmarried),religiousness=mean(marriage$religiousness))
testdata
testdata$prob <- predict(fit.reduced,newdata=testdata,type="response")
testdata

fit <- glm(yn~age+yearsmarried+religiousness+rating,data=marriage,family=binomial())

#2. 泊松回归
install.packages("robust")
library("robust")
data(breslow.dat,package="robust")
names(breslow.dat)
summary(breslow.dat[c(6,7,8,10)])
fit <- glm(sumY~Base+Age+Trt,data=breslow.dat,family=poisson())
summary(fit)
coef(fit)

install.packages("qcc")
library("qcc")
qcc.overdispersion.test(breslow.dat$sumY,type="poisson")