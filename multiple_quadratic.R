#install ggiraphExtra package
library(ggplot2)
library(plyr)
library(ggiraphExtra)

report_r=read.csv("D://sem3 materials//labs//R programming//lab_project//water_data.csv")
report_r=report_r[,c(4:11)]
report_r=na.omit(report_r)

#DO
lm1<-lm(DO~Temp+BOD,data=report_r)
summary(lm1)
a=data.frame(Temp=29.2,BOD=1.5)
result=predict(lm1,a)
result[1]
ggPredict(lm1,se=TRUE,interactive=TRUE)


lm_do=lm(DO~.,data=report_r)
summary(lm_do)
a=data.frame(Temp=29.2,PH=6.3,CONDUCTIVITY=100,BOD=1.5,NITRATE=0.1,F_COLIFORM=7942,T_COLIFORM=13575)
result=predict(lm_do,a)
result[1]


#CONDUCTIVITY
#linear
lm2<-lm(report_r$CONDUCTIVITY~report_r$PH)
summary(lm2)
a=data.frame(PH=7.5)
result=predict(lm2,a)
result[1]
ggplot(report_r,aes(y=CONDUCTIVITY,x=PH))+geom_point()+geom_smooth(method="lm")

#Polynomial(Quadratic) regression
x1<-(report_r$PH)^2
qm<-lm(report_r$CONDUCTIVITY~x1+report_r$PH)
summary(qm)
a=data.frame(PH=7.5)
result=predict(qm,a)
result[1]
ggplot(report_r,aes(y=CONDUCTIVITY,x=PH^2+PH))+geom_point()+geom_smooth(method="lm")



#NITRATE
#multiple regression
lm3<-lm(report_r$NITRATE~report_r$F_COLIFORM+report_r$T_COLIFORM)
summary(lm3)
a=data.frame(F_COLIFORM=11,T_COLIFORM=27)
result=predict(lm3,a)
result[1]



