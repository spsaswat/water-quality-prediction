library(glmnet)
library(tidyverse)
library(caret)
report_r=read.csv("D://sem3 materials//labs//R programming//lab_project//water_data.csv")
report_r=report_r[,c(4:11)]
report_r=na.omit(report_r)

#partition
set.seed(103)#The seed number (n) you choose is the starting point used in the generation of a sequence of random numbers
ind=sample(2,nrow(report_r),replace=T,prob=c(0.8,0.2))
train=report_r[ind==1,]
test=report_r[ind==2,]
str(test)






#DO with BOD and temp
x=model.matrix(DO~BOD+Temp, train)[,-1]
y=train$DO
cv = cv.glmnet(x, y, alpha = 0)
cv$lambda.min
Ridge_model = glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
Ridge_model
summary(Ridge_model)
coef(Ridge_model)
#predictions
x_test = model.matrix(DO~BOD+Temp, test)[,-1]
result_rid = Ridge_model %>% predict(x_test) %>% as.vector()
#single value prediction
5.65026689+-0.01540126*1.5+0.03136930*29.2



cv = cv.glmnet(x, y, alpha = 1)
cv$lambda.min
Lasso_model = glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
Lasso_model
summary(Lasso_model)
coef(Lasso_model)
#predictions
x_test = model.matrix(DO~BOD+Temp, test)[,-1]
result_las = Lasso_model %>% predict(x_test) %>% as.vector()
#single value prediction
5.61717393+-0.01712410*1.5+0.03311161*29.2


Elastic_net= train(DO~Temp+BOD,data=train,method="glmnet",trControl=trainControl("cv",number=10),tuneLength=10)
Elastic_net_model = glmnet(x, y, alpha = Elastic_net$bestTune$alpha, lambda = Elastic_net$bestTune$lambda)
summary(Elastic_net_model)
coef(Elastic_net_model)
#predictions
x_test = model.matrix(DO~BOD+Temp, test)[,-1]
result_elas = Elastic_net_model %>% predict(x_test) %>% as.vector()
#single value prediction
5.68355435+-0.01546892*1.5+0.03006168*29.2

#comparision 
data.frame(RMSE = RMSE(result_rid, test$DO),Rsquare = R2(result_rid, test$DO))

data.frame(RMSE = RMSE(result_las, test$DO),Rsquare = R2(result_las, test$DO))

data.frame(RMSE = RMSE(result_elas, test$DO),Rsquare = R2(result_elas, test$DO))







#DO with all variables 
x=model.matrix(DO~., train)[,-1]
y=train$DO
cv = cv.glmnet(x, y, alpha = 0)
cv$lambda.min
Ridge_model = glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
Ridge_model
summary(Ridge_model)
coef(Ridge_model)

#predictions
x_test = model.matrix(DO~., test)[,-1]
result_rid = Ridge_model %>% predict(x_test) %>% as.vector()
#single value prediction
6.340615e+00 + 1.397444e-04*29.2 + 8.815697e-08*6.3 + -1.464299e-07*100 + -6.658398e-05*1.5 + -2.395029e-04*0.1 + -1.273718e-10*7942 + -7.772523e-11*13575




cv = cv.glmnet(x, y, alpha = 1)
cv$lambda.min
Lasso_model = glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
Lasso_model
summary(Lasso_model)
coef(Lasso_model)
#predictions
x_test = model.matrix(DO~., test)[,-1]
result_las = Lasso_model %>% predict(x_test) %>% as.vector()
#single value prediction
6.396498431+-0.008600281*1.5


Elastic_net= train(DO~.,data=train,method="glmnet",trControl=trainControl("cv",number=10),tuneLength=10)
Elastic_net_model = glmnet(x, y, alpha = Elastic_net$bestTune$alpha, lambda = Elastic_net$bestTune$lambda)
summary(Elastic_net_model)
coef(Elastic_net_model)
#predictions
x_test = model.matrix(DO~., test)[,-1]
result_elas = Elastic_net_model %>% predict(x_test) %>% as.vector()
#single value prediction
6.391311540+-0.007765156*1.5

#comparision 
data.frame(RMSE = RMSE(result_rid, test$DO),Rsquare = R2(result_rid, test$DO))

data.frame(RMSE = RMSE(result_las, test$DO),Rsquare = R2(result_las, test$DO))

data.frame(RMSE = RMSE(result_elas, test$DO),Rsquare = R2(result_elas, test$DO))






#NITRATE
x1=model.matrix(NITRATE~F_COLIFORM+T_COLIFORM, train)[,-1]
y1=train$DO
cv = cv.glmnet(x1, y1, alpha = 0)
cv$lambda.min
Ridge_model <- glmnet(x1, y1, alpha = 0, lambda = cv$lambda.min)
Ridge_model
summary(Ridge_model)
coef(Ridge_model)
#predictions
x_test = model.matrix(NITRATE~F_COLIFORM+T_COLIFORM, test)[,-1]
result_rid = Ridge_model %>% predict(x_test) %>% as.vector()

cv = cv.glmnet(x1, y1, alpha = 1)
cv$lambda.min
Lasso_model <- glmnet(x1, y1, alpha = 1, lambda = cv$lambda.min)
Lasso_model
summary(Lasso_model)
coef(Lasso_model)
x_test = model.matrix(NITRATE~F_COLIFORM+T_COLIFORM, test)[,-1]
result_las = Lasso_model %>% predict(x_test) %>% as.vector()
#single value prediction
6.292096e+00 + -1.326642e-08*11  +   -1.016187e-09*27


Elastic_net= train(NITRATE~F_COLIFORM+T_COLIFORM,data=train,method="glmnet",trControl=trainControl("cv",number=10),tuneLength=10)
Elastic_net_model <-  glmnet(x1, y1, alpha = Elastic_net$bestTune$alpha, lambda = Elastic_net$bestTune$lambda)
summary(Elastic_net_model)
coef(Elastic_net_model)
result_elas = Elastic_net_model %>% predict(x_test) %>% as.vector()


#comparision 
data.frame(RMSE = RMSE(result_rid, test$NITRATE),Rsquare = R2(result_rid, test$NITRATE))

data.frame(RMSE = RMSE(result_las, test$NITRATE),Rsquare = R2(result_las, test$NITRATE))

data.frame(RMSE = RMSE(result_elas, test$NITRATE),Rsquare = R2(result_elas, test$NITRATE))

