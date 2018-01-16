oil<-read.csv("Data_base.csv")
oil<- oil[ !(oil$WellID %in% c(265,462,2179)),]
validation<-read.csv("validation_base_cand.csv")
oil<-na.omit(oil)
library(caTools)
set.seed(88)
split<-sample.split(oil$EUR_o..Mstb.,SplitRatio = 0.8)
train<-subset(oil,split==T)
test<-subset(oil,split==F)
summary(oil)
levels(train$Operator)
levels(train$County)
cor(train$EUR_o..Mstb.,train$Surface.Longitude)
hist(oil$Fluid.Water..Gals.^(1/4))
cor(oil$EUR_o..Mstb.,(oil$Fluid.Water..Gals.)^1/5,use = "complete.obs")

model1<-lm(EUR_o..Mstb.~Subarea+County+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy+Deepest_Zone+Between_Zone+Frac.Stages+Treatment.Records+Proppant...Total..lbs.+Fluid...Total..lbs.,data = train)
summary(model1)

model2<-lm(EUR_o..Mstb.~County+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy+Deepest_Zone+Frac.Stages+Treatment.Records+Proppant...Total..lbs.+Fluid...Total..lbs.,data = train)

summary(model2)

Decision_model1<-rpart(EUR_o..Mstb.~Subarea+County+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy+Deepest_Zone+Between_Zone+Frac.Stages+Treatment.Records+Proppant...Total..lbs.+Fluid...Total..lbs.,data = train,minbucket=1)
summary(Decision_model1)
prp(Decision_model1)
Pred = predict(Decision_model1,newdata = train)

sse_train= sum((Pred-train$EUR_o..Mstb.)^2)
RMSE<-sqrt(sse_train/nrow(train))

sst_train=sum((train$EUR_o..Mstb.-mean(train$EUR_o..Mstb.))^2)
Rsq =1-(sse_train/sst_train)

#predicting on test 

pred_test<-predict(Decision_model1,newdata = test)
SSE_test<-sum((test$EUR_o..Mstb.-pred_test)^2)
SST_test <-sum((test$EUR_o..Mstb.-mean(train$EUR_o..Mstb.))^2)

Rsq_test<- 1 - SSE_test/SST_test

Forest_model1=randomForest(EUR_o..Mstb.~Subarea+County+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft. + WB.Spacing.Proxy+SPBY.Spacing.Proxy+Deepest_Zone+Between_Zone+Frac.Stages+Treatment.Records + Proppant...Total..lbs.+Fluid...Total..lbs.,data=train,ntree=1000,mtry=4)
summary(Forest_model1)
Forest_model1

train<-na.omit(train)
set.seed(20)
Forest_model2=randomForest(EUR_o..Mstb.~.-WellID-Completion.Date-County-Other..Gals.-Fluid.Water..Gals.+log(Fluid.Water..Gals.+1),data=train,ntree=300)

Forest_model2
sqrt(477.191)
plot(Forest_model2)
cor(oil$EUR_o..Mstb.,oil$Completion.Year)
varImpPlot(Forest_model2)
rf_test<-predict(Forest_model2,newdata = test)
head(rf_test,10)
#head(,10)
SSE_test_rf<-sum((test$EUR_o..Mstb.-rf_test)^2)
SST_test_rf <-sum((test$EUR_o..Mstb.-mean(train$EUR_o..Mstb.))^2)
RMSE_rf<- sqrt(SSE_test_rf/nrow(test))
Rsq_rf<- 1 - SSE_test_rf/SST_test_rf



cor(train$EUR_o..Mstb.,train$Depth.Total.Driller..ft.)


hist(oil$EUR_o..Mstb.)
tapply(oil$EUR_o..Mstb.,oil$SPBY.Spacing.Proxy,mean)

tapply(oil$EUR_o..Mstb.,oil$Completion.Year,summary)

tapply(oil$EUR_o..Mstb.,oil$Operator,median)

quantile(oil$Fluid...Total..lbs.,probs = seq(0,1,0.01))

quantile(oil$EUR_o..Mstb.,probs = seq(0,1,0.05))
quantile(oil$EUR_o..Mstb.,probs = seq(0.99,1,0.001))

#oil2$Completion.Date<-as.Date(oil2$Completion.Date,format ="%m/%d/%Y")

oil$Between_Zone[oil$Between_Zone=="#VALUE!"]<-NA
oil<-na.omit(oil)
oil <-oil[oil$EUR_o..Mstb.<=120,]
hist(oil$EUR_o..Mstb.)
hist(log(oil$Fluid...Total..lbs.))
hist(oil$Surface.Latitude^2)
hist(log(oil$Proppant...Total..lbs.))
hist((oil$Depth.Total.Driller..ft.))
cor(oil$EUR_o..Mstb.,(oil$Depth.Total.Driller..ft.)^1/2)
hist(oil3$Surface.Longitude)
hist(oil3$WellID)
hist(oil3$Gel.x.link..Gals.)
hist(oil$Fluid.Water..Gals.)
hist(oil3$Acid..Gals.)
hist(oil3$Frac.Stages)
hist(log(oil$Treatment.Records))
hist(oil3$Other..Gals.)

cor(oil$EUR_o..Mstb.,oil$Treatment.Records)

plot(oil$Depth.Total.Driller..ft.,oil$EUR_o..Mstb.)
plot(oil$Proppant...Total..lbs.,oil$EUR_o..Mstb.)

oil <- oil[oil$Fluid...Total..lbs.>25651.92,]
oil<-oil[oil$Fluid...Total..lbs.<41663370,]

oil<-oil[oil$Proppant...Total..lbs.>74300.0,]
oil<-oil[oil$Proppant...Total..lbs.<2194438400.0,]
oil$Proppant...Total..lbs.<-log(oil$Proppant...Total..lbs.)
oil$Fluid...Total..lbs.<-log(oil$Fluid...Total..lbs.)
##oil$Depth.Total.Driller<-ifelse(oil$Depth.Total.Driller..ft.>9500,1,0)

set.seed(20)

Forest_model3=randomForest(EUR_o..Mstb.~Subarea+Operator
+Completion.Year+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy
+Deepest_Zone+Between_Zone+Frac.Stages+(Proppant...Total..lbs.)+Depth.Total.Driller+Fluid...Total..lbs.,data=train,ntree=300)

Forest_model3
rf_test<-predict(Forest_model3,newdata = test)

SSE_test_rf<-sum((test$EUR_o..Mstb.-rf_test)^2)
SST_test_rf <-sum((test$EUR_o..Mstb.-mean(train$EUR_o..Mstb.))^2)
RMSE_rf<- sqrt(SSE_test_rf/nrow(test))
Rsq_rf<- 1 - SSE_test_rf/SST_test_rf
varImpPlot(Forest_model3)

library(xgboost)
library(Matrix)
sparse_train<-sparse.model.matrix(EUR_o..Mstb.~Subarea+Operator
                                  +Completion.Year+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy
                                  +Deepest_Zone+Between_Zone+Frac.Stages+(Proppant...Total..lbs.)+(Fluid...Total..lbs.),data = train)

dim(sparse_train)


sparse_test<-sparse.model.matrix(EUR_o..Mstb.~Subarea+Operator
                                  +Completion.Year+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy
                                  +Deepest_Zone+Between_Zone+Frac.Stages+(Proppant...Total..lbs.)+(Fluid...Total..lbs.),data = test)
dim(sparse_test)

dtrain<-xgb.DMatrix(data = sparse_train,label=train$EUR_o..Mstb.)
dim(dtrain)
dtest<-xgb.DMatrix(data = sparse_test,label=test$EUR_o..Mstb.)
watchlist<-list(train=dtrain,test=dtest)
model_xgb<-xgb.train(data = dtrain,nrounds = 1000,eta=0.001,max_depth=1,watchlist=watchlist,objective="reg:linear")
validation<-read.csv("validation_base_cand.csv",header= T)
#in the validation set data for Fluid...Total..lbs. field replace NA with median

validation$Fluid...Total..lbs.[is.na(validation$Fluid...Total..lbs.)]<-median(validation$Fluid...Total..lbs.,na.rm = T)
validation$Proppant...Total..lbs.[is.na(validation$Proppant...Total..lbs.)]<-median(validation$Proppant...Total..lbs.,na.rm = T)
validation$Frac.Stages[is.na(validation$Frac.Stages)]<-median(validation$Frac.Stages,na.rm = T)
#taking log on the column
validation$Proppant...Total..lbs.<-log(validation$Proppant...Total..lbs.)
validation$Fluid...Total..lbs.<-log(validation$Fluid...Total..lbs.)
#applying the prediction on the validation file
set.seed(20)
Forest_model3=randomForest(EUR_o..Mstb.~Subarea+Operator
                           +Completion.Year+Surface.Latitude+Surface.Longitude+Depth.Total.Driller..ft.+WB.Spacing.Proxy+SPBY.Spacing.Proxy
                           +Deepest_Zone+Frac.Stages+(Proppant...Total..lbs.)+Fluid...Total..lbs.,data=train,ntree=300)
Forest_model3=randomForest(EUR_o..Mstb.~ Completion.Year+Surface.Latitude+Surface.Longitude+
                             Depth.Total.Driller..ft.+(Proppant...Total..lbs.)+Fluid...Total..lbs.
                           +Frac.Stages+Between_Zone,data=train,ntree=300)

Forest_model4<- randomForest(EUR_o..Mstb.~Subarea+Operator,data=train,ntree=400,nodesize=5)

pred_validation<-predict(Forest_model4,test)
levels(train$Operator)
levels(validation$Operator)
write.csv(validation)


validation$EUR_o..Mstb.<-NA
check<-rbind(oil,validation)
train<-check[1:1162,]
test<-check[1163:1493,]
pred<-predict(Forest_model3,test)
final<-cbind(pred,test$WellID)
write.csv(x = final,file = "final.csv")
sqrt(265.6478)
