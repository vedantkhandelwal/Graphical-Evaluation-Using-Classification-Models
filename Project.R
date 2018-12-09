library(C50)
library(Metrics)
library(caret)
library(ggplot2)
library(gains)
library(rpart)
library(rpart.plot)
library(lift)
library(StatMeasures)
dataset_train=read.delim("C:\\Users\\vedant khandelwal\\Downloads\\Project_10\\Loans_Training", sep = ",", stringsAsFactor = TRUE)
dataset_test=read.delim("C:\\Users\\vedant khandelwal\\Downloads\\Project_10\\Loans_Test", sep = ",", stringsAsFactor = TRUE)
dataset_train$Approval=factor(dataset_train$Approval,
                              levels=c('FALSE','TRUE'),
                              labels=c(0,1))
dataset_test$Approval=factor(dataset_test$Approval,
                             levels=c('FALSE','TRUE'),
                             labels=c(0,1))
summary(dataset_train)
#CART MODEL
tree <- rpart(formula = Approval~Debt.to.Income.Ratio+FICO.Score+Request.Amount,
              data = dataset_train,
              method = "class")
print(tree)
summary(tree)
rpart.plot(x =tree, yesno = 2, type = 0, extra = 0)

class_prediction <- predict(object = tree,
                            
                            newdata = dataset_test,
                            
                            type = "class")
table(dataset_test[,1],class_prediction)
confusionMatrix(data = class_prediction,reference = dataset_test$Approval)

#C5.0 Model
C50_mod<- C5.0(dataset_train[,c(-1,-5)],dataset_train[,1])
print(C50_mod)
summary(C50_mod)
plot(C50_mod)
C50_model=C5.0(dataset_train[,c(-1,-5)],dataset_train[,1],control = C5.0Control(CF=0.0000001))
print(C50_model)
summary(C50_model)
plot(C50_model)
pred=predict(C50_model,dataset_test,type="class")
table(dataset_test[,1],pred)
confusionMatrix(data = pred,
                reference = dataset_test$Approval) 


#lift chart for CART MODEL
plotLift(class_prediction, labels=c(0,1), cumulative = TRUE, n.buckets = 100,col="blue")
#lift chart for C5.0 Model
plotLift(pred, labels=c(0,1), cumulative = TRUE, n.buckets = 100,col="blue")


attach(dataset_test)
#gain chart for CART model
set.seed(1)
for_lift <- data.frame(Class = factor(rep(1:2, each = 50)),
                       Approval = sort(runif(100), decreasing = TRUE),
                       cart_prediction = runif(100))
lift_curve <- lift(Class ~ Approval+cart_prediction, data = for_lift)
xyplot(lift_curve, auto.key = list(columns = 3))
#gain chart for c5.0 model
for_lift <- data.frame(Class = factor(rep(1:2, each = 50)),
                       Approval = sort(runif(100), decreasing = TRUE),
                       pred= runif(100))


lift_curve <- lift(Class ~ Approval + pred, data = for_lift)
xyplot(lift_curve, auto.key = list(columns = 3))
dataset_test$roi=(dataset_test$Interest/dataset_test$Request.Amount)*100
dataset_test$class_pred=class_prediction
dataset_test$pred=pred
dataca=dataset_test[which(dataset_test$class_pred==1),]
datac5=dataset_test[which(dataset_test$pred==1),]


#Response chart for cart model
plot(class_prediction,main="Response chart for CART")
#response chart for c5.0 model
plot(pred,main="Response Chart for C5.0")


#profile chart for cart model
plot(decile(dataca$Interest),dataca$Interest,xlab="decile Approval=TRUE",ylab="Profit(Interest)",col="green",main="Profile Curve for CART")
#Profile chart for C5.0
plot(decile(datac5$Interest),datac5$Interest,xlab="decile Approval=TRUE",ylab="Profit(Interest)",col="green",main="Profile Curve for C5.0")


#ROI chart for CART MODEL
plot(dataca$roi,xlab="No of predicted true",ylab="ROI",main="ROI Curve for CART",col="blue")
#ROI chart for c5.0
plot(datac5$roi,xlab="No of predicted true",ylab="ROI",main="ROI Curve for C5.0",col="blue")

