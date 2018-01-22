library(tree)
library(rpart)
library(e1071)
library(ggplot2)

mushroom.train = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\Mushroom\\train.csv")
mushroom.test = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\Mushroom\\test.csv")


### Naive Bayes

accuracy_vector_nb = c()
test_accuracy_vector_nb = c()
train_accuracy_vector_nb = c()

for (lmda in seq(0,50))
{
  nb.model = naiveBayes(V1~.,data = mushroom.train,laplace = lmda)
  nb.pred_t <- predict(nb.model, mushroom.test, type="class")
  confMat <- table(mushroom.test$V1,nb.pred_t,dnn=c("Prediction","Actual"))
  accuracy_nb <- sum(diag(confMat))/sum(confMat)
  test_accuracy_vector_nb = append(test_accuracy_vector_nb,accuracy_nb)
  
}



for (lmda in seq(0,50))
 {
  nb.model = naiveBayes(V1~.,data = mushroom.train,laplace = lmda)
  nb.pred_t <- predict(nb.model, mushroom.train, type="class")
  confMat <- table(mushroom.train$V1,nb.pred_t,dnn=c("Prediction","Actual"))
  accuracy_nb <- sum(diag(confMat))/sum(confMat)
  train_accuracy_vector_nb = append(train_accuracy_vector_nb,accuracy_nb)
  
 }

lambda = seq(0,50)
plot(lambda,train_accuracy_vector_nb, ylim = c(0.915,0.945), type="o",col = "blue")
lines(lambda, test_accuracy_vector_nb, col = "red")



### Decision Tree

accuracy_vector_dt = c()
train_accuracy_vector_dt = c()
test_accuracy_vector_dt = c()

for (s in seq(4,64,4))
{
  
  tree.model <- rpart(V1~., data = mushroom.train, control=rpart.control(minsplit = s), method = "class", parms = list (split = c("gini")))
  dt.pred_t <- predict(tree.model, mushroom.train, type="class")
  confMat <- table(mushroom.train$V1,dt.pred_t,dnn=c("Prediction","Actual"))
  accuracy_dt <- sum(diag(confMat))/sum(confMat)
  accuracy_vector_dt = append(accuracy_vector_dt,accuracy_dt)
  train_accuracy_vector_dt = accuracy_vector_dt
}

for (s in seq(4,64,4))
{
 
  tree.model <- rpart(V1~., data = mushroom.train, control=rpart.control(minsplit = s), method = "class", parms = list (split = c("gini")))
  dt.pred_t <- predict(tree.model, mushroom.test, type="class")
  confMat <- table(mushroom.test$V1,dt.pred_t,dnn=c("Prediction","Actual"))
  accuracy_dt <- sum(diag(confMat))/sum(confMat)
  test_accuracy_vector_dt = append(test_accuracy_vector_dt,accuracy_dt)
  
}

 
size = seq(4,64,4)
plot(size,train_accuracy_vector_dt,ylim = c(0.99,1), type="o",col = "blue")
lines(size, test_accuracy_vector_dt, col = "red")

#size = seq(1,20)
#plot()

#plot(tree.model)
#text(tree.model)
