library(MASS)
library(e1071)

base_path = "X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\MNIST\\"


### Combine data train

df_train = data.frame(matrix(ncol = 786, nrow = 0))
colnames(df_train) = c()

for (i in seq(0,9))
{
  read_path = paste0(base_path,"train",i,".csv")
  train = na.omit(read.csv(read_path))
  train$class = i
  df_train = rbind(df_train,train)
}

write_path = paste0(base_path,"FISHER\\train",".csv")
write.csv(df_train,write_path)

### Combine data test

df_test = data.frame(matrix(ncol = 786, nrow = 0))
colnames(df_test) = c()

for (i in seq(0,9))
{
  read_path = paste0(base_path,"test",i,".csv")
  test = na.omit(read.csv(read_path))
  test$class = i
  df_test = rbind(df_test,test)
}

write_path = paste0(base_path,"FISHER\\test",".csv")
write.csv(df_test,write_path)

### PCA

train.data = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\MNIST\\FISHER\\train.csv")
test.data = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\MNIST\\FISHER\\test.csv")
pcaobj = prcomp(train.data[-c(1,2,787)])
train_pca = data.frame(pcaobj$x)[c(1:9)]
train_pca$class = train.data$class

test_pca = as.data.frame(predict(pcaobj,newdata = test.data[-c(1,2,787)]))[c(1:9)]
test_pca$class = test.data$class

### LDA

train.data = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\MNIST\\FISHER\\train.csv")
test.data = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\MNIST\\FISHER\\test.csv")
train.data = train.data[-c(1,2)]
train.data = train.data[-c(1:32,53:58,83:86,111:113,140:142,169,449,477,533,561,589,617,645,646,672:674,699:703,727:732,754:760,781:784)]
test.data = test.data[-c(1,2)]
test.data = test.data[-c(1:32,53:58,83:86,111:113,140:142,169,449,477,533,561,589,617,645,646,672:674,699:703,727:732,754:760,781:784)]


ldaobj = lda(class~., data = train.data)
train_lda = as.matrix(train.data[,1:702])%*%ldaobj$scaling
train_lda = as.data.frame(train_lda)
train_lda$class = train.data$class

projecteddata = as.matrix(test.data[,1:702])%*%ldaobj$scaling
test_lda = as.data.frame(projecteddata)[c(1:9)]
test_lda$class = test.data$class


### PCA data qda - for full cov-matrix

qda_pca.model <- qda(class ~.,data = train_pca)
qda_pca_pred <- predict(qda_pca.model, test_pca, type="class")
qda_pca_confMat <- table(test_pca$class,qda_pca_pred$class,dnn=c("Prediction","Actual"))
qda_pca_test_accuracy <- sum(diag(qda_pca_confMat))/sum(qda_pca_confMat)
qda_pca_test_accuracy


### PCA data Naive Bayes - for diagonal cov-matrix

nb_pca = naiveBayes(as.factor(class)~.,data = train_pca)
nb_pca_pred = predict(nb_pca, test_pca, type="class")
nb_pca_confMat = table(test_pca$class,nb_pca_pred,dnn=c("Prediction","Actual"))
nb_pca_test_accuracy <- sum(diag(nb_pca_confMat))/sum(nb_pca_confMat)
nb_pca_test_accuracy

### LDA data qda - for full cov-matrix

qda_lda.model <- qda(class ~.,data = train_lda)
qda_lda_pred <- predict(qda_lda.model, test_lda, type="class")
qda_lda_confMat <- table(test_lda$class,qda_lda_pred$class,dnn=c("Prediction","Actual"))
qda_lda_test_accuracy <- sum(diag(qda_lda_confMat))/sum(qda_lda_confMat)
qda_lda_test_accuracy



### LDA data Naive Bayes - for diagonal cov-matrix

nb_lda = naiveBayes(as.factor(class)~.,data = train_lda)
nb_lda_pred = predict(nb_lda, test_lda, type="class")
nb_lda_confMat = table(test_lda$class,nb_lda_pred,dnn=c("Prediction","Actual"))
nb_lda_test_accuracy <- sum(diag(nb_lda_confMat))/sum(nb_lda_confMat)
nb_lda_test_accuracy

