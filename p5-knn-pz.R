library(MASS)
library(class)
library(KernelKnn)


'''
train_pca = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P4-MNIST\\train_pca.csv")[-c(1)]
test_pca = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P4-MNIST\\test_pca.csv")[-c(1)]
train_lda = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P4-MNIST\\train_lda.csv")[-c(1)]
test_lda = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P4-MNIST\\test_lda.csv")[-c(1)]

'''
###################################################################################
######################################## knn  #####################################
###################################################################################

### knn PCA

knn_pca_accuracy_vector_test = c()
knn_pca_accuracy_vector_train = c()

for (i in seq(1,17,2))
{
  knn_pca_pred = knn(train_pca, train_pca, cl=train_pca$class, k = i, l = 0, prob = FALSE, use.all = TRUE)
  knn_pca_confMat = table(knn_pca_pred,train_pca$class)
  knn_pca_train_accuracy <- sum(diag(knn_pca_confMat))/sum(knn_pca_confMat)
  knn_pca_accuracy_vector_train = append(knn_pca_accuracy_vector_train,knn_pca_train_accuracy)
}

#i = 5
for (i in seq(1,17,2))
{
  knn_pca_pred = knn(train_pca, test_pca, cl=train_pca$class, k = i, l = 0, prob = FALSE, use.all = TRUE)
  knn_pca_confMat = table(knn_pca_pred,test_pca$class)
  knn_pca_test_accuracy <- sum(diag(knn_pca_confMat))/sum(knn_pca_confMat)
  knn_pca_accuracy_vector_test = append(knn_pca_accuracy_vector_test,knn_pca_test_accuracy)
}

k=seq(1,17,2)
plot(k,knn_pca_accuracy_vector_train,ylim=c(0.975,1),type="o",col = "blue")
lines(k,knn_pca_accuracy_vector_test, col = "red")



### knn LDA

knn_lda_accuracy_vector_test = c()
knn_lda_accuracy_vector_train = c()

for (i in seq(1,17,2))
{
  knn_lda_pred = knn(train_lda, train_lda, cl=train_lda$class, k = i, l = 0, prob = FALSE, use.all = TRUE)
  knn_lda_confMat = table(knn_lda_pred,train_lda$class)
  knn_lda_train_accuracy <- sum(diag(knn_lda_confMat))/sum(knn_lda_confMat)
  knn_lda_accuracy_vector_train = append(knn_lda_accuracy_vector_train,knn_lda_train_accuracy)
}

#i = 3
for (i in seq(1,17,2))
{
  knn_lda_pred = knn(train_lda, test_lda, cl=train_lda$class, k = i, l = 0, prob = FALSE, use.all = TRUE)
  knn_lda_confMat = table(knn_lda_pred,test_lda$class)
  knn_lda_test_accuracy <- sum(diag(knn_lda_confMat))/sum(knn_lda_confMat)
  knn_lda_accuracy_vector_test = append(knn_lda_accuracy_vector_test,knn_lda_test_accuracy)
}

k=seq(1,17,2)
plot(k,knn_lda_accuracy_vector_train,ylim=c(0.975,1),type="o",col = "blue")
lines(k,knn_lda_accuracy_vector_test, col = "red")













###################################################################################
############################## Parzen Window(PCA - D1) ############################
###################################################################################

train_pca_class = train_pca[,c(10)]+1
test_pca_class = test_pca[,c(10)]+1
train_lda_class = train_lda[,c(10)]+1
test_lda_class = test_lda[,c(10)]+1

# Parzen Window (KernelKnn) model on PCA training dataset

pzw_pca_training_accuracy_vector = c()
#Sigma=0.1
for (Sigma in seq(0.1,3.0,0.1))
{
  
  model.pzw.pca.train = KernelKnn(train_pca[-c(10)], TEST_data = NULL, y=train_pca_class, Levels = c(1,2,3,4,5,6,7,8,9,10), h=Sigma, weights_function = 'gaussian', regression = FALSE)
  
  model.pzw.pca.train_pred = as.data.frame(model.pzw.pca.train)
  model.pzw.pca.train_pred = colnames(model.pzw.pca.train_pred)[apply(model.pzw.pca.train_pred,1,which.max)]
  model.pzw.pca.train_pred = gsub("V","",model.pzw.pca.train_pred)
  model.pzw.pca.train_pred = as.numeric(model.pzw.pca.train_pred)
  
  pzw_pca_confMat_train = table(model.pzw.pca.train_pred, train_pca_class,dnn=c("Prediction","Actual"))
  #pzw_pca_confMat_train
  pzw_pca_accuracy_train = sum(diag(pzw_pca_confMat_train))/sum(pzw_pca_confMat_train)
  #pzw_pca_accuracy_train
  pzw_pca_training_accuracy_vector = append(pzw_pca_training_accuracy_vector,pzw_pca_accuracy_train)
  
}

pzw_pca_training_accuracy = data.frame(pzw_pca_training_accuracy_vector)

# Parzen Window (KernelKnn) model on PCA test dataset

pzw_pca_test_accuracy_vector = c()
#Sigma=1
for (Sigma in seq(0.1,3.0,0.1))
{
  
  model.pzw.pca.test = KernelKnn(test_pca[-c(10)], TEST_data = NULL, y=test_pca_class, Levels = c(1,2,3,4,5,6,7,8,9,10), h=Sigma, weights_function = 'gaussian', regression = FALSE)
  
  model.pzw.pca.test_pred = as.data.frame(model.pzw.pca.test)
  model.pzw.pca.test_pred = colnames(model.pzw.pca.test_pred)[apply(model.pzw.pca.test_pred,1,which.max)]
  model.pzw.pca.test_pred = gsub("V","",model.pzw.pca.test_pred)
  model.pzw.pca.test_pred = as.numeric(model.pzw.pca.test_pred)
  
  pzw_pca_confMat_test = table(model.pzw.pca.test_pred, test_pca_class,dnn=c("Prediction","Actual"))
  #pzw_pca_confMat_test
  pzw_pca_accuracy_test = sum(diag(pzw_pca_confMat_test))/sum(pzw_pca_confMat_test)
  #pzw_pca_accuracy_test
  pzw_pca_testing_accuracy_vector = append(pzw_pca_testing_accuracy_vector,pzw_pca_accuracy_test)
  
}

pzw_pca_test_accuracy = data.frame(pzw_pca_test_accuracy_vector)




###################################################################################
############################## Parzen Window(LDA - D2) ############################
###################################################################################

# Parzen Window (KernelKnn) model on LDA training dataset

pzw_lda_training_accuracy_vector = c()
#Sigma=0.7
for (Sigma in seq(0.1,3.0,0.1))
{
  
  model.pzw.lda.train = KernelKnn(train_lda[-c(10)], TEST_data = NULL, y=train_lda_class, Levels = c(1,2,3,4,5,6,7,8,9,10), h=Sigma, weights_function = 'gaussian', regression = FALSE)
  
  model.pzw.lda.train_pred = as.data.frame(model.pzw.lda.train)
  model.pzw.lda.train_pred = colnames(model.pzw.lda.train_pred)[apply(model.pzw.lda.train_pred,1,which.max)]
  model.pzw.lda.train_pred = gsub("V","",model.pzw.lda.train_pred)
  model.pzw.lda.train_pred = as.numeric(model.pzw.lda.train_pred)
  
  pzw_lda_confMat_train = table(model.pzw.lda.train_pred, train_lda_class,dnn=c("Prediction","Actual"))
  #pzw_lda_confMat_train
  pzw_lda_accuracy_train = sum(diag(pzw_lda_confMat_train))/sum(pzw_lda_confMat_train)
  #pzw_lda_accuracy_train
  pzw_lda_training_accuracy_vector = append(pzw_lda_training_accuracy_vector,pzw_lda_accuracy_train)
  
}

pzw_lda_training_accuracy = data.frame(pzw_lda_training_accuracy_vector)


# Parzen Window (KernelKnn) model on LDA test dataset

pzw_lda_test_accuracy_vector = c()
#Sigma=1
for (Sigma in seq(0.1,3.0,0.1))
{
  
  model.pzw.lda.test = KernelKnn(test_lda[-c(10)], TEST_data = NULL, y=test_lda_class, Levels = c(1,2,3,4,5,6,7,8,9,10), h=Sigma, weights_function = 'gaussian', regression = FALSE)
  
  model.pzw.lda.test_pred = as.data.frame(model.pzw.lda.test)
  model.pzw.lda.test_pred = colnames(model.pzw.lda.test_pred)[apply(model.pzw.lda.test_pred,1,which.max)]
  model.pzw.lda.test_pred = gsub("V","",model.pzw.lda.test_pred)
  model.pzw.lda.test_pred = as.numeric(model.pzw.lda.test_pred)
  
  pzw_lda_confMat_test = table(model.pzw.lda.test_pred, test_lda_class,dnn=c("Prediction","Actual"))
  #pzw_lda_confMat_test
  pzw_lda_accuracy_test = sum(diag(pzw_lda_confMat_test))/sum(pzw_lda_confMat_test)
  #pzw_lda_accuracy_test
  pzw_lda_test_accuracy_vector = append(pzw_lda_test_accuracy_vector,pzw_lda_accuracy_test)
  
}

pzw_lda_test_accuracy = data.frame(pzw_lda_test_accuracy_vector)

sigma = seq(0.1,3.0,0.1)
plot(sigma, pzw_lda_training_accuracy_vector, type="o",col = "blue")
lines(sigma,pzw_lda_test_accuracy_vector ,col = "red")