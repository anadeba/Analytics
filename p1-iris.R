### Import libraries
library(ggfortify)
library(MASS)
library(lfda)
### Load the dataset

iris.train = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\IRIS\\train.csv")
iris.test = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\IRIS\\test.csv")
iris = rbind(iris.train, iris.test)
iris = iris[,-c(1)]
iris.test = iris.test[,-c(1)]
iris.train = iris.train[,-c(1)]

pairs(iris[1:4], main = "Debanjan's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)],oma=c(4,4,6,12))

par(xpd=TRUE)
legend(0.85, 0.7, as.vector(unique(iris$Species)),  
       fill=c("red", "green3", "blue"))

### Summarize the dataset

summary(iris)

### Check distribution of dimensions

hist(iris$Sepal.Length, col = "blue")
hist(iris$Sepal.Width, col = "blue")
hist(iris$Petal.Length, col = "blue")
hist(iris$Petal.Width, col = "blue")

### PCA

iris.pca = prcomp(iris[,-c(5)], center = TRUE, scale. = TRUE) 
summary(iris.pca)


### Plot
plot(iris[,-c(5)],col=c("red","green","blue"))
autoplot(iris.pca, data = iris, colour = 'Species')



##################################################################
##################################################################

### Combining 2 class and recreate data set

iris.combined.train = iris.train
iris.combined.train$Species = as.character(iris.combined.train$Species)
iris.combined.train[iris.combined.train == "virginica" | iris.combined.train == "versicolor"] = "class-x"


### LDA - Class3 with Meta class

LDA_model1 = lda(Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris.combined.train)
LDA_model1_pred = predict(LDA_model1, iris.combined.train)
iris.combined.train$LDA1 = LDA_model1_pred$x
iris.combined.train$Pred_Class = LDA_model1_pred$class
ggplot(iris.combined.train, aes(y=Species))+scale_colour_manual(name="",  values =c("green", "red"))+geom_point(aes(x=LDA1, color=Pred_Class), size=4)
summary(LDA_model1)

##################################################################
##################################################################





##################################################################
##################################################################

### Subsetting versicolor and virginica

iris.subset.train = subset(iris.train, iris.train$Species == "versicolor" | iris.train$Species == "virginica")


### LDA - within meta class

LDA_model2 = lda(Species~ ., data=iris.subset.train)
LDA_model2_pred = predict(LDA_model2, iris.subset.train)
iris.subset.train$LDA2 = LDA_model2_pred$x
iris.subset.train$Pred_Class = LDA_model2_pred$class
ggplot(iris.subset.train, aes(y=Species))+scale_colour_manual(name="",  values =c("orange", "purple"))+geom_point(aes(x=LDA2, color=Pred_Class), size=4)

##################################################################
##################################################################



##################################################################
### Predict Test data based on LDA_model1 and LDA_model2
##################################################################

iris.combined.test = iris.test
iris.combined.test$Species = as.character(iris.combined.test$Species)
iris.combined.test[iris.combined.test == "virginica" | iris.combined.test == "versicolor"] = "class-x"

### First Fisher projection on test data

LDA_model1_testpred = predict(LDA_model1, iris.combined.test)
iris.combined.test$LDA1 = LDA_model1_testpred$x
iris.combined.test$Pred_Class = LDA_model1_testpred$class
ggplot(iris.combined.test, aes(y=Species))+scale_colour_manual(name="",  values =c("green", "red"))+geom_point(aes(x=LDA1, color=Pred_Class), size=4)
summary(LDA_model1)

### Second Fisher projection test data

iris.subset.test = subset(iris.test, iris.test$Species == "versicolor" | iris.test$Species == "virginica")

LDA_model2_testpred = predict(LDA_model2, iris.subset.test)
iris.subset.test$LDA2 = LDA_model2_testpred$x
iris.subset.test$Pred_Class = LDA_model2_testpred$class
ggplot(iris.subset.test, aes(y=Species))+scale_colour_manual(name="",  values =c("orange", "purple"))+geom_point(aes(x=LDA2, color=Pred_Class), size=4)

