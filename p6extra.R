library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(e1071)

### Import the raw documents
data_matrix = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\documents.csv")[-c(1)]

### Import dictionary and create sub-dictionary 5k and 10k words separately

dict = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\dictionary.csv")
colnames(dict) = c("term","doc_freq")
dict5000 = head(dict[order(-dict$doc_freq),],n=5000)
dict10000 = head(dict[order(-dict$doc_freq),],n=10000)

### Partition the data in train and test sets

sample_size = floor(0.80*nrow(data_matrix))
set.seed(123)
train_index = sample(nrow(data_matrix), size = sample_size)
data.train = data_matrix[train_index,]
data.test = data_matrix[-train_index,]

### Create train and test DTMs

news.corpus <- Corpus(VectorSource(data.train$doc))
news.corpus = tm_map(news.corpus,removeWords,stopwords("english"))
dtm.train.nb <- DocumentTermMatrix(news.corpus, control=list(dictionary = dict5000))

news.corpus <- Corpus(VectorSource(data.test$doc))
news.corpus = tm_map(news.corpus,removeWords,stopwords("english"))
dtm.test.nb <- DocumentTermMatrix(news.corpus, control=list(dictionary = dict5000))

### Naive Bayes Model
data.train.nb = data.frame(inspect(dtm.train.nb[,]))
nb_model = naiveBayes(as.factor(data_matrix$class)~.,data = data.frame(inspect(dtm.train.nb[,])), laplace = 30)



