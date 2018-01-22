library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(e1071)



********************* This code takes time to complete ***************************
************ Run this commented code once and save the output to csv *************

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

base_path = "X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\71620021\\20news-19997\\20_newsgroups\\"
folders = list.files(base_path)
data_matrix = data.frame(matrix(ncol = 2, nrow = 0))
colnames(data_matrix) = c("doc","class")

for (folder_name in folders)
 {
  folder_path = paste0(base_path, folder_name, "\\")
  files = list.files(folder_path)
  for (file_name in files)
   {
    file_path = paste0(folder_path, file_name)
    data = as.data.frame(read_file(file_path))
    data$class = folder_name
    data_matrix = rbind(data_matrix, data)
    print(paste0(folder_name," ",file_name))
   }
  
 }

documents = data_matrix
colnames(documents) = c("doc","class")

documents$doc = text.clean(documents$doc)
documents$doc = wordStem(documents$doc, language = "english")
write.csv(documents,file = "X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\documents.csv")




### Read documents.csv and create TDM

data_matrix = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\documents.csv")[-c(1)]
news.corpus <- Corpus(VectorSource(data_matrix$doc))
news.corpus = tm_map(news.corpus,removeWords,stopwords("english"))
tdm = TermDocumentMatrix(news.corpus)

### This code parse the TDM in 1000X1000 blocks to create document frequencies for each block ###
### Few variables are hard coded for the above TDM(11023X19997) ###
### 

y = data.frame(matrix(nrow = 0, ncol = 20))

for (i in seq(0,110000,1000))
{
  if (i == 110000)
    x = data.frame(matrix(nrow = 213, ncol = 0))
  else
    x = data.frame(matrix(nrow = 1000, ncol = 0))
  for (j in seq(0,19000,1000))
  {
    
    tempdf = data.frame(rowSums(data.frame(ifelse(inspect(tdm[(i+1):ifelse(i>=110000,110213,i+1000),(j+1):ifelse(j+1>=19000,19997,j+1000)])>=1,1,0))))
    x = cbind(x,tempdf)
    print(paste0(i+1," ", j+1))
    rm(tempdf)
    gc(reset=T) ### Free up memory
  }
  
  y = rbind(y,x)
}

colnames(y) = c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20")

dict = data.frame(rowSums(y))
colnames(dict) = c("doc_freq")

write.csv(dict,file = "X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\dictionary.csv")


### Read dictionary and create DTM with first 5000 words

dict = read.csv("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\dictionary.csv")
colnames(dict) = c("term","doc_freq")
dict5000 = head(dict[order(-dict$doc_freq),],n=5000)
dict10000 = head(dict[order(-dict$doc_freq),],n=10000)

news.corpus <- Corpus(VectorSource(data_matrix$doc))
news.corpus = tm_map(news.corpus,removeWords,stopwords("english"))
DTM = DocumentTermMatrix(news.corpus)

#################################################################################
###################### Naive Bayes for 5000 words ###############################
#################################################################################


index = match(dict5000$term,DTM$dimnames$Terms)
DTM5000 = data.frame(inspect(DTM[,as.double(index)]))
DTM5000$document_class = data_matrix$class

### Sampling the document in 80-20 Train-Test set

sample_size = floor(0.80*nrow(DTM5000))
set.seed(123)
train_index = sample(nrow(DTM5000), size = sample_size)
DTM5000_train = DTM5000[train_index,]
DTM5000_test = DTM5000[-train_index,]


### Applying Naive Bayes to predict test set and calculating accuracy

nb_model = naiveBayes(as.factor(document_class)~.,data = DTM5000_train, laplace = 30)
nb_model_pred = predict(nb_model, DTM5000_test, type="class")
nb_model_confMat = table(DTM5000_test$document_class,nb_model_pred,dnn=c("Prediction","Actual"))
nb_model_accuracy <- sum(diag(nb_model_confMat))/sum(nb_model_confMat)
nb_model_accuracy


#################################################################################
###################### Naive Bayes for 10000 words ##############################
#################################################################################


index = match(dict10000$term,DTM$dimnames$Terms)
DTM10000 = data.frame(inspect(DTM[,as.double(index)]))
DTM10000$document_class = data_matrix$class

### Sampling the document in 80-20 Train-Test set

sample_size = floor(0.80*nrow(DTM10000))
set.seed(123)
train_index = sample(nrow(DTM10000), size = sample_size)
DTM10000_train = DTM10000[train_index,]
DTM10000_test = DTM10000[-train_index,]


### Applying Naive Bayes to predict test set and calculating accuracy

nb_model = naiveBayes(as.factor(document_class)~.,data = DTM10000_train, laplace = 30)
nb_model_pred = predict(nb_model, DTM10000_test, type="class")
nb_model_confMat = table(DTM10000_test$document_class,nb_model_pred,dnn=c("Prediction","Actual"))
nb_model_accuracy <- sum(diag(nb_model_confMat))/sum(nb_model_confMat)
nb_model_accuracy


















'''
==============================================================================================

### Sampling the document in 80-20

sample_size = floor(0.80*nrow(data_matrix))
set.seed(123)
train_index = sample(nrow(data_matrix), size = sample_size)
data_matrix_train = data_matrix[train_index,]
data_matrix_test = data_matrix[-train_index,]

### create an empty dataframe
term_prob_matrix = data.frame(matrix(ncol = 2, nrow = 0))
colnames(term_prob_matrix) = c()

cls = "comp.graphics"
for (cls in unique(data_matrix$class))
{
  document4class = subset(data_matrix_train, data_matrix_train$class == cls)
  colnames(document4class) = c("doc","class")
  news.corpus <- Corpus(VectorSource(document4class$doc))
  news.corpus = tm_map(news.corpus,stripWhitespace)
  news.corpus = tm_map(news.corpus,removeWords,stopwords("english"))
  news.corpus = tm_map(news.corpus,stemDocument)
  dtm4class = DocumentTermMatrix(news.corpus)
  tf4class = data.frame(colSums(as.matrix(dtm4class)))
  colnames(tf4class) = c("freq")
  tempdf = data.frame(row.names(tf4class))
  total_occurance = sum(tf4class$freq)
  tf4class$probability = apply(tf4class,1,function(x) tf4class$freq/total_occurance)
  tf4class$class = cls
  #term_prob_matrix = rbind(term_prob_matrix,tf4class)
  write.csv(tf4class,file = paste0("X:\\ISB\\Course\\Term3\\Visit1\\DMG2\\Assignment1\\P6\\train\\",cls,".csv"))
  print(cls)
}

====================================================================================================
'''
