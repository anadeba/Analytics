library(plyr)
library(ggplot2)

### Import dataset
dataset = read.csv("X:\\Hackathon\\Kaggle_Titanic\\completedata.csv")

### Input class variable field name
class_var_name = "Survived"

### Lookup class variable column number from the dataset
class_var_col_no = which(names(dataset) == class_var_name)

### Lookup feature names from the dataset
feature_names = colnames(dataset[-c(1,class_var_col_no)])

### Calculate number of features
no_of_features = length(feature_names)

### Create empty vectors
gini_feature_vector = c()
entropy_feature_vector = c()
accuracy_feature_vector = c()

#feature_names = "Fare"

### This loop will parse each feature column
for (f_name in feature_names)
 {
  ### For the feature create a temporary subset of a dataframe
  temp_df = data.frame(dataset[c(class_var_name,f_name)])
  
  ### Calculate unique feature values
  feature_values = unique(temp_df[c(2)])
  
  ### Use log_k for a feature with k values
  log_base = nrow(feature_values)
  
  ### Create a frequency table of the temporary dataframe, this gives Frequency per region per class
  freq_table = as.data.frame(table(temp_df))
  
  ### Aggregate frequency count per region level, store it as separate column
  freq_table = ddply(freq_table,f_name,transform,summ=sum(Freq))
  
  ### Calculate total record count in the feature
  total_records = sum(freq_table$Freq)
  
  ### Calculate probability of each class in the feature, store it as separate column
  freq_table = ddply(freq_table,class_var_name,transform,classprob_overall=sum(Freq)/total_records)
  
  ### Calculate probability of each class per region in the feature, store it as separate column
  freq_table$classprob_regionwise = freq_table$Freq/freq_table$summ
  
  ### Calculate weights for each region
  freq_table$weights = freq_table$summ/total_records
  
  ### Accuracy calculations
  freq_table = ddply(freq_table,f_name,transform,accuracy_regionwise=max(classprob_regionwise))
  freq_table = ddply(freq_table,f_name,transform, weighted_accuracy_regionwise=mean(accuracy_regionwise)*mean(weights))
  accuracy_feature = colSums(aggregate(freq_table$weighted_accuracy_regionwise,by=list(region = freq_table[[f_name]]),mean)[2])
  accuracy_feature_vector = append(accuracy_feature_vector,as.character(accuracy_feature))
  
  ### Gini Index calculations
  freq_table = ddply(freq_table,f_name,transform,gini_index_regionwise=sum((classprob_regionwise)^2))
  freq_table = ddply(freq_table,f_name,transform, weighted_gini_regionwise=mean(gini_index_regionwise)*mean(weights))
  gini_feature = colSums(aggregate(freq_table$weighted_gini_regionwise,by=list(region = freq_table[[f_name]]),mean)[2]) 
  gini_feature_vector = append(gini_feature_vector,as.character(gini_feature))
    
  ### 1-Entropy calculations
  freq_table = ddply(freq_table,f_name,transform,entropy_regionwise = round(sum(-classprob_regionwise*log(0.0000000001+classprob_regionwise,log_base+0.000000001)),7))
  freq_table = ddply(freq_table,f_name,transform,weighted_entropy_regionwise=mean(entropy_regionwise)*mean(weights))
  entropy_feature = colSums(aggregate(freq_table$weighted_entropy_regionwise,by=list(region = freq_table[[f_name]]),mean)[2])
  entropy_feature = max((1 - entropy_feature),0)
  entropy_feature_vector = append(entropy_feature_vector,as.character(entropy_feature))
  
  ### Order by region
  #freq_table = freq_table[order(f_name)]
 }

### Information gain matrix

ig_matrix = data.frame(feature_names,accuracy_feature_vector,gini_feature_vector,entropy_feature_vector)
write.csv(ig_matrix, file = "X:\\Hackathon\\Kaggle_Titanic\\igmatrix.csv" )

### Scatterplot - Accuracy vs.(1-Entropy)

#df$Accuracy = round(ig_matrix$accuracy_feature_vector,2)


ggplot(ig_matrix, aes(y=entropy_feature_vector))+geom_point(aes(x=accuracy_feature_vector), size=4)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

