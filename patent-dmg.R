library("sqldf")

### Import datasets

citation_table = read.csv("X:\\ISB\\Course\\Term2\\DMG1\\DMg1 - Group Project-20170219\\Project dataset\\citations_2004.csv")
patent_table = read.csv("X:\\ISB\\Course\\Term2\\DMG1\\DMg1 - Group Project-20170219\\Project dataset\\Patents_2004.csv")
patent_table_firmlevel = read.csv("X:\\ISB\\Course\\Term2\\DMG1\\DMg1 - Group Project-20170219\\DataMiningGroupProject-v1\\FirmLevelData.csv")


temp_df=aggregate(citing~cited,citation_table,length)


### Join to datasets based on patent id

temp_df2 = sqldf("
            
            SELECT * FROM patent_table JOIN temp_df ON patent_table.patent = temp_df.cited

            ")


write.csv(temp_df2,"X:\\ISB\\Course\\Term2\\DMG1\\DMg1 - Group Project-20170219\\Project dataset\\cite_count.csv")

###  Hierarchial clustering

m = cbind(df1$at_1,df1$at1,df1$ni_1,df1$ni1,df1$sale_1,df1$sale1,df1$xrd_1,df1$xrd1)
df4 = data.frame(m)
colnames(df4) = c("at_1","at1","ni_1","ni1","sale_1","sale1","xrd_1","xrd1")
d = dist(df4, method = "euclidean")
H.fit <- hclust(d, method="ward.D")
