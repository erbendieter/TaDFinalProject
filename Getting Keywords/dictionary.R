rm(list = ls())
relief <- read.csv('a854756.csv', stringsAsFactors=F)
relief <- subset(relief, relevance>4.5)
# Changed relevance to 4.5 to get a smaller subset to choose from

topic_names <- read.csv('topic_names.csv', stringsAsFactors=F)
unique_topics2 <- topic_names$topic

mydict_list2 <- list()
topic_names <- list()
unique_topics = unique(relief$topic)
# 13 topics
for (i in 1:13){
  relief_topic <- relief[relief$topic == unique_topics[i],]
  mydict_list[[i]] <- unlist(unique(relief_topic$term))
  #print(unique_topics[i])
  #mydict_list[[i]] <- relief$term[relief$topic==unique(relief$topic)[i]]
}

topics = unlist(unique(relief$topic))

# For each topic, we have a list of terms used to describe the topic.
# Now, we will choose 10 terms per topic. We choose the most representative terms for
# each topic.

vocabulary <- setNames(data.frame(matrix(ncol = 13, nrow = 5)), topics)

# See terms per topic to choose

for (i in 1:13){
  print(topics[i])
  print(unique(unlist(mydict_list[i])))
}

# Selecting 5 terms for each topic
vocabulary$`Violent Civil Unrest` <- unique(unlist(mydict_list[1]))[c(2,18,24,33,45)]
vocabulary$Shelter <- unique(unlist(mydict_list[2]))[c(3,5,12,28,35)]
vocabulary$Food <- unique(unlist(mydict_list[3]))[c(2,6,12,14,21)]
vocabulary$`Extreme Violence, Terrorism` <- unique(unlist(mydict_list[4]))[c(2,7,21,41,49)]
vocabulary$Sanitation <- unique(unlist(mydict_list[5]))[c(8,9,24,25,29)]
vocabulary$Energy <- unique(unlist(mydict_list[6]))[c(2,13,19,26,44)]
vocabulary$`Search and Rescue` <- unique(unlist(mydict_list[7]))[c(4,7,13,19,22)]
vocabulary$Evacuation <- unique(unlist(mydict_list[8]))[c(4,7,13,16,17)]
vocabulary$Medical <- unique(unlist(mydict_list[9]))[c(1,8,35,91,105)]
vocabulary$Water <- unique(unlist(mydict_list[10]))[c(3,4,17,24,27)]
vocabulary$`Elections and Politics` <- unique(unlist(mydict_list[11]))[c(8,10,14,24,54)]
vocabulary$Intervention <- unique(unlist(mydict_list[12]))[c(1,4,9,18,22)]
vocabulary$`Infrastructure and Utilities` <- unique(unlist(mydict_list[13]))[c(3,4,5,6,8)]

write.csv(vocabulary, file = "vocab_topic.csv")

all_vocab <- unlist(vocabulary,use.names = FALSE)
write(all_vocab, file = "vocab.txt")

noquote_list <- list()
for (i in 1:13){
  noquote_list[i] <- noquote(relief$topic[i])
}

mydict <- dictionary(list(Water = mydict_list[[10]], SearchAndRescue  = mydict_list[[7]],
                          InfraAndUtilities = mydict_list[[13]], ExtreViolenceTerro = mydict_list[[4]],
                          Medical = mydict_list[[9]], VioCiviUnre = mydict_list[[1]], 
                          Evacuation = mydict_list[[8]], Energy = mydict_list[[6]],
                          Shelter = mydict_list[[2]], Intervention = mydict_list[[12]],
                          Food = mydict_list[[3]], Sanitation = mydict_list[[5]],
                          ElecAndPolit = mydict_list[[11]]))




df <- read.csv(file="clean.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

df$text <- as.character(df$text)
dfcorpus <- corpus(df$text)
topicdfm <- dfm(dfcorpus, dictionary = mydict)
df$Water <- as.numeric(topicdfm[,1])
df$SearchAndRescue <- as.numeric(topicdfm[,2])
df$InfraAndUtilities <- as.numeric(topicdfm[,3])
df$ExtreViolenceTerro <- as.numeric(topicdfm[,4])
df$Medical <- as.numeric(topicdfm[,5])
df$VioCiviUnre <- as.numeric(topicdfm[,6])
df$Evacuation <- as.numeric(topicdfm[,7])
df$Energy <- as.numeric(topicdfm[,8])
df$Shelter <- as.numeric(topicdfm[,9])
df$Intervention <- as.numeric(topicdfm[,10])
df$Food <- as.numeric(topicdfm[,11])
df$Sanitation <- as.numeric(topicdfm[,12])
df$ElecAndPolit <- as.numeric(topicdfm[,13])


head(df)

newdf <- df[c(6:18)]

matches <- rowSums(newdf != 0)
pred_topic <- colnames(newdf)[apply(newdf,1,which.max)]

for (i in 1:nrow(newdf)){
  if (matches[i] == 0) {
    pred_topic[i] = '-'
  }
}

newdf$pred <- pred_topic
dict_topcs <- unlist(unique(newdf$pred))

dict_top_num <- list()

for (i in 1:nrow(newdf)){
  if (pred_topic[i] == dict_topcs[1]){
    dict_top_num[i] = 1
  } else if (pred_topic[i] == dict_topcs[2]){
    dict_top_num[i] = 2
  } else if (pred_topic[i] == dict_topcs[4]){
    dict_top_num[i] = 10
  } else if (pred_topic[i] == dict_topcs[5]){
    dict_top_num[i] = 9
  } else if (pred_topic[i] == dict_topcs[6]){
    dict_top_num[i] = 13
  } else if (pred_topic[i] == dict_topcs[7]){
    dict_top_num[i] = 8
  } else if (pred_topic[i] == dict_topcs[8]){
    dict_top_num[i] = 5
  } else if (pred_topic[i] == dict_topcs[9]){
    dict_top_num[i] = 12
  } else if (pred_topic[i] == dict_topcs[10]){
    dict_top_num[i] = 7
  } else if (pred_topic[i] == dict_topcs[11]){
    dict_top_num[i] = 11
  } else if (pred_topic[i] == dict_topcs[12]){
    dict_top_num[i] = 6
  } else if (pred_topic[i] == dict_topcs[13]){
    dict_top_num[i] = 3
  } else if (pred_topic[i] == dict_topcs[14]){
    dict_top_num[i] = 4
  } else {
    dict_top_num[i] = NA
  }
}

newdf$topic <- unlist(dict_top_num)

positive_word <- read.csv("positivewords.txt",stringsAsFactors=F)
negative_word <- read.csv("negative-words.txt",stringsAsFactors=F)
sentidict <- dictionary(list(negative = negative_word,
                             positive = positive_word))
poswords <- unlist(positive_word[[1]])
negwords <- unlist(negative_word[[1]])
sentidict <- dictionary(list(negative = negwords,
                             positive = poswords))
senti <- dfm(dfcorpus, dictionary = sentidict)
df$senti_score <- as.numeric(senti[,2]) - as.numeric(senti[,1])
head(df$senti_score)

write.csv(newdf, file = "dictionary_label.csv")
write.csv(df, file = "senti_score.csv")

lda_model <- read.csv('lda_preds.csv', stringsAsFactors=F)

lda_clean <- na.omit(cbind(lda_model$label,lda_model$pred_r, newdf$topic))

library(MASS)
library(caret)

# Chi Square LDA and Dictionary
tbl = table(lda_clean[,2], lda_clean[,3])
chisq.test(tbl)

# Conf Matrix Dictionary
tbl = table(lda_clean[,1], lda_clean[,3])
confusionMatrix(tbl)

# Conf Matrix LDA
tbl = table(lda_clean[,1], lda_clean[,2])
confusionMatrix(tbl)
