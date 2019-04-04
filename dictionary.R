rm(list = ls())
relief <- read.csv('a854756.csv', stringsAsFactors=F)
relief <- subset(relief, relevance>4)

mydict_list <- list()
# 13 topics
for (i in 1:13){
  mydict_list[[i]] <- relief$term[relief$topic==unique(relief$topic)[i]]
}
noquote_list <- list()
for (i in 1:13){
  noquote_list[i] <- noquote(relief$topic[i])
}

mydict <- dictionary(list(Water = mydict_list[[1]], SearchAndRescue  = mydict_list[[2]],
                          InfraAndUtilities = mydict_list[[3]], ExtreViolenceTerro = mydict_list[[4]],
                          Medical = mydict_list[[5]], VioCiviUnre = mydict_list[[6]], 
                          Evacuation = mydict_list[[7]], Energy = mydict_list[[8]],
                          Shelter = mydict_list[[9]], Intervention = mydict_list[[10]],
                          Food = mydict_list[[11]], Sanitation = mydict_list[[12]],
                          ElecAndPolit = mydict_list[[13]]))





