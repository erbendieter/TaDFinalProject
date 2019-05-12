library('dplyr')
library('stringr')

# Reading the data after removing other characters
preprocessed_df <- read.csv(file="prepro.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Dropping the articles not correctly scraped or with errors
# Set the word count at 900 after inspecting the entries
preprocessed_df <- preprocessed_df[str_count(preprocessed_df$text) > 900,]


# Frequencies by keyword
table(preprocessed_df$keyword)

##################
# NOTE: Have to decide if we want to keep the same number of articles per keyword
##################

# Reading topic mapping by keyword
keyword_topic_map <- read.csv(file="vocab_topic.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)


# Merging datasets to get topic label
prepro_labels <- merge(x = preprocessed_df, y = keyword_topic_map, by = "keyword", all.x = TRUE)

# Adding topic number instead for modeling
df <- prepro_labels %>% 
  mutate(topic_no = as.integer(factor(topic)))

write.csv(df, file = "clean.csv")



