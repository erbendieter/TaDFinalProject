library('dplyr')
library('stringr')
library('quanteda')
library('topicmodels')

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

# Creating DTM
relief_dfm <- dfm(df$text, tolower=TRUE, stem=TRUE,
                remove_punct = TRUE, remove_numbers=TRUE,
                remove = stopwords("english"))


# No trimming features: 89,515
# Min 2: 46,677
# Min 5: 22,324
# Min 10: 14,147
# Min 15: 10,882
# Min 20: 9,013
# Min 25: 7,831
# Min 30: 6,955


# Topic Modeling!

trim_fts <- c(2,5,10,15,20,25,30,50,100)
rev_trim <- rev(trim_fts)


start.time <- Sys.time()
subs_dfm <- dfm_trim(relief_dfm, min_termfreq = 20)
top_mod <- LDA(subs_dfm, k = 13, method = "Gibbs")
end.time <- Sys.time()
time.taken <- end.time - start.time
cat('The time for',i,'min fts is',time.taken)
