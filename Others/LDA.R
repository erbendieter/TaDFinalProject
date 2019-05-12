library('quanteda')
library('topicmodels')
library('tidyr')
library('tidytext')
library('ggplot2')
library('lubridate')
library('ldatuning')
library('parallel')
library('dplyr')

df <- read.csv(file="clean.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Had to first create DTM, then remove stopwords because stemming was giving issues
relief_dfm <- dfm(df$text, tolower=TRUE, stem=TRUE,
                  remove_punct = TRUE, remove_numbers=TRUE) %>% 
  dfm_remove(c(stopwords("english"))) 


# No trimming features: 89,515
# Min 2: 46,677
# Min 5: 22,324
# Min 10: 14,147
# Min 15: 10,882
# Min 20: 9,013
# Min 25: 7,831
# Min 30: 6,955


# Topic Modeling!

# How many features, check here
trim_fts <- c(2,5,10,15,20,25,30,50,100)
rev_trim <- rev(trim_fts)

FindTopicsNumber_plot(k_optimize_blm)

start.time <- Sys.time()
subs_dfm <- dfm_trim(relief_dfm, min_termfreq = 20)

# How many k? Have to run this, will take a while
k_optimize_rel <- FindTopicsNumber(
  subs_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)

FindTopicsNumber_plot(k_optimize_rel)
# Increase topics for more acc

top_mod <- LDA(subs_dfm, k = 25, method = "Gibbs")
top_mod@loglikelihood
rel_topics <- tidy(top_mod, matrix = "beta") 

# Removing the one letter words that are here somehow still there...
rel_topics <- rel_topics[nchar(rel_topics$term) > 1, ]

rel_top_terms <- rel_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

rel_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

doc_topics <- t(top_mod@gamma)
# Find the top topic per day
max <- apply(doc_topics, 2, which.max)

# Creating dataframe with the columns we want
top <- data.frame(article = seq.int(2763), top_topic = max, label = df$topic_no)

top <- top[complete.cases(top), ]


topic_freqs <- top %>% 
  count(topic, top_topic) 

max_freq <- topic_freqs %>% group_by(topic) %>% top_n(1, n)
max_freq$top_num <- seq.int(nrow(max_freq))

topcss <- merge(x = top, y = max_freq, by = "top_topic", all.x = TRUE)

topic_matches <- topcss[c('article','label','top_num')]
topic_matches <- rename(topic_matches, c("top_num"="pred"))

topic_matches <- topic_matches[order(topic_matches$article),] 

write.csv(topic_matches, file = "lda_pred_topics.csv")

write.csv(max_freq, file = "max_freq.csv")