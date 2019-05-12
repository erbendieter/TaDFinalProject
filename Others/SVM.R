library("RSQLite")
library(dplyr)
library(tidyr)

# Reading all the news file
sqlite.driver <- dbDriver("SQLite")
connection <- dbConnect(sqlite.driver, dbname = '/Users/dietererben/Desktop/School/Text as Data/Project/all-the-news.db')
table <- dbListTables(connection)
all_news <- dbReadTable(connection, table, row.names = NULL)

# Reading the relief file
relief_news <- read.csv(file="clean.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Selecting columns and rows for relief fiel
news_cols_keep <- all_news %>%
  select(title, content, publication)

news_cols_keep <- news_cols_keep %>% drop_na(content)
news_cont <- news_cols_keep[nchar(news_cols_keep$content) > 80,]
news_sample <- news_cont[sample(nrow(news_cont), 2763), ]

# Creating df's for each source
news_df <- data.frame("text" = news_sample$content,'class' = 0, stringsAsFactors = FALSE)
rel_df <- data.frame("text" = relief_news$text,'class' = 1, stringsAsFactors = FALSE)

# Combining the df's
comb_data <- rbind(news_df, rel_df)


  #----------------------------------------
# 3. Support Vector Machine (SVM) using Caret ---
#----------------------------------------
library(caret)
library(quanteda)

# Were getting memory errors, so decided to trim the dfm, Talk about this later
subs_dfm <- dfm_trim(dfm(comb_data$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")), min_termfreq = 20) %>% convert("matrix")

# A. the caret package has it's own partitioning function
set.seed(1984)
ids_train <- createDataPartition(1:nrow(subs_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- subs_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- comb_data$class[ids_train] %>% as.factor()  # train set labels
test_x <- subs_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- comb_data$class[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear")

saveRDS(svm_mod_linear, "./svm_model.rds")


# load the model
svm_model <- readRDS("./svm_model.rds")

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
