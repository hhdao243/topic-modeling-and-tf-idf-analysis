train <- read.csv("train.csv",header=F)
source("eda.R")

data <- feature_eng(train)

class_tbl <- data_frame(Class = as.factor(c(1:4)),
                    name = c("World", "Sports","Business","Sci/Tech"))
c <- data %>% left_join(class_tbl) %>% 
  select(name) 
data$class_name <- unlist(as.vector(c))
data$name <- as.factor(c(1:120000))

library(dplyr)
library(janeaustenr)
library(tidytext)

words <- data %>% unnest_tokens(word,Description)

words_count <- words %>%
  group_by(Topic,word) %>%
  summarise(n = n())

word_tfidf <- words_count %>%
  bind_tf_idf(word, Topic, n)


word_tfidf %>%
  filter(Topic %in% data$Topic[1:4]) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Topic) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = Topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Topic, ncol = 2, scales = "free") +
  coord_flip()


