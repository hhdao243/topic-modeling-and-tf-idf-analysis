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
library(tidytext)

words <- data %>% unnest_tokens(word,Description)

words_count <- words %>%
  group_by(class_name,word) %>%
  summarise(n = n())

### tf_idf matrix
word_tfidf <- words_count %>%
  bind_tf_idf(word, class_name, n)

### Visualization
word_tfidf %>%
 # filter(Topic %in% data$Topic[1:4]) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(class_name) %>% 
  top_n(20) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = class_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~class_name, ncol = 2, scales = "free") +
  coord_flip()




