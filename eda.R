library(dplyr)
library(ggplot2)


feature_eng <- function(train)
{
  colnames(train) <- c("Class","Topic","Description")
  
  train$Class <- as.factor(train$Class)
  train$Topic <- as.character(train$Topic)
  train$Description <- as.character(train$Description)
  
  
  train$topic_words <- lengths(gregexpr("\\W+", train$Topic))
  train$desc_words <- lengths(gregexpr("\\W+", train$Description))
  
  
  # Plot histogram of topic words to categorize into short, medium and long
  train %>%
    ggplot(aes(x=topic_words)) + 
    geom_bar(color="black",fill="blue",stat="count")
  
  t_short <- 4
  t_long <- 10  
  
  # Plot histogram of desc  words to categorize into short, medium and long
  train %>%
    ggplot(aes(x=desc_words)) + 
    geom_bar(color="black",fill="blue",stat="count")
  
  d_short <- 20
  d_long <- 50
  
  train$topic_length <- ifelse(train$topic_words<=t_short,"Short",
                               ifelse(train$topic_words>t_long,"Long","Medium"))
  
  train$desc_length <- ifelse(train$desc_words<=d_short,"Short",
                              ifelse(train$desc_words>d_long,"Long","Medium"))
  
  
  return(train)
  
}

eda <- function()
{
  
  eda <- data %>%
    group_by(Class,topic_length,desc_length) %>%
    summarise(Freq = n())
  
  eda$Class_Topic <- paste0(eda$Class,eda$topic_length)
  eda$Class_desc <- paste0(eda$Class,eda$desc_length)
  
  
}

train <- read.csv("train.csv",header=F)
data <- train

data <- feature_eng(data)
eda()

data %>%
  ggplot(aes(x=desc_words)) + 
  geom_bar(color="black",fill="blue",stat="count")

data %>%
  ggplot(aes(x=topic_words)) + 
  geom_bar(color="black",fill="blue",stat="count")