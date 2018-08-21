library(tidyverse)
install.packages('topicmodels')
library(topicmodels)
library(tidytext)
library(SnowballC)
install.packages('LDAvis')
library(LDAvis)
library("RColorBrewer")

class_tbl <- data_frame(Class = as.factor(c(1:4)),
                        name = c("World", "Sports","Business","Sci/Tech"))
c <- data %>% left_join(class_tbl) %>% 
  select(name) 
data$class_name <- unlist(as.vector(c))
data$name <- as.factor(c(1:120000))

#===========wordcloud
set.seed(1234)
wordcloud::wordcloud(words = wordCount$word,freq = wordCount$n, min.freq = 1,
                     max.words=100, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2"))


#===========
dataTidy <- data %>%
  filter(class_name == 'Sports') %>%
  select(name,Description) %>%
  unnest_tokens(word, Description)%>%
  anti_join(stop_words)

wordCount <- dataTidy %>%
  count(word,sort = TRUE)

## remove words that are too common (they won't help us in identifying topics since they occur everywhere)
## and stem reviews 
commonWords <- c('39','1','6','2','3','4','5','7','8','9','0','game','team','season','sports')
  
## remove words that are too common (they won't help us in identifying topics since they occur everywhere)
## and stem reviews 
commonWords <- c('lt','gt','ap','39',"monday","tuesday","wednesday","thursday","friday")

#=========== 

dataTidy <- dataTidy %>%
  filter(!word %in% commonWords) %>%
  mutate(word = wordStem(word))

wordCount <- dataTidy %>%
  count(word,sort = TRUE)

## remove infrequent words - they are also not interesting
## here we just pick the top 6000 words

wordCut <- 6000

vocab <- wordCount %>%
  slice(1:wordCut)

dataTidy <- dataTidy %>%
  filter(word %in% vocab$word)

## count words per review 
dataLength <- dataTidy %>%
  count(name)

minLength <- 20

dataLength <- dataLength %>%
  filter(n >= minLength)

## create document term matrix for use in LDA 

dtm <- dataTidy %>%
  filter(name %in% dataLength$name)%>%
  count(name,word) 
dtm$name <- as.character(dtm$name)

dtm <- cast_dtm(dtm,name,word,n) 
matrix <- as.matrix(dtm)



## @knitr runLDATrip


##
## Warning: this takes a while to run 
##
# numTopics <- c(20,30,40)
#  
# for (theNum in c(1:length(numTopics))){
#   theLDA <- LDA(dtm, k = numTopics[theNum], method="Gibbs",
#                 control = list(alpha = 1/numTopics[theNum],iter=5000,burnin=1000,seed = 1234))
#   
#    saveRDS(theLDA,file=paste0('ldaTrip',numTopics[theNum],'.rds'))
# }

theNumTopics <- 5
#theLDA <- read_rds(paste0('ldaTrip',theNumTopics,'.rds'))

numTopics <- 5
     theLDA <- LDA(dtm, k = numTopics, method="Gibbs",
                   control = list(alpha = 1/numTopics,iter=5000,burnin=1000,seed = 1234))
     
     saveRDS(theLDA,file=paste0('ldaTrip',numTopics[theNum],'.rds'))



## study estimated topics - look at top words in each topic
theTopicsBeta <- tidy(theLDA, matrix = "beta")

TopicsTop <- theTopicsBeta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

plTopicWeights <- TopicsTop %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free', nrow = 1) +
  scale_x_continuous(breaks = TopicsTop$x,
                     labels = TopicsTop$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic - Sports',
       subtitle = paste0(numTopics,' Topic LDA of ',
                         prettyNum(nrow(dataLength),big.mark=",",scientific=FALSE), ' News'),
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size = 10))

plTopicWeights



theTopicsGamma <- tidy(theLDA, matrix = "gamma")

theTopicsGammaMeta <- theTopicsGamma %>%
  inner_join(reviews,by=c("document"="reviewID"))


