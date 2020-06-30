library('topicmodels')


#data provided a Document Term Matrix - collection of 2246 news articles
data("AssociatedPress")

#creating a two topic LDA model for data
set.seed(123)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

#interpreting the model

#ectracting the per-topic-per-word probabilities beta

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#showing top 10 terms per topic

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#words with greatest difference between the two topics

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

#document topic probabilities

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents


#work on songs dataset
library(tm)

library(dplyr)
library(tibble)

songs_dataset_stemmed_1000 = songs_dataset_stemmed[1:10000,]

songs_dataset_stemmed_1000_metal = subset(songs_dataset_stemmed_1000, genre == "Metal")

songs_dataset_stemmed_1000_metal_pop = subset(songs_dataset_stemmed_1000, genre == "Metal" | genre == "Pop")  %>% rownames_to_column("rowname")

rownames_metal <- subset(songs_dataset_stemmed_1000_metal_pop, genre == "Metal")$rowname
rownames_pop <- subset(songs_dataset_stemmed_1000_metal_pop, genre == "Pop")$rowname

songs_dataset_stemmed_1000_pop = subset(songs_dataset_stemmed_1000, genre == "Pop")

songs_dataset_stemmed_1000_rock = subset(songs_dataset_stemmed_1000, genre == "Rock")


dfCorpus_metal = Corpus(VectorSource(songs_dataset_stemmed_1000_metal$clean))

dfCorpus_metal_pop = Corpus(VectorSource(songs_dataset_stemmed_1000_metal_pop$clean))

dfCorpus_pop = Corpus(VectorSource((songs_dataset_stemmed_1000_pop$clean)))

dfCorpus_rock = Corpus(VectorSource((songs_dataset_stemmed_1000_rock$clean)))


metal_dtm <- DocumentTermMatrix(dfCorpus_metal)

metal_pop_dtm <- DocumentTermMatrix(dfCorpus_metal_pop)

pop_dtm <- DocumentTermMatrix(dfCorpus_pop)

rock_dtm <- DocumentTermMatrix(dfCorpus_rock)




#lda for metal

rowTotals <- apply(metal_dtm , 1, sum) #Find the sum of words in each Document
dtm.new.metal   <- metal_dtm[rowTotals> 0, ] 


metal_lda <- LDA(dtm.new.metal, k = 5, control = list(seed = 1234))


#interpreting the model

#ectracting the per-topic-per-word probabilities beta

ap_topics_metal <- tidy(metal_lda, matrix = "beta")
ap_topics_metal

#showing top 10 terms per topic

ap_top_terms_metal <- ap_topics_metal %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_metal %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#words with greatest difference between the two topics

#for topics 1 and 2
beta_spread_metal_1 <- ap_topics_metal %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  top_n(-10,log_ratio) 

beta_spread_metal_2 <- ap_topics_metal %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  top_n(10,log_ratio) 
library(gridExtra)

p1 <- ggplot(data = beta_spread_metal_1) +
  geom_point(mapping = aes(x = term, y = log_ratio), color = "blue")
p2 <- ggplot(data = beta_spread_metal_2) +
  geom_point(mapping = aes(x = term, y = log_ratio), color = "blue")
grid.arrange(p1, p2, nrow = 1)


#for topics 1 and 5
beta_spread_metal_1 <- ap_topics_metal %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log2(topic5 / topic1)) %>%
  top_n(-10,log_ratio) 

beta_spread_metal_2 <- ap_topics_metal %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log2(topic5 / topic1)) %>%
  top_n(10,log_ratio) 

p1 <- ggplot(data = beta_spread_metal_1) +
  geom_point(mapping = aes(x = term, y = log_ratio), color = "blue")
p2 <- ggplot(data = beta_spread_metal_2) +
  geom_point(mapping = aes(x = term, y = log_ratio), color = "blue")
grid.arrange(p1, p2, nrow = 1)


#document topic probabilities

ap_documents_metal <- tidy(metal_lda, matrix = "gamma")
ap_documents_metal

ap_documents_metal_top50 <- ap_documents_metal %>%
  filter(gamma > 0.5)
ap_documents_metal_top50_song1 <- ap_documents_metal_top50 %>%
  filter(document == 1)
ap_documents_metal_top50_topic4 <- ap_documents_metal_top50 %>%
  filter(gamma > 0.9) %>%
  filter(topic == 4)


#topic analysis for pop genre

#lda for pop

rowTotals_pop <- apply(pop_dtm , 1, sum) #Find the sum of words in each Document
dtm.new.pop   <- pop_dtm[rowTotals_pop> 0, ] 


pop_lda <- LDA(dtm.new.pop, k = 5, control = list(seed = 1234))


#interpreting the model


#ectracting the per-topic-per-word probabilities beta

ap_topics_pop <- tidy(pop_lda, matrix = "beta")
ap_topics_pop

#showing top 10 terms per topic

ap_top_terms_pop <- ap_topics_pop %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_pop %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#document topic probabilities 

ap_documents_pop <- tidy(pop_lda, matrix = "gamma")
ap_documents_pop

ap_documents_pop_top50 <- ap_documents_pop %>%
  filter(gamma > 0.5)
ap_documents_pop_top50_song1 <- ap_documents_pop_top50 %>%
  filter(document == 1)
ap_documents_pop_top50_topic2 <- ap_documents_pop_top50 %>%
  filter(gamma > 0.9) %>%
  filter(topic == 2)



#lda for rock

rowTotals_rock <- apply(rock_dtm , 1, sum) #Find the sum of words in each Document
dtm.new.rock <- rock_dtm[rowTotals_rock> 0, ] 


rock_lda <- LDA(dtm.new.rock, k = 5, control = list(seed = 1234))


#interpreting the model

#ectracting the per-topic-per-word probabilities beta

ap_topics_rock <- tidy(rock_lda, matrix = "beta")
ap_topics_rock

#showing top 10 terms per topic

ap_top_terms_rock <- ap_topics_rock %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_rock %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



#analysis on both metal and pop together


#lda for metal and pop

rowTotals_metal_pop <- apply(metal_pop_dtm , 1, sum) #Find the sum of words in each Document
dtm.new.metal_pop   <- metal_pop_dtm[rowTotals_metal_pop> 0, ] 


metal_pop_lda <- LDA(dtm.new.metal_pop, k = 2, control = list(seed = 1234))


#interpreting the model

#ectracting the per-topic-per-word probabilities beta

ap_topics_metal_pop <- tidy(metal_pop_lda, matrix = "beta")

#showing top 10 terms per topic

ap_top_terms_metal_pop <- ap_topics_metal_pop %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_metal_pop %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#document topic probabilities 

ap_documents_metal_pop <- tidy(metal_pop_lda, matrix = "gamma")
ap_documents_metal_pop

ap_documents_metal_pop_top50 <- ap_documents_metal_pop %>%
  filter(gamma > 0.5) 

sum = 0

for (element in ap_documents_metal_pop_top50) {
  print(element)
  if ( element$topic == 1 && element$document %in% rownames_pop) {
    sum = sum +1
  }
  if (element$topic == 2 && element$document %in% rownames_metal) {
    sum = sum + 1
  }
  
}




ap_documents_pop_top50_song1 <- ap_documents_pop_top50 %>%
  filter(document == 1)
ap_documents_pop_top50_topic2 <- ap_documents_pop_top50 %>%
  filter(gamma > 0.9) %>%
  filter(topic == 2)
