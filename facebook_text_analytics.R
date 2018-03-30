## script to analyze facebook timeline using TF-IDF and Topic Modelling
## assumes UTF-8 encoding
## download your facebook data and unzip into the same folder that this script sits in.  
## results will be written into the same folder
##
## written by Keith McNulty


# load libraries

library(stringr)
library(tm)
library(RColorBrewer)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(ldatuning)
library(openxlsx)

# set directories

homedir <- getwd()
setwd("C:/R/facebook_analytics") # <----- change to directory where facenook data file was unzipped

# read in HTML code

doc.html <- readLines("html/timeline.htm", encoding = "UTF-8")

# unlist and create single character string

doc.html.unlisted <- unlist(doc.html)
doc.html.combined <- paste(doc.html.unlisted, collapse = " ")

#create list of posts

post_list <- stringr::str_match_all(doc.html.combined, '<div class=\"comment\">(.+?)</div>')

# create vector of posts stripped of html wrappers

post_vec <- unlist(post_list[[1]][,2])

# adjust encoding issues

post_vec <- gsub("&#039;", "'", post_vec)


# create corpus


corpus <- (tm::VectorSource(post_vec))
corpus <- tm::Corpus(corpus)

# sort out encoding

corpus<- tm::tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

# remove punctuation
corpus <- tm::tm_map(corpus, content_transformer(removePunctuation))

# remove stopwords
corpus <- tm::tm_map(corpus, content_transformer(tolower))
corpus <- tm::tm_map(corpus, content_transformer(removeWords), 
                     stopwords("english"))

# strip whitespace
corpus <- tm::tm_map(corpus, stripWhitespace) 

# remove numbers
corpus <- tm::tm_map(corpus, content_transformer(removeNumbers))

# optional replacement of private names or terms (use repeatedly for multiple terms)
# corpus <- tm::tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "term", replacement = "replacement")))

# create term document matrix

TDM <- tm::TermDocumentMatrix(corpus)

# rank terms by count

m1 <- as.matrix(TDM)
v1 <- sort(rowSums(m1), decreasing=TRUE)
d1 <- data.frame(word = names(v1), freq = v1)


# created weighted tf-idf term document matrix

TDM_tfidf <-tm::TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#rank terms by average tfidf weighting

m2 <- as.matrix(TDM_tfidf)
v2 <- sort(rowMeans(m2), decreasing=TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

# write wordclouds

jpeg("wordcloud_count.jpg", units = "in", width = 10, height = 10, res = 600)
cp <- brewer.pal(7,"YlOrRd")
wordcloud::wordcloud(d1$word, d1$freq, max.words = 100, random.order = FALSE, colors = cp)
dev.off()

jpeg("wordcloud_tfidf.jpg", units = "in", width = 10, height = 10, res = 600)
cp <- brewer.pal(7,"YlOrRd")
wordcloud::wordcloud(d2$word, d2$freq, max.words = 100, random.order = FALSE, colors = cp)
dev.off()


# TOPIC MODELLING

# ---------------------------------------------------------------------------

# optional functions for LDA tuning k in topic modelling (operate on DTM, set min and max topics to test)
# LDA tuning is computationally intensive - stay below max 20 topics

gibbs.ctl <- list(seed = 123, iter = 5000, burnin = 2000)

lda.tune <- function (x, mintops, maxtops) {ldatuning::FindTopicsNumber(
  x,
  topics = seq(mintops, maxtops),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = gibbs.ctl,
  mc.cores = 3L
)
}

plot.tuning <- function(x) {
  ldatuning::FindTopicsNumber_plot(x)
}


# -----------------------------------------------------------------------------

# enter value of ntop (required number of topics - consider using LDA tuning functions to help)

ntop <- 8  # <--- desired number of topics

# Generate Document Term Matrix

DTM <- tm::DocumentTermMatrix(corpus)

# remove empty rows from DTM and corpus

rowTotals <- apply(DTM, 1, sum) 

empty.rows <- DTM[rowTotals == 0, ]$dimnames[1][[1]]
corpus <- corpus[-as.numeric(empty.rows)]

DTM <- tm::DocumentTermMatrix(corpus)


# tun LDA process, setting a seed so that the output of the model is predictable
corpus_lda <- LDA(DTM, k = ntop, control = list(seed = 1234))

# tidy words in topics

corpus_topics <- tidy(corpus_lda, matrix = "beta")


# generare top ten word lists

topic_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# get proportion of comments by most likely topic
prop <- topicmodels::get_topics(corpus_lda)

prop_topics <- table(prop) %>% 
  prop.table() 


# write top terms and prop comments plots


jpeg("top_terms.jpg")
topic_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
dev.off()



jpeg("prop_comments.jpg")
prop_topics %>% 
  barplot(xlab = "topic ID", ylab = "Proportion of comments")
dev.off()

# write results to excel 

l <- list("Wordcounts" = d1, "TF-IDF Results" = d2, "topic top terms" = topic_top_terms, "prop_topics" = prop_topics)
write.xlsx(l, "facebook_text_analytics_results.xlsx")

# return home

setwd(homedir)
