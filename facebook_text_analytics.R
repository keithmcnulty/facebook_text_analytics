## script to analyze facebook timeline using TF-IDF and Topic Modelling
## assumes UTF-8 encoding
## download your facebook data and unzip into the same folder that this script sits in.  
## results will be written into the 'results' folder
## run webshot::install_phantomjs() if necessary to ensure image capture of plotly charts
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
library(webshot)
library(plotly)

# set directories

homedir <- getwd()
setwd("C:/R/facebook_analytics") # <----- change to directory where facebook data file was unzipped

if(!dir.exists("results")) {
  dir.create("results")
}

# read in HTML code

doc.html <- readLines("html/timeline.htm", encoding = "UTF-8")
setwd("./results")

# unlist and create single character string

doc.html.unlisted <- unlist(doc.html)
doc.html.combined <- paste(doc.html.unlisted, collapse = " ")

#create list of posts and dates

post_list <- stringr::str_match_all(doc.html.combined, '<div class=\"comment\">(.+?)</div>')

datetime_list <- stringr::str_match_all(doc.html.combined, '<div class=\"meta\">(.+?)</div>')

#strip out dates and hours of activity

datetime_vec <- unlist(datetime_list[[1]][,2])

date_vec <- as.POSIXct(datetime_vec, format = "%A, %d %B %Y", tz="GMT")

day_vec <- format(date_vec, "%A") %>% as.factor()

dom_vec <- format(date_vec, "%d") %>% as.factor()

mon_vec <- format(date_vec, "%B") %>% as.factor() %>% factor(levels = month.name) 

year_vec <- dom_vec <- format(date_vec, "%Y") %>% as.factor()

time_vec <- sub('.*at ', '', datetime_vec)

time_vec <- as.POSIXct(time_vec, format = "%H", tz="GMT")

hour_vec <- format(time_vec, "%H") %>% as.factor()

datemon_vec <- format(date_vec, "%B %Y") %>%  as.factor()

date_df <- data.frame(year_vec, hour_vec, datemon_vec)

# analyze by year, month, dom, day, hour

year_totals <- date_df %>% dplyr::group_by(year_vec) %>% dplyr::summarize(total = n())
hour_totals <- date_df %>% dplyr::group_by(hour_vec) %>% dplyr::summarize(total = n()) 
date_totals <- date_df %>% dplyr::group_by(datemon_vec) %>% dplyr::summarize(total = n()) 

# create monthly activity scatter

date_totals$datemon_vec <- paste("01", date_totals$datemon_vec)
date_totals$datemon_vec <- as.Date(date_totals$datemon_vec, format = "%d %B %Y")
p <- ggplot2::qplot(data = date_totals, datemon_vec, total, xlab = "Year", ylab = "Posts")
p1 <- p + ggplot2::geom_smooth(method = "loess", size = 1.5) + ggplot2::ggtitle("Activity timeline")

jpeg("activity_by_month_scatter.jpeg")
plot(p1)
dev.off()

# activity by year bar chart

p2 <- plotly::plot_ly(year_totals, x = ~year_vec, y = ~total, type = 'bar') %>% 
  plotly::layout(title = "Posts by year", xaxis = list(title = "Year"), yaxis = list(title = "Posts"))
export(p2, "yearly_activity_bar.jpeg")

# hour of day bar chart

p3 <- plotly::plot_ly(hour_totals, x = ~hour_vec, y = ~total, type = 'bar', name = 'Posts') %>% 
  plotly::layout(title = "Posts by hour of day", xaxis = list(title = "Hour"), yaxis = list(title = "Posts")) %>%  
  plotly::add_lines(y = ~fitted(loess(total ~ as.numeric(hour_vec))), line = list(color = 'red'),
            name = "Loess Smoother", showlegend = TRUE)
export(p3, "hourly_activity_bar.jpeg")                                                                                                                



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

# harmonize and anonymize kids names


corpus <- tm::tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "gracie", replacement = "child1")))
corpus <- tm::tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "isla", replacement = "child2")))


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

jpeg("wordcloud_count.jpeg", units = "in", width = 10, height = 10, res = 600)
cp <- brewer.pal(7,"YlOrRd")
wordcloud::wordcloud(d1$word, d1$freq, max.words = 100, random.order = FALSE, colors = cp)
dev.off()

jpeg("wordcloud_tfidf.jpeg", units = "in", width = 10, height = 10, res = 600)
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
corpus_lda <- topicmodels::LDA(DTM, k = ntop, control = list(seed = 1234))

# tidy words in topics

corpus_topics <- tidytext::tidy(corpus_lda, matrix = "beta")


# generare top ten word lists

topic_top_terms <- corpus_topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(10, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta)


# get proportion of comments by most likely topic
prop <- topicmodels::get_topics(corpus_lda)

prop_topics <- table(prop) %>% 
  prop.table() 


# write total_activity, top terms and prop comments plots

total_days <- as.integer(max(date_vec) - min(date_vec))
total_posts <- length(corpus)

chart_df <- data.frame(x = c("Days", "Posts"), y = c(total_days, total_posts))

p4 <- plotly::plot_ly(chart_df, x = ~x, y = ~y, type = 'bar') %>% 
  plotly::add_annotations(x = ~x, y = ~y, text = ~y, yanchor = 'bottom', showarrow = FALSE) %>% 
  plotly::layout(title = "Total Facebook activity", xaxis = list(title = ""), 
         yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE))
export(p4, "total_activity_bar.jpeg")

p5 <- topic_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

jpeg("topic_top_terms.jpeg")
plot(p5)
dev.off()

p6 <- plotly::plot_ly(as.data.frame(prop_topics), x = ~prop, y = ~Freq, type = 'bar') %>% 
  plotly::layout(xaxis = list(title = "Topic ID"), yaxis = list(title = "Proportion of posts"))
export(p6, "proportion_posts_by_topic.jpeg")

# write results to excel 

l <- list("Wordcounts" = d1, "TF-IDF Results" = d2, "topic top terms" = topic_top_terms, "prop_topics" = prop_topics)
openxlsx::write.xlsx(l, "facebook_text_analytics_results.xlsx")

# return home

setwd(homedir)
