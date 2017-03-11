library(tm)
library(twitteR)
library(RColorBrewer)
library(wordcloud)
library(e1071)
library(class)

#Connect to the Twitter
setup_twitter_oauth(consumer_key='FuzkA8SS76XPD6izHNRs1TVt8', consumer_secret='MtPZepdC3XGp1MUs8pHw6flg0OZiQJwmgaykLz5LMAhamyQpj6', access_token='2907029881-daxkNwOFEHhWUB6WkfE3ZpswnqR0afUAmKOjCOF', access_secret='6svj827OyNHRkd0EOvbHEHKcmwNhJnQPW8ZvFezNNAQfm')

##
soccer.tweets <- searchTwitter('Modi', n= 1000, lang = 'en')

soccer.text <- sapply ( soccer.tweets, function(x) x$getText())

soccer.text <- iconv(soccer.text,'UTF-8', 'ASCII')

soccer.corpus <- Corpus(VectorSource(soccer.text))

term <- TermDocumentMatrix(soccer.corpus, control = list(removePunctuation = TRUE, stopwords('english'), 
                                                         removeNumbers= TRUE, tolower= T))

term <- as.matrix(term)

word.freq <- sort(rowSums(term), decreasing = T)
dm <- data.frame( word= names(word.freq), freq = word.freq)

#Create the word cloud 
wordcloud(dm$word, dm$freq, random.order = F, colors = brewer.pal(8,'Dark2'))

