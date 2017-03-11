library(tm)
df <- read.csv("Book.csv", stringsAsFactors = F)

df.text <- paste(df , collapse =" ")
print(head(df.text))
df.corpus<- Corpus(VectorSource(df.text))

df.text <- TermDocumentMatrix(df.corpus, control = list(removePunctuation = TRUE, stopwords('english'), 
                                                        removeNumbers= TRUE, tolower= T))
df.text <- as.matrix(df.text)

print(head(df.text))
word.freq <- sort(rowSums(df.text), decreasing = T)
dm <- data.frame( word= names(word.freq), freq = word.freq)
#Create the word cloud 
wordcloud(dm$word, dm$freq, random.order = F, colors = brewer.pal(8,'Dark2'))