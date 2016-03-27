# loading the data
blogs <- readLines("Coursera-Swiftkey/final/en_US/en_US.blogs.txt")
news <- readLines("Coursera-Swiftkey/final/en_US/en_US.news.txt")
twitter <- readLines("Coursera-Swiftkey/final/en_US/en_US.twitter.txt")
#data sets lengths
blogslines <- length(blogs)
newslines <- length(news)
twitlines <- length(twitter)
# sampling the data
samplerate <- 0.15
blogsample <- blogs[sample(1:blogslines, blogslines*samplerate)]
newsample <- news[sample(1:newslines, newslines*samplerate)]
twitsample <- twitter[sample(1:twitlines, twitlines*samplerate)]
sampledata <- list(blogsample, newsample, twitsample)
#tokenization the data
library(tm)
tsampledata <- VCorpus(VectorSource(sampledata))
tsampledata <- tm_map(tsampledata, removeNumbers) 
tsampledata <- tm_map(tsampledata, stripWhitespace)
tsampledata <- tm_map(tsampledata, content_transformer(tolower))
tsampledata <- tm_map(tsampledata, removePunctuation)
badwords <- readLines("./badwords.txt")
tsampledata <- tm_map(tsampledata, removeWords, badwords)
#bad words source: http://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip
#frequencies analysis
library(RWekajars)
library(RWeka)
library(wordcloud)
require(openNLP)
require(reshape)
NgramTokenizer <- function(l) {
  function(x) unlist(lapply(ngrams(words(x), l), paste, collapse = " "), use.names = FALSE)
}

##generate unigram data set
GenerateNgramData <- function(n) {
  if(n == 1) {
    ng_tdm <- TermDocumentMatrix(tsampledata)
  } else {
    ng_tdm <- TermDocumentMatrix(tsampledata, control = list(tokenize = NgramTokenizer(n)))
  }
  
  ng_matrix <- as.matrix(ng_tdm)
  ng_matrix <- rowSums(ng_matrix)
  ng_matrix <- sort(ng_matrix, decreasing = TRUE)
  final_ngram <- data.frame(terms = names(ng_matrix), freq = ng_matrix)
  
  if(n == 2) columns <- c('one', 'two')
  if(n == 3) columns <- c('one', 'two', 'three')
  if(n == 4) columns <- c('one', 'two', 'three', 'four')
  
  if(n > 1) {
    final_ngram <- transform(final_ngram, terms = colsplit(terms, split = " ", names = columns ))
  }
  
  rownames(final_ngram) <- NULL
  final_ngram
}
final_unigram <- GenerateNgramData(1)
final_bigram <- GenerateNgramData(2)
final_trigram <- GenerateNgramData(3)
final_fourgram <- GenerateNgramData(4)

#Calculate probabilities
unigram_count <- sum(final_unigram$freq)
bigram_count <- sum(final_bigram$freq)
trigram_count <- sum(final_trigram$freq)
fourgram_count <- sum(final_fourgram$freq)

final_unigram <- transform(final_unigram, p = freq / unigram_count, pw = 0)
final_bigram <- transform(final_bigram, p = freq / bigram_count, pone = 0, termone = terms$one, termtwo = terms$two, terms = NULL)
final_trigram <- transform(final_trigram, p = freq / trigram_count, pw = 0, termone = terms$one, termtwo = terms$two, termthree = terms$three, terms = NULL)
final_fourgram <- transform(final_fourgram, p = freq / fourgram_count, pw = 0, termone = terms$one, termtwo = terms$two, termthree = terms$three, termfour = terms$four, terms = NULL)
#save final output for fast performace of Shiny App
saveRDS(final_unigram, file = "final_unigram.Rda")
saveRDS(final_bigram, file = "final_bigram.Rda")
saveRDS(final_trigram, file = "final_trigram.Rda")
saveRDS(final_fourgram, file = "final_fourgram.Rda")

#Significantly reduce data size by only keeping grams greater than the avg count
final_bigram_sm <- final_bigram[final_bigram$freq > mean(final_bigram$freq),]
final_trigram_sm <- final_trigram[final_trigram$freq > mean(final_trigram$freq),]
final_fourgram_sm <- final_fourgram[final_fourgram$freq > mean(final_fourgram$freq),]

saveRDS(final_bigram_sm, file = "final_bigram_sm.Rda")
saveRDS(final_trigram_sm, file = "final_bigram_sm.Rda")
saveRDS(final_fourgram_sm, file = "final_bigram_sm.Rda")

