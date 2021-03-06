library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("reshape2")

my.key <-"gWrXsP5Oq08XB2Ua21HnIUsUG"
my.secret <-"MMCcf0EZZeOgiDUt084J0lG5WK4XBCd4SVAfAE2nP7UKWwoNBQ"

cred <- OAuthFactory$new(consumerKey=my.key,
                         consumerSecret=my.secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL= 'https://api.twitter.com/oauth/authorize')

save(cred, file="twitter_authentication.Rdata")

cred$handshake()

registerTwitterOAuth(cred)

tweets <- searchTwitter("@google",n=200, lang="en", since='2014-12-04', until='2014-12-05')

length(tweets)

tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

head(tweets.text)

df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df, file = "google4.txt", append = TRUE)

pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

tweets.corpus <- Corpus(VectorSource(tweets.text))

tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})

match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

simple.sentiment <- match.matrix[,1] - match.matrix[,2]

hist(simple.sentiment)

sum(simple.sentiment)

googleData <- read.csv(file = "google.csv")

googleData$Start <- NULL
googleData$End <- NULL
googleData$Stock.Price <- NULL
googleData$Sentiment.Score <- NULL

molten.googleData <- melt(googleData, id.vars = c("Day", "Tweets"), 
              measure.vars = c("Sentiment.Prediction", "Actual"))

ggplot(data = molten.googleData, aes(x = Day, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  ggtitle("Sentiment Analysis - Google Stock") +
  scale_fill_manual(values=c("blue", "red"))