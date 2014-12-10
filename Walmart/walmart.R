library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("reshape2")

my.key <-"1PE8eCeguQTdpiPl6oyiUDx6K"
my.secret <-"DTEQfmrba0kaKB7IphzQACtwpWhh9pAS7qdH2SUilurnW69ZZb"

cred <- OAuthFactory$new(consumerKey=my.key,
                         consumerSecret=my.secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL= 'https://api.twitter.com/oauth/authorize')

save(cred, file="twitter_authentication.Rdata")

cred$handshake()
#WAIT and authorize your application API key

registerTwitterOAuth(cred)

tweets <- searchTwitter("@Walmart",n=200, lang="en", since='2014-12-04', until='2014-12-05')

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
write.table(df, file = "walmart4.txt", append = TRUE)

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


#Create your CSV Data file out of sentiment analysis and actual data from
#Read this data
WalmartData <- read.csv(file = "Walmart.csv")

WalmartData$Start <- NULL
WalmartData$End <- NULL

WalmartData

WalmartData$Stock.Price <- NULL
WalmartData$Sentiment.Score <- NULL

molten.WalmartData <- melt(WalmartData, id.vars = c("Day", "Tweets"), 
                           measure.vars = c("Sentiment.Prediction", "Actual"))

ggplot(data = molten.WalmartData, aes(x = Day, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  ggtitle("Google Stock Sentiment Analysis") +
  scale_fill_manual(values=c("blue", "red"))
