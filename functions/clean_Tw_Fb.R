cleanTweets<-function(tweet){
  # Clean the tweet for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet
}

cleanTweets2 <-function(tweet){
  # Clean the tweet for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  # tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("([#])|[[:punct:]]", " \\1", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet
}

cleanTweets3 <-function(tweet){
  tweet= gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet= gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet= gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet= gsub("([#@])|[[:punct:]]", " \\1", tweet)
  # Thenremovenumbers, weneedonlytext for analytics
  # tweet= gsub("[[:digit:]]", " ", tweet)
  # finally, weremoveunnecessaryspaces(whitespaces, tabsetc)
  tweet= gsub("[ \t]{2,}", " ", tweet)
  tweet= gsub("^\\s+|\\s+$", "", tweet)
  tweet
}

cleanFB <- function(x) {
  x = gsub('http\\S+\\s*', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub("\\d", '', x)
  x = gsub('[[:punct:]]', ' ', x)
  x = gsub("^[[:space:]]*","",x)
  x = gsub("[[:space:]]*$","",x)
  x = gsub(' +',' ',x)
  x
}