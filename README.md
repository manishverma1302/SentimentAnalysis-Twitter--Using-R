# SentimentAnalysis-Twitter--Using-R
Create a developer account at apps.twitter.com to create an app and to get the following keys  api_key  &lt;- '' api_secret  &lt;- '' access_token  &lt;- '' access_token_secret  &lt;- ''. Using only these keys, you will be able to do Twitter Analytics. We are going to use R for the Sentiment Analysis. First we will create the .csv for further analysis. Using that .csv file we will create bar plot, which will shows the frequency of the word. We can also create the meaningful Word Cloud. After that, we will do the sentiment analysis using NRC Lexicon. You can replace nrc with other Lexicon to see the difference between them.
# Create a developer account at apps.twitter.com to create an app and to get the following keys

api_key  <- ''
api_secret  <- ''
access_token  <- ''
access_token_secret  <- ''


# Now we have to load library
library(twitteR)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Getting tweets
tweets <- searchTwitter('lucky charms', n=5000, lang='en',  geocode='44.963323,-93.268284, 80mi')
tweets

#We will convert tweets into dataframe
tweetsdf <- twListToDF(tweets)


# We have to save the file in computer

write.csv(tweetsdf, file = "give your path name\\filename.csv", row.names = F)

# Read file from computer
mankatocereal <- read.csv(file.choose(), header = T)

# Look for structure of the table
str(mankatocereal)

# tm stands for text mining
library(tm)

# Build corpus
# corpus <- iconv(apple$text, to = "utf-8-mac")  # optional for MAc OS

mani <- Corpus(VectorSource(mankatocereal$text))

# Clean text 
mani <- tm_map(mani, tolower) # convert all into lower case

mani <- tm_map(mani, removePunctuation) # remove all puntuations

mani <- tm_map(mani, removeNumbers) # remove all numeric numbers

cleanset <- tm_map(mani, removeWords, stopwords('english')) # remove common english words

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)

cleanset <- tm_map(cleanset, content_transformer(removeURL)) # remove URL

# we should remove common words, otherwise it will overshadow

cleanset <- tm_map(cleanset, removeWords, c('lucky', 'charms', 'cereal'))

cleanset <- tm_map(cleanset, stripWhitespace) # remove white space

# Term document matrix (convert unstructure Data to Structure Data)
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
#tdm[1:10, 1:20]  # to see first 1:10 tweets in 1:20 documents



# Bar plot
w <- rowSums(tdm)   # If we run w it will give frequency of words
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)

wordcloud(words = names(w),
          freq = w,
          random.order = F,
          min.freq = 5,
          color = rainbow(50),
          scale = c(3, 0.5),
          rot.per = 0.3)
#############################################################################################################

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file from computer
verma <- read.csv(file.choose(), header = T)
#tweets <- iconv(verma$text, to = 'utf-8-mac')  # optional for MAc OS
tweets <- as.character(verma$text)

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Heading of the Chart')
###############################################################################################################
