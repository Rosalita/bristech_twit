# Twitter Scraping and sentiment analysis of Tweets about Bristech conference 
# held on Thursday 3rd November 2016

#install.packages("tm")

library(twitteR)
library(ROAuth)
library(httr)
library(stringr)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)

# set working directory to project root
setwd("C:/Dev/git/bristech_twit")

# make sure Twitter account has a phone number attached.
# go to Twitter apps page (https://apps.twitter.com/) and create a new app
# once app is created, this will give Keys and Access tokens

# for security, to keep secrets secret they are stored in environment variables! 
# API Secret and Access Token Secret should never be human readable,

#TWITAPISECRET <- Sys.getenv("TWITAPISECRET") 
#TWITTOKENSECRET <- Sys.getenv("TWITTOKENSECRET")

# set API Keys 
#api_key <- "aVXP1fw3fyxFFYfSDsAKje3vy"
#api_secret <- TWITAPISECRET
#access_token <- "69366834-DdbmXBAxgxybC27MSBK3gaojj26Qcdr5Mi1rSzGpd"
#access_token_secret <- TWITTOKENSECRET 
#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# as of 04-11-2016 there are 764 tweets tagged #bristech2016 available
# so collect them all
#bristech2016_tweets <- searchTwitter("#bristech2016", n=764)

# looks like as of 04-11-2016 there are also 32 tweets tagged #bristech
# so collect these too  
#bristech_tweets <- searchTwitter("#bristech", n=2840)

# convert to data frame
#bristech2016df <- twListToDF(bristech2016_tweets)
#bristechdf <- twListToDF(bristech_tweets)


# remove the one tweet from bristechdf which also contain the text #bristech2016
#bristechdf <- bristechdf[-15,]

# join both searches together by combining bristech2016df and bristechdf together
#bristweets <- rbind(bristech2016df,bristechdf)

# fix row names by reindexing them
#row.names(bristweets) <- 1:nrow(bristweets)

# save the dataframe object containing these tweets as a .Rda file
#saveRDS(bristweets,file="bristweets.Rda")

# load dataframe object containing tweets to perform analysis on
bristweets <- readRDS("bristweets.Rda")


# Conference day was 03-11-2016 so create a subset of conference day tweets
#separate out the date from the tweet creation time stamp
justdate <- as.Date(bristweets$created)

#bind date onto the dataframe
bristweets <- cbind(bristweets, justdate)

# subset the data to identify tweets created on 03-11-2016, the day of the conference. 
index <- which(bristweets$justdate == "2016-11-03")
confdaytweets <- bristweets[index,]

# There were 663 Tweets on the day of the conference

#correct row names for confdaytweets dataframe
row.names(confdaytweets) <- 1:nrow(confdaytweets)

# extract text of tweets
tweettext <- confdaytweets$text  

# for a starting point, used the Hu and Liu Opinion Lexicon 
# from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar

# import the good words and get them into a vector
good <- read_file("positive-words.txt", locale = default_locale())
good = gsub('[[:cntrl:]]', ' ',good)  # replace control characters, like \n or \r with a space 
good.list = str_split(good, '\\s+')   # split into a list of words
good_text = unlist(good.list)         # make sure words is a vector, not a list
good_text = good_text[1:(length(good_text) -1)] # the last item appears to be "", so just trim it off

# import the bad words and get them into a vector
bad <- read_file("negative-words.txt", locale = default_locale())
bad = gsub('[[:cntrl:]]', ' ',bad)  # replace control characters, like \n or \r with a space 
bad.list = str_split(bad, '\\s+')   # split into a list of words
bad_text = unlist(bad.list)         # make sure words is a vector, not a list
bad_text = bad_text[1:(length(bad_text) -1)] # the last item appears to be "", so just trim it off

# initialise some global variables
positivity <- NULL
negativity <- NULL

# Now score the text of each tweet based on count of positive and negative words used.

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up each sentence with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)  # strips out punctuation
    sentence = gsub('[[:cntrl:]]', '', sentence)  # strips out control characters, like \n or \r 
    sentence = gsub('\\d+', '', sentence)         # strips out numbers
    sentence <- iconv(sentence, to='UTF-8')       # convert to UTF8
    sentence = tolower(sentence)                  # converts all text to lower case     
    word.list = str_split(sentence, '\\s+')       # split into a list of words
    words = unlist(word.list)                     # make sure words is a vector, not a list
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # convert matches to TRUE/FALSE instead
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE is treated as 1/0 by sum(), so add up the score
    score = sum(pos.matches) - sum(neg.matches)
    
    #if any positive matches
    if (any(pos.matches)){
      pos.matches = match(words, good_text)
      pos.words = good_text[pos.matches] # apply index of pos matches to get pos words
      pos.words = pos.words[!is.na(pos.words)] # remove any NA values
      # append positive words to global positivity variable
      positivity <<- append(positivity, pos.words)
    }
    
    # identify the words which matched positively or negatively
    # maybe use <<- to set pos.words and neg.words as global variables?
    if (any(neg.matches)){
      neg.matches = match(words, bad_text)
      neg.words = bad_text[neg.matches] # apply index of neg matches to get neg words
      neg.words = neg.words[!is.na(neg.words)] # remove any NA values 
      #append negative words to global negativity variable
      negativity <<- append(negativity, neg.words)
    }
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# call the score sentiment function and return a data frame
feelings <- score.sentiment(tweettext, good_text, bad_text, .progress='text')

sentiment_score <- feelings$score

# bind the sentiment scores onto the tweet dataframe
confdaytweets <- cbind(confdaytweets,sentiment_score)

library(tm)
library(wordcloud)

# extract just the tweet text 
textdata <- confdaytweets$text

#check encoding
Encoding(textdata) # mixture of all kinds of encoding

#Apply Native encoding on the vector    
textdata <- enc2native(textdata)

#Apply UTF-8 encoding on the vector
textdata <- enc2utf8(textdata)

# This removes all weird characters
# some are changed to format \u0085
# and others in format <U+0096>


# Can start to do a bit tiny of cleaning at this stage
# Any '&' characters will now be '&amp;', simply replace them with the word 'and'
textdata = gsub("\\&amp;", "and", textdata)

# Any ">" or "<" characters will now be '&gt;' and '&lt;', so just remove these with gsub
textdata = gsub("\\&gt;", " ", textdata)
textdata = gsub("\\&lt;", " ", textdata)

# identify tweets containing an @username mention, they will contain a words that start with @
index <- grep("(^|[^@\\w])@(\\w{1,15})\\b",textdata)
mentions <- textdata[index]

#split each of the tweets with a mention down into individual words
mentions <- unlist(strsplit(mentions, " "))

regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @
users <- gsub(regex2, "", mentions[grep(regex1, mentions, perl = T)])

# element 40 in the username vector looks like it has some leftover emojii remnants stuck to it
users[40]

# tidy up edU00A0U00BDedU00B8U00BBedU00A0U00BDedU00B8U00BB@openbionics as this is just an edge case
users[40] <- "@openbionics"

#TWitter usernames aren't case sensitive so convert them all to lowercase
users <- tolower(users)

# some times people tweet "@username's" and the apostrophe is stripped out 
# This can result in two versions of the user name being captured, with and without an s on the end
# manually correct additional usernames ending in s

#samhogys to samhogy
users[which(users =="@samhogys")] <- "@samhogy"
#pimterrys to pimterry
users[which(users =="@pimterrys")] <- "@pimterry"
#subliminos to sublimino
users[which(users =="@subliminos")] <- "@sublimino"
#nwplanets to nwplanets
users[which(users =="@nwplanets")] <- "@nwplanet"

# All users mentioned in tweets
total <- table(users)

sort(total)
unique(users)

length(unique(users))

# remove the usernames from the text data now they have been collected
textdata = gsub("(^|[^@\\w])@(\\w{1,15})\\b", " ", textdata)

# Can do a bit more cleaning now
textdata = gsub('[[:punct:]]', '', textdata)  # strips out punctuation
textdata = gsub('[[:cntrl:]]', ' ', textdata)  # strips out control characters, like \n or \r 
textdata = gsub('\\d+', '', textdata)         # strips out numbers

#Look at a sample of tweets to see if any more cleaning is needed

textdata = gsub("RT", " ", textdata) #remove any instances of "RT" as this isn't a real word




# there are some orphaned "s" on their own created previous cleanings
textdata = gsub("^s\\s", " ", textdata) #remove any single "s" followed by a space,

#strip out remains of links
textdata = gsub('ht(\\w{1,60})\\b', '', textdata) #remove any words between 1 and 60 chars starting with "ht"


textdata[617] # emoji residue edUAUBCedUBUACedUAUBCedUBUA
textdata[647] # emojii residue edUAUBDedUBUDedUAUBCedUBFUBB
textdata[477] # more emojii residue edUAUBDedUBUA & edUAUBDedUBUU

#strip out emoji residue, anything starting with edUAU
textdata = gsub('edUAU(\\w{1,60})\\b', '', textdata) #remove any words upto 60 chars long starting "edUAU"


# convert textdata to dataframe so can transfer it to corpus later
textdataframe <- as.data.frame(textdata)


# now convert all tweet text to vector so can split it up on each word
vectortext <- as.character(textdata[,1])

# convert text in the vector to all lowercase
vectortext <- tolower(vectortext)

# extract the words by splitting up the vector
words <- unlist(strsplit(vectortext, " "))

# to do remove blanks "" and plot a barchart of words


# The main structure for managing text is tm package is a corpus. 
# A Corpus represents a collection of text documents.
# Vcorpus is a volatile corpus, this is an R object held in memory if you delete it all your text is gone
# Pcorpus is a Permanent corpus, this is Permanent Corpus the text is stored outside of R (e.g. in a database)

# To get your text into a corpus there are three main source functions that can help:
# DirSource() uses a path to a file to get text
# VectorSource() gets text from a vector
# DataframeSource() gets text from a data frame

# to make a corpus, use Corpus() on one of these source functions

# make a corpus of all tweet text
tweetcorpus <- Corpus(DataframeSource(textdataframe))

# if done this correctly tweetcorpus should be a VCorpus
tweetcorpus

# a corpus could contain several thousand documents, like a database
# might not be possible to view a whole corpus on screen
# following functions exist to inspect corpus

# to see the meta data for the first tweet
inspect(tweetcorpus[1])

# to see the contents of the first tweet
tweetcorpus[[1]]$content

# Now all tweet text is in a corpus can use the tm package to clean it up
# tm has a number of transformations that can be done via the tm_map() function
# tm_map() maps a function to all elements of the corpus.

# Eliminitate extra whitespace
tweetcorpus <- tm_map(tweetcorpus, stripWhitespace)

# Convert to lowercase 
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))


# Stop words are words which do not contain important information such as "the", "and", "to"
# tm package can strip these out of the tweets
tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords("english"))

# remove white space again after stripping out stop words
tweetcorpus <- tm_map(tweetcorpus, stripWhitespace)

# Look at the first tweet to check now lowercase, no extra whitespace, no stop words
tweetcorpus[[1]]$content

# to do analyse word frequency in corpus

# can now create a term/document matrixs from the corpus

# documents are tweets, terms are words
dtm <- DocumentTermMatrix(tweetcorpus)
tdm <- TermDocumentMatrix(tweetcorpus)

# can look at a small section of the matrix
inspect(dtm[18:23,582:587])

#operations on document term matricies
#find all the terms which occur at least 500 times
findFreqTerms(dtm, 11)
findFreqTerms(dtm, lowfreq = 5, highfreq = 100)

# most frequent word typed 652 times in tweets was "bristech"
# second most frequent word typed 118 times was "talk"


?findFreqTerms

# Start a new plot frame
plot.new()

# Wordcloud
wordcloud(tweetcorpus, 
          scale=c(2,0.6), 
          max.words=2000,
          min.freq=2,
          random.order=FALSE, 
          rot.per=0.2, 
          use.r.layout=FALSE, 
          # Nice custom blue to green sequential colours
          colors = c(#"#ACF8A5",
            #"#8DE99B",
            #"#77DB9D", 
            "#63CDA4", 
            "#50BFAE",
            "#3FA7B1", 
            "#307EA2", 
            "#235594", 
            "#172F86",
            "#100E78",
            "#200569"))



