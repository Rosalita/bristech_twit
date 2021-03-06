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

# subset the data to identify tweets created on 03-11-2016, the day of the conference. 
index <- which(as.Date(bristweets$created) == "2016-11-03")
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

textdata = gsub("RT", " ", textdata) #remove any instances of "RT" as this isn't a real word

# there are some orphaned "s" on their own created previous cleanings
textdata = gsub("^s\\s", " ", textdata) #remove any single "s" followed by a space,

#strip out remains of links - commented out because fear this turned word 'delight' into delig'
# textdata = gsub('ht(\\w{1,60})\\b', '', textdata) #remove any words between 1 and 60 chars starting with "ht"

#strip out emoji residue, anything starting with edUAU
#This cleans up leftovers like edUAUBDedUBUA, edUAUBDedUBUU and edUAUBDedUBUDedUAUBCedUBFUBB
textdata = gsub('edUAU(\\w{1,130})\\b', '', textdata) #remove any words upto 130 chars long starting "edUAU"

#convert all text to lowercase
textdata <- tolower(textdata)

# convert textdata to dataframe so can transfer it to corpus later
textdataframe <- as.data.frame(textdata)

# extract the words by splitting up the text data
words <- unlist(strsplit(textdata, " "))

# remove blank ""'s in the vector of words
words <- words[words != ""]

#total up the words in a table
wordtable <- table(words)

#store totals in a data frame
worddf <- as.data.frame(wordtable, stringsAsFactors=FALSE)

#remove stop words
#read in a list of stop words
stopwords <- read_file("stop-words.txt", locale = default_locale())
#clean up control chars
stopwords = gsub('[[:cntrl:]]', ' ',stopwords)  # replace control characters, like \n or \r with a space 
# split the stopwords up into a vector of words
stopwords <- unlist(strsplit(stopwords, " "))
# remove blank ""'s
stopwords <- stopwords[stopwords != ""]

#make an index of stopwords which are in the words dataframe
index <- which(worddf[,1] %in% stopwords)

#remove the stopwords from the words dataframe
worddf <- worddf[-index,]

#re-index
row.names(worddf) <- 1:nrow(worddf)

# rows 476 - 673 are all partial link text so discard these values
index <- 476:673
worddf <- worddf[-index,]
#relabel rows
row.names(worddf) <- 1:nrow(worddf)

#correct micro$oft as the $ got stripped out
worddf[634,1] <- "micro$ofts"

#looking at the worddf the longest word tweeted was
worddf[1028,1]
#tweet containing the longest word
confdaytweets[113,1]

# Now score each word on whether it is positive or negative.
library(plyr)
# ddply() takes a dataframe, does stuff to it, returns a dataframe

# Score all the words and output as dataframe
scoredwords <- ddply(worddf, "words", function(x) {
  wordtocheck <- x$words
  # compare the word to check to the dictionaries of positive & negative terms
  pos.match = match(wordtocheck, good_text)
  neg.match = match(wordtocheck, bad_text)

  # match() returns the position of the matched term or NA
  # convert matches to TRUE/FALSE instead
  pos.match = !is.na(pos.match)
  neg.match = !is.na(neg.match)
  
  # TRUE/FALSE is treated as 1/0 by sum(), so add up the score
  score = sum(pos.match) - sum(neg.match)
  })


# bind the word frequencies onto this dataframe
scoredwords <- cbind(scoredwords, worddf$Freq)

# tidy up column names on this new dataframe to sentiment, -1 is negative, 0 is neutral, +1 is positive
colnames(scoredwords) <- c("words", "sentiment", "freq")

# sentiment is currently stored as an int which is continuous data type
# for plotting purposes, change it to char which is discrete 
scoredwords$sentiment <- as.character(scoredwords$sentiment)

#find the popular words
popularwordindex <- which(scoredwords$freq > 5)

popularwords <- scoredwords[popularwordindex,]

#plot word frequency
p <- ggplot(popularwords, aes(x=reorder(words, -freq),y=freq, fill=popularwords$sentiment )) +
  geom_bar(stat="identity")+ 
  coord_cartesian(xlim = NULL, ylim = c(0,130), expand = TRUE)+
  annotate("text", x = 18, y = 125, label = "'bristech' frequency = 652 (extends off chart)")+
  labs(x="Popular Words (5+ mentions)", y="Frequency", fill="Sentiment")+
 # scale_x_discrete(trans ="reverse")+
  scale_fill_manual(breaks = c("-1", "0", "1"),
                    labels = c("Negative", "Neutral", "Positive"),
                    values = c("#FF0000", "#d3d3d3", "#00ff00"),
                    limits = c(-1, 0, 1))+   
  ggtitle("Tweeted Words by Frequency and Sentiment")
 # coord_flip()
#ggtitle("Strategies for Using Homework Solution and Mini-Lecture Screencasts")

p +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))


# extract just the tweet text 
text <- confdaytweets$text

# initialise some global variables
positivity <- NULL
negativity <- NULL

# Now score the text of each tweet based on count of positive and negative words used.

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)

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

# Call the score sentiment function and return a data frame
feelings <- score.sentiment(text, good_text, bad_text, .progress='text')

sentiment_score <- feelings$score

confdaytweets <- cbind(confdaytweets,sentiment_score)

#tally up all the positive and negative words in a table.
ptable <- table(positivity)
ntable <- table(negativity)

# Word clouds

library(tm)
library(wordcloud)

# make a corpus for positive and negative words
pcorp = Corpus(VectorSource(positivity))
ncorp = Corpus(VectorSource(negativity))

#pcorp <- tm_map(pcorp, PlainTextDocument)
#ncorp <- tm_map(ncorp, PlainTextDocument)

# The main structure for managing text in tm package is a corpus. 
# A Corpus represents a collection of text documents.
# Vcorpus is a volatile corpus, this is an R object held in memory if you delete it all your text is gone
# Pcorpus is a Permanent corpus, this is Permanent Corpus the text is stored outside of R (e.g. in a database)

# To get your text into a corpus there are three main source functions that can help:
# DirSource() uses a path to a file to get text
# VectorSource() gets text from a vector
# DataframeSource() gets text from a data frame

# to make a corpus, use Corpus() on one of these source functions


# Start a new plot frame
plot.new()

# Set the display a 2 by 2 grid
par(mfrow=c(1,2))

# Outer Margins
par(oma=c(0.5,0.1,0.5,0.1))
# Margins, bottom, left, top, right (default is  c(5,4,4,2))
par(mar=c(0.1,3,0.1,3))

# par(mar=c(9.3,4.1,4.1,2.1))
# par(mfrow=c(2,2))
# par(cex.axis=1.3)
# par(cex.main=1.3)


# Positive Wordcloud
wordcloud(pcorp, 
          scale=c(2,0.6), 
          max.words=200,
          min.freq=-1,
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

text(x=-0.03, y=0.5, "Positive Words", srt=90)


# Negative Wordcloud
wordcloud(ncorp, 
          scale=c(2,0.6), 
          max.words=200, 
          min.freq=-1,
          random.order=FALSE, 
          rot.per=0.2, 
          use.r.layout=FALSE, 
          # Nice custom yellow to red colours
          colors = c(#"#FFDE6A",
            #"#F4C55C",
            "#E9AC4F", 
            "#DF9343", 
            "#D47A37",
            "#CA612D", 
            "#BF4A23", 
            "#B4331A", 
            "#AA1D11",
            "#9F0A0C"))


text(x=1.03, y=0.5, "Negative Words", srt=270)




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


# plot tweets by time and sentiment


#bind a column on to conference day tweets to hold key times to divide the plot
keytimes <- NA
confdaytweets <- cbind(confdaytweets, keytimes)

#indicate first line is at 08:00 am
confdaytweets[1,18] = "2016-11-03 08:30:00 GMT" # start of registration and networking
confdaytweets[2,18] = "2016-11-03 09:00:00 GMT" # end of registration
confdaytweets[3,18] = "2016-11-03 09:15:00 GMT" # end of welcome
confdaytweets[4,18] = "2016-11-03 09:30:00 GMT" # talk 1 start
confdaytweets[5,18] = "2016-11-03 10:20:00 GMT" # talk 1 end
confdaytweets[6,18] = "2016-11-03 10:30:00 GMT" # talk 2 start
confdaytweets[7,18] = "2016-11-03 11:20:00 GMT" # talk 2 end
confdaytweets[8,18] = "2016-11-03 11:50:00 GMT" # talk 3 start
confdaytweets[9,18] = "2016-11-03 12:40:00 GMT" # talk 3 end
confdaytweets[10,18] = "2016-11-03 13:40:00 GMT" # talk 4 start
confdaytweets[11,18] = "2016-11-03 14:30:00 GMT" # talk 4 end
confdaytweets[12,18] = "2016-11-03 14:40:00 GMT" # talk 5 start
confdaytweets[13,18] = "2016-11-03 15:30:00 GMT" # talk 5 end
confdaytweets[14,18] = "2016-11-03 15:55:00 GMT" # talk 6 start
confdaytweets[15,18] = "2016-11-03 16:45:00 GMT" # talk 6 end
confdaytweets[16,18] = "2016-11-03 17:00:00 GMT" # closing goodbye end

# add time stamps for mid points so some labels can be plotted in the middle of rects
confdaytweets[17,18] = "2016-11-03 08:45:00 GMT" # mid registration
confdaytweets[18,18] = "2016-11-03 09:07:30 GMT" # mid welcome
confdaytweets[19,18] = "2016-11-03 09:55:00 GMT" # mid talk 1
confdaytweets[20,18] = "2016-11-03 10:55:00 GMT" # mid talk 2
confdaytweets[21,18] = "2016-11-03 12:15:00 GMT" # mid talk 3
confdaytweets[22,18] = "2016-11-03 14:05:00 GMT" # mid talk 4
confdaytweets[23,18] = "2016-11-03 15:05:00 GMT" # mid talk 5
confdaytweets[24,18] = "2016-11-03 16:20:00 GMT" # mid talk 6
confdaytweets[25,18] = "2016-11-03 16:52:30 GMT" # mid closing goodbye

# add time stamps for xlim on plots
confdaytweets[31,19] = "2016-10-21 05:30:00 GMT"
confdaytweets[32,19] = "2016-10-22 01:30:00 GMT"  

# convert location of lines to dates to POSIXct
confdaytweets$keytimes <- as.POSIXct(confdaytweets$keytimes, tz="GMT")

str(confdaytweets$keytimes)


# plot tweets onthe day of the conference by time and sentiment
plot <- ggplot(confdaytweets, aes(x = created, y = sentiment_score))+
  geom_jitter(alpha = 0.4)+ 
  ggtitle("Tweets by Time and Positivity for #bristech")+
  labs(x="Time", y="Positivity Index")+
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")+ #use scale_*_datetime for POSIXct variables
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3,4)) +
  
  # Registration 
  annotate("rect", xmin=confdaytweets[1,18], xmax=confdaytweets[2,18],ymin=-5, ymax=7, alpha=0.3, fill="#4285F4")+
  # Registration  Label
  annotate("label", x=confdaytweets[17,18], y=8, label= "Registration", color="black", fill ="#4285F4", alpha=0.3) + 
  
  # Welcome
  annotate("rect", xmin=confdaytweets[2,18], xmax=confdaytweets[3,18],ymin=-5, ymax=7, alpha=0.3, fill="#EA4335")+
  # welcome Label
  annotate("label", x=confdaytweets[18,18], y=-6, label= "Welcome", color="black", fill ="#EA4335", alpha=0.3)+ 
  
  # talk 1
  annotate("rect", xmin=confdaytweets[4,18], xmax=confdaytweets[5,18],ymin=-5, ymax=7, alpha=0.3, fill="#FBBC05")+
  # talk 1 Label
  annotate("label", x=confdaytweets[19,18], y=8, label= "Talk 1", color="black", fill ="#FBBC05", alpha=0.3)+  
  
  # talk 2
  annotate("rect", xmin=confdaytweets[6,18], xmax=confdaytweets[7,18],ymin=-5, ymax=7, alpha=0.3, fill="#4285F4")+
  # talk 2 Label
  annotate("label", x=confdaytweets[20,18], y=-6, label= "Talk 2", color="black", fill ="#4285F4", alpha=0.3) + 
  
  # talk 3
  annotate("rect", xmin=confdaytweets[8,18], xmax=confdaytweets[9,18],ymin=-5, ymax=7, alpha=0.3, fill="#EA4335")+
  # talk 3 Label
  annotate("label", x=confdaytweets[21,18], y=8, label= "Talk 3", color="black", fill ="#EA4335", alpha=0.3) + 
  
  # talk 4
  annotate("rect", xmin=confdaytweets[10,18], xmax=confdaytweets[11,18],ymin=-5, ymax=7, alpha=0.3, fill="#FBBC05")+
  # talk 4 Label
  annotate("label", x=confdaytweets[22,18], y=-6, label= "Talk 4", color="black", fill ="#FBBC05", alpha=0.3) +
  
  # Talk 5
  annotate("rect", xmin=confdaytweets[12,18], xmax=confdaytweets[13,18],ymin=-5, ymax=7, alpha=0.3, fill="#4285F4")+
  # Talk 5 Label
  annotate("label", x=confdaytweets[23,18], y=8, label= "Talk 5", color="black", fill ="#4285F4", alpha=0.3) +
  
  # Talk 6
  annotate("rect", xmin=confdaytweets[14,18], xmax=confdaytweets[15,18],ymin=-5, ymax=7, alpha=0.3, fill="#EA4335")+
  # Talk 6 Label
  annotate("label", x=confdaytweets[24,18], y=-6, label= "Talk 6", color="black", fill ="#EA4335", alpha=0.3) + 
  
  # Goodbye
  annotate("rect", xmin=confdaytweets[15,18], xmax=confdaytweets[16,18],ymin=-5, ymax=7, alpha=0.3, fill="#FBBC05")+
  # Goodbye Label
  annotate("label", x=confdaytweets[25,18], y=8, label= "Goodbye", color="black", fill ="#FBBC05", alpha=0.3) 



plot

plot + geom_smooth(method ="loess", span=0.1, colour="yellow" )


# Top 3 most Favourited tweets
index <- which(confdaytweets$favoriteCount > 30)
top3 <- confdaytweets[index,]



# try extract platform data for conference day tweets
# Start by binding a column named 'platform' containing NA onto dataframe which will be used to hold this data
platform <- NA
confdaytweets <- cbind(confdaytweets, platform)


#Tweet Lanes is an android apps so include it in android sources
index <- grep("Twitter for Android|TweetCaster for Android|Echofon  Android|Tweet Lanes", confdaytweets$statusSource)
confdaytweets$platform[index] <- "Android"


index <- grep("Twitter for iPhone", confdaytweets$statusSource)
confdaytweets$platform[index] <- "iPhone"

index <- grep("Tweetbot for i??S", confdaytweets$statusSource)
confdaytweets$platform[index] <- "ios"

index <- grep("Twitter for iPad", confdaytweets$statusSource)
confdaytweets$platform[index] <- "iPad"

index <- grep("Twitter for Mac", confdaytweets$statusSource)
confdaytweets$platform[index] <- "Mac"

index <- grep("Mobile Web", confdaytweets$statusSource)
confdaytweets$platform[index] <- "Mobile Web"

index <- grep("Twitter Web Client", confdaytweets$statusSource)
confdaytweets$platform[index] <- "Web Client"

index <- grep("Twitter for Windows", confdaytweets$statusSource)
confdaytweets$platform[index] <- "Windows"

# Any values still set to NA change their platform to 'Unknown' 
index <- which(is.na(confdaytweets$platform))
confdaytweets$platform[index] <- "Unknown"




# Tweet Frequency polygon

plot2 <- ggplot(confdaytweets, aes(x =confdaytweets$created)) +
  ggtitle("Tweet Count for #bristech")+
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")+
  
  scale_y_continuous(breaks = seq(0,120, by=10))+
  labs(x="Time", y="Tweet Count", colour ="Tweets by platform")+
  #                xlim(confdaytweets[31,19],confdaytweets[32,19])+
  geom_freqpoly(binwidth = 1000, aes(x=confdaytweets$created, colour="All Platforms"))+
  scale_color_manual(values=c("#000000", "#009939", "#3369e8", "#d50f25",
                              "#eeb211", "#00FFFF","#ff00ff", "#ffff00", "#00ff00"))+
  
  
  
  # Registration 
  annotate("rect", xmin=confdaytweets[1,18], xmax=confdaytweets[2,18],ymin=0, ymax=75, alpha=0.3, fill="#4285F4")+
  # Registration  Label
  annotate("label", x=confdaytweets[1,18], y=80, label= "Registration", color="black", fill ="#4285F4", alpha=0.3) + 
  
  # Welcome
  annotate("rect", xmin=confdaytweets[2,18], xmax=confdaytweets[3,18],ymin=0, ymax=75, alpha=0.3, fill="#EA4335")+
  # welcome Label
  annotate("label", x=confdaytweets[18,18], y=-6, label= "Welcome", color="black", fill ="#EA4335", alpha=0.3)+ 
  
  # talk 1
  annotate("rect", xmin=confdaytweets[4,18], xmax=confdaytweets[5,18],ymin=0, ymax=75, alpha=0.3, fill="#FBBC05")+
  # talk 1 Label
  annotate("label", x=confdaytweets[19,18], y=80, label= "Talk 1", color="black", fill ="#FBBC05", alpha=0.3)+  
  
  # talk 2
  annotate("rect", xmin=confdaytweets[6,18], xmax=confdaytweets[7,18],ymin=0, ymax=75, alpha=0.3, fill="#4285F4")+
  # talk 2 Label
  annotate("label", x=confdaytweets[20,18], y=-6, label= "Talk 2", color="black", fill ="#4285F4", alpha=0.3) + 
  
  # talk 3
  annotate("rect", xmin=confdaytweets[8,18], xmax=confdaytweets[9,18],ymin=0, ymax=75, alpha=0.3, fill="#EA4335")+
  # talk 3 Label
  annotate("label", x=confdaytweets[21,18], y=80, label= "Talk 3", color="black", fill ="#EA4335", alpha=0.3) + 
  
  # talk 4
  annotate("rect", xmin=confdaytweets[10,18], xmax=confdaytweets[11,18],ymin=0, ymax=75, alpha=0.3, fill="#FBBC05")+
  # talk 4 Label
  annotate("label", x=confdaytweets[22,18], y=-6, label= "Talk 4", color="black", fill ="#FBBC05", alpha=0.3) +
  
  # Talk 5
  annotate("rect", xmin=confdaytweets[12,18], xmax=confdaytweets[13,18],ymin=0, ymax=75, alpha=0.3, fill="#4285F4")+
  # Talk 5 Label
  annotate("label", x=confdaytweets[23,18], y=80, label= "Talk 5", color="black", fill ="#4285F4", alpha=0.3) +
  
  # Talk 6
  annotate("rect", xmin=confdaytweets[14,18], xmax=confdaytweets[15,18],ymin=0, ymax=75, alpha=0.3, fill="#EA4335")+
  # Talk 6 Label
  annotate("label", x=confdaytweets[24,18], y=-6, label= "Talk 6", color="black", fill ="#EA4335", alpha=0.3) + 
  
  # Goodbye
  annotate("rect", xmin=confdaytweets[15,18], xmax=confdaytweets[16,18],ymin=0, ymax=75, alpha=0.3, fill="#FBBC05")+
  # Goodbye Label
  annotate("label", x=confdaytweets[25,18], y=80, label= "Goodbye", color="black", fill ="#FBBC05", alpha=0.3) 



plot2 


# as well as total show quantity of tweets for each platform
plot2 + geom_freqpoly(binwidth = 1000, aes(x =confdaytweets$created, colour=platform))






