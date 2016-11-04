# Twitter Scraping and sentiment analysis of Tweets about Bristech conference 
# Held on Thursday 3rd November 2016

library(twitteR)
library(ROAuth)
library(httr)
library(stringr)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)

# Set working directory to project root
setwd("C:/Dev/git/bristech_twit")

# Make sure Twitter account has a phone number attached.
# Go to Twitter apps page (https://apps.twitter.com/) and create a new app
# Once app is created, this will give Keys and Access tokens

# For security, to keep secrets secret they are stored in environment variables! 
# API Secret and Access Token Secret should never be human readable,

#TWITAPISECRET <- Sys.getenv("TWITAPISECRET") 
#TWITTOKENSECRET <- Sys.getenv("TWITTOKENSECRET")

# Set API Keys 
#api_key <- "aVXP1fw3fyxFFYfSDsAKje3vy"
#api_secret <- TWITAPISECRET
#access_token <- "69366834-DdbmXBAxgxybC27MSBK3gaojj26Qcdr5Mi1rSzGpd"
#access_token_secret <- TWITTOKENSECRET 
#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# As of 04-11-2016 there are 764 tweets tagged #bristech2016 available
# So collect them all
#bristech2016_tweets <- searchTwitter("#bristech2016", n=764)

# Looks like as of 04-11-2016 there are also 32 tweets tagged #bristech
# So collect these too  
#bristech_tweets <- searchTwitter("#bristech", n=2840)

#convert to data frame
#bristech2016df <- twListToDF(bristech2016_tweets)
#bristechdf <- twListToDF(bristech_tweets)


# remove the one tweet from bristechdf which also contain the text #bristech2016
#bristechdf <- bristechdf[-15,]

#join both searches together by combining bristech2016df and bristechdf together
#bristweets <- rbind(bristech2016df,bristechdf)

#fix row names by reindexing them
#row.names(bristweets) <- 1:nrow(bristweets)


# saved the dataframe object containing these tweets as a .Rda file
#saveRDS(bristweets,file="bristweets.Rda")


# load dataframe object containing tweets to perform analysis on
bristweets <- readRDS("bristweets.Rda")

