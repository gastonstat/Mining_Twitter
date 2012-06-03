# =========================================================================
# Title:        Sentiment_Analysis_using_Viralheat.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to perform a sentiment analysis using
#               the Sentiment Analysis API of Viralheat.
#				http://viralheat.com/developer/sentiment_api
#               This example is based on the analysis done by Jeff Allen
#				https://github.com/trestletech/Sermon-Sentiment-Analysis
#				We'll apply the sentiment analysis to compare the opinions
#				of tweets about McDonalds -vs- tweets about Burger King
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# -------------------------------------------------------------------------
# Important Note:
# In order to use the Sentiment Analysis API, you need an API key which
# you can get by registering for a free developer account in Viralheat.
# The key should look something like this (you need to get your own):
# JosYnzSHwhszhjhBgABC
# -------------------------------------------------------------------------

# load packages
require(twitteR)
require(RCurl)
require(RJSONIO)
require(stringr)

# getSentiment function
getSentiment <- function (some_text, key)
{
  # text url-encoded
  code_text = URLencode(some_text)

  # save all the spaces
  code_text = str_replace_all(code_text, "%20", " ")
  # get rid of the weird characters that break the API
  code_text = str_replace_all(code_text, "%\\d\\d", "")
  # convert back the URL-encoded spaces
  code_text = str_replace_all(code_text, " ", "%20")

  # viralheat sentiment url
  vh_url = "http://www.viralheat.com/api/sentiment/review.json?text="
  # send query and get an answer from viralheat 
  vh_answer = getURL(paste(vh_url, code_text, "&api_key=", key, sep=""))

  # extract elements in the answer (i.e. prob, text, mood)
  js = fromJSON(vh_answer, asText=TRUE)

  # get mood probability
  score = js$prob

  # positive, negative or neutral?
  if (js$mood != "positive")
  {
    if (js$mood == "negative") {
       score = -1 * score
    } else {
       # neutral
       score = 0
    }
  }

  return(list(mood=js$mood, score=score))
}

# Let's create a function to clean the text
clean.text <- function(some_txt)
{
   some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
   some_txt = gsub("@\\w+", "", some_txt)
   some_txt = gsub("[[:punct:]]", "", some_txt)
   some_txt = gsub("[[:digit:]]", "", some_txt)
   some_txt = gsub("http\\w+", "", some_txt)
   some_txt = gsub("[ \t]{2,}", "", some_txt)
   some_txt = gsub("^\\s+|\\s+$", "", some_txt)

   # define "tolower error handling" function
   try.tolower = function(x)
   {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
         y = tolower(x)
      return(y)
   }

   some_txt = sapply(some_txt, try.tolower)
   some_txt = some_txt[some_txt != ""]
   names(some_txt) = NULL
   return(some_txt)
}

# harvest tweets from McDonalds and Burger King
mc_tweets = searchTwitter("mcdonalds", n=200, lang="en")
bk_tweets = searchTwitter("burgerking", n=200, lang="en")

# get text
mc_txt = sapply(mc_tweets, function(x) x$getText())
bk_txt = sapply(bk_tweets, function(x) x$getText())

# clean text
mc_clean = clean.text(mc_txt)
bk_clean = clean.text(bk_txt)

# Get sentiment of each mcdonalds text
# how many tweets
mcnum = length(mc_clean)

# data frame (text, sentiment, score)
mc_df = data.frame(text=mc_clean, sentiment=rep("", mcnum),
   score=1:mcnum, stringsAsFactors=FALSE)

# apply function getSentiment
sentiment = rep(0, mcnum)
for (i in 1:mcnum)
{
   tmp = getSentiment(mc_clean[i], mykey)
   mc_df$sentiment[i] = tmp$mood
   mc_df$score[i] = tmp$score
}

# Get sentiment of each burgerking text
# how many tweets
bknum = length(bk_clean)

# data frame (text, sentiment, score)
bk_df = data.frame(text=bk_clean, sentiment=rep("", bknum),
   score=1:bknum, stringsAsFactors=FALSE)

# apply getSentiment
sentiment = rep(0, bknum)
for (i in 1:bknum)
{
   tmp = getSentiment(bk_clean[i], mykey)
   bk_df$sentiment[i] = tmp$mood
   bk_df$score[i] = tmp$score
}

# Let's check the sentiments
# how many positives and negatives in mcdonalds
table(mc_df$sentiment)

# how many positives and negatives in burgerking
table(bk_df$sentiment)

