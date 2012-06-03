# =========================================================================
# Title:        Icecream_Frequency_Analysis.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to perform a basic frequency analysis
#               applied to text data from tweets about "icecream" 
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# load packages
require(XML)
require(tm)
require(ggplot2)

# Let's use the XML package to parse some tweets containing the term "icecream"
# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"

# vector to store results
results = character(0)

# paginate 20 times to harvest tweets
for (page in 1:20)
{
   # create twitter search query to be parsed
   # tweets in english containing 'icecream'
   twitter_search = paste(twitter_url, "q=icecream",
      "&rpp=100&lang=en&page", page, sep="")

   # let's parse with xmlParseDoc
   tmp = xmlParseDoc(twitter_search, asText=FALSE)

   # extract titles
   results = c(results, xpathSApply(tmp, "//s:entry/s:title", xmlValue,
      namespaces=c('s'='http://www.w3.org/2005/Atom')))
}

# how many tweets
length(results)

# How many characters per tweet?
chars_per_tweet = sapply(results, nchar)
summary(chars_per_tweet)

# How many words per tweets?
# split words
words_list = strsplit(results, " ")
# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
   main="Distribution of words per tweet", cex.main=1)

# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
# barplot
barplot(table(round(wsize_per_tweet)), border=NA,
   xlab = "word length in number of characters",
   main="Distribution of words length per tweet", cex.main=1)

# How many unique words per tweet?
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), border=NA,
   main="Distribution of unique words per tweet", cex.main=1)

# How many hashtags per tweet?
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))

# How many @mentions per tweet?
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))

# how many http links per tweet?
links_per_tweet = sapply(words_list, function(x) length(grep("http", x)))
table(links_per_tweet)
prop.table(table(links_per_tweet))

# let's create a data frame with all the calculated stuff and make some plots
icedf = data.frame(
   chars=chars_per_tweet,
   words = words_per_tweet,
   lengths = wsize_per_tweet,
   uniqs = uniq_words_per_tweet,
   hashs = hash_per_tweet,
   ats = ats_per_tweet,
   links = links_per_tweet
)

# words -vs- chars
ggplot(icedf, aes(x=words, y=chars)) +
geom_point(colour="gray20", alpha=0.2) +
stat_smooth(method="lm") +
labs(x="number of words per tweet", y="number of characters per tweet") +
opts(title = "Tweets about 'icecream' \nNumber of words -vs- Number of characters",
     plot.title = theme_text(size=12))

# words -vs- word length
ggplot(icedf, aes(x=words, y=lengths)) +
geom_point(colour="gray20", alpha=0.2) +
stat_smooth(method="lm") +
labs(x="number of words per tweet", y="size of words per tweet") +
opts(title = "Tweets about 'icecream' \nNumber of words -vs- Length of words",
     plot.title = theme_text(size=12))

# Lexical diversity: number of unique tokens / number of total tokens
# unique words in total
uniq_words = unique(unlist(words_list))

# lexical diversity
length(uniq_words) / length(unlist(words_list))

# What are the most frequent words?
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)


