# =========================================================================
# Title:        Machine_Learning_Wordcloud.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Simple script showing how to produce a basic wordcloud in R
#               from tweets containing the term 'machine learning'. Note 
#				that there is not too much text cleaning in this example.
#				
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# load required packages
require(twitteR)
require(tm)
require(wordcloud)
require(RColorBrewer)

# let's get some tweets in english containing "machine learning"
mach_tweets = searchTwitter("machine learning", n=500, lang="en")

# extract the text from tweets
mach_text = sapply(mach_tweets, function(x) x$getText())

# Construct the lexical Corpus
# use the function "Corpus" create the corpus, and the function VectorSource 
# to indicate that the text is in the character vector 'mach_text' 
mach_corpus = Corpus(VectorSource(mach_text))

# create document term matrix applying some transformations
# such as removing numbers, punctuation symbols, lower case, etc.
tdm = TermDocumentMatrix(mach_corpus,
	control = list(removePunctuation = TRUE,
	stopwords = c("machine", "learning", stopwords("english")),
	removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)

# get word counts in decreasing order
# (after inspecting the words, you could remove similar terms if you want)
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# if you wish, you can save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

