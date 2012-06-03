# =========================================================================
# Title:        Genetics_Genomics_Word_Graph.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to produce a word graph (i.e. network)
#               from tweets containing the terms 'genetics' and 'genomics'. 
#				In this example we parse the tweets using the XML package 
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# load required packages
require(XML)
require(tm)
require(igraph)
require(RColorBrewer)

# Let's get some tweets containing "genetics" and "genomics"
# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"

# encode query
query = URLencode("genetics AND genomics")

# vector to store results
tweets = character(0)

# paginate 17 times to harvest tweets
for (page in 1:17)
{
   # create twitter search query to be parsed
   twitter_search = paste(twitter_url, "q=", query,
      "&rpp=100&lang=en&page", page, sep="")

   # let's parse with xmlParseDoc
   tmp = xmlParseDoc(twitter_search, asText=FALSE)

   # extract titles
   tweets = c(tweets, xpathSApply(tmp, "//s:entry/s:title",
      xmlValue, namespaces=c('s'='http://www.w3.org/2005/Atom')))
}

# Let's pre-process the data (cleaning)
results = tweets
# remove retweet entities
results = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", results)
# remove at people
results = gsub("@\\w+", "", results)
# remove punctuation
results = gsub("[[:punct:]]", "", results)
# remove numbers
results = gsub("[[:digit:]]", "", results)
# remove html links
results = gsub("http\\w+", "", results)
# remove unnecessary spaces
results = gsub("[ \t]{2,}", "", results)
results = gsub("^\\s+|\\s+$", "", results)

# Sometimes the function tolower doesn't behave as we expect
# and it returns weird error messages. That's why need 
# to create our own tolower version that skip those errors
# Define "tolower error handling" function 
tryTolower = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
      y = tolower(x)
   # result
   return(y)
}

# convert to lowercase using tryTolower with sapply 
results = sapply(results, tryTolower)
names(results) = NULL

# remove empty results (if any)
results = results[results != ""]

# Create Lexical Corpus
corpus = Corpus(VectorSource(results))

# remove stopwords in english
skipwords = c(stopwords("english"), 
   "genetics", "genomics", "genetic", "genome")
corpus = tm_map(corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert tdm to matrix
m = as.matrix(tdm)

# create a matrix 'good' with most frequent words
# word counts
wc = rowSums(m)
# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]

# remove columns (documents) with zeros
good = good[,colSums(good)!=0]

# Obtain an adjacency matrix and create a graph
M = good %*% t(good)

# set zeros in the diagonal
diag(M) = 0

# create a graph based on the adjacency matrix
g = graph.adjacency(M, weighted=TRUE, mode="undirected",
	add.rownames=TRUE)
# get a graph layout (eg fruchterman.reinglod)
glay = layout.fruchterman.reingold(g)

# let's superimpose a cluster structure with k-means clustering
# in this example I'm using 8 clusters but you will probably 
# need to change this number depending on your analysis
kmg = kmeans(M, centers=8)
gk = kmg$cluster

# Prepare a nice color palette
# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
	gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}

# Prepare ingredients for plotting the network
# (you can tweak a lot of parameters in your graph)
V(g)$size = 10
V(g)$label = V(g)$name
V(g)$degree = degree(g)
V(g)$label.color = hsv(0, 0, 0.2, 0.55)
V(g)$frame.color = NA
V(g)$color = gcols
E(g)$color = hsv(0, 0, 0.7, 0.3)

# Let's create the graph so we can see what people is talking about
# plot network
plot(g, layout=glay)
title("\nGraph of tweets about genetics and genomics",
	col.main="gray40", cex.main=1.5, family="serif")

# if want to plot the labels with different sizes 
# just change the label.cex parameter
V(g)$label.cex = 1.5 * log10(V(g)$degree)

# plot network again with words in different sizes
plot(g, layout=glay)
title("\nGraph of tweets about genetics and genomics",
    col.main="gray40", cex.main=1.5, family="serif")

