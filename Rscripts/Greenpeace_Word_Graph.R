# =========================================================================
# Title:        Greenpeace_Word_Graph.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to produce a tagcloud produced with
#               a graph structure following the approch described by 
#				Drew Conway in his analysis of poliscijobrumours. 
#				This example is based on words from Greenpeace tweets.
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# required packages
require(tm)
require(igraph)
require(ggplot)
require(RColorBrewer)

# get tweets from @Greenpeace
gp_tweets = userTimeline("Greenpeace", n=1000)

# extract text
gp_text = sapply(gp_tweets, function(x) x$getText())

# create a corpus via VectorSource
gp_corpus = Corpus(VectorSource(gp_text))

# define list of transformations
gp_stopwords = unique(c(stopwords(), "greenpeace", "via"))
# list of transformations
trans = list(weighting=weightTf, stopwords=gp_stopwords,
   removePunctuation=TRUE,
   tolower=TRUE,
   minWordLength=4,
   removeNumbers=TRUE)

# create a term-document matrix
gp_tdm = TermDocumentMatrix(gp_corpus, control=trans)

# Remove sparse terms from matrix
gp_clean = removeSparseTerms(gp_tdm, .995)

# convert as matrix
gp_clean = as.matrix(gp_clean)

# In order to get the graph, we first need to create a word affiliations matrix
affi_matrix = gp_clean %*% t(gp_clean)

# then create an adjacency matrix with zeros in its diagonal
adja_matrix = affi_matrix
diag(adja_matrix) = 0

# Finally, let's build the graph
gp_graph = graph.adjacency(adja_matrix, weighted=TRUE)

# coordinates for visualization
posi_matrix = layout.fruchterman.reingold(gp_graph, list(weightsA=E(gp_graph)$weight))
posi_matrix = cbind(V(gp_graph)$name, posi_matrix)

# create a data frame
gp_df = data.frame(posi_matrix, stringsAsFactors=FALSE)
names(gp_df) = c("word", "x", "y")
gp_df$x = as.numeric(gp_df$x)
gp_df$y = as.numeric(gp_df$y)

# let's make a first attempt
# size effect
se = diag(affi_matrix) / max(diag(affi_matrix))
# plot
par(bg = "gray15")
with(gp_df, plot(x, y, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n"))
with(gp_df, text(x, y, labels=word, cex=log10(diag(affi_matrix)),
	col=hsv(0.95, se, 1, alpha=se)))

# To improve our graph, we can perform a k-means cluster analysis to find groups
# k-means with 7 clusters
words_km = kmeans(cbind(as.numeric(posi_matrix[,2]), as.numeric(posi_matrix[,3])), 7)

# add frequencies and clusters in a data frame
gp_df = transform(gp_df, freq=diag(affi_matrix), cluster=as.factor(words_km$cluster))
row.names(gp_df) = 1:nrow(gp_df)

# here's the final plot
# graphic with ggplot
gp_words = ggplot(gp_df, aes(x=x, y=y)) +
geom_text(aes(size=freq, label=gp_df$word, alpha=.90, color=as.factor(cluster))) +
labs(x="", y="") +
scale_size_continuous(breaks = c(10,20,30,40,50,60,70,80,90), range = c(1,8)) +
scale_colour_manual(values=brewer.pal(8, "Dark2")) +
scale_x_continuous(breaks=c(min(gp_df$x), max(gp_df$x)), labels=c("","")) +
scale_y_continuous(breaks=c(min(gp_df$y), max(gp_df$y)), labels=c("","")) +
opts(panel.grid.major=theme_blank(),
    legend.position="none",
    panel.background=theme_rect(fill="gray10", colour="gray10"),
    panel.grid.minor=theme_blank(),
    axis.ticks=theme_blank(),
    title = "Graph of words from @Greenpeace Tweets - 05/22/2012",
    plot.title = theme_text(size=12))

# save the image in pdf format
ggsave(plot=gp_words, filename="Greenpeace_wordgraph.pdf", height=10, width=10)


