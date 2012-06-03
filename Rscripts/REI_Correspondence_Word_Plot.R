# =========================================================================
# Title:        REI_Correspondence_Word_Plot.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to produce a correspondence analysis map
#				of words contained in tweets from REI.
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# Load the necessary packages
require(twitteR)
require(tm)
require(cluster)
require(FactoMineR)
require(RColorBrewer)
require(ggplot2)

# harvest some tweets from each REI
rei_tweets = userTimeline("REI", n=1000)

# dump tweets information into a data frame
rei_df = twListToDF(rei_tweets)

# get the text
rei_txt = rei_df$text

# remove retweet entities
rei_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", rei_txt)
# remove Atpeople
rei_clean = gsub("@\\w+", "", rei_clean)
# remove punctuation symbols
rei_clean = gsub("[[:punct:]]", "", rei_clean)
# remove numbers
rei_clean = gsub("[[:digit:]]", "", rei_clean)
# remove links
rei_clean = gsub("http\\w+", "", rei_clean)

# corpus
rei_corpus = Corpus(VectorSource(rei_clean))

# convert to lower case
rei_corpus = tm_map(rei_corpus, tolower)
# remove stoprwords
rei_corpus = tm_map(rei_corpus, removeWords, c(stopwords("english"), "rei"))
# remove extra white-spaces
rei_corpus = tm_map(rei_corpus, stripWhitespace)

# term-document matrix
tdm = TermDocumentMatrix(rei_corpus)

# convert as matrix
m = as.matrix(tdm)

# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# Let's keep exploring by applying a cluster analysis
# This will let us discover more about groups of words
# First we define the distance matrix with binary distance
m1dist = dist(m1, method="binary")

# apply cluster with ward method
clus1 = hclust(m1dist, method="ward")

# plot dendrogram
plot(clus1, cex=0.7)

# For a better visualization, we can apply a
# a Correspondence Analysis (using package FactoMineR)
rei_ca = CA(m1, graph=FALSE)

# default plot of words
plot(rei_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(rei_ca$row$coord[,1], rei_ca$row$coord[,2], labels=rownames(m1),
   col=hsv(0,0,0.6,0.5))
title(main="@REI Correspondence Analysis of tweet words", cex.main=1)

# To improve the correspondance analysis plot, we can apply a clustering method
# like k-means or partitioning around medoids (pam)
# Let's try partitioning around medoids with 6 clusters
k = 6

# pam clustering
rei_pam = pam(rei_ca$row$coord[,1:2], k)

# get clusters
clusters = rei_pam$clustering

# Let's try to get a nicer plot
# first we need to define a color palette
gbrew = brewer.pal(8, "Dark2")

# I like to use hsv encoding
gpal = rgb2hsv(col2rgb(gbrew))

# colors in hsv (hue, saturation, value, transparency)
gcols = rep("", k)
for (i in 1:k) {
   gcols[i] = hsv(gpal[1,i], gpal[2,i], gpal[3,i], alpha=0.65)
}

# plot words with size reflecting their frequency
wcex = log10(rowSums(m1))
plot(mca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
title("@REI Correspondence Analysis of tweet words", cex.main=1)
for (i in 1:k)
{
   tmp <- clusters == i
   text(mca$row$coord[tmp,1], mca$row$coord[tmp,2],
        labels=rownames(m1)[tmp], cex=wcex[tmp],
        col=gcols[i])
}

# For the ggploters like me, a similar graphic can be obtained like this
# create data frame
rei_words_df = data.frame(
	words = rownames(m1),
	dim1 = rei_ca$row$coord[,1],
	dim2 = rei_ca$row$coord[,2],
	freq = rowSums(m1),
	cluster = as.factor(clusters))

# plot
ggplot(rei_words_df, aes(x=dim1, y=dim2, label=words)) +
geom_text(aes(size=freq, colour=cluster), alpha=0.7) +
scale_size_continuous(breaks=seq(20,80,by=10), range=c(3,8)) +
scale_colour_manual(values=brewer.pal(8, "Dark2")) +
labs(x="", y="") +
opts(title = "What does @REI tweet about?",
     plot.title = theme_text(size=12),
     axis.ticks=theme_blank(),
     legend.position = "none",
     axis.text.x = theme_blank(),
     axis.text.y = theme_blank()
)


