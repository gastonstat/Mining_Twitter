# =========================================================================
# Title:		Hashtags_Wordclouds.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to produce wordclouds with hashtags
#               from tweets of different users.
#				This example uses tweets from three USA government federal
#				agencies: EPA, NIH, and CDC
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# load packages
require(twitter)
require(stringr)
require(wordcloud)

# harvest tweets from each user account
epa_tweets = userTimeline("EPAgov", n=500)
nih_tweets = userTimeline("NIHforHealth", n=500)
cdc_tweets = userTimeline("CDCgov", n=500)

# dump tweets information into data frames
epa_df = twListToDF(epa_tweets)
nih_df = twListToDF(nih_tweets)
cdc_df = twListToDF(cdc_tweets)

# get the hashtags
epa_hashtags = str_extract_all(epa_df$text, "#\\w+")
nih_hashtags = str_extract_all(nih_df$text, "#\\w+")
cdc_hashtags = str_extract_all(cdc_df$text, "#\\w+")

# put tags in vector
epa_hashtags = unlist(epa_hashtags)
nih_hashtags = unlist(nih_hashtags)
cdc_hashtags = unlist(cdc_hashtags)

# calculate hashtag frequencies
epa_tags_freq = table(epa_hashtags)
nih_tags_freq = table(nih_hashtags)
cdc_tags_freq = table(cdc_hashtags)

# put all tags in a single vector
all_tags = c(epa_tags_freq, nih_tags_freq, cdc_tags_freq)

# EPA hashtags wordcloud
wordcloud(names(epa_tags_freq), epa_tags_freq, random.order=FALSE, 
    colors="#1B9E77")
title("\n\nHashtags in tweets from @EPAgov",
    cex.main=1.5, col.main="gray50")

# NIH hashtags wordcloud
wordcloud(names(nih_tags_freq), nih_tags_freq + 7, random.order=FALSE, 
    colors="#7570B3")
title("\nHashtags in tweets from @NIHforHealth",
    cex.main=1.5, col.main="gray50")

# CDC hashtags wordcloud
wordcloud(names(cdc_tags_freq), cdc_tags_freq, random.order=FALSE, 
    colors="#D95F02")
title("\n\nHashtags in tweets from @CDCgov",
    cex.main=1.5, col.main="gray50")

# Now let's plot one single wordcloud
# vector of colors
cols = c(
    rep("#1B9E77", length(epa_tags_freq)),
    rep("#7570B3", length(nih_tags_freq)),
    rep("#D95F02", length(cdc_tags_freq))
)

# wordcloud
wordcloud(names(all_tags), all_tags, random.order=FALSE, min.freq=1, 
    colors=cols, ordered.colors=TRUE)
mtext(c("@EPAgov", "@NIHforHealth", "@CDCgov"), side=3,
    line=2, at=c(0.25, 0.5, 0.75), col=c("#1B9E77", "#7570B3", "#D95F02"),
    family="serif", font=2, cex=1.5)


