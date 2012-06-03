# =========================================================================
# Title:        Obama_Romney_Comparative_ConwayCloud.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Script showing how to produce Drew Conway's better cloud 
#				This example uses text from twitter accounts of Obama and
#				Mitt Romney.
#               
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# load required packages
require(twitteR)
require(tm)
require(ggplot2)

# collect tweets from Obama and Romney
obama_tweets = userTimeline("BarackObama", n=1500)
romney_tweets = userTimeline("MittRomney", n=1500)

# get the text
obama_txt = sapply(obama_tweets, function(x) x$getText())
romney_txt = sapply(romney_tweets, function(x) x$getText())

# clean text function
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

# clean text
obama_clean = clean.text(obama_txt)
romney_clean = clean.text(romney_txt)

# join cleaned texts in a single vector
obamas = paste(obama_clean, collapse=" ")
romneys = paste(romney_clean, collapse=" ")
oba_rom = c(obamas, romneys)


# Corpus
or_corpus = Corpus(VectorSource(oba_rom))

# remove stopwords
skipwords = c(stopwords("english"), "president", "presidents",
"obama", "obamas", "video", "todays", "reads", "live", "watch")
or_corpus = tm_map(or_corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(or_corpus)

# create data frame
or_df = as.data.frame(inspect(tdm))
names(or_df) = c("obama.txt", "romney.txt")

# get rid of low frequency words
or_df = subset(or_df, obama.txt>2 & romney.txt>2)

# calculate frequency differences
or_df$freq.dif = or_df$obama.txt - or_df$romney.txt

# twitted more often by Obama
obama_df = subset(or_df, freq.dif > 0)

# twitted more often by Romney
romney_df = subset(or_df, freq.dif < 0)

# twitted equally
both_df = subset(or_df, freq.dif == 0)

# create function to get the words spacing for the plot
optimal.spacing <- function(spaces)
{
   if(spaces > 1) {
      spacing <- 1 / spaces
      if(spaces%%2 > 0) {
         lim = spacing * floor(spaces/2)
         return(seq(-lim, lim, spacing))
      }
      else {
         lim = spacing * (spaces-1)
         return(seq(-lim, lim, spacing*2))
      }
   }
   else {
      # add some jitter when 0
      return(jitter(0, amount=0.2))
   }
}

# Get spacing for each frequency type
obama_spacing = sapply(table(obama_df$freq.dif),
function(x) optimal.spacing(x))

romney_spacing = sapply(table(romney_df$freq.dif),
function(x) optimal.spacing(x))

both_spacing = sapply(table(both_df$freq.dif),
function(x) optimal.spacing(x))

# add spacings to data frames
obama_optim = rep(0, nrow(obama_df))
for(n in names(obama_spacing)) {
   obama_optim[obama_df$freq.dif == as.numeric(n)] <- obama_spacing[[n]]
}
obama_df = transform(obama_df, Spacing=obama_optim)

romney_optim = rep(0, nrow(romney_df))
for(n in names(romney_spacing)) {
   romney_optim[romney_df$freq.dif == as.numeric(n)] <- romney_spacing[[n]]
}
romney_df = transform(romney_df, Spacing=romney_optim)

both_df$Spacing = as.vector(both_spacing)

# plot comparitive cloud
ggplot(obama_df, aes(x=freq.dif, y=Spacing)) +
geom_text(aes(size=obama.txt, label=row.names(obama_df),
   colour=freq.dif), alpha=0.7, family='Times') +
geom_text(data=romney_df, aes(x=freq.dif, y=Spacing,
   label=row.names(romney_df), size=romney.txt, color=freq.dif),
   alpha=0.7, family='Times') +
geom_text(data=both_df, aes(x=freq.dif, y=Spacing,
   label=row.names(both_df), size=obama.txt, color=freq.dif),
   alpha=0.7, family='Times') +
scale_size(range=c(3,11)) +
scale_colour_gradient(low="red3", high="blue3", guide="none") +
scale_x_continuous(breaks=c(min(romney_df$freq.dif), 0, max(obama_df$freq.dif)),
	labels=c("Twitted More by Romney","Twitted Equally","Twitted More by Obama")) +
scale_y_continuous(breaks=c(0), labels=c("")) +
labs(x="", y="", size="Word Frequency") +
theme_bw() +
opts(panel.grid.major = theme_blank(),
   panel.grid.minor = theme_blank(),
   title="Conway's Word Cloud, Tweets (Obama -vs- Romney)",
   plot.title = theme_text(family="Times", size=18))

# if you want to save the plot in pdf:
ggsave("Obama_Romney_ModifyCloud.pdf", width=13, height=8, units="in")

