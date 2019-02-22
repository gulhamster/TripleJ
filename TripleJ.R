#######################################################################################################################
#
# Triple J Hottest 100 Data Analysis + Sentiment Analysis
# Goal: How trends & Sentiment in the Triple J Hottest 100 has changed over the years
#
#
# Author: Lovisa Lannesand
#
#######################################################################################################################


library(data.table)
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(devtools)
library(corrplot)
library(genius)
library(yarrr) # For pirate plot
library(gdata)
library(tidytext) #Text mining
library(gridExtra)

# Set plot theme 
theme_set(theme_bw())

# Goal: Visaulise the latest Triple J hottest 100
# Make shiny + flesdashboard with the output 
# Should be able to search, then play the song in youtube for the result

#######################################################################################################################
#
# READ IN DATA 
# CLEAN DATA
# 
#######################################################################################################################

data <- data.table(fread('Desktop/R Practice/data.tsv'))
head(data)

# Check what class each variable in the data is 
sapply(data, class) 

#######################################################################################################################
#
# NEW VARIABLES + CLEANING 
#######################################################################################################################

# Create position ID (The data is orderded, but there is no variable indicating position)
data[, position := seq_along(title), by = .(year)]

# Create top 10 group
data[, top_10 := cut(position, breaks = c(0,10,Inf), labels = c("[1-10]", "[11-100]"))]

# Create numeric minute & second variable, add these two to get the total duration
data[, ':=' (min = as.numeric(gsub("\\:.*","",duration))*60,
             sec = as.numeric(gsub(".*:","",duration)))]

# Create duration & remove min+sec
data[, ':=' (duration_sec = min+sec,
             min = NULL, 
             sec = NULL)]

# Create duration group
data[, ':=' (duration_sec_group = cut(duration_sec, breaks = c(0, 150, 200, 250, 300, 400, Inf)), 
             year_group = cut(year, breaks = c(1992, 2000, 2005, 2010, 2015, 2017)))]

#######################################################################################################################
## PREP FOR TEXT ANALYSIS

# Genius lyric names does not include information such as Live, or featured artists
# Use gsub to remove anything like this

# Remove anything including (Live)
data[, song := gsub("'|\\.|\\(Live)", "", title)]
# Remove anything within paranthesis
data[, song := gsub("\\s*\\([^\\)]+\\)","", song)]
data[, song := gsub("Ü","U", song)]
data[, artist2 := gsub("'|\\.", "", artist)]

# Change to remove anything within brackets and if (feat). 

# Need the lyrics for all the song to count # of uniqe words 
# We want to write a loop do dowload the lyrics for all songs in the 
# Create a function that looks thorugh the lyrics for unique words, and creates a table with unique words + words per lyrics 

#######################################################################################################################
# 
# SCRAPE ALL LYRICS TO GET UNIQUE WORD COUNT
#######################################################################################################################

# Create function which gets the lyric from Genius based on artist and song name
get_word_count <- function(artist, song) {
  
  song_url <- gen_song_url(artist, song)
  
  # If the song does not exist on Genius, return NA for all values
  if(url.exists(song_url) == FALSE) {
    
    lyric <- NA
    lyric_words <- NA
    unique_words <- NA
    unique_words_count <- NA
    word_ratio <- NA
    
    summary <- data.table(cbind(artist, song, lyric_words, unique_words_count,word_ratio ,lyric, unique_words))
    
  } else {
    

temp <- data.table(genius_lyrics(artist = artist, song = song))
# Count words and number of unique words # We should do this by genre 
lyric <- tolower(paste(temp$lyric, collapse = ' '))
# Count of words in lyric 
lyric_words <- sapply(strsplit(lyric, "\\s+"), length)
# Count of unique words in lyric 
lengths(lapply(strsplit(lyric, split = ' '), unique))

# Unlist - not perfec't bc it's splits any word with "'", e.g. don't 
temp <- unlist(strsplit(lyric, "[^[:alnum:]]"))
temp <- data.table(table(temp))
temp <- temp[N == 1]
unique_words <- tolower(paste(temp$temp, collapse = ' '))
unique_words_count <- nrow(temp)
word_ratio <- unique_words_count/lyric_words

summary <- data.table(cbind(artist, song, lyric_words, unique_words_count,word_ratio ,lyric, unique_words))
  
}
  
return(summary)

}

# Create list of all artists & song names
list_artist <- data$artist2
list_song <- data$song

# Create empty data table
# Loop the get_word_count function over each title + artist
# This one takes a few minutes, so you might want to get a coffee or something

result <- data.table()
for (i in 1:length(list_artist)) {

  summary <- get_word_count(artist = list_artist[i], song = list_song[i])
  result <- rbind(result, summary)
  # Print what # song we are up to
  print(i)

}

# Save the data so we don't have to run the function each time
# saveRDS(result, 'result_feb')
# keep(data, result_all, sure = T)
# result <- readRDS('result_feb')

# Check how many songs that did not exist on genius
result[, sum(is.na(lyric))] # ~ 200
temp <- result[is.na(lyric)]
# Merge back with the original data 
temp <- merge(data, result[!is.na(lyric)], by.x = c("artist2", "song"), by.y = c("artist", "song"))
dt <- temp[!duplicated(song)]

# Change a bunch of variables to numeric
vars <- c("unique_words_count", "lyric_words", "word_ratio")
dt[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]

#######################################################################################################################
#
# INITIAL ANALYSIS
#######################################################################################################################

# What % of songs are Australian 
round(prop.table(table(data$country)), digits = 4)*100 # 44%

# Density plots of song duration by year + top 10
ggplot(dt, aes(x=duration_sec, colour=year, group = year)) + 
  geom_density() +
  facet_grid(~top_10)

# Density plots of unique word count
# Looks like the 'darker' colors have a higher word count - make pirate plot
ggplot(dt, aes(x=unique_words_count, colour=year, group = year)) + 
  geom_density() +
  facet_grid(~top_10)

ggplot(dt, aes(x=word_ratio, colour=year, group = year)) + 
  geom_density() +
  facet_grid(~top_10)

# Pirate plot to look at the mean of word_ratio over the different years
# It looks like the unique word clount is getting smaller
# Could potentially be due to the rise of EMD/similar genres with less words (would be interesting to look at this by genre in the future)

pirateplot(formula =  word_ratio ~ year_group + top_10, #Formula
           data = dt, #Data frame
           xlab = NULL, ylab = "Unique/Total Lyric Words", #Axis labels
           main = "Lyric Diversity Per Year Group (& top 10 or not)", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

#######################################################################################################################
# LOOK AT CORRELATION BETWEEN VARIABLES 

songs_cor <- dt %>%
  select(c(position, duration_sec, unique_words_count)) %>%
  cor() %>%
  corrplot() 

songs_cor # Weak to non-existant correlation, we can still fit a regression for fun

# Let's fit a logistic regression, 
# Dependent variable: Top 10 or not top 10

modelling_data <- dt
modelling_data[, outcome := case_when(top_10 == "[1-10]" ~ 1, TRUE ~ 0)]

model <- glm(outcome ~ 
               duration_sec + 
               unique_words_count,
             family=binomial(link='logit'),
             data = modelling_data)

summary(model) 

# Does not look like we can predict if a song will end up in the top ten based on ONLY lyrics (not suprised)
# Future improvement: Include Spotify data such as genre, loudness etc.

#######################################################################################################################
#
# SINGLE WORD SENTIMENT ANALYSIS
# Question: Has the Triple J hottest 100 sentiment changed over time? 
#######################################################################################################################

ggplot(dt[!is.na(word_ratio)], 
       aes(x=word_ratio, colour=year, group = year)) + geom_density()

# Unique words in lyrics over time, and by top 10
# As described before, looks like the unique word count is declining over time
ggplot(dt[!is.na(word_ratio)], aes(x=unique_words_count, colour=top_10, group = top_10)) + 
  geom_density() +
  facet_wrap(~year)

# To create the single word sentiment analysis we need to to three things
# Step 1: Untoken text

# Filter out variables we need for the sentiment analysis
dt_sentiment <- dt[, .(artist, song, lyric, year, country, position, top_10, duration_sec, lyric_words,unique_words_count, word_ratio)]

# Filter out some common words in lyrics to exclude
undesirable_words <- c("yeah", "baby","alright", "wanna", "gonna", "chorus", "verse",
                       "whoa",  "ooh", "uurh", " ai ", " ca ", " la ", "hey", " na ",
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats", "la", "da", "uh", "ah")

# Untoken text and remove undesirable words
data_tidy <- dt_sentiment %>%
  unnest_tokens(word, lyric) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

# Step 2: Chose lexicon 
# The bing lexicon has a positive/negative association to words and include 6,788 words

# Get sentiments using the bing lexicon
sentiment <- data.table(get_sentiments("bing"))

# Merge data and sentiment
# Remove duplicated words by song & artist

temp <- data_tidy[!duplicated(word, song)]
setDT(temp)
dt_sentiment <- merge(data_tidy, sentiment, by = "word")

# Create sentiment score for each year + top 10
score_summary <- dt_sentiment[, .N, by = .(sentiment, year, top_10)]
score_summary <- spread(score_summary, key = c('sentiment'), value = 'N')
# Create net score
score_summary[, net_score := (as.numeric(positive) - as.numeric(negative))]

# Net sentiment over time 
ggplot(score_summary, aes(year, net_score, fill = year, color = year, group = year)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = score_summary$year) +
  facet_grid(~top_10) +
  labs(x = 'year', y = 'Net sentiment', title = 'Triple J Top 10 - Net sentiment top 10')


# Wordcloud of main contributors to sentiment analysis
temp <- rbind(dt_sentiment[sentiment == 'positive', .N, by = .(word)][, sentiment := 'positive'][order(-N)][1:10],
              dt_sentiment[sentiment == 'negative', .N, by = .(word)][, sentiment := 'negative'][order(-N)][1:10])

g1 <- ggplot(temp[sentiment == 'positive'], 
             aes(x = reorder(word, N), y = N, fill = sentiment, group = sentiment, alpha = 0.4)) +
  scale_fill_manual(values = "#58D68D") +
  geom_col(show.legend = FALSE) +
  theme(axis.title.y=element_blank()) +
  coord_flip()

g2 <- ggplot(temp[sentiment == 'negative'], 
             aes(x = reorder(word, N), y = N, fill = sentiment, group = sentiment, alpha = 0.4)) +
  scale_fill_manual(values = "#F5B7B1") +
  geom_col(show.legend = FALSE) +
  theme(axis.title.y=element_blank()) +
  coord_flip()

grid.arrange(g1,g2)

# Word cloud of main contributers to text analysis

data_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F5B7B1", "#58D68D"),
                   max.words = 100)





