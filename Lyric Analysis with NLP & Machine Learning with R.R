#####
#load libraries
library(dplyr) #data manipulation
library(ggplot2)#visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations

#Visualizations!
library(ggrepel) #`geom_label_repel`
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
#####

#Read the data (Scraped and configured dataset final-MJ)
MJ_read <- read.csv("final-MJ.csv", stringsAsFactors = FALSE)

#Columns in dataframe
names(MJ_read)

#Drop unwanted fields by selecting those wanted
#Rename text to lyrics and replace . with _
final_MJ <- MJ_read %>% 
  select(Song, lyrics,Year, Album, Peak, chart_level)
names(final_MJ)
#View data in a transposed view
glimpse(final_MJ[1,])

#Dimensions of table (209 by 6)
dim(final_MJ)

str(final_MJ[1,]$lyrics, nchar.max = 500)

### Data Conditioning ###

# Function created to expand contractions
fix.contractions <- function(con) {
  con <- gsub("won't", "will not", con)
  con <- gsub("ain't", "am not", con)
  con <- gsub("can't", "can not", con)
  con <- gsub("n't", " not", con)
  con <- gsub("'ll", " will", con)
  con <- gsub("'re", " are", con)
  con <- gsub("'ve", " have", con)
  con <- gsub("'m", " am", con)
  con <- gsub("'d", " would", con)
  # 's could be 'is' or could be possessive: it has no expansion
  con <- gsub("'s", "", con)
  return(con)
}

# fix (expand) contractions. Applies fix.contractions on lyrics
final_MJ$lyrics <- sapply(final_MJ$lyrics, fix.contractions)
  
#function to remove special characters
removeSpecialChars <- function(sc) gsub("([^a-zA-Z0-9!])", " ",sc)

#Remove special characters
final_MJ$lyrics <- sapply(final_MJ$lyrics,removeSpecialChars)

#Convert everything to lower case
final_MJ$lyrics <- sapply(final_MJ$lyrics, tolower)

#Examine lyrics (must show a neater version)
str(final_MJ[1,]$lyrics, nchar.max = 300)

summary(final_MJ)

#Look for song trends across time (group years into decades)
final_MJ <- final_MJ %>%
  mutate(decade = 
           ifelse(final_MJ$Year %in% 1972:1979, "1970s", 
                  ifelse(final_MJ$Year %in% 1980:1989, "1980s", 
                         ifelse(final_MJ$Year %in% 1990:1999, "1990s", 
                                ifelse(final_MJ$Year %in% 2000:2009, "2000s", 
                                       ifelse(final_MJ$Year %in% 2010:2015, "2010s", 
                                              "NA"))))))


#Song stats (most active decade was 1980s)
theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#View full data set at disposal (The 80s spawned the most hits - it was also the most active year for MJ)
final_MJ %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Decade", y = "Song Count") +
  ggtitle("All Songs in Data")

#Songs that reached number 1
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
final_MJ %>%
  filter(Peak == "1") %>%
  select(Year, Song, Peak) %>%
  arrange(Year) %>%
  mutate(Year = color_tile("lightblue", "lightgreen")(Year)) %>%
  mutate(Peak = color_tile("lightgreen", "lightgreen")(Peak)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Michael Jackson's No. 1 Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


#### Text Mining/Text Analytics ####

# Lexical Complexity is the following:
# Word Frequency (number of words per song)
# Word Length (average length of individual words in a text)
# Lexical Diversity (number of unique words used in a text)
# Lexical Density (number of unique words/total number of words)

# Manually cleaning superfuous words
undesirable_words <- c("michael", "jackson", "Miscellaneous", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "oooh", "hee", "woo", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")
# Random list of stop words
  head(sample(stop_words$word, 15),15)

# Create dataset that is the tidytext version i.e. without stop words, undesirable words and 1 to 3 character words
MJ_filter <- final_MJ %>%
  unnest_tokens(word, lyrics) %>% #tokenization and transform to tidy data
  anti_join(stop_words) %>% #Removes stop words
  distinct() %>% #Removes duplicate words
  filter(!word %in% undesirable_words) %>% #Removes undesirable words
  filter(nchar(word) > 3) #Remove characters with fewer than 4 characters like 'hey' yea'

# Table showing tokenized, unsummarized, tidy data structure
MJ_filter %>% 
  filter(word == "world") %>%
  select(word, Song, Year, Peak, decade, chart_level) %>%
  arrange() %>%
  top_n(10,Song) %>%
  mutate(Song = color_tile("lightblue","lightblue")(Song))%>%
  mutate(word = color_tile("lightyellow","lightyellow")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Example - world") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)
# Word frequency (correlation between word frequency and hit songs)
full_word_count <- final_MJ %>%
  unnest_tokens(word, lyrics) %>% # No filtering needed as to get true count of word frequency across songs
  group_by(Song,chart_level) %>% #grouping songs with their respective chart level
  summarise(num_words = n()) %>% # summarises number of words in each song
  arrange(desc(num_words)) #arrange number of words in decreasing order

# Top ten songs with highest number of words in descending order
full_word_count[1:10,] %>% #Following creates a better looking table use kableExtra package
  ungroup(num_words, Song) %>% 
  mutate(num_words = color_bar("lightgreen")(num_words)) %>%
  mutate(Song = color_tile("lightpink","lightpink")(Song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

# Histogram showcasing word count distribution across all songs
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = chart_level)) +
  xlab("Word Count per Song") +
  ylab("Song Count") +
  ggtitle("Word Count Distribution") +
  
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Spotting an anomaly in the histogram, there's a top 10 song that has over 700 words (song has a rapper in it that is why its soo high)
full_word_count %>%
  filter(chart_level == 'Top 10' & num_words > 700) %>%
  left_join(final_MJ, by = "Song") %>%
  select(Song = Song, 
         "Word Count" = num_words, 
         "Peak Position" = Peak) %>%
  kable("html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

#### Top Words ####
colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Love is the most common topic
MJ_filter %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most frequent words found in MJ's lyrics") +
  coord_flip()

# Generate a word cloud, this is highly visual shows topics artist sings about
mjword_cloud <- MJ_filter %>%
  count(word,sort=TRUE)
wordcloud2(mjword_cloud[1:300,])

# Generate custom model word cloud
wordcloud2(mjword_cloud, figPath = "heehee.png")


#### Popular words ####
popular_words <- MJ_filter %>% 
  group_by(chart_level) %>%
  count(word, chart_level, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(chart_level,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = chart_level)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~chart_level, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

#### Timeless Words ####
timeless_words <- MJ_filter %>% 
  filter(decade != 'NA') %>%
  group_by(decade) %>%
  count(word, decade, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade,n) %>%
  mutate(row = row_number())

timeless_words %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Timeless Words") + 
  theme(plot.title = element_text(hjust = 0.1)) +
  theme_lyrics() +  
  facet_wrap(~decade, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = timeless_words$row, # notice need to reuse data frame
    labels = timeless_words$word) +
  coord_flip()

#### Word Length ####
#unnest and remove undesirable words, but leave in stop and short words
mj_length_of_words <- final_MJ %>%
  unnest_tokens(word, lyrics) %>%
  group_by(Song,decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word)) 

mj_length_of_words %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 1), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

# Word cloud for very long words (issues wit)
wc <- mj_length_of_words %>%
  ungroup() %>%
  select(word, word_length) %>%
  distinct() %>%
  arrange(desc(word_length))

wordcloud2(wc[1:300, ], 
           size = .15,
           minSize = .0005,
           ellipticity = .3, 
           rotateRatio = 1, 
           fontWeight = "bold")


#### Lexical Diversity, Lexical Density & Chart History ####
lex_diversity_per_year <- final_MJ %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(Song,Year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(Year, lex_diversity)) +
  geom_point(color = colors[3],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = Year, y = lex_diversity), se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = colors) +
  theme_classic() + 
  theme_lyrics()

diversity_plot # over the years MJ has increase the vocabulary range in his songs

lex_density_per_year <- final_MJ %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(Song,Year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(Year, lex_density)) + 
  geom_point(color = colors[4],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", 
              se = FALSE, 
              method = "lm") +
  geom_smooth(aes(x = Year, y = lex_density), 
              se = FALSE,
              color = "blue", 
              lwd = 2) +
  ggtitle("Lexical Density") + 
  xlab("") + 
  ylab("") +
  scale_color_manual(values = colors) +
  theme_classic() + 
  theme_lyrics()

density_plot # lexical density has decreased over the number of years which means songs have become more repetitive

chart_history <- final_MJ %>%
  filter(Peak > 0) %>%
  group_by(Year, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot(aes(Year, number_of_songs)) + 
  geom_point(color = colors[5],
             alpha = .4, 
             size = 4, 
             position = "jitter") +
  geom_smooth(aes(x = Year, y = number_of_songs), 
              se = FALSE, 
              method = "lm", 
              color = "black" ) +
  geom_smooth(aes(x = Year, y = number_of_songs), 
              se = FALSE,
              color = "blue", 
              lwd = 2) +
  ggtitle("Chart History") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = colors) +
  theme_classic() + 
  theme_lyrics()

grid.arrange(diversity_plot, density_plot, chart_history, ncol = 3)

#### Sentiment Lexicons ####
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

# Differences between each lexicon
new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

# Lyrics found in Lexicons
MJ_filter %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_lyrics) %>%
  select(lexicon, lex_match_words,  words_in_lyrics, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightyellow")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Lyrics Found In Lexicons")

# Sentiment Analysis using different lexicons (AFINN, nrc & bing)
new_sentiments %>%
  filter(word %in% c("baby", "disappointed", "manipulate",
                     "world", "black")) %>%
  arrange(word) %>% #sort
  select(-score) %>% #remove this field
  mutate(word = color_tile("lightblue", "lightblue")(word),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Specific Words")

#### Create Sentiment Datasets ####

# Overall sentiment of MJ's songs
library('memery')
library('magick')

MJ_bing <- MJ_filter %>%
  inner_join(get_sentiments("bing")) #bing for binary sentiments

MJ_nrc <- MJ_filter %>%
  inner_join(get_sentiments("nrc")) #nrc for categorical sentiments

# nrc plot
MJ_nrc_plot <- MJ_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 2000)) + #Hard code the axis limit
  ggtitle("King of Pop NRC-Sentiment Analysis")


img <- "hoohoo.jpg" #Load the background image
lab <- ""  #Turn off the label
#Overlay the plot on the image and create the meme file
meme(img, lab, "meme_nrc.jpg", inset = MJ_nrc_plot)
#Read the file back in and display it!
MJ_nrc_meme <- image_read("meme_nrc.jpg")
plot(MJ_nrc_meme)

# bing plot (positive vs negative sentiment analysis) #negative higher
bing_plot <- MJ_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1000)) +
  ggtitle("King of Pop Bing Sentiment Analysis") +
  coord_flip()

img1 <- "shukachuka.jpg"
lab1 <- ""
meme(img1, lab1, "meme_bing.jpg", inset = bing_plot)
x <- image_read("meme_bing.jpg")
plot(x)

# Are top charted songs positive or negative? and how do they differ to those that are uncharted?
MJ_polarity <- MJ_bing %>%
  count(sentiment, chart_level) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percentage_positive = positive / (positive + negative)*100)

pos_plot <- MJ_polarity %>%
  ggplot( aes(chart_level, percentage_positive, fill = chart_level)) +
  geom_col() +
  scale_fill_manual(values = c(colors[3:5])) +
  geom_hline(yintercept = 0, color = "red") +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive By Chart Level")

grid.arrange(pos_plot, ncol = 2)

#### Analysing sentiment of lyrics in MJ's songs  ####

# Remove positive and negative sentiments from  lexicon
MJ_nrc_noposneg <- MJ_filter %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

# Sentiment categories for couple of songs

MJ_nrc_noposneg %>%
  filter(Song %in% c("Thriller", "Man in the Mirror", "Bad", 
                     "Ain't No Sunshine", "Blood on the Dance Floor",
                     "Dirty Diana")) %>%
  count(Song, sentiment, Year) %>%
  mutate(sentiment = reorder(sentiment, n), Song = reorder(Song, n)) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
  facet_wrap(Year ~ Song, scales = "free_x", labeller = label_both) +
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL) +
  ggtitle("Sentiment Analysis on MJ's songs") +
  coord_flip()

# Pairwise comparison and correlation 
library(widyr)

pair_comp <- MJ_filter %>%
  filter(n() >= 20) %>%  #High counts
  pairwise_count(word, Song, sort = TRUE) %>% #co-occurrence
  filter(item1 %in% c("love", "girl", "night", "stay")) %>%
  group_by(item1) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  mutate(row = -row_number()) #Descending order

pair_comp %>%
  ggplot(aes(row, n, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~item1, scales = "free") +
  scale_x_continuous(  #This handles replacement of row
    breaks = pair_comp$row, #Notice need to reuse data frame
    labels = pair_comp$item2) +
  theme_lyrics() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Comparison Counts") +
  coord_flip()


MJ_filter %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, Song, sort = TRUE) %>%
  filter(item1 %in% c("love", "girl", "night", "stay")) %>%
  group_by(item1) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~item1, scales = 'free') +
  theme_lyrics() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Correlation") +
  coord_flip()
