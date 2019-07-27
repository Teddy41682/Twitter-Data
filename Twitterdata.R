library(twitteR)
library(tidyverse)
library(tidytext)
library(scales)

lexicon <- get_sentiments("afinn")

#Needed codes to access Twitter API
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

#Setting up Twitter Authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Pulling Nike Twitter data and converting it into a data frame
#----------------------------------
Nike <- searchTwitter("Nike",n = 3500)

df.nike <- twListToDF(Nike)
#------------------------------------

#Removing all retweets 
df.nike <- df.nike %>% 
  filter(isRetweet == FALSE) %>% 
  select(text,favoriteCount,created,screenName,retweetCount) %>%
  mutate(id = row_number())

#Changes created column to date formating
df.nike$created <- as.Date(df.nike$created)

#Patterns for reges to Id to remove URL's and parse Twitter data
replace_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
unnest_reg  <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#Code to tolkenize and break down twitter data into indvidual words
tidy_tweets <- df.nike %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% 
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

#Code to calculate number of positve and negative tweets about nike
sentiment_scores <- tidy_tweets %>% merge(lexicon,by="word") %>% group_by(id) %>% summarise(sentiment_score=sum(score))

#added sentiment scores to original data set
df.nike_sentiment <- df.nike %>% merge(sentiment_scores, by="id", all.x = TRUE)
df.nike_sentiment$sentiment_score[is.na(df.nike_sentiment$sentiment_score)] <- 0

#df.nike_sentiment$sentiment_score <- as.factor(df.nike_sentiment$sentiment_score)
top_nike_tweeters <- df.nike_sentiment %>% count(screenName) %>% arrange(desc(n))

#Createes Graphic displaying the sentiment of the top 20 Nike Tweeters
for(i in 1:dim(df.nike_sentiment)[1]){
  if(df.nike_sentiment$sentiment_score[i] > 0) {
    df.nike_sentiment$sentiment_score[i] <- "Positive"
  }
  else if(df.nike_sentiment$sentiment_score[i] < 0) {
    df.nike_sentiment$sentiment_score[i] <- "Negative"
  }
  else{
    df.nike_sentiment$sentiment_score[i] <- "Neutral"
  }
}

graph <- df.nike_sentiment %>% filter(screenName %in% top_nike_tweeters$screenName[1:20])

ggplot(graph, aes(screenName, fill=sentiment_score)) + 
  geom_bar() + 
  coord_flip() +
  labs(title="Figure 2", y="Number of Tweets", x="Twitter Screen Name")

#_--------------------------------
#code for producing Trump obama Graphic 
#This code came from the following blog post
# https://medium.com/the-artificial-impostor/analyzing-tweets-with-r-92ff2ef990c6

#Pulls in Tweets from Twitter API from presidents Twitter 
trump <- userTimeline("realDonaldTrump", n=3200, includeRts=T)
obama <- userTimeline("BarackObama", n=3200, includeRts=T)
president.obama <- userTimeline("POTUS44", n=3200, includeRts=T)

#Creates Data frame out of Tweets
#---------------------------------------
df.trump <- twListToDF(trump)
df.obama <- twListToDF(obama)
df.president.obama <- twListToDF(president.obama)

tweets <- rbind((df.trump %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)),
                (df.obama %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)),
                (df.president.obama %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)))
#-------------------------------------------------
#Pattern for removing URL and parsing Twitter data
replace_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
unnest_reg  <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#parses out Tweets in to invidual word for Lexicon Analysis
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(id = row_number()) %>% unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% 
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

#Calculatees the frequency of a word per screenanme
frequency <- tidy_tweets %>% 
  group_by(screenName) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(screenName) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency.spread <- frequency %>% 
  select(screenName, word, freq) %>% 
  spread(screenName, freq) %>%
  arrange(desc(BarackObama), desc(realDonaldTrump))

#Outputs the graphic that compares Obama and Donald Trumps Tweets
ggplot(frequency.spread, aes(BarackObama, realDonaldTrump)) +
  geom_jitter(
    alpha = 0.1, size = 2.5, width = 0.15, height = 0.15) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 0) +    
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") + theme_bw() +
   labs(title="Figure 1", y="Presdient Trump", x="Presdient Obama")

             

