library(twitteR)
library(tidyverse)
library(tidytext)
library(scales)

#This code came from the following blog post
# https://medium.com/the-artificial-impostor/analyzing-tweets-with-r-92ff2ef990c6
consumer_key <- "oXFk74DS3yAY68tpSiwPInPEh"
consumer_secret <- "uQH95x6c31uWi6QObf5e5FGo7r9xMtIqSU4IgNhpEkXxp8cQpN"
access_token <- "396371309-s5JAtNxV977SCdrMAFFUCNsQ8qrtoPsglj2Kpsmy"
access_secret <- "klrMJUpc0o2rixeIaPTH70dfF0wE7bar562Tux1eumsjv"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

trump <- userTimeline("realDonaldTrump", n=3200, includeRts=T)
obama <- userTimeline("BarackObama", n=3200, includeRts=T)
president.obama <- userTimeline("POTUS44", n=3200, includeRts=T)

df.trump <- twListToDF(trump)
df.obama <- twListToDF(obama)
df.president.obama <- twListToDF(president.obama)

tweets <- rbind((df.trump %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)),
                (df.obama %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)),
                (df.president.obama %>% filter(isRetweet==F) %>% select(text, favoriteCount, screenName, created, retweetCount)))

ggplot(tweets, aes(x=created, fill=screenName)) + 
  geom_histogram(position = 'identity', bins=50, show.legend = F) +
  facet_wrap(~screenName, ncol = 1, scales = 'free_y') + 
  labs(title = "Distribution of Twitter Data")

replace_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
unnest_reg  <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(id = row_number()) %>% unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% 
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

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
ggplot(frequency.spread, aes(BarackObama, realDonaldTrump)) +
  geom_jitter(
    alpha = 0.1, size = 2.5, width = 0.15, height = 0.15) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 0) +    
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") + theme_bw()

             