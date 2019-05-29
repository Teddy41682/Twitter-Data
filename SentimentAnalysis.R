library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#This code comes from Tidy Text Website
# https://www.tidytextmining.com/sentiment.html

#Creating a tibble that has linenumber and chapter included in it
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Only selecting joy words out of the nrc lexicon
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

#Filtering down to get word count of joyful words from emma book
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Creates a tibble that has the number occurances of positve and negative words and the overall
#sentiment for so many lines. 
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Creating a graphic to represent our findings
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
