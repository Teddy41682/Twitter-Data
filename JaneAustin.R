library(tidyverse)
library(tidytext)
library(stringr)
library(janeaustenr)

#This code from the TidyText mining website 
#https://www.tidytextmining.com/sentiment.html

#Creating a tibble that has sentances organized into chapter and line
original_books <- austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#Tokanizing the sentances so that each row is a word 
tidy_book <- original_books %>% unnest_tokens(word,text)

data(stop_words)

#Cleaning up tibble and removing common words that arnt helpful for analysis
tidy_book <- tidy_book %>% anti_join(stop_words)

tidy_book %>% count(word, ,sort=T) %>% filter(n > 600) %>% ggplot(aes(x=word, y=n)) + geom_col() +coord_flip()

tidy_book %>% group_by(chapter) %>% count(word,sort=T) %>% filter(n >40) %>% ggplot(aes(x=word, y=n, fill=chapter)) + geom_col(position = 'identity') + coord_flip()
 