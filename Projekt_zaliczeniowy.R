#-------------------------------------------------------#
#                     Text Mining                       #
#                        CDV                            #
#                Projekt zaliczeniowy                   #
#                  Marek Miłosierny                     #
#-------------------------------------------------------#

library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)
library(lubridate)
library(tm)
library(hunspell)

load("Projekt_zal_dane.RData")

# Pobranie numerów działów
all_articles <- articles %>% 
  mutate(dzial = str_sub(url, 43, 48))

# Przekształcenie daty na osobne kolumny
all_articles <- all_articles %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date))

# Timeseries dla ogółu danych
all_articles %>% 
  count(year, month) %>% 
  ggplot() +
  geom_col(aes(make_date(year, month, 1), n)) +
  xlab("Data") +
  ylab("Ilość artykułów") +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Wypisanie nr działów
unique(all_articles$dzial)

# Przypisanie działom nazw
all_articles <- all_articles %>% 
  mutate(dzial = case_when(.$dzial == "114881" ~ "Świat",
                           .$dzial == "114884" ~ "Polityka",
                           .$dzial == "114883" ~ "Polska",
                           .$dzial == "173952" ~ "Koronawirus",
                           .$dzial == "157710" ~ "Lotto",
                           .$dzial == "127561" ~ "Deutsche Welle",
                           .$dzial == "114871" ~ "Najnowsze",
                           .$dzial == "156046" ~ "Edukacja",
                           .$dzial == "143907" ~ "Wybory",
                           .$dzial == "166611" ~ "Wiadomości dnia",
  ))


# Liczba artykułów dla działu w miesiącu

all_articles %>% 
  count(dzial, year, month) %>% 
  mutate(date = make_date(year, month)) %>% 
  ggplot() +
  geom_col(aes(date, n, fill = dzial), position = position_stack(reverse = TRUE)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  labs(fill = "Działy") +
  xlab("Data") +
  ylab("Ilość artykułów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = "top")

# Wybranie badanego okresu i działu

articles <- all_articles %>% 
  filter(date >= "2019-06-01", date < "2020-01-01") %>% 
  filter(dzial == "Świat")

# Liczba artykułów w wybranym dziale w miesiącu

articles %>% 
  count(dzial, year, month) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  ggplot() +
  geom_col(aes(date, n, fill = dzial), position = position_stack(reverse = TRUE)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  labs(fill = "Dział") +
  xlab("Data") +
  ylab("Ilość artykułów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "bottom")

# Lematyzatyzacja

pl_stopwords <- read_lines("stop_words_polish.txt")

lead_words <- articles %>% 
  unnest_tokens(word, lead, token = "words") %>%  
  filter(nchar(word) > 3) %>% 
  filter(!word %in% pl_stopwords)

lead_words <- lead_words %>%
  mutate(stem_word = hunspell_stem(word, dict = dictionary('pl_PL')))
lead_words$stem_word <- unlist(lapply(lead_words$stem_word, function(x) x[1]))

# Top 30 słów dla lead

lead_words %>% 
  filter(!is.na(stem_word)) %>% 
  count(stem_word) %>% 
  top_n(30, n) %>% 
  ggplot() +
  geom_bar(aes(reorder(stem_word, -n), n), stat = "identity", color='skyblue',fill='steelblue')+
  xlab("Słowa") +
  ylab("Ilość słów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Wordcloud

lead_words_count <- lead_words %>% 
  count(stem_word)

wordcloud(lead_words_count$stem_word, lead_words_count$n, max.words = 100, scale = c(50, 0.5),
          colors=brewer.pal(8, "Dark2"))

# Pojedycze słowa dla body

body_words <- articles %>% 
  unnest_tokens(word, body, token = "words") %>% 
  filter(nchar(word) > 3) %>%
  filter(!word %in% pl_stopwords)

# Nie działa - wywala rstudio
body_words <- body_words %>%
  mutate(stem_word = hunspell_stem(word, dict = dictionary('pl_PL')))
body_words$stem_word <- unlist(lapply(lead_words$stem_word, function(x) x[1]))

# Top 30 słów dla body

body_words %>% 
  count(word) %>% 
  top_n(30, n) %>% 
  ggplot() +
  geom_bar(aes(reorder(word, -n), n), stat = "identity", color='skyblue',fill='steelblue') +
  xlab("Słowa") +
  ylab("Ilość słów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

