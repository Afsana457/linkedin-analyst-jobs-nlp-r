library(tidyverse)

# Set your working directory (optional if you've already done it)
setwd("D:/linkedin_job_analysis_project")

# Load the CSV (with double .csv name)
job_data <- read_csv("linkedin_job_postings_2023_2024.csv.csv")

# Check structure
glimpse(job_data)
job_data_clean <- job_data %>%
  filter(!is.na(description)) %>%
  filter(str_detect(tolower(title), "analyst")) %>%
  select(title, description, location)
library(tidytext)

job_words <- job_data_clean %>%
 unnest_tokens(word, description)
data("stop_words")

job_words <- job_words %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 2)  # remove short words
word_counts <- job_words %>%
  count(word, sort = TRUE)

head(word_counts, 20)  # Top 20 words
top_words <- word_counts %>% slice_max(n, n = 20)

head(word_counts)
top_words <- word_counts %>% slice_max(n, n = 20)
top_words
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Keywords in Analyst Job Descriptions",
       x = "Keyword", y = "Frequency")
library(wordcloud)
library(RColorBrewer)

set.seed(123)

wordcloud(words = word_counts$word,
          freq = word_counts$n,
          min.freq = 15,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))






