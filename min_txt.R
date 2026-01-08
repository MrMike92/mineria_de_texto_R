# Instalar si no se tiene
# packs <- c("tidyverse", "tidytext", "janeaustenr", "textdata", "stringi",
#            "SnowballC", "topicmodels", "widyr", "wordcloud", "igraph", "ggraph")
# to_install <- packs[!packs %in% installed.packages()[, "Package"]]
# if(length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringi)
library(SnowballC)
library(topicmodels)
library(widyr)
library(wordcloud)
library(igraph)
library(ggraph)
library(dplyr)

# Para ver los libros
# austen_books() %>% 
#   group_by(book) %>%
#   summarise(total_lines = n())

# 1 - Selección de conjunto
data <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(id = row_number()) %>%
  select(id, book, text)

glimpse(data)

# 2- Limpieza de texto (normalización + filtrado)
clean_text <- function(x){
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>% # quita acentos
    str_replace_all("http\\S+|www\\S+", " ") %>% # links
    str_replace_all("[^a-z\\s]", " ") %>% # solo letras
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

data_clean <- data %>%
  mutate(text_clean = clean_text(text)) %>%
  filter(text_clean != "")

# 3- Tokenización + stopwords + stemming
tokens <- data_clean %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) >= 3) %>%
  mutate(word_stem = SnowballC::wordStem(word, language = "english"))

head(tokens)

# 4 - Análisis exploratorio: palabras más frecuentes
top_words <- tokens %>%
  count(word_stem, sort = TRUE) %>%
  slice_head(n = 20)

top_words

top_words %>%
  ggplot(aes(x = reorder(word_stem, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Palabra (stem)", y = "Frecuencia", title = "Top 20 palabras")

# 5 - N-gramas (bigramas) y asociaciones
bigrams <- data_clean %>%
  unnest_tokens(bigram, text_clean, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("w1", "w2"), sep = " ") %>%
  filter(!w1 %in% stop_words$word, !w2 %in% stop_words$word) %>%
  filter(str_length(w1) >= 3, str_length(w2) >= 3) %>%
  unite(bigram, w1, w2, sep = " ")

top_bigrams <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 20)

top_bigrams

# 6 - Representación numérica: Matriz TF-IDF
tfidf <- tokens %>%
  count(id, word_stem) %>%
  bind_tf_idf(word_stem, id, n)

tfidf %>% arrange(desc(tf_idf)) %>% slice_head(n = 20)

dtm <- tokens %>%
  count(id, word_stem) %>%
  cast_dtm(id, word_stem, n)

dtm

# 7 - Modelado exploratorio: LDA (tópicos)
set.seed(42)
lda_model <- LDA(dtm, k = 5, control = list(seed = 42))
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "beta", title = "Top términos por tópico (LDA)")

# 8 - Exploratorio extra: similitud entre palabras (co-ocurrencia)
word_pairs <- tokens %>%
  pairwise_count(word_stem, id, sort = TRUE, upper = FALSE)

word_pairs %>% slice_head(n = 20)
set.seed(42)
edges <- word_pairs %>% filter(n >= 30)
g <- graph_from_data_frame(edges)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(alpha = n)) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Red de co-ocurrencia (palabras)")
