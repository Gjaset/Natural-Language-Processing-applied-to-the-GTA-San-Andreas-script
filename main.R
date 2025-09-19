### Natural Language Processing applied to the GTA San Andreas script ###


suppressMessages(suppressWarnings(library(reader)))
suppressMessages(suppressWarnings(library(tidyverse)))

guion_gta <- read_lines ("guionGTA.txt")
guion_gta <- unlist(c(guion_gta))

names (guion_gta)

head(guion_gta, n=3)

guion_gta <- tibble(
  line = seq_along(guion_gta),
  text = guion_gta
)

class(guion_gta)

dim(guion_gta)
head(guion_gta,3)

#tokenization

suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(magrittr)))

guion_gta %<>%
  unnest_tokens(input = text, output = word)%>%
  filter(!is.na(word))
class(guion_gta)

dim(guion_gta)
head(guion_gta, n=10)

#text normalization

guion_gta %>%
  filter(grepl(pattern = "[0-9]", x = word)) %>%
  count(word, sort = TRUE)

#remove tokens containing numbers

guion_gta %<>% 
  filter(!grepl(pattern = "[0-9]", x = word))
dim(guion_gta)

#Load built-in stop words in tidy text

data(stop_words)
class(stop_words)

dim(stop_words)
head(stop_words, n=10)

table(stop_words$lexicon)

#remove stop word English

guion_gta %<>%
  anti_join(x = ., y=stop_words)
dim(guion_gta)
head(guion_gta, n=10)

#remove accents from text

#replacement_list <- list(
#  'á' = 'a',
#  'é' = 'e',
#  'í' = 'i',
#  'ó' = 'o',
#  'ú' = 'u'
#)

#guion_gta %<>%
#  mutate(word = chartr(
#    old = names(replacement_list) %>% str_c(collapse = ""),
#    new = replacement_list %>% str_c(collapse = "")
#  ))
#dim(guion_gta)
#head(guion_gta, n=10)


#top 10 most frequent tokens

guion_gta %>%
  count(word, sort = TRUE) %>%
  head(n = 10)

#visualization of most frequent tokens

suppressMessages(suppressWarnings(library(gridExtra)))

guion_gta %>%
  count(word, sort = TRUE) %>%
  filter (n  >= 100) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_col(fill="darkolivegreen4", alpha = 0.8)+
  theme_light()+
  coord_flip()+
  xlab(NULL)+
  ylab("Frecuencia")+
  ggtitle("GTA SA: Conteo de Palabras") -> p1
grid.arrange(p1, ncol=2)

#word cloud visualization 

suppressMessages(suppressWarnings(library(wordcloud)))
par(mfrow = c(1,2), mar=c(1,1,2,1), mgp=c(1,1,1))

set.seed(123)
guion_gta %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(
    words = word,
    freq = n,
    max.words = 50,
    colors = "black"
  ))
title(main = "GTA SA")

#relative word frequencies by author

bind_rows(
  mutate(.data = guion_gta, author = "cj"),
  mutate(.data = guion_gta, author = "sweet")
  #mutate(.data = guion_gta, author = "ryder")
) %>%
  count(author, word)%>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n ) %>%
  
  spread(author, proportion, fill = 0) -> frec

frec %<>%
  select(word, cj, sweet)
dim(frec)
head(frec, n = 10)

#top 10 most frequent words shared by both characters

frec %>%
  filter(cj !=0, sweet !=0) %>%
  arrange(desc(cj), desc(sweet)) -> frec_comun
dim(frec_comun)
head(frec_comun, n=10)

#proportion of shared words
############revisar#############
prop_words_comun <- dim(frec_comun)[1]/dim(frec_comun)[1]
prop_words_comun


#correlation of word frequencies
 
#correlation over the entire vocabulary
cor.test(x=frec$sweet, y=frec$cj)

#correlation based only on shared words
cor.test(x=frec_comun$sweet, y=frec_comun$cj)



#bootstrap for the relation between relative frequencies


cor_func <- function(data, indices){
  d <- data[indices, ]
  return(cor(d$sweet,d$cj))
}

data_cor <- frec %>%
  select(sweet, cj)

set.seed(123)
boot_cor <- boot(data = data_cor, statistic = cor_func, R=2000)
boot.ci(boot_cor,  type = "perc")

hist (boot_cor$t,
      main="Distribucion de Bootstrap correlacion",
      xlab="r",
      col = "steelblue",
      border = "white")


#bootstrap analysis for the correlation of shared words

cor_func_comun <- function(data, indices){
  d <- data[indices, ]
  return(cor(d$sweet, d$cj))
}
data_cor_comun <- frec_comun %>%
  select(sweet, cj)

set.seed(123)
boot_cor_comun <- boot(data = data_cor_comun, statistic = cor_func_comun, R = 2000)
boot.ci(boot_cor_comun, type = "perc")

hist(boot_cor_comun$t,
     main = "Distribución bootstrap (palabras en comun)",
      xlab ="r",
      col ="steelblue",
      border ="white")

#sentiment dictionary

positive_words <- get_sentiments("bing") %>%
  mutate(sentiment = "Positivo")
negative_words <- get_sentiments("bing") %>%
  mutate(sentiment = "Negativo")
get_sentiments("bing") %>%
  count(sentiment)

#visualization of emotionally charged words

suppressMessages(suppressWarnings(library(RColorBrewer)))

guion_gta %>%
  inner_join(sentiments_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter ( n > 25) %>%
  mutate (n = ifelse(sentiment == "Negativo", -n, n)) %>%
  mutate(word = reorder (word, n))%>%
  ggplot (aes(x = word, y=n, fill = sentiment))+
  geom_col ()+
  scale_fill_manual(values = brewer.pal(8,"Dark2")[c(2,5)])+
  coord_flip(ylim=c(-7,7))+
  labs(
    title = "GTA SA: Conteo de sentimiento",
    y = "Frecuencia",
    x = NULL
  )+
  theme_minimal() -> p1
grid.arrange(p1, ncol=2)

#comparative sentiment visualization using word clouds

suppressMessages(suppressWarnings(library(reshape2)))

par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
set.seed(123)
guion_gta %>%
  inner_join(sentiments_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = brewer.pal(8, "Dark2")[c(2, 5)],
    max.words = 50,
    title.size = 1.5
  )
  
  title("GTA SA")

  #Bigrams
  
  suppressWarnings(
    guion_gta <- read_csv("guionGTA.txt",
    col_names = FALSE,
    show_col_type = FALSE) %>%
    unlist(use.names = FALSE)
  )
names(guion_gta) <- NULL
guion_gta <- tibble ( 
  line = 1:length(guion_gta),
  text = guion_gta)

#Bigram tokenization

guion_gta %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2)%>%
  filter(!is.na(bigram)) -> guion_gta_bi 
dim(guion_gta_bi)
head(guion_gta_bi, n=10)

#top 10 frequent bigrams

guion_gta_bi %>%
  count(bigram, sort = TRUE) %>%
  head(n=10)
#skip stopt words and clean bigrams

guion_gta_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  mutate(word1 = chartr(
    old = names(replacement_list) %>% str_c(collapse = ''),
    new = replacement_list %>% str_c(collapse = ''),
    x = word1)) %>%
  
  mutate(word2 = chartr(
    old = names(replacement_list) %>% str_c(collapse = ''),
    new = replacement_list %>% str_c(collapse = ''),
    x = word2)) %>%
  
  filter(!is.na(word1)) %>%
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> guion_gta_bi_counts

dim(guion_gta_bi_counts)
head(guion_gta_bi_counts, n = 10)

#define a network from bigram frequency (weights)

suppressMessages(suppressWarnings(library(igraph)))

g <- guion_gta_bi_counts %>%
  filter(weight > 16) %>%
  graph_from_data_frame(directed = FALSE)

set.seed(123)
plot(
  g,
  layout = layout_with_fr,
  vertex.color = 1,
  vertex.frame.color = 1,
  vertex.size = 3,
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = 1,
  main = "Umbral = 3"
)

#Network using a different threshold

g <- guion_gta_bi_counts %>%
  filter(weight > 2) %>%
  graph_from_data_frame(directed = FALSE)

set.seed(123)
plot(
  g,
  layout = layout_with_kk,      
  vertex.color = 1,
  vertex.frame.color = 1,
  vertex.size = 3,
  vertex.label = NA,            
  main = "Umbral = 1"           
)

