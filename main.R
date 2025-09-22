# ### Natural Language Processing applied to the GTA San Andreas script ###
# 
# 
suppressMessages(suppressWarnings(library(reader)))
suppressMessages(suppressWarnings(library(tidyverse)))

gta_script <- read_lines ("guionGTA.txt")
gta_script <- unlist(c(gta_script))

names (gta_script)

head(gta_script, n=50)

gta_script <- tibble(
  line = seq_along(gta_script),
  text = gta_script
)

class(gta_script)

dim(gta_script)
head(gta_script,3)

#tokenization

suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(magrittr)))

gta_script %<>%
  unnest_tokens(input = text, output = word)%>%
  filter(!is.na(word))
class(gta_script)

dim(gta_script)
head(gta_script, n=10)

#text normalization

gta_script %>%
  filter(grepl(pattern = "[0-9]", x = word)) %>%
  count(word, sort = TRUE)

#remove tokens containing numbers

gta_script %<>%
  filter(!grepl(pattern = "[0-9]", x = word))
dim(gta_script)

#load built-in stop words in tidy text

data(stop_words)
class(stop_words)

dim(stop_words)
head(stop_words, n=10)

table(stop_words$lexicon)

#remove english stop words (but preserve character names)

#First, identify character names BEFORE removing stop words
character_names <- c("CJ", "CARL", "SWEET", "RYDER", "BIG", "SMOKE", "CESAR", 
                     "KENDL", "WOOZIE", "CATALINA", "TENPENNY", "PULASKI", 
                     "ZERO", "TRUTH", "TORENO", "MADD", "DOGG")

#Check what names appear in the text
cat("Checking character names in text:\n")
characters_in_text <- gta_script %>%
  filter(toupper(word) %in% character_names | word %in% character_names) %>%
  count(word, sort = TRUE)
print(characters_in_text)

#Create stop words list excluding character names
character_names_lower <- tolower(character_names)
filtered_stop_words <- stop_words %>%
  filter(!word %in% character_names_lower)

#Remove stop words while preserving character names
gta_script %<>%
  anti_join(x = ., y = filtered_stop_words)
dim(gta_script)
head(gta_script, n=10)

#Check character names again after filtering stop words
cat("Character names after filtering stop words:\n")
characters_after <- gta_script %>%
  filter(toupper(word) %in% character_names | word %in% character_names) %>%
  count(word, sort = TRUE)
print(characters_after)

#remove offensive and inappropriate words using tidytext lexicons

#Available lexicons in tidytext are: "afinn", "bing", "nrc", "loughran"
cat("Using available lexicons: afinn, bing, nrc\n")

#Use NRC lexicon to filter words with extreme negative emotions
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("anger", "disgust", "fear")) %>%
  select(word)

#Use AFINN for highly negative words (< -3)
afinn_negative <- get_sentiments("afinn") %>%
  filter(value <= -3) %>%
  select(word)

#Combine words to filter
offensive_words <- bind_rows(nrc_negative, afinn_negative) %>%
  distinct(word) %>%
  # Exclude character names that might be marked negatively
  filter(!tolower(word) %in% tolower(character_names))

cat("Words to filter for offensive content:", nrow(offensive_words), "\n")
cat("Words before filtering offensive content:", nrow(gta_script), "\n")

#Show some words that will be filtered (for verification)
cat("Examples of words to be filtered:\n")
print(head(offensive_words, 15))

#Filter offensive words
gta_script %<>%
  anti_join(offensive_words, by = "word")

cat("Words after filtering offensive content:", nrow(gta_script), "\n")

#Show most frequent words for verification
cat("Most frequent words after filtering:\n")
print(gta_script %>% count(word, sort = TRUE) %>% head(10))

#remove accents from text

# replacement_list <- list(
#   'á' = 'a',
#   'é' = 'e',
#   'í' = 'i',
#   'ó' = 'o',
#   'ú' = 'u'
# )

# gta_script %<>%
#  mutate(word = chartr(
#    old = names(replacement_list) %>% str_c(collapse = ""),
#    new = replacement_list %>% str_c(collapse = "")
#  ))
# dim(gta_script)
# head(gta_script, n=10)


#top 10 most frequent tokens

gta_script %>%
  count(word, sort = TRUE) %>%
  head(n = 10)

#visualization of most frequent tokens

suppressMessages(suppressWarnings(library(gridExtra)))

gta_script %>%
  count(word, sort = TRUE) %>%
  filter (n  >= 100) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_col(fill="#335f3f", alpha = 0.8)+
  theme_light()+
  coord_flip()+
  xlab(NULL)+
  ylab("Frequency")+
  ggtitle("GTA SA: Word Count") -> p1
grid.arrange(p1, ncol=2)

#word cloud visualization 

suppressMessages(suppressWarnings(library(wordcloud)))
par(mfrow = c(1,2), mar=c(1,1,2,1), mgp=c(1,1,1))

set.seed(123)
gta_script %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(
    words = word,
    freq = n,
    max.words = 50,
    colors = "#335f3f"
  ))
title(main = "GTA SA")

#relative word frequencies by character

#DIAGNOSIS: First search for how characters appear

cat("Searching for character names in text...\n")
found_characters <- gta_script %>%
  filter(str_detect(word, "CJ|CARL|SWEET|RYDER|BIG|SMOKE|CESAR|KENDL|WOOZIE|CATALINA|TENPENNY")) %>%
  count(word, sort = TRUE)

print("Found characters:")
print(found_characters)

get_main_characters <- function(word){
  # Make search case-insensitive
  word_upper <- toupper(word)
  case_when(
    word_upper %in% c("CJ", "CARL") ~ "cj",
    word_upper == "SWEET" ~ "sweet",
    word_upper == "RYDER" ~ "ryder",
    word_upper == "CESAR" ~ "cesar",
    word_upper == "BIG" ~ "big_smoke",  
    word_upper == "SMOKE" ~ "big_smoke",
    # word_upper == "KENDL" ~ "kendl",
    # word_upper == "WOOZIE" ~ "woozie",
    # word_upper == "CATALINA" ~ "catalina",
    # word_upper == "TENPENNY" ~ "tenpenny",
    # word_upper == "PULASKI" ~ "pulaski",
    # word_upper == "ZERO" ~ "zero",
    # word_upper == "TRUTH" ~ "truth",
    # word_upper == "TORENO" ~ "toreno",
    # word_upper == "MADD" ~ "madd_dogg",
    # word_upper == "DOGG" ~ "madd_dogg",
    TRUE ~ NA_character_
  )
}

#list main characters
main_characters <- c(
  "CJ", "CARL",           
  "SWEET",                
  "CESAR",                
  "RYDER",                
  "BIG", "SMOKE",         
  # "KENDL",                
  # "WOOZIE",               
  # "CATALINA",             
  # "TENPENNY",             
  # "PULASKI",              
  # "ZERO",                 
  # "TRUTH",                
  # "TORENO",               
  # "MADD", "DOGG" 
)

script_with_characters <- gta_script %>%
  mutate(
    is_character = !is.na(get_main_characters(word)),
    character = get_main_characters(word)
  )%>%
  fill(character, .direction = "down")%>%
  filter(!is_character, !is.na(character))%>%
  select(word, character)

cat("Words per character after processing:\n")
script_with_characters %>%
  count(character, sort = TRUE)

create_character_dataset <- function(character_name){
  script_with_characters %>% 
    filter(character == character_name) %>%
    select(word)
}

script_cj <- create_character_dataset("cj")
script_sweet <- create_character_dataset("sweet")
script_ryder <- create_character_dataset("ryder")
script_cesar <- create_character_dataset("cesar")
script_big_smoke <- create_character_dataset("big_smoke")
# script_kendl <- create_character_dataset("kendl")
# script_woozie <- create_character_dataset("woozie")
# script_catalina <- create_character_dataset("catalina")
# script_tenpenny <- create_character_dataset("tenpenny")
# script_pulaski <- create_character_dataset("pulaski")
# script_zero <- create_character_dataset("zero")
# script_truth <- create_character_dataset("truth")
# script_toreno <- create_character_dataset("toreno")
# script_madd_dogg <- create_character_dataset("madd_dogg")

cat("CJ:", nrow(script_cj), "words \n")
cat("Sweet:", nrow(script_sweet), "words \n")
cat("Ryder:", nrow(script_ryder), "words \n")
cat("Big Smoke:", nrow(script_big_smoke), "words \n")
cat("Cesar:", nrow(script_cesar), "words \n")
# cat("Kendl:", nrow(script_kendl), "words \n")
# cat("Woozie:", nrow(script_woozie), "words \n")
# cat("Catalina:", nrow(script_catalina), "words \n")
# cat("Tenpenny:", nrow(script_tenpenny), "words \n")
# cat("Pulaski:", nrow(script_pulaski), "words \n")
# cat("Zero:", nrow(script_zero), "words \n")
# cat("Truth:", nrow(script_truth), "words \n")
# cat("Toreno:", nrow(script_toreno), "words \n")
# cat("Madd Dogg:", nrow(script_madd_dogg), "words \n")

#Check if we found characters before proceeding
if(nrow(script_with_characters) > 0) {
  
  bind_rows(
    mutate(.data = script_cj, author = "cj"),
    mutate(.data = script_sweet, author = "sweet")
    #mutate(.data = script_ryder, author = "ryder")
  ) %>%
    count(author, word)%>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n ) %>%
    spread(author, proportion, fill = 0) -> word_frequencies
  
  #Only proceed if word_frequencies has expected columns
  if("cj" %in% names(word_frequencies) && "sweet" %in% names(word_frequencies)) {
    word_frequencies %<>%
      select(word, cj, sweet)
    dim(word_frequencies)
    head(word_frequencies, n = 10)
    
    #top 10 most frequent words shared by both characters
    word_frequencies %>%
      filter(cj !=0, sweet !=0) %>%
      arrange(desc(cj), desc(sweet)) -> shared_words
    dim(shared_words)
    head(shared_words, n=10)
    
    #proportion of shared words
    shared_words_proportion <- dim(shared_words)[1]/dim(word_frequencies)[1]
    shared_words_proportion
    
    #correlation of word frequencies
    
    #correlation over the entire vocabulary
    cor.test(x=word_frequencies$sweet, y=word_frequencies$cj)
    
    #correlation based only on shared words
    cor.test(x=shared_words$sweet, y=shared_words$cj)
    
  } else {
    cat("Error: Not enough data found for CJ and Sweet\n")
  }
  
} 

#bootstrap for the relation between relative frequencies

suppressMessages(suppressWarnings(library(boot)))

correlation_function <- function(data, indices){
  d <- data[indices, ]
  return(cor(d$sweet,d$cj))
}

correlation_data <- word_frequencies %>%
  select(sweet, cj)

set.seed(123)
bootstrap_correlation <- boot(data = correlation_data, statistic = correlation_function, R=2000)
boot.ci(bootstrap_correlation,  type = "perc")

hist (bootstrap_correlation$t,
      main="Bootstrap Correlation Distribution",
      xlab="r",
      col = "#039BE5",
      border = "white")


#bootstrap analysis for the correlation of shared words

shared_correlation_function <- function(data, indices){
  d <- data[indices, ]
  return(cor(d$sweet, d$cj))
}
shared_correlation_data <- shared_words %>%
  select(sweet, cj)

set.seed(123)
bootstrap_shared_correlation <- boot(data = shared_correlation_data, statistic = shared_correlation_function, R = 2000)
boot.ci(bootstrap_shared_correlation, type = "perc")

hist(bootstrap_shared_correlation$t,
     main = "Bootstrap Distribution (shared words)",
     xlab ="r",
     col ="#039BE5",
     border ="white")

#sentiment dictionary

suppressMessages(suppressWarnings(library(tidytext)))

positive_words <- get_sentiments("bing") %>%
  filter(sentiment == "positive") %>%
  mutate(sentiment = "Positive")
negative_words <- get_sentiments("bing") %>%
  filter(sentiment == "negative") %>%
  mutate(sentiment = "Negative")

sentiment_words <- bind_rows(positive_words, negative_words)

get_sentiments("bing") %>%
  count(sentiment)

#visualization of emotionally charged words

suppressMessages(suppressWarnings(library(RColorBrewer)))

gta_script %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter ( n > 8) %>%
  mutate (n = ifelse(sentiment == "Negative", -n, n)) %>%
  mutate(word = reorder (word, n))%>%
  ggplot (aes(x = word, y=n, fill = sentiment))+
  geom_col ()+
  scale_fill_manual(values = brewer.pal(8,"Dark2")[c(2,5)])+
  coord_flip(ylim=c(-7,7))+
  labs(
    title = "GTA SA: Sentiment Count",
    y = "Frequency",
    x = NULL
  )+
  theme_minimal() -> p1
grid.arrange(p1, ncol=2)

#comparative sentiment visualization using word clouds

suppressMessages(suppressWarnings(library(reshape2)))

par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
set.seed(123)
gta_script %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = brewer.pal(8, "Dark2")[c(2, 5)],
    max.words = 50,
    title.size = 1.5
  )

title("GTA SA")

#bigrams

suppressWarnings(
  gta_raw_script <- read_lines("guionGTA.txt")
)

gta_raw_script <- tibble ( 
  line = 1:length(gta_raw_script),
  text = gta_raw_script)

#bigram tokenization

gta_raw_script %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2)%>%
  filter(!is.na(bigram)) -> gta_script_bigrams 
dim(gta_script_bigrams)
head(gta_script_bigrams, n=10)

#top 10 frequent bigrams

gta_script_bigrams %>%
  count(bigram, sort = TRUE) %>%
  head(n=10)

#skip stop words and clean bigrams

replacement_list <- list(
  'á' = 'a',
  'é' = 'e',
  'í' = 'i',
  'ó' = 'o',
  'ú' = 'u'
)

gta_script_bigrams %>%
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
  rename(weight = n) -> gta_bigram_counts

dim(gta_bigram_counts)
head(gta_bigram_counts, n = 10)

#define a network from bigram frequency (weights)

suppressMessages(suppressWarnings(library(igraph)))

g <- gta_bigram_counts %>%
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

#network using a different threshold

g <- gta_bigram_counts %>%
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

