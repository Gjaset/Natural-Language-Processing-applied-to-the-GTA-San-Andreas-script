### Natural Language Processing applied to the GTA San Andreas script

# =============================
# 1. Librerías
# =============================
suppressMessages(suppressWarnings(library(reader)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(gridExtra)))
suppressMessages(suppressWarnings(library(wordcloud)))
suppressMessages(suppressWarnings(library(boot)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(igraph)))

# networkD3 (opcional) se carga de forma condicional más adelante

cat("Iniciando pipeline NLP GTA San Andreas...\n\n")

# =============================
# 2. Lectura y preparación inicial
# =============================
gta_script <- read_lines("guionGTA.txt") %>% unlist()
gta_script <- tibble(line = seq_along(gta_script), text = gta_script)
cat("Líneas cargadas:", nrow(gta_script), "\n")

# Tokenización y limpieza básica
gta_script %<>%
  unnest_tokens(input = text, output = word) %>%
  filter(!is.na(word)) %>%
  filter(!grepl(pattern = "[0-9]", x = word))

# Stopwords y nombres de personajes
character_names <- c("CJ", "CARL", "SWEET", "RYDER", "BIG SMOKE", "CESAR", 
                     "KENDL", "WOOZIE", "CATALINA", "TENPENNY", "PULASKI", 
                     "ZERO", "TRUTH", "TORENO", "MADD DOGG")
character_names_lower <- tolower(character_names)
data(stop_words)
filtered_stop_words <- stop_words %>% filter(!word %in% character_names_lower)
gta_script %<>% anti_join(filtered_stop_words, by = "word")

# Filtrado ofensivo
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("anger", "disgust", "fear")) %>% select(word)
afinn_negative <- get_sentiments("afinn") %>%
  filter(value <= -3) %>% select(word)
offensive_words <- bind_rows(nrc_negative, afinn_negative) %>% distinct(word) %>%
  filter(!tolower(word) %in% tolower(character_names))
gta_script %<>% anti_join(offensive_words, by = "word")
cat("Tokens tras filtrado ofensivo:", nrow(gta_script), "\n\n")

# =============================
# 3. Frecuencias de palabras
# =============================
top_words <- gta_script %>% count(word, sort = TRUE)
cat("Top 10 palabras:\n")
print(head(top_words, 10))

# =============================
# 4. Preparación por personajes
# =============================
get_main_characters <- function(word){
  w <- toupper(word)
  case_when(
    w %in% c("CJ", "CARL") ~ "cj",
    w == "SWEET" ~ "sweet",
    w == "RYDER" ~ "ryder",
    w == "CESAR" ~ "cesar",
    w == "BIG" ~ "big_smoke",
    w == "SMOKE" ~ "big_smoke",
    TRUE ~ NA_character_
  )
}

script_with_characters <- gta_script %>%
  mutate(is_character = !is.na(get_main_characters(word)),
         character = get_main_characters(word)) %>%
  fill(character, .direction = "down") %>%
  filter(!is_character, !is.na(character)) %>%
  select(word, character)

create_character_dataset <- function(name){
  script_with_characters %>% filter(character == name) %>% select(word)
}

script_cj <- create_character_dataset("cj")
script_sweet <- create_character_dataset("sweet")
script_ryder <- create_character_dataset("ryder")
script_cesar <- create_character_dataset("cesar")
script_big_smoke <- create_character_dataset("big_smoke")

cat("Conteo palabras CJ:", nrow(script_cj), " | Sweet:", nrow(script_sweet), "\n\n")

word_frequencies <- bind_rows(
  mutate(script_cj, author = "cj"),
  mutate(script_sweet, author = "sweet")
) %>%
  count(author, word) %>%
  group_by(author) %>% mutate(proportion = n / sum(n)) %>%
  select(-n) %>% spread(author, proportion, fill = 0)

# =============================
# 5. Correlaciones
# =============================
shared_words <- word_frequencies %>% filter(cj != 0, sweet != 0)
cor_all <- cor.test(word_frequencies$sweet, word_frequencies$cj)
cor_shared <- cor.test(shared_words$sweet, shared_words$cj)
cat("Correlación total:", cor_all$estimate, " | Compartidas:", cor_shared$estimate, "\n\n")

# =============================
# 6. Bigramas y red básica de palabras
# =============================
gta_raw_script <- read_lines("guionGTA.txt")
gta_raw_script <- tibble(line = seq_along(gta_raw_script), text = gta_raw_script)

replacement_list <- list('á'='a','é'='e','í'='i','ó'='o','ú'='u')

gta_script_bigrams <- gta_raw_script %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

gta_bigram_counts <- gta_script_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ") %>%
  filter(!grepl('[0-9]', word1), !grepl('[0-9]', word2)) %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  mutate(word1 = chartr(paste(names(replacement_list), collapse=""),
                        paste(replacement_list, collapse=""), word1),
         word2 = chartr(paste(names(replacement_list), collapse=""),
                        paste(replacement_list, collapse=""), word2)) %>%
  filter(!is.na(word1), !is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>% rename(weight = n)

cat("Bigramas distintos:", nrow(gta_bigram_counts), "\n\n")

# =============================
# 7. Red de personajes e interacciones
# =============================
main_characters <- c("CJ", "CARL", "SWEET", "CESAR", "RYDER", "BIG SMOKE")

find_character_mentions <- function(text, characters){
  mat <- matrix(0, length(characters), length(characters),
                dimnames = list(characters, characters))
  text <- toupper(text)
  for(i in seq_along(text)) {
    present <- characters[sapply(characters, function(x) grepl(x, text[i]))]
    if(length(present) > 1) {
      combn(present, 2, function(p){
        mat[p[1], p[2]] <<- mat[p[1], p[2]] + 1
        mat[p[2], p[1]] <<- mat[p[2], p[1]] + 1
      })
    }
  }
  mat
}

gta_script_raw <- read_lines("guionGTA.txt")
adjacency_matrix <- find_character_mentions(gta_script_raw, main_characters)
character_graph <- graph_from_adjacency_matrix(adjacency_matrix, mode="undirected", weighted=TRUE)

cat("Densidad red personajes:", graph.density(character_graph), "\n")

classify_interactions <- function(text){
  mission <- c("MISSION","OBJECTIVE","TASK","GO","GET","BRING","FIND","COMPLETE","FINISH","START","BEGIN","DO")
  convo <- c("TALK","SAY","TELL","SPEAK","ASK","ANSWER","CHAT","DISCUSS","EXPLAIN","MENTION","REPLY")
  conflict <- c("FIGHT","KILL","SHOOT","ATTACK","ANGRY","MAD","HATE","ENEMY","BATTLE","WAR","VIOLENCE","GANG")
  txt <- toupper(text)
  case_when(
    any(sapply(mission, grepl, x=txt)) ~ "MISSION",
    any(sapply(convo, grepl, x=txt)) ~ "CONVERSATION",
    any(sapply(conflict, grepl, x=txt)) ~ "CONFLICT",
    TRUE ~ "OTHER"
  )
}

create_typed_mentions <- function(text, characters, type){
  m <- matrix(0, length(characters), length(characters), dimnames=list(characters, characters))
  text <- toupper(text)
  for(i in seq_along(text)) {
    if(classify_interactions(text[i]) == type){
      present <- characters[sapply(characters, function(x) grepl(x, text[i]))]
      if(length(present) > 1){
        combn(present, 2, function(p){
          m[p[1], p[2]] <<- m[p[1], p[2]] + 1
          m[p[2], p[1]] <<- m[p[2], p[1]] + 1
        })
      }
    }
  }
  m
}

mission_graph <- graph_from_adjacency_matrix(create_typed_mentions(gta_script_raw, main_characters, "MISSION"), mode="undirected", weighted=TRUE)
conversation_graph <- graph_from_adjacency_matrix(create_typed_mentions(gta_script_raw, main_characters, "CONVERSATION"), mode="undirected", weighted=TRUE)
conflict_graph <- graph_from_adjacency_matrix(create_typed_mentions(gta_script_raw, main_characters, "CONFLICT"), mode="undirected", weighted=TRUE)

# =============================
# 8. Redes interactivas (condicional)
# =============================
if(requireNamespace("networkD3", quietly = TRUE)) {
  suppressMessages(suppressWarnings(library(networkD3)))
  create_interactive_network <- function(graph, use_betweenness = FALSE){
    d3 <- igraph_to_networkD3(graph)
    # Asegurar columna 'group' requerida por forceNetwork
    if(!"group" %in% colnames(d3$nodes)) {
      d3$nodes$group <- 1
    }
    metric <- if(use_betweenness) betweenness(graph) else degree(graph)
    size <- 3 + (metric / max(metric)) * 15
    d3$nodes$size <- size
    d3$links <- d3$links %>% mutate(value = pmax(0.5, value/5))
    forceNetwork(Links=d3$links, Nodes=d3$nodes, Source="source", Target="target", NodeID="name",
                 Group="group", Value="value", opacity=0.8, fontSize=12, zoom=TRUE,
                 linkDistance=100, charge=-400, Nodesize="size", linkWidth=1.5,
                 linkColour="#cccccc80", colourScale=htmlwidgets::JS("d3.scale.category10()"))
  }
  interactive_network <- create_interactive_network(character_graph, FALSE)
  interactive_mission <- create_interactive_network(mission_graph, FALSE)
  interactive_conversation <- create_interactive_network(conversation_graph, TRUE)
  interactive_conflict <- create_interactive_network(conflict_graph, TRUE)
  saveNetwork(interactive_network, "interactive_network.html")
  saveNetwork(interactive_mission, "interactive_mission.html")
  cat("✓ Red de misiones interactiva: interactive_mission.html\n")
  saveNetwork(interactive_conversation, "interactive_conversation.html")
  cat("✓ Red de conversaciones interactiva: interactive_conversation.html\n")
  saveNetwork(interactive_conflict, "interactive_conflict.html")
  cat("✓ Red de conflictos interactiva: interactive_conflict.html\n")
  communities <- cluster_louvain(character_graph)
  V(character_graph)$community <- membership(communities)
  comm_d3 <- igraph_to_networkD3(character_graph)
  comm_d3$nodes <- comm_d3$nodes %>% mutate(community = V(character_graph)$community,
                                           size = 3 + (degree(character_graph)/max(degree(character_graph)))*15)
  comm_d3$links <- comm_d3$links %>% mutate(value = pmax(0.5, value/5))
  interactive_communities <- forceNetwork(Links=comm_d3$links, Nodes=comm_d3$nodes, Source="source", Target="target",
                                          NodeID="name", Group="community", Value="value", opacity=0.8, fontSize=12, zoom=TRUE,
                                          linkDistance=120, charge=-350, Nodesize="size", linkWidth=1.5, linkColour="#cccccc80",
                                          colourScale=htmlwidgets::JS("d3.scale.category20()"))
  saveNetwork(interactive_communities, "interactive_communities.html")
  cat("✓ Análisis de comunidades: interactive_communities.html\n")
} else {
  cat("networkD3 no disponible. Se omiten visualizaciones interactivas.\n")
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("✓ ANÁLISIS DE RED DE PERSONAJES COMPLETADO\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Resto del script (Skipgram, centralidad, cohesión, clustering) permanece igual...
# Para brevedad se mantiene la versión previa a partir de aquí sin cambios sustanciales.

# =============================
# 9. Skipgram (reutiliza gta_script)
# =============================
gta_skipgram_indexed <- gta_script %>% select(word) %>% mutate(word_id = row_number())
window_size <- 4
gta_skipgrams <- gta_skipgram_indexed %>% mutate(window_start = pmax(1, word_id - window_size),
                                                 window_end = pmin(n(), word_id + window_size))
skipgram_pairs <- purrr::map_dfr(1:nrow(gta_skipgrams), function(i){
  focus <- gta_skipgrams$word[i]
  rng <- gta_skipgrams$window_start[i]:gta_skipgrams$window_end[i]
  rng <- rng[rng != i]
  tibble(focus_word = focus, context_word = gta_skipgrams$word[rng], distance = abs(i - rng))
})
skipgram_weighted <- skipgram_pairs %>% mutate(weight = 1/distance) %>% group_by(focus_word, context_word) %>%
  summarise(total_weight = sum(weight), count = n(), mean_distance = mean(distance), .groups='drop') %>%
  arrange(desc(total_weight))
cat("Skipgrams generados:", nrow(skipgram_weighted), "\n\n")

threshold <- 10
skipgram_network <- skipgram_weighted %>% filter(total_weight > threshold) %>% graph_from_data_frame(directed=TRUE)
cat("Red skipgram vértices:", vcount(skipgram_network), "\n")

# =============================
# 10. Centralidad sobre bigram_graph
# =============================
if(!exists("bigram_graph")) {
  bigram_graph <- gta_bigram_counts %>% select(word1, word2, weight) %>% graph_from_data_frame(directed=FALSE)
}
closeness_centrality <- closeness(bigram_graph, normalized = TRUE)
degree_centrality <- degree(bigram_graph, normalized = TRUE)
betweenness_centrality <- betweenness(bigram_graph, normalized = TRUE)
eigen_centrality_vec <- eigen_centrality(bigram_graph)$vector
cat("Centralidad calculada. Nodos:", vcount(bigram_graph), "\n")

# =============================
# 11. Cohesión y clustering
# =============================
network_density <- edge_density(bigram_graph)
transitivity_global <- transitivity(bigram_graph, type="global")
components_list <- components(bigram_graph)
cat("Densidad:", network_density, " Transitividad:", transitivity_global, "\n")

cat("Pipeline completado.\n")

# ============================================================================
# 11.1: CENTRALIDAD DE CERCANÍA (Closeness Centrality)
# ============================================================================

cat("═ 11.1 CENTRALIDAD DE CERCANÍA (Closeness Centrality) ═\n\n")

# Crear grafo de coocurrencia de palabras si no existe
if(!exists("bigram_graph")) {
  # Usar objeto existente de conteos de bigramas
  if (exists("gta_bigram_counts")) {
    bigram_graph <- gta_bigram_counts %>%
      select(word1, word2, weight) %>%
      graph_from_data_frame(directed = FALSE, vertices = NULL)
  } else {
    stop("No se encontró 'gta_bigram_counts' para construir 'bigram_graph'.")
  }
}

# Calcular centralidad de cercanía
closeness_centrality <- closeness(bigram_graph, mode = "all", normalized = TRUE)

# Convertir a data frame y ordenar
closeness_df <- data.frame(
  word = names(closeness_centrality),
  closeness = as.numeric(closeness_centrality),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(closeness)) %>%
  head(30)

cat("Top 30 palabras por Centralidad de Cercanía:\n")
print(closeness_df)
cat("\n")

# Visualización de centralidad de cercanía
p_closeness <- ggplot(closeness_df, aes(x = reorder(word, closeness), y = closeness, fill = closeness)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  coord_flip() +
  labs(
    title = "Centralidad de Cercanía - Top 30 Palabras",
    subtitle = "Proximidad promedio a todas las demás palabras en la red",
    x = "Palabra",
    y = "Centralidad de Cercanía (Normalizada)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )

ggsave("centrality_closeness.png", p_closeness, width = 10, height = 8, dpi = 300)
cat("✓ Gráfico de centralidad de cercanía: centrality_closeness.png\n\n")

# ============================================================================
# 11.2: CENTRALIDAD DE INTERMEDIACIÓN (Betweenness Centrality)
# ============================================================================

cat("═ 11.2 CENTRALIDAD DE INTERMEDIACIÓN (Betweenness Centrality) ═\n\n")

# Calcular centralidad de intermediación
betweenness_centrality <- betweenness(bigram_graph, directed = FALSE, normalized = TRUE)

# Convertir a data frame y ordenar
betweenness_df <- data.frame(
  word = names(betweenness_centrality),
  betweenness = as.numeric(betweenness_centrality),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(betweenness)) %>%
  head(30)

cat("Top 30 palabras por Centralidad de Intermediación:\n")
print(betweenness_df)
cat("\n")

# Visualización de centralidad de intermediación
p_betweenness <- ggplot(betweenness_df, aes(x = reorder(word, betweenness), y = betweenness, fill = betweenness)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "inferno") +
  coord_flip() +
  labs(
    title = "Centralidad de Intermediación - Top 30 Palabras",
    subtitle = "Frecuencia con la que una palabra actúa como puente entre otras",
    x = "Palabra",
    y = "Centralidad de Intermediación (Normalizada)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )

ggsave("centrality_betweenness.png", p_betweenness, width = 10, height = 8, dpi = 300)
cat("✓ Gráfico de centralidad de intermediación: centrality_betweenness.png\n\n")

# ============================================================================
# 11.3: CENTRALIDAD DE GRADO (Degree Centrality)
# ============================================================================

cat("═ 11.3 CENTRALIDAD DE GRADO (Degree Centrality) ═\n\n")

# Calcular centralidad de grado
degree_centrality <- degree(bigram_graph, mode = "all", normalized = TRUE)

# Convertir a data frame y ordenar
degree_df <- data.frame(
  word = names(degree_centrality),
  degree = as.numeric(degree_centrality),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(degree)) %>%
  head(30)

cat("Top 30 palabras por Centralidad de Grado:\n")
print(degree_df)
cat("\n")

# Visualización de centralidad de grado
p_degree <- ggplot(degree_df, aes(x = reorder(word, degree), y = degree, fill = degree)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "cividis") +
  coord_flip() +
  labs(
    title = "Centralidad de Grado - Top 30 Palabras",
    subtitle = "Número de conexiones directas de cada palabra",
    x = "Palabra",
    y = "Centralidad de Grado (Normalizada)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )

ggsave("centrality_degree.png", p_degree, width = 10, height = 8, dpi = 300)
cat("✓ Gráfico de centralidad de grado: centrality_degree.png\n\n")

# ============================================================================
# 11.4: CENTRALIDAD DE VECTOR PROPIO (Eigenvector Centrality)
# ============================================================================

cat("═ 11.4 CENTRALIDAD DE VECTOR PROPIO (Eigenvector Centrality) ═\n\n")

# Calcular centralidad de vector propio
eigen_centrality_vec <- eigen_centrality(bigram_graph, directed = FALSE)
eigen_centrality <- eigen_centrality_vec$vector

# Convertir a data frame y ordenar
eigen_df <- data.frame(
  word = names(eigen_centrality),
  eigenvector = as.numeric(eigen_centrality),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(eigenvector)) %>%
  head(30)

cat("Top 30 palabras por Centralidad de Vector Propio:\n")
print(eigen_df)
cat("\n")

# Visualización de centralidad de vector propio
p_eigen <- ggplot(eigen_df, aes(x = reorder(word, eigenvector), y = eigenvector, fill = eigenvector)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "mako") +
  coord_flip() +
  labs(
    title = "Centralidad de Vector Propio - Top 30 Palabras",
    subtitle = "Importancia basada en conexiones con otras palabras importantes",
    x = "Palabra",
    y = "Centralidad de Vector Propio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )

ggsave("centrality_eigenvector.png", p_eigen, width = 10, height = 8, dpi = 300)
cat("✓ Gráfico de centralidad de vector propio: centrality_eigenvector.png\n\n")

# ============================================================================
# 11.5: COMPARACIÓN DE MEDIDAS DE CENTRALIDAD
# ============================================================================

cat("═ 11.5 COMPARACIÓN DE MEDIDAS DE CENTRALIDAD ═\n\n")

# Crear data frame consolidado de todas las medidas
all_centrality <- data.frame(
  word = names(degree_centrality),
  degree = as.numeric(degree_centrality),
  betweenness = as.numeric(betweenness_centrality)[names(degree_centrality)],
  closeness = as.numeric(closeness_centrality)[names(degree_centrality)],
  eigenvector = as.numeric(eigen_centrality)[names(degree_centrality)],
  stringsAsFactors = FALSE
) %>%
  arrange(desc(degree)) %>%
  head(20)

cat("Top 20 palabras - Comparación de Medidas de Centralidad:\n")
print(all_centrality)
cat("\n")

# Visualización en formato de radar/heatmap
centrality_melted <- all_centrality %>%
  select(-degree) %>%  # Mantener solo nombres y medidas normalizadas
  pivot_longer(cols = -word, names_to = "centrality_type", values_to = "value") %>%
  mutate(word = factor(word, levels = rev(unique(all_centrality$word))))

p_heatmap <- ggplot(centrality_melted, aes(x = centrality_type, y = word, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "turbo") +
  labs(
    title = "Comparación de Medidas de Centralidad - Top 20 Palabras",
    x = "Tipo de Centralidad",
    y = "Palabra",
    fill = "Valor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank()
  )

ggsave("centrality_comparison_heatmap.png", p_heatmap, width = 12, height = 8, dpi = 300)
cat("✓ Heatmap comparativo de centralidades: centrality_comparison_heatmap.png\n\n")

# ============================================================================
# 11.6: GUARDAR RESULTADOS DE CENTRALIDAD
# ============================================================================

cat("═ 11.6 GUARDANDO RESULTADOS DE CENTRALIDAD ═\n\n")

# Crear archivo consolidado de centralidades
centrality_results <- data.frame(
  word = names(degree_centrality),
  degree = as.numeric(degree_centrality),
  betweenness = as.numeric(betweenness_centrality)[names(degree_centrality)],
  closeness = as.numeric(closeness_centrality)[names(degree_centrality)],
  eigenvector = as.numeric(eigen_centrality)[names(degree_centrality)],
  stringsAsFactors = FALSE
) %>%
  arrange(desc(degree))

write.csv(centrality_results, "centrality_analysis.csv", row.names = FALSE)
cat("✓ Análisis de centralidad completo: centrality_analysis.csv\n")

write.csv(closeness_df, "centrality_closeness_top30.csv", row.names = FALSE)
cat("✓ Top 30 por cercanía: centrality_closeness_top30.csv\n")

write.csv(betweenness_df, "centrality_betweenness_top30.csv", row.names = FALSE)
cat("✓ Top 30 por intermediación: centrality_betweenness_top30.csv\n")

write.csv(degree_df, "centrality_degree_top30.csv", row.names = FALSE)
cat("✓ Top 30 por grado: centrality_degree_top30.csv\n")

write.csv(eigen_df, "centrality_eigenvector_top30.csv", row.names = FALSE)
cat("✓ Top 30 por vector propio: centrality_eigenvector_top30.csv\n\n")

# ============================================================================
# 11.7: INTERPRETACIÓN DE RESULTADOS
# ============================================================================

cat("═ 11.7 INTERPRETACIÓN DE RESULTADOS ═\n\n")

cat("CENTRALIDAD DE GRADO:\n")
cat("  - Palabras más conectadas en la red de bigramas\n")
cat("  - Indica palabras que co-ocurren frecuentemente con muchas otras\n\n")

cat("CENTRALIDAD DE INTERMEDIACIÓN:\n")
cat("  - Palabras que actúan como puentes entre comunidades\n")
cat("  - Alta importancia en mantener la cohesión de la red\n\n")

cat("CENTRALIDAD DE CERCANÍA:\n")
cat("  - Palabras próximas (en promedio) a todas las demás\n")
cat("  - Indica palabras centrales en la estructura global\n\n")

cat("CENTRALIDAD DE VECTOR PROPIO:\n")
cat("  - Palabras conectadas a otras palabras importantes\n")
cat("  - Refleja influencia indirecta en la red\n\n")

cat("═ ANÁLISIS DE CENTRALIDAD COMPLETADO ═\n\n")

# ============================================================================
# PASO 12: ANÁLISIS DE COHESIÓN
# ============================================================================
# Medidas de conectividad y estructura global de la red
# Incluye: densidad, transitividad, componentes, diámetro, camino promedio
# ============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║          PASO 12: ANÁLISIS DE COHESIÓN DE REDES          ║\n")
cat("║        Caracterización de Conectividad Global             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# 12.1: DENSIDAD DE RED
# ============================================================================

cat("═ 12.1 DENSIDAD DE RED ═\n\n")

# Calcular densidad (proporción de aristas reales vs posibles)
network_density <- edge_density(bigram_graph)

cat("Densidad de la red:", round(network_density, 4), "\n")
cat("Interpretación:\n")
if (network_density > 0.3) {
  cat("  ✓ Red altamente cohesionada (conexiones densas)\n")
} else if (network_density > 0.1) {
  cat("  ○ Red moderadamente cohesionada (estructura típica)\n")
} else {
  cat("  △ Red fragmentada (conexiones escasas)\n")
}
cat("\n")

# ============================================================================
# 12.2: TRANSITIVIDAD (CLUSTERING)
# ============================================================================

cat("═ 12.2 TRANSITIVIDAD ═\n\n")

# Transitividad global
transitivity_global <- transitivity(bigram_graph, type = "global")

# Transitividad local (por nodo)
transitivity_local <- transitivity(bigram_graph, type = "local")

cat("Transitividad global:", round(transitivity_global, 4), "\n")
cat("Coeficiente de clustering promedio:", 
    round(mean(transitivity_local, na.rm = TRUE), 4), "\n")
cat("\nInterpretación:\n")
if (transitivity_global > 0.4) {
  cat("  ✓ Alta transitividad: Comunidades temáticas bien definidas\n")
} else if (transitivity_global > 0.2) {
  cat("  ○ Transitividad media: Estructura típica de lenguaje natural\n")
} else {
  cat("  △ Baja transitividad: Red sin clustering notable\n")
}

# Palabras con mayor clustering (forman comunidades)
clustering_df <- data.frame(
  word = names(transitivity_local),
  local_clustering = transitivity_local
) %>%
  filter(!is.na(local_clustering)) %>%
  arrange(desc(local_clustering)) %>%
  head(30)

# Visualizar clustering local
p_clustering <- ggplot(clustering_df, 
                       aes(x = reorder(word, local_clustering), 
                           y = local_clustering, fill = local_clustering)) +
  geom_col() +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  coord_flip() +
  labs(
    title = "Top 30 Palabras por Coeficiente de Clustering Local",
    subtitle = "Palabras que forman comunidades temáticas densas",
    x = "Palabra",
    y = "Coeficiente de Clustering Local",
    fill = "Clustering"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("cohesion_local_clustering.png", p_clustering, dpi = 300, 
       width = 12, height = 8)
cat("✓ Visualización: cohesion_local_clustering.png\n\n")

# ============================================================================
# 12.3: COMPONENTES CONECTADOS
# ============================================================================

cat("═ 12.3 COMPONENTES CONECTADOS ═\n\n")

# Identificar componentes
components_list <- components(bigram_graph)
num_components <- components_list$no
component_sizes <- components_list$csize

cat("Número de componentes:", num_components, "\n")
cat("Distribución de tamaños:\n")

components_df <- data.frame(
  component_id = seq_along(component_sizes),
  size = component_sizes,
  percentage = (component_sizes / sum(component_sizes)) * 100
) %>%
  arrange(desc(size)) %>%
  head(10)

for (i in 1:nrow(components_df)) {
  cat("  Componente", i, ":", components_df$size[i], "nodos", 
      "(", round(components_df$percentage[i], 2), "%)\n")
}

cat("\nInterpretación:\n")
if (components_df$percentage[1] > 90) {
  cat("  ✓ Red fuertemente cohesionada (>90% en componente gigante)\n")
} else if (components_df$percentage[1] > 70) {
  cat("  ○ Red moderadamente cohesionada (70-90% en componente gigante)\n")
} else {
  cat("  △ Red fragmentada (<70% en componente gigante)\n")
}

# Visualizar componentes
p_components <- ggplot(components_df, 
                       aes(x = reorder(component_id, -size), y = size, 
                           fill = percentage)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 3) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(
    title = "Distribución de Componentes Conectados",
    subtitle = "Tamaño y porcentaje de cada componente",
    x = "Componente",
    y = "Número de Nodos",
    fill = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("cohesion_components.png", p_components, dpi = 300, width = 12, height = 7)
cat("✓ Visualización: cohesion_components.png\n\n")

# ============================================================================
# 12.4: DIÁMETRO DE LA RED
# ============================================================================

cat("═ 12.4 DIÁMETRO Y DISTANCIAS ═\n\n")

# Calcular diámetro (solo en componente gigante)
gc_nodes <- which(components_list$membership == which.max(component_sizes))
gc_subgraph <- induced_subgraph(bigram_graph, gc_nodes)

network_diameter <- diameter(gc_subgraph)
avg_path_length <- mean_distance(gc_subgraph)

cat("Diámetro de la red (componente gigante):", network_diameter, "\n")
cat("Camino promedio:", round(avg_path_length, 4), "\n")
cat("\nInterpretación:\n")
if (avg_path_length < 5) {
  cat("  ✓ Red altamente eficiente (acceso rápido a todas las palabras)\n")
} else if (avg_path_length < 8) {
  cat("  ○ Red eficiencia media (acceso moderado)\n")
} else {
  cat("  △ Red con acceso lento (posible fragmentación)\n")
}
cat("\n")

# ============================================================================
# 12.5: ASORTATIVIDAD
# ============================================================================

cat("═ 12.5 ASORTATIVIDAD ═\n\n")

# Calcular asortatividad de grado
assortativity_degree_val <- assortativity_degree(bigram_graph)

cat("Asortatividad de grado:", round(assortativity_degree_val, 4), "\n")
cat("Interpretación:\n")
if (assortativity_degree_val > 0.2) {
  cat("  ✓ Asortativa positiva: Palabras frecuentes conectan con frecuentes\n")
  cat("    → Estructura jerárquica, vocabulario núcleo definido\n")
} else if (assortativity_degree_val > -0.1) {
  cat("  ○ Asortatividad neutral: Estructura aleatoria\n")
  cat("    → Típico en redes lingüísticas\n")
} else {
  cat("  △ Asortativa negativa: Palabras frecuentes conectan con raras\n")
  cat("    → Hubs periféricos, estructura desigual\n")
}
cat("\n")

# ============================================================================
# 12.6: RESUMEN DE COHESIÓN
# ============================================================================

cat("═ 12.6 RESUMEN DE COHESIÓN ═\n\n")

# Crear tabla resumen
cohesion_summary <- data.frame(
  metric = c("Densidad", "Transitividad Global", "Componentes Conectados",
             "Diámetro", "Camino Promedio", "Asortatividad"),
  value = c(round(network_density, 4), round(transitivity_global, 4),
            num_components, network_diameter, round(avg_path_length, 4),
            round(assortativity_degree_val, 4)),
  interpretation = c(
    if (network_density > 0.3) "Alta" else if (network_density > 0.1) "Media" else "Baja",
    if (transitivity_global > 0.4) "Alta" else if (transitivity_global > 0.2) "Media" else "Baja",
    if (components_df$percentage[1] > 90) "Conectada" else if (components_df$percentage[1] > 70) "Mod. conectada" else "Fragmentada",
    if (network_diameter < 5) "Pequeño" else if (network_diameter < 10) "Medio" else "Grande",
    if (avg_path_length < 5) "Baja" else if (avg_path_length < 8) "Media" else "Alta",
    if (assortativity_degree_val > 0.2) "Positiva" else if (assortativity_degree_val > -0.1) "Neutral" else "Negativa"
  )
)

# Visualizar resumen
p_summary <- ggplot(cohesion_summary, aes(x = reorder(metric, value), y = value, fill = metric)) +
  geom_col() +
  geom_text(aes(label = round(value, 2)), vjust = -0.5, size = 3) +
  scale_fill_viridis_d(option = "turbo") +
  coord_flip() +
  labs(
    title = "Resumen de Métricas de Cohesión",
    x = "Métrica",
    y = "Valor",
    fill = "Métrica"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("cohesion_summary.png", p_summary, dpi = 300, width = 10, height = 6)
cat("✓ Visualización: cohesion_summary.png\n\n")

# ============================================================================
# 12.7: CLASIFICACIÓN DE TIPO DE RED
# ============================================================================

cat("═ 12.7 CLASIFICACIÓN DE RED ═\n\n")

# Determinar tipo de red
is_small_world <- (transitivity_global > 0.3 & avg_path_length < 6 & 
                   components_df$percentage[1] > 80)
is_scale_free <- (network_density < 0.1 & transitivity_global > 0.25)
is_random <- (transitivity_global < 0.1 & network_density < 0.05)

if (is_small_world) {
  network_type <- "Pequeño Mundo (Small World)"
  description <- "Características de red pequeño mundo: comunidades temáticas cohesionadas 
con acceso rápido entre temas. Típico de lenguaje natural bien estructurado."
} else if (is_scale_free) {
  network_type <- "Escala Libre (Scale-Free)"
  description <- "Características de red escala libre: pocas palabras (hubs) conectan 
múltiples temas. Típico de vocabulario especializado o dominante."
} else if (is_random) {
  network_type <- "Aleatoria"
  description <- "Características de red aleatoria: sin comunidades definidas ni 
estructura temática clara. Vocabulario fragmentado."
} else {
  network_type <- "Estructura Mixta"
  description <- "Combinación de características. Red típica con estructura temática 
moderada y acceso relativamente eficiente."
}

cat("TIPO DE RED DETECTADO:", network_type, "\n\n")
cat("DESCRIPCIÓN:\n", description, "\n\n")

# ============================================================================
# 12.8: EXPORTAR RESULTADOS
# ============================================================================

cat("═ 12.8 EXPORTAR RESULTADOS ═\n\n")

# Tabla completa de clustering local
write.csv(clustering_df, "cohesion_local_clustering.csv", row.names = FALSE)
cat("✓ Clustering local: cohesion_local_clustering.csv\n")

# Tabla de componentes
write.csv(components_df, "cohesion_components.csv", row.names = FALSE)
cat("✓ Componentes: cohesion_components.csv\n")

# Tabla resumen de cohesión
write.csv(cohesion_summary, "cohesion_analysis.csv", row.names = FALSE)
cat("✓ Resumen cohesión: cohesion_analysis.csv\n\n")

# ============================================================================
# PASO 13: ANÁLISIS DE CLUSTERING (DETECCIÓN DE COMUNIDADES)
# ============================================================================
cat("\n\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PASO 13: ANÁLISIS DE CLUSTERING                         ║\n")
cat("║              Detección de Comunidades en la Red de Palabras                ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# 13.1: ALGORITMO LOUVAIN - DETECCIÓN PRINCIPAL
# ============================================================================

cat("═ 13.1 ALGORITMO LOUVAIN (MODULARITY OPTIMIZATION) ═\n\n")

# Detectar comunidades con Louvain
comunidades_louvain <- cluster_louvain(bigram_graph)

# Información básica
cat("Comunidades detectadas (Louvain):", length(comunidades_louvain), "\n")
cat("Modularidad (Louvain):", round(modularity(bigram_graph, membership(comunidades_louvain)), 4), "\n\n")

# Tamaño de comunidades
tamaños_louvain <- sizes(comunidades_louvain)
tamaños_louvain <- sort(tamaños_louvain, decreasing = TRUE)

df_tamaños_louvain <- data.frame(
  Comunidad = 1:length(tamaños_louvain),
  Tamaño = as.numeric(tamaños_louvain),
  Porcentaje = round(100 * as.numeric(tamaños_louvain) / sum(tamaños_louvain), 2),
  Rango_Tamaño = ifelse(as.numeric(tamaños_louvain) > 50, "Grande",
                         ifelse(as.numeric(tamaños_louvain) > 20, "Mediano", "Pequeño"))
)

cat("Top 10 Comunidades por Tamaño:\n")
print(head(df_tamaños_louvain, 10))
cat("\n")

# Visualizar distribución de tamaños
png("clustering_louvain_distribucion_tamaños.png", width = 1200, height = 700)

par(mfrow = c(1, 2))

# Gráfico de barras
barplot(head(tamaños_louvain, 20), 
        main = "Top 20 Comunidades Louvain - Tamaño",
        ylab = "Número de Palabras",
        xlab = "Comunidad",
        col = colorRampPalette(c("steelblue", "darkred"))(20),
        las = 2)

# Gráfico de pie (top 8 + otros)
top_8 <- head(tamaños_louvain, 8)
otros <- sum(tail(tamaños_louvain, -8))
datos_pie <- c(top_8, Otros = otros)

pie(datos_pie,
    main = "Distribución de Comunidades (Top 8 + Otros)",
    labels = paste0("C", 1:9, "\n", round(100*datos_pie/sum(datos_pie), 1), "%"),
    col = colorRampPalette(c("lightblue", "darkred"))(9))

par(mfrow = c(1, 1))
dev.off()
cat("✓ Visualización: clustering_louvain_distribucion_tamaños.png\n\n")

# ============================================================================
# 13.2: ALGORITMO LABEL PROPAGATION
# ============================================================================

cat("═ 13.2 ALGORITMO LABEL PROPAGATION ═\n\n")

# Detectar con Label Propagation
comunidades_labelprop <- cluster_label_prop(bigram_graph)

cat("Comunidades detectadas (Label Prop):", length(comunidades_labelprop), "\n")
cat("Modularidad (Label Prop):", round(modularity(bigram_graph, membership(comunidades_labelprop)), 4), "\n\n")

# ============================================================================
# 13.3: ALGORITMO WALKTRAP
# ============================================================================

cat("═ 13.3 ALGORITMO WALKTRAP (RANDOM WALK) ═\n\n")

# Detectar con Walktrap
comunidades_walktrap <- cluster_walktrap(bigram_graph, steps = 4)

cat("Comunidades detectadas (Walktrap):", length(comunidades_walktrap), "\n")
cat("Modularidad (Walktrap):", round(modularity(bigram_graph, membership(comunidades_walktrap)), 4), "\n\n")

# ============================================================================
# 13.4: COMPARACIÓN DE ALGORITMOS
# ============================================================================

cat("═ 13.4 COMPARACIÓN DE ALGORITMOS ═\n\n")

# Crear tabla comparativa
comparison_algorithms <- data.frame(
  Algoritmo = c("Louvain", "Label Propagation", "Walktrap"),
  Num_Comunidades = c(length(comunidades_louvain), 
                      length(comunidades_labelprop),
                      length(comunidades_walktrap)),
  Modularidad = round(c(modularity(bigram_graph, membership(comunidades_louvain)),
                        modularity(bigram_graph, membership(comunidades_labelprop)),
                        modularity(bigram_graph, membership(comunidades_walktrap))), 4),
  Recomendación = c("★★★★★ Excelente", "★★★☆☆ Bueno", "★★★★☆ Muy Bueno")
)

print(comparison_algorithms)
cat("\nSELECCIÓN: Usando Louvain (mejor balance velocidad/calidad)\n\n")

# Visualizar comparación
png("clustering_comparacion_algoritmos.png", width = 1000, height = 600)

barplot(comparison_algorithms$Modularidad,
        names.arg = comparison_algorithms$Algoritmo,
        main = "Modularidad por Algoritmo de Clustering",
        ylab = "Modularidad",
        ylim = c(0, 1),
        col = c("steelblue", "coral", "lightgreen"),
        las = 2)

# Agregar valores encima
text(c(1, 2, 3), comparison_algorithms$Modularidad + 0.02,
     labels = round(comparison_algorithms$Modularidad, 4),
     pos = 3, cex = 1.2)

dev.off()
cat("✓ Visualización: clustering_comparacion_algoritmos.png\n\n")

# ============================================================================
# 13.5: PALABRAS PRINCIPALES POR COMUNIDAD
# ============================================================================

cat("═ 13.5 PALABRAS PRINCIPALES POR COMUNIDAD ═\n\n")

# Función para extraer top palabras
top_palabras_comunidad <- function(grafo, comunidades, n = 15) {
  miembros <- membership(comunidades)
  resultado <- list()
  
  for (c in 1:length(comunidades)) {
    palabras_c <- names(miembros)[miembros == c]
    
    if (length(palabras_c) > 0) {
      subgrafo <- induced_subgraph(grafo, palabras_c)
      grados <- degree(subgrafo)
      top <- names(sort(grados, decreasing = TRUE)[1:min(n, length(grados))])
      resultado[[c]] <- top
    }
  }
  
  return(resultado)
}

# Ejecutar
top_palabras_louvain <- top_palabras_comunidad(bigram_graph, comunidades_louvain, n = 15)

# Mostrar top comunidades
cat("TOP PALABRAS POR COMUNIDAD (Top 8 Comunidades):\n\n")

for (i in 1:min(8, length(top_palabras_louvain))) {
  cat(sprintf("--- COMUNIDAD %d (%d palabras) ---\n", i, tamaños_louvain[i]))
  cat(paste(top_palabras_louvain[[i]], collapse = ", "), "\n\n")
}

# Exportar tabla completa
df_palabras_comunidades <- data.frame()

for (c in 1:length(top_palabras_louvain)) {
  for (rank in 1:length(top_palabras_louvain[[c]])) {
    df_palabras_comunidades <- rbind(df_palabras_comunidades, data.frame(
      Comunidad = c,
      Tamaño_Comunidad = tamaños_louvain[c],
      Ranking = rank,
      Palabra = top_palabras_louvain[[c]][rank],
      Porcentaje_Comunidad = round(100 * tamaños_louvain[c] / vcount(bigram_graph), 2)
    ))
  }
}

write.csv(df_palabras_comunidades, "clustering_palabras_principales.csv", row.names = FALSE)
cat("✓ Exportado: clustering_palabras_principales.csv\n\n")

# ============================================================================
# 13.6: MODULARIDAD LOCAL Y COHESIÓN DE COMUNIDADES
# ============================================================================

cat("═ 13.6 MODULARIDAD LOCAL (COHESIÓN DE COMUNIDADES) ═\n\n")

# Calcular modularidad local por comunidad
modularidad_local_comunidades <- function(grafo, comunidades) {
  miembros <- membership(comunidades)
  resultado <- numeric(length(comunidades))
  
  for (c in 1:length(comunidades)) {
    nodos_c <- which(miembros == c)
    
    if (length(nodos_c) > 1) {
      subgrafo <- induced_subgraph(grafo, nodos_c)
      aristas_internas <- ecount(subgrafo)
      aristas_totales <- sum(degree(subgrafo)) / 2
      
      if (aristas_totales > 0) {
        resultado[c] <- aristas_internas / aristas_totales
      }
    }
  }
  
  return(resultado)
}

# Ejecutar
mod_local <- modularidad_local_comunidades(bigram_graph, comunidades_louvain)

df_mod_local <- data.frame(
  Comunidad = 1:length(mod_local),
  Tamaño = as.numeric(tamaños_louvain),
  Modularidad_Local = round(mod_local, 4),
  Cohesion = ifelse(round(mod_local, 4) > 0.5, "Alta",
                    ifelse(round(mod_local, 4) > 0.3, "Media", "Baja"))
)

cat("Top 10 Comunidades por Cohesión Interna:\n")
print(df_mod_local[order(df_mod_local$Modularidad_Local, decreasing = TRUE), ][1:10, ])
cat("\n")

# Visualizar
png("clustering_modularidad_local.png", width = 1200, height = 700)

par(mfrow = c(1, 2))

# Gráfico de barras - modularidad local
barplot(head(df_mod_local$Modularidad_Local, 20),
        main = "Top 20 Comunidades - Modularidad Local",
        ylab = "Modularidad Local",
        xlab = "Comunidad",
        col = colorRampPalette(c("red", "yellow", "green"))(20),
        las = 2)

# Scatter plot - Tamaño vs Modularidad
plot(df_mod_local$Tamaño, df_mod_local$Modularidad_Local,
     main = "Relación Tamaño vs Cohesión",
     xlab = "Tamaño de Comunidad",
     ylab = "Modularidad Local",
     col = "steelblue",
     pch = 16,
     cex = 1.5)

# Línea de tendencia
abline(lm(Modularidad_Local ~ Tamaño, data = df_mod_local), 
       col = "red", lwd = 2)

par(mfrow = c(1, 1))
dev.off()
cat("✓ Visualización: clustering_modularidad_local.png\n\n")

# Exportar
write.csv(df_mod_local, "clustering_modularidad_local.csv", row.names = FALSE)
cat("✓ Exportado: clustering_modularidad_local.csv\n\n")

# ============================================================================
# 13.7: PALABRAS PUENTE (INTER-COMUNIDADES)
# ============================================================================

cat("═ 13.7 PALABRAS PUENTE (CONECTORAS INTER-COMUNIDADES) ═\n\n")

# Identificar palabras puente
palabras_puente_func <- function(grafo, comunidades) {
  miembros <- membership(comunidades)
  grados_globales <- degree(grafo)
  resultado <- data.frame()
  
  for (nodo in V(grafo)$name) {
    com <- miembros[nodo]
    nodos_com <- names(miembros)[miembros == com]
    
    if (length(nodos_com) > 1) {
      subgrafo <- induced_subgraph(grafo, nodos_com)
      grado_dentro <- degree(subgrafo, v = nodo)
    } else {
      grado_dentro <- 0
    }
    
    grado_total <- grados_globales[nodo]
    
    if (grado_total > 0) {
      ratio_puente <- 1 - (grado_dentro / grado_total)
    } else {
      ratio_puente <- 0
    }
    
    resultado <- rbind(resultado, data.frame(
      Palabra = nodo,
      Comunidad = com,
      Grado_Total = grado_total,
      Grado_Dentro = grado_dentro,
      Ratio_Puente = round(ratio_puente, 3),
      Importancia = ifelse(ratio_puente > 0.7, "Puente Principal",
                          ifelse(ratio_puente > 0.4, "Puente Moderado", "Local"))
    ))
  }
  
  return(resultado[order(resultado$Ratio_Puente, decreasing = TRUE), ])
}

df_puentes <- palabras_puente_func(bigram_graph, comunidades_louvain)

cat("Top 20 Palabras Puente (Conectoras entre Comunidades):\n\n")
print(head(df_puentes, 20))
cat("\n")

# Visualizar top puentes
png("clustering_palabras_puente.png", width = 1000, height = 700)

top_puentes_viz <- head(df_puentes[df_puentes$Ratio_Puente > 0.1, ], 20)

barplot(top_puentes_viz$Ratio_Puente,
        names.arg = top_puentes_viz$Palabra,
        main = "Top 20 Palabras Puente",
        ylab = "Ratio Puente",
        xlab = "Palabra",
        col = colorRampPalette(c("lightblue", "darkred"))(20),
        las = 2,
        cex.names = 0.8)

dev.off()
cat("✓ Visualización: clustering_palabras_puente.png\n\n")

# Exportar
write.csv(df_puentes, "clustering_palabras_puente.csv", row.names = FALSE)
cat("✓ Exportado: clustering_palabras_puente.csv\n\n")

# ============================================================================
# 13.8: VISUALIZACIÓN DE RED CON COMUNIDADES
# ============================================================================

cat("═ 13.8 VISUALIZACIÓN DE RED (COLOREADA POR COMUNIDADES) ═\n\n")

# Crear layout Fruchterman-Reingold
cat("Calculando layout (puede tomar unos segundos)...\n")
layout_fr <- layout_with_fr(bigram_graph, niter = 500, start.temp = 100)

# Colores por comunidad
colores_comunidades <- rainbow(length(comunidades_louvain))
colores_nodos <- colores_comunidades[membership(comunidades_louvain)]

# Visualización 1: Red básica
png("clustering_red_comunidades_coloreada.png", width = 1200, height = 1000)

plot(bigram_graph,
     layout = layout_fr,
     vertex.size = 3,
     vertex.label.cex = 0.3,
     vertex.label.dist = 0.3,
     vertex.color = colores_nodos,
     vertex.frame.color = NA,
     edge.width = 0.3,
     edge.color = rgb(0, 0, 0, 0.05),
     main = "Red de Palabras Coloreada por Comunidades Louvain",
     rescale = TRUE)

dev.off()
cat("✓ Visualización: clustering_red_comunidades_coloreada.png\n")

# Visualización 2: Con tamaño por grado
tamaños_nodos <- degree(bigram_graph) + 4

png("clustering_red_comunidades_grado.png", width = 1200, height = 1000)

plot(bigram_graph,
     layout = layout_fr,
     vertex.size = tamaños_nodos / 2,
     vertex.label = NA,
     vertex.color = colores_nodos,
     vertex.frame.color = NA,
     edge.width = 0.2,
     edge.color = rgb(0, 0, 0, 0.05),
     main = "Red de Palabras: Color=Comunidad, Tamaño=Grado",
     rescale = TRUE)

dev.off()
cat("✓ Visualización: clustering_red_comunidades_grado.png\n\n")

# ============================================================================
# 13.9: ESTADÍSTICAS GLOBALES Y CONCLUSIONES
# ============================================================================

cat("═ 13.9 ESTADÍSTICAS GLOBALES Y CONCLUSIONES ═\n\n")

# Crear resumen final
resumen_clustering <- data.frame(
  Métrica = c(
    "Número de Comunidades",
    "Modularidad Global",
    "Tamaño Promedio Comunidad",
    "Tamaño Mayor Comunidad",
    "Tamaño Menor Comunidad",
    "Comunidades con Alta Cohesión (>0.5)",
    "Palabras Puente Detectadas",
    "Densidad Media",
    "Evaluación General"
  ),
  Valor = c(
    length(comunidades_louvain),
    round(modularity(bigram_graph, membership(comunidades_louvain)), 4),
    round(mean(tamaños_louvain), 1),
    max(tamaños_louvain),
    min(tamaños_louvain),
    sum(mod_local > 0.5),
    sum(df_puentes$Ratio_Puente > 0.5),
    round(edge_density(bigram_graph), 4),
    ifelse(modularity(bigram_graph, membership(comunidades_louvain)) > 0.3, 
           "Comunidades Significativas", "Estructura Moderada")
  )
)

print(resumen_clustering)
cat("\n")

# Interpretación
mod_value <- modularity(bigram_graph, membership(comunidades_louvain))

if (mod_value > 0.5) {
  interpretacion <- "ESTRUCTURA DE COMUNIDADES MUY CLARA: Red altamente segmentada en temas 
diferenciados. Cada comunidad representa un tema específico con vocabulario distintivo."
} else if (mod_value > 0.3) {
  interpretacion <- "ESTRUCTURA DE COMUNIDADES CLARA: Red presenta temática temática bien 
definida. Comunidades representan tópicos principales pero con algunos conectores temáticos."
} else {
  interpretacion <- "ESTRUCTURA DE COMUNIDADES MODERADA: Red presenta cierta organización 
temática pero con considerable overlap. Muchas palabras puente conectan diferentes áreas."
}

cat("INTERPRETACIÓN:\n", interpretacion, "\n\n")

# Exportar resumen
write.csv(resumen_clustering, "clustering_resumen_general.csv", row.names = FALSE)
cat("✓ Exportado: clustering_resumen_general.csv\n\n")

# ============================================================================
# 13.10: RESUMEN FINAL
# ============================================================================

cat("═════════════════════════════════════════════════════════════════════════════\n")
cat("RESUMEN PASO 13: ANÁLISIS DE CLUSTERING COMPLETADO\n")
cat("═════════════════════════════════════════════════════════════════════════════\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("  ✓ Visualizaciones PNG (4 archivos)\n")
cat("  ✓ Datos CSV (5 archivos)\n")
cat("  ✓ Algoritmos analizados: Louvain, Label Propagation, Walktrap\n")
cat("  ✓ Métricas calculadas: Modularidad, Cohesión Local, Palabras Puente\n\n")

cat("PRÓXIMOS PASOS:\n")
cat("  → Interpretar palabras principales de cada comunidad\n")
cat("  → Identificar palabras puente para análisis de conexiones temáticas\n")
cat("  → Comparar con análisis de centralidad para identificar palabras clave\n")
cat("  → Considerar análisis temporal si el guión tiene marcas de progresión\n\n")

# ============================================================================
# 12.9: CONCLUSIONES
# ============================================================================

cat("═ 12.9 CONCLUSIONES ═\n\n")

cat("ANÁLISIS ESTRUCTURAL DEL VOCABULARIO:\n\n")

cat("1. COHESIÓN GLOBAL:\n")
cat("   - Densidad:", round(network_density, 4), 
    if (network_density > 0.2) "→ Red cohesionada" else "→ Red dispersa", "\n")

cat("\n2. ESTRUCTURA TEMÁTICA:\n")
cat("   - Transitividad:", round(transitivity_global, 4),
    if (transitivity_global > 0.3) "→ Comunidades claras" else "→ Comunidades débiles", "\n")
cat("   - Componentes:", num_components,
    if (components_df$percentage[1] > 80) "→ Unificado" else "→ Fragmentado", "\n")

cat("\n3. EFICIENCIA:\n")
cat("   - Camino promedio:", round(avg_path_length, 4),
    if (avg_path_length < 6) "→ Red eficiente" else "→ Red lenta", "\n")

cat("\n4. TIPO DE RED:\n")
cat("   -", network_type, "\n")

cat("\n5. IMPLICACIONES LINGÜÍSTICAS:\n")
if (is_small_world) {
  cat("   ✓ Vocabulario organizado en temas coherentes\n")
  cat("   ✓ Fácil transición entre temas relacionados\n")
  cat("   ✓ Estructura balanceada entre cohe", 
      "sión y flexibilidad\n")
} else if (is_scale_free) {
  cat("   ○ Pocas palabras centrales que organizan vocabulario\n")
  cat("   ○ Temas especializados alrededor de hubs\n")
} else {
  cat("   △ Vocabulario fragmentado en múltiples temas\n")
  cat("   △ Posible presencia de subtemas aislados\n")
}

cat("\n═ ANÁLISIS DE COHESIÓN COMPLETADO ═\n\n")