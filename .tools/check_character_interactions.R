library(tidyverse)
library(igraph)

# Define main_characters (same as in Rmd)
main_characters <- c(
  "CJ","SWEET","CESAR","KENDL","RYDER","BIG SMOKE",
  "CATALINA","WOOZIE","TENPENNY","PULASKI","TRUTH","TORENO",
  "MADD DOGG","ZERO"
)

# Read script
raw_lines <- read_lines("guionGTA.txt")
raw_lines <- str_replace_all(raw_lines, "\\bCARL\\b", "CJ")

# define functions from the chunk
classify_interactions <- function(text) {
  mission_keywords <- c("MISSION", "OBJECTIVE", "TASK", "GO", "GET", "BRING", "FIND", 
                        "COMPLETE", "FINISH", "START", "BEGIN", "DO")
  conversation_keywords <- c("TALK", "SAY", "TELL", "SPEAK", "ASK", "ANSWER", 
                             "CHAT", "DISCUSS", "EXPLAIN", "MENTION", "REPLY")
  conflict_keywords <- c("FIGHT", "KILL", "SHOOT", "ATTACK", "ANGRY", "MAD",
                         "HATE", "ENEMY", "BATTLE", "WAR", "VIOLENCE", "GANG")
  txt <- toupper(text)
  dplyr::case_when(
    any(sapply(mission_keywords, grepl, x = txt)) ~ "MISSION",
    any(sapply(conversation_keywords, grepl, x = txt)) ~ "CONVERSATION",
    any(sapply(conflict_keywords, grepl, x = txt)) ~ "CONFLICT",
    TRUE ~ "OTHER"
  )
}

create_typed_mentions <- function(text, characters, interaction_type) {
  mentions <- matrix(0, nrow = length(characters), ncol = length(characters))
  rownames(mentions) <- characters
  colnames(mentions) <- characters
  text <- toupper(text)
  for(i in seq_along(text)) {
    if(classify_interactions(text[i]) == interaction_type) {
      present_chars <- characters[sapply(characters, function(x) grepl(x, text[i]))]
      if(length(present_chars) > 1) {
        for(j in 1:(length(present_chars)-1)) {
          for(k in (j+1):length(present_chars)) {
            mentions[present_chars[j], present_chars[k]] <- mentions[present_chars[j], present_chars[k]] + 1
            mentions[present_chars[k], present_chars[j]] <- mentions[present_chars[k], present_chars[j]] + 1
          }
        }
      }
    }
  }
  mentions
}

# Run and catch errors
result <- tryCatch({
  adj <- create_typed_mentions(raw_lines, main_characters, "CONVERSATION")
  g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
  cat("Success: graph has", vcount(g), "vertices and", ecount(g), "edges\n")
  TRUE
}, error = function(e) {
  cat("ERROR: ", conditionMessage(e), "\n")
  FALSE
})

if(!result) quit(status = 1)
