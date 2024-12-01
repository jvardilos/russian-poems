setwd("~/russian-poems")

library(readr)
library(stringr)
library(dplyr)

translation <- read_csv("translation.csv")
all_poems <- read_csv("poems.csv")
poems <- all_poems[1:200, ]

get_tokens <- function(text) {
  tokens <- unlist(str_split(text, "\\s+"))
  return(tokens[grepl("^\\b\\w+\\b$", tokens)])
}

translate <- function(text, translation) {
  translation$en <- paste0("[", translation$en, "]")
  tm <- setNames(translation$en, translation$ru)

  result <- lapply(tolower(text), function(text) {
    tokens <- get_tokens(text)
    translated_tokens <- tm[tokens[tokens %in% names(tm)]]
    rate <- length(translated_tokens) / length(tokens)

    tokens[tokens %in% names(tm)] <- translated_tokens
    translated <- paste(tokens, collapse = " ")
    return(list(translated = translated, rate = rate))
  })

  result_df <- do.call(rbind, lapply(result, as.data.frame))
  return(result_df)
}

average_words_per_line <- function(text) {
  words <- 0
  lines <- str_split(text, "\n")
  total_lines <- length(lines[[1]])

  for (line in lines) {
    tokens <- get_tokens(line)
    words <- words + length(tokens)
  }
  average <- words / total_lines

  return(average)
}


translation <- translate(poems$text, translation)
poems$translation <- translation$translated
poems$translation_rate <- as.numeric(translation$rate)

poems$lines_rate <- sapply(poems$text, average_words_per_line)

hist(scale(poems$translation_rate))

out <- poems[, c("translation", "translation_rate", "lines_rate")]
write.csv(out, file = "translation_effort.csv", row.names = FALSE)