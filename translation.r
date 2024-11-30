setwd("~/russian-poems")

library(readr)
library(stringr)
library(dplyr)

translation <- read_csv("translation.csv")
all_poems <- read_csv("poems.csv")
poems <- all_poems[1:2000, ]

make_translation_map <- function(translation) {
  translation$en <- paste0("[", translation$en, "]")
  return(setNames(translation$en, translation$ru))
}

translate <- function(text, translation) {
  tm <- make_translation_map(translation)

  lapply(tolower(text), function(text) {
    tokens <- unlist(str_split(text, "\\s+"))
    tokens <- tokens[grepl("^\\b\\w+\\b$", tokens)]
    tokens[tokens %in% names(tm)] <- tm[tokens[tokens %in% names(tm)]]
    combined <- paste(tokens, collapse = " ")
    return(combined)
  })
}

poems$translation <- translate(poems$text, translation)
poems$translation[1:23]
