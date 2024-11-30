setwd("~/russian-poems")

library(readr)
library(stringr)
library(dplyr)

translation <- read_csv("translation.csv")
poems <- read_csv("poems.csv")

make_translation_map <- function(translation) {
  translation$en <- paste0("[", translation$en, "]")
  return(setNames(translation$en, translation$ru))
}

translate <- function(text, translation) {
  tm <- make_translation_map(translation)

  lapply(tolower(text[1]), function(text) {
    tokens <- unlist(str_split(text, "\\s+"))
    tokens <- tokens[grepl("^\\b\\w+\\b$", tokens)]
    print(tokens)
    return(str_replace_all(tokens, tm))
  })
}

poems$translation <- translate(poems$text, translation)
poems$translation[1]
