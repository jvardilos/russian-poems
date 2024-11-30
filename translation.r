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

fun <- function(tokens, tm) {
  tokens[tokens %in% names(tm)] <- tm[tokens[tokens %in% names(tm)]]
  return(tokens)
}

translate <- function(text, translation) {
  tm <- make_translation_map(translation)

  lapply(tolower(text), function(text) {
    tokens <- unlist(str_split(text, "\\s+"))
    tokens <- tokens[grepl("^\\b\\w+\\b$", tokens)]
    return(fun(tokens, tm))
  })
}


poems$translation <- translate(poems$text, translation)
poems$translation[1:23]
