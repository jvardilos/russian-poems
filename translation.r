setwd("~/russian-poems")

library(readr)
library(stringr)
library(dplyr)

translation <- read_csv("translation.csv")
all_poems <- read_csv("poems.csv")
poems <- all_poems

get_tokens <- function(text) {
  tokens <- unlist(str_split(text, "\\s+"))
  return(tokens[grepl("^\\b\\w+\\b$", tokens)])
}

translate <- function(text, translation) {
  translation$en <- paste0("[", translation$en, "]")
  tm <- setNames(translation$en, translation$ru)

  result <- lapply(tolower(text), function(text) {
    tokens <- get_tokens(text)
    tokens_copy <- tokens
    translated_tokens <- tm[tokens[tokens %in% names(tm)]]
    rate <- length(translated_tokens) / length(tokens)

    tokens[tokens %in% names(tm)] <- translated_tokens
    translated <- paste(tokens, collapse = " ")

    # Return tokens and translated tokens
    return(list(translated = translated, rate = rate, tokens = tokens_copy, translated_tokens = translated_tokens))
  })

  # Convert to data frame with tokens
  result_df <- do.call(rbind, lapply(result, function(x) {
    data.frame(translated = x$translated,
               rate = x$rate,
               tokens = I(list(x$tokens)),
               translated_tokens = I(list(x$translated_tokens)))
  }))

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
poems$tokens <- translation$tokens
poems$translated_tokens <- translation$translated_tokens
poems$lines_rate <- sapply(poems$text, average_words_per_line)


### end cleaning

aggregate_authors <- function(poems) {
  ag <- poems %>%
    group_by(writer) %>%
    summarise(
      translation_rate = mean(translation_rate),
      lines_rate = mean(lines_rate),
      favorite_word = {
        freq_table <- sort(table(unlist(tokens)), decreasing = TRUE)
        names(freq_table[1])
      },
      favorite_translated_word = {
        freq_table <- sort(table(unlist(translated_tokens)), decreasing = TRUE)
        names(freq_table[1])
      },
    )
  # remove NaNs from the aggregation
  ag$translation_rate[is.nan(ag$translation_rate)] <- 0
  return(ag)
}


ag <- aggregate_authors(poems)


highest_translation_rate <- ag[order(ag$translation_rate, decreasing = TRUE), ]
highest_lines_rate <- ag[order(ag$lines_rate, decreasing = TRUE), ]
lowest_lines_rate <- ag[order(ag$lines_rate, decreasing = FALSE), ]
favorite_word <- ag[order(ag$favorite_word, decreasing = TRUE), ]
favorite_translated_word <- ag[order(ag$favorite_translated_word, decreasing = TRUE), ]

### favorite word by author
### authors most used translated word - mode
### shortest Lines by author

write.csv(highest_translation_rate, file = "translation_effort.csv", row.names = FALSE)
