setwd("~/russian-poems")

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
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
    return(list(
      translated = translated,
      rate = rate,
      tokens = tokens_copy,
      translated_tokens = translated_tokens
    ))
  })

  # Convert to data frame with tokens
  result_df <- do.call(rbind, lapply(result, function(x) {
    data.frame(
      translated = x$translated,
      rate = x$rate,
      tokens = I(list(x$tokens)),
      translated_tokens = I(list(x$translated_tokens))
    )
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


tco <- c("translation", "text")
write.csv(poems[tco], file = "poems-translated.csv", row.names = FALSE)

### end cleaning

index <- 2
aggregate_authors <- function(poems) {
  ag <- poems %>%
    group_by(writer) %>%
    summarise(
      translation_rate = mean(translation_rate),
      lines_rate = mean(lines_rate),
      favorite_words = {
        freq_table <- sort(table(unlist(tokens)), decreasing = TRUE)
        list(names(freq_table[1:100]))
      },
      favorite_translated_words = {
        freq_table <- sort(table(unlist(translated_tokens)), decreasing = TRUE)
        list(names(freq_table[1:100]))
      },
    )
  # remove NaNs from the aggregation
  ag$translation_rate[is.nan(ag$translation_rate)] <- 0
  return(ag)
}


ag <- aggregate_authors(poems)

# Sort aggregated authors to find what I need
htr <- ag[order(ag$translation_rate, decreasing = TRUE), ]
largest_lines <- ag[order(ag$lines_rate, decreasing = TRUE), ]
smallest_lines <- ag[order(ag$lines_rate, decreasing = FALSE), ]

# Get Top 5 Poets who are the most translatable
htr$writer[1:5]
htr$translation_rate[1:5]

# get min and max line rate of each author
largest_lines$lines_rate[1:5]
largest_lines$writer[1:5]

smallest_lines$lines_rate[1:5]
smallest_lines$writer[1:5]

# Generate the wordclouds of Russian and translated

words <- unlist(htr$favorite_words)
translated_words <- unlist(htr$favorite_translated_words)
distribution_words <- sort(table(words), decreasing = TRUE)
distribution_words_translated <- sort(table(translated_words), decreasing = TRUE)

wordcloud2(data = distribution_words, size = 1.6, color = 'random-dark')
wordcloud2(data = distribution_words_translated, size = 1.6, color = "random-dark")
