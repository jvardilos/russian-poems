setwd("~/russian-poems")

library(readr)
library(stringr)
library(dplyr)

translation <- read_csv("translation.csv")
all_poems <- read_csv("poems.csv")
poems <- all_poems[1:3000, ]

str(all_poems)

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
    # print(as.list(tokens))
    return(list(translated = translated, rate = rate))
  })

  result_df <- do.call(rbind, lapply(result, as.data.frame))
  # print(result_df)
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

translation$tokens
translation$translated
poems$translation <- translation$translated
poems$translation_rate <- as.numeric(translation$rate)
# poems$tokens <- translation$tokens
# poems$translated_tokens <- translation$translated_tokens
poems$lines_rate <- sapply(poems$text, average_words_per_line)


### end cleaning

aggregate_authors <- function(poems) {
  ag <- poems %>%
    group_by(writer) %>%
    summarise(
      translation_rate = mean(translation_rate),
      lines_rate = mean(lines_rate),
      # favorite_word = mode(tokens),
      # favorite_translated_word = mode(translated_tokens),
    )
  # remove NaNs from the aggregation
  ag$translation_rate[is.nan(ag$translation_rate)] <- 0
  return(ag)
}


ag <- aggregate_authors(poems)


highest_translation_rate <- ag[order(ag$translation_rate, decreasing = TRUE), ]
highest_lines_rate <- ag[order(ag$lines_rate, decreasing = TRUE), ]
lowest_lines_rate <- ag[order(ag$lines_rate, decreasing = FALSE), ]

# plot(highest_lines_rate$translation_rate)

### favorite word by author
### authors most used translated word - mode
### shortest Lines by author

write.csv(highest_translation_rate, file = "translation_effort.csv", row.names = FALSE)
