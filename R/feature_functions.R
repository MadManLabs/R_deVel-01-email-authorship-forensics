# Functions

source("R/packages.R")


# Overview here: https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html

full_text <- lipsum::lipsum[1]


is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)

# Get all words
allWords = function(d) {
  words <- strsplit(d, " ")%>% 
    unlist() %>% 
    gsub('[[:punct:] ]+','',.)
  return(words)
}
# allWords(full_text)

# Get unique words
allUniqueWords = function(d) {
  words <- strsplit(d, " ")%>% 
    unlist() %>% 
    gsub('[[:punct:] ]+','',.) %>% 
    unique()
  return(words)
}
# allUniqueWords(full_text)

# Count words
countWords = function(d) {
  return(length(allWords(d)))
}
countWords(full_text)

# Count Unique words
countUniqueWords = function(d) {
  return(length(allUniqueWords(d)))
}
countUniqueWords(full_text)

# Count Characters
countCharacters <- function(d) {
  nchar(d)
}
# countCharacters(full_text)

# Short words

countShortWords = function(d) {
  return(length(strsplit(d, " ")[[1]] %>% .[str_length(.) < 4]) )
}

# Count punctuation

countPunctuation <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:punct:]")),collapse = "") %>% nchar()
}
# countPunctuation(full_text)


only_chars <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:alnum:]")),collapse = "")
}
# only_chars(full_text)


alphabetic_chars <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:alpha:]")),collapse = "")
}

upper_chars <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:upper:]")),collapse = "")
}

digit_chars <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:digit:]")),collapse = "")
}

space_chars <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, "[:space:]")),collapse = "")
}

white_space <- function(x) {
  paste(unlist(stringi::stri_extract_all_regex(x, " ")),collapse = "")
}

# Greeting formula

greet <- c("hello",
           "hi",
           "dear"
           )

Greet <- greet %>% str_to_title()

# Farewell Formula

farewell <- c("best",
              "yours sincerely",
              "greetings",
              "take care",
              "thanks",
              "cheers",
              "regards")

Farewell <- farewell %>% str_to_title()

## Functionwords

fw_url <- "https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/cook1988_function_words.txt"
functionwords <- read.table(file = fw_url,fill = TRUE,stringsAsFactors = F,encoding = "UTF-8")[,1]
functionwords %<>% str_replace_all("\\n","")

# signature text

