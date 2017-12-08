# Functions

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


