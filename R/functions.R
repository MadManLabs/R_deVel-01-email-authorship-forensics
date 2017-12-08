mail_summary <- function(input_data) {
  summary_table <- 
    input_data %>% group_by(sender) %>% summarize(total_mails = n(),
                                                  number_words = mean(number_words,na.rm = T) %>% round(1),
                                                  total_char = mean(total_char,na.rm = T) %>% round(1),
                                                  unique_words = mean(unique_words,na.rm = T)%>% round(1),
                                                  total_char = mean(total_char,na.rm = T)%>% round(1),
                                                  sentence_length = mean(sentence_length,na.rm = T)%>% round(1),
                                                  total_char = mean(total_char,na.rm = T)%>% round(1),
                                                  unique_words = mean(unique_words,na.rm = T)%>% round(1),
                                                  sentence_length = mean(sentence_length,na.rm = T)%>% round(1),
                                                  average_word_length = mean(average_word_length,na.rm = T)%>% round(1),
                                                  vocab_richness = mean(vocab_richness,na.rm = T)%>% round(1),
                                                  number_stopwords = mean(number_stopwords,na.rm = T)%>% round(1),
                                                  rate_stopwords = mean(rate_stopwords,na.rm = T) %>% round(1)
    ) 
  return(summary_table)
}

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}


#' 
#' Clean_String <- function(string){
#'   # Lowercase
#'   temp <- tolower(string)
#'   #' Remove everything that is not a number or letter (may want to keep more 
#'   #' stuff in your actual analyses). 
#'   temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
#'   # Shrink down to just one white space
#'   temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
#'   # Split it
#'   temp <- stringr::str_split(temp, " ")[[1]]
#'   # Get rid of trailing "" if necessary
#'   indexes <- which(temp == "")
#'   if(length(indexes) > 0){
#'     temp <- temp[-indexes]
#'   } 
#'   return(temp)
#' }
#' 
#' ############################################################################
#' 
#' #' function to clean text
#' Clean_Text_Block <- function(text){
#'   if(length(text) <= 1){
#'     # Check to see if there is any text at all with another conditional
#'     if(length(text) == 0){
#'       cat("There was no text in this document! \n")
#'       to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
#'     }else{
#'       # If there is , and only only one line of text then tokenize it
#'       clean_text <- Clean_String(text)
#'       num_tok <- length(clean_text)
#'       num_uniq <- length(unique(clean_text))
#'       to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
#'     }
#'   }else{
#'     # Get rid of blank lines
#'     indexes <- which(text == "")
#'     if(length(indexes) > 0){
#'       text <- text[-indexes]
#'     }  
#'     # Loop through the lines in the text and use the append() function to 
#'     clean_text <- Clean_String(text[1])
#'     for(i in 2:length(text)){
#'       # add them to a vector 
#'       clean_text <- append(clean_text,Clean_String(text[i]))
#'     }
#'     # Calculate the number of tokens and unique tokens and return them in a 
#'     # named list object.
#'     num_tok <- length(clean_text)
#'     num_uniq <- length(unique(clean_text))
#'     to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
#'   }
#'   return(to_return)
#' }
