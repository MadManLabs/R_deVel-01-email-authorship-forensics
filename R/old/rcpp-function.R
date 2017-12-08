con <- file("http://mjdenny.com/workshops/Obama_Speech_2-24-09.txt", "r", blocking = FALSE)
text <- readLines(con)
close(con) 
class(text)

clean_speech <- Clean_Text_Block(text)

# Read in the file
con <- file("http://mjdenny.com/workshops/Obama_Speech_1-27-10.txt", "r", blocking = FALSE)
text2 <- readLines(con)
close(con)  

# Clean and tokenize the text
clean_speech2 <- Clean_Text_Block(text2)


#######

Rcpp::sourceCpp('Generate_Document_Word_Matrix.cpp')

#' Create a list containing a vector of tokens in each document for each
#' document. These can be extracted from the cleaned text objects as follows.
doc_list <- list(clean_speech$text,clean_speech2$text)  

#' Create a vector of document lengths (in tokens)
doc_lengths <- c(clean_speech$num_tokens,clean_speech2$num_tokens)  

#' Generate a vector containing the unique tokens across all documents.
unique_words <- unique(c(clean_speech$text,clean_speech2$text))  

#' The number of unique tokens across all documents
n_unique_words <- length(unique_words)  

#' The number of documents we are dealing with. 
ndoc <- 2  

#' Now feed all of this information to the function as follows:
Doc_Term_Matrix <- Generate_Document_Word_Matrix(
  number_of_docs = ndoc,
  number_of_unique_words = n_unique_words,
  unique_words = unique_words,
  Document_Words = doc_list,
  Document_Lengths = doc_lengths
)  

#' Make sure to add column names to you Doc-Term matrix, then take a look! 
colnames(Doc_Term_Matrix) <- unique_words
