source("R/packages.R")
#source("R/functions.R")

# Data loading and preparation

datapath <- "data/offline/pan11/pan11-authorship-attribution-test-dataset-small-2015-10-20/pan11-authorship-attribution-test-dataset-small-2015-10-20"

dir(datapath)
candidates <- dir(datapath) %>% .[str_detect(.,"candidate")]

# Create List of all files
files <- list()
for (c in candidates){
  files[[c]] <- dir(paste0(datapath,"/",c))
}

# Remove following

#00006 + 00037
files[["candidate00006"]] <- files[["candidate00006"]] %>% .[. != "known00037.txt"]

# Throw out candidate 20 as a whole (to many probs)

files[["candidate00020"]] <- files[["candidate00020"]] %>% .[. != "known00054.txt"]
files[["candidate00020"]] <- files[["candidate00020"]] %>% .[. != "known00070.txt"]
files[["candidate00020"]] <- files[["candidate00020"]] %>% .[. != "known00072.txt"]
files[["candidate00020"]] <- files[["candidate00020"]] %>% .[. != "known00073.txt"]
files[["candidate00024"]] <- files[["candidate00024"]] %>% .[. != "known00014.txt"]
files[["candidate00026"]] <- files[["candidate00026"]] %>% .[. != "known00051.txt"]
files[["candidate00026"]] <- files[["candidate00026"]] %>% .[. != "known00070.txt"]


content <- list()

for (c in candidates){
  candi <- list()
  
  for (t in 1:length(files[[c]])){

    text <- 
      read.table(file = paste0(datapath,"/",c,"/",files[[c]][t]),
                 fill = T,
                 blank.lines.skip = F,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8",
                 row.names=NULL) %>% 
      as.data.frame()
    candi[[paste0(files[[c]][t])]] <- text
  }
  
  content[[c]] <- candi
}

saveRDS(content,"data/raw_content.rds")

#  Features: 

source("R/feature_functions.R")
###

df <- data.frame()


##################
content[[c]][[t]]
# problem at 0006 ~ 0037
c = "candidate00026"
t = 69

#################

for (c in candidates){
  
  print(c)
  
  ts <- 1:length(files[[c]])
  for (t in ts){
    
    print(t)
    
    ## fixes
    
    ## 
    
    rowwise_text <- 
      content[[c]][[t]] %>% 
      as.data.frame() %>% 
      rowwise() %>% 
      do(text = paste0(.,collapse = " ")) %>% 
      .$text
    
    dim(rowwise_text)
    
    vector_row_text <- 
      rowwise_text %>% paste0()

    full_text <- 
      paste0(vector_row_text,collapse = " ")
    
    # Cut additional spaces
    full_text <- 
      str_replace(gsub("\\s+", " ", str_trim(full_text)), "B", "b")
    
    # Get sentences: WHICH ONE?
    
    sentences1 <- 
      full_text %>% 
      gsub(" +", " ", .) %>% 
      strsplit(split = "[\\.?!] ")
    
    sentences <- 
      full_text %>% 
      str_split(., boundary("sentence"))
    
    # Get words
    words <- 
      allWords(full_text)
    
    # M = total number of tokens
    
    M <- countWords(full_text)  
    
    # V = total number of types(unique)
    
    V <- countUniqueWords(full_text)
    
    # C = C = total number of characters in e-mail body
    
    C <- countCharacters(full_text)
    
    
    
    #1: Number of blank lines/total number of lines
    line_blank <- 
      lapply(rowwise_text,is.letter) %>% unlist()
    
    blank_lines <- length(which(line_blank))
    total_lines <- length(line_blank)
    blank_line_ratio <- blank_lines/total_lines
    
    #2: Average sentence length
    average_sentence_length <- 
      lapply(X = sentences,FUN = function(x) str_length(x)) %>% 
      unlist() %>% 
      mean(na.rm = TRUE)
    

    #3: Average word length
    average_word_length <- 
      lapply(X = words,FUN = function(x) str_length(x)) %>% 
      unlist() %>% 
      mean(na.rm = TRUE)
    
    #4: Vocabulary Richness (V/M)
    vocabulary_richness <- V/M
    
    #5: Total number of function words/M 
    
    '!!!!!!!!!!!!!!!!!!!!!'
    
    fwurl <- "https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/cook1988_function_words.txt"
    
    functionwords <- 
    number_stopwords <- which (words %in% stopwords("english"))
    number_stopwords %<>% ifelse(purrr::is_empty(.),0,.)
    ratio_function_words <- number_stopwords/M
      
    #6: Function word frequency distribution (122 features)
    
    '!!!!!!!!!!!!!!!!!!!!!???????'
    
    #7: Total number of short words/M
    number_shortwords <- countShortWords(full_text)
    ratio_shortwords <- number_shortwords/M
    
    #8: Count of hapax legomena/M
    hapax_m <- length(which((table(words) %>% as.vector())  == 1))/M
    
    #9: Count of hapax legomena/V
    hapax_v <- length(which((table(words) %>% as.vector())  == 1))/V
    
    #10: Total number of characters in words/C
    ratio_char_in_words <- nchar(only_chars(full_text))/C
    
    #11: Total number of alphabetic characters in words/C
    ratio_alphabetic <- nchar(alphabetic_chars(full_text))/C
    
    #12: Total number of upper-case characters in words/C
    ratio_upper <- nchar(upper_chars(full_text))/C
    
    #13: Total number of digit characters in words/C
    ratio_digit <- nchar(digit_chars(full_text))/C
    
    #19: Total number of punctuations/C
    ratio_punctuation <- countPunctuation(full_text)/C
    
    #20: Word length frequency distribution
    for (i in 1:30){
      eval(parse(text = paste0("l_",i," <- ",length(which(str_length(words) == i))/M)))
    }
    freqs <- eval(parse(text = paste0("data.frame(",paste0("l_",1:30,collapse = ","),")")))
    
    
    
    #Put it together
  new_row <- data.frame(candidate = c,
                        text = files[[c]][t],
                        t = t,
                        blank_lines,
                        total_lines,
                        blank_line_ratio,
                        average_sentence_length,
                        average_word_length,
                        vocabulary_richness,
                        number_stopwords,
                        ratio_function_words,
                        number_shortwords,
                        ratio_shortwords,
                        hapax_m,
                        hapax_v,
                        ratio_char_in_words,
                        ratio_alphabetic,
                        ratio_upper,
                        ratio_digit, # 13
                        ratio_punctuation, # 19
                        freqs
                        )
  
  
    
    
  df <- 
    bind_rows(df,
              new_row)
  }
}

saveRDS(df,"data/processed_data.rds")



###################################################################

### Structural Attributes (all not implementable to the moment)

## Has a greeting acknowledgment
## Uses a farewell acknowledgment
## Contains signature text
## Number of attachments
## Position of requoted text within e-mail body
## HTML tag frequency distribution/total number of HTML tags (16 features)






