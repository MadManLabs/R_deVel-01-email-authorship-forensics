# All Senders

formattable(mail_summary(df) %>% filter(total_mails > 50), 
            list(
  area(col = c(total_mails)
  ) ~ normalize_bar("lightgreen", 0.2),
  area(col = c(number_words)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(total_char)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(unique_words)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(sentence_length)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(average_word_length)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(vocab_richness)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(number_stopwords)
  ) ~ normalize_bar("pink", 0.2),
  area(col = c(rate_stopwords)
  ) ~ normalize_bar("pink", 0.2)
  )
) %>% as.htmlwidget() %>% 
  saveWidget(file = "descriptive_covariates.html",selfcontained = TRUE)



# Subset for first try

# c("MillsCD@state.gov" , "Verveer, Melanne S" ,"Muscatine, Lissa"  )

formattable(mail_summary(df2), 
            list(
              area(col = c(total_mails)
              ) ~ normalize_bar("lightgreen", 0.2),
              area(col = c(number_words)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(total_char)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(unique_words)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(sentence_length)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(average_word_length)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(vocab_richness)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(number_stopwords)
              ) ~ normalize_bar("pink", 0.2),
              area(col = c(rate_stopwords)
              ) ~ normalize_bar("pink", 0.2)
            )
) %>% as.htmlwidget() %>% 
  saveWidget(file = "covariates_subset_1.html",selfcontained = TRUE)

##

t1_df <-  as.data.frame(t1)
correct <- 
  t1_df %>% 
  filter(predicted == actual_author) %>% 
  ungroup() %>% 
  select(Freq)%>% 
  as.data.frame %>% 
  .[,1] 


t1_df <- 
  t1_df %>% group_by(actual_author) %>% mutate(label_pos = rev(cumsum(rev(Freq)))-0.5*Freq)

gg <- 
  ggplot(data = t1_df) + 
  geom_bar(aes(x = actual_author,
           y = Freq,
           fill = predicted),
           stat = "identity")+
  geom_text(aes(x = actual_author,
                y = label_pos,
                label =   paste0(format((round(Freq,3)*100),digits = 2,decimal.mark = ",")," %")))+
  labs(x = "Actual Author",
       y = "Predicted Author Frequency",
       title = paste0("Author Sample ",q),
       subtitle = paste0("Correct predictions in ",format((round(correct[1],3)*100),digits = 2,decimal.mark = ",")," %, ",format((round(correct[2],3)*100),digits = 2,decimal.mark = ",")," %, and ",format((round(correct[1],3)*100),digits = 2,decimal.mark = ",")," % of cases"))+
  theme_ipsum(grid = "")
  
  
  ggsave(filename=paste0("Author_Set_",q,".html"),
         plot=gg,
         pointsize = 24, 
         width = 18 ,
         height = 10,
         scale = 0.5,
         dpi = 800)

  
  #######
  
  # library(ggplot2)   
  # 
  # p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  #   geom_bar(stat = "identity") + 
  #   theme(axis.text.x=element_text(angle=45, hjust=1))
  # p   
  
  
  # Wordcloud of Function Words: 
  
  # fix of errror
  simpleFix = function(inputFile, outputFile){
    a = readLines(inputFile)
    output = paste(a, collapse = "\n")
    output = gsub(">\n\n</div>","></div>",output)
    writeLines(output, outputFile)
    invisible(NULL)
  }
  
  wc1 <- wordcloud2(data = wf_ordered[1:122,] %>% mutate(freq = log(freq)),size = 0.5);wc1
  
  #saveWidget(wc1,"wordcloud_function_words_log.html",selfcontained = TRUE)
  #simpleFix("wordcloud_function_words_log.html", "wordcloud_function_words_log.html")
  
  wc2 <- wordcloud2(data = wf_ordered[1:122,] %>% mutate(freq = (freq)),size = 3);wc2
  
  #saveWidget(wc2,"wordcloud_function_words_natural.html",selfcontained = TRUE)
  #simpleFix("wordcloud_function_words_natural.html", "wordcloud_function_words_natural.html")
  