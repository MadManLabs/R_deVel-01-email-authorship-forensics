## Loop for diferent author sets: 


df_summarizing_results <- NULL

svm_tune2 <- 
  tune(svm, factor(sender)~ ., data = df_training,
       kernel="radial", type = "C-classification",
       ranges=list(gamma.range = (2^seq(-5, 0, 1)),
                   cost.range = c(1, 4, 8, 16, 24, 32)))

svm_tune2$best.parameters$cost <- 1.5
svm_tune2$best.parameters$gamma <- 1.1


for (q in 1:50) {
  
  # Pick 3
  filter <- df$sender %>% table() %>% sort() %>% .[.>50 ] %>% names() %>% as.vector() %>% sample(size = 3,replace = FALSE)
  filter 
  
  
df_all <- df %>% filter(sender %in% filter)
# mail_summary(df_all)

# Splitting in training and test data

sample_training <- sample(1:nrow(df_all),round(nrow(df_all)*0.75),replace = FALSE)

training <- c(1:nrow(df_all)) %in% sample_training
prediction <- !c(1:nrow(df_all)) %in% training

df_training <- 
  df_all [training,]

df_prediction <- 
  df_all [prediction,]


## classification mode
# default with factor response:
model <- svm(sender ~ 
               number_words +
               total_char +
               unique_words +
               sentence_length +
               average_word_length +
               vocab_richness +
               number_stopwords +
               rate_stopwords,
             data = df_training,
             gamma = svm_tune2$best.parameters$gamma,
             cost = svm_tune2$best.parameters$cost,
             kernel = "radial",
             type = "C-classification")

pred2 <- predict(model,df_prediction)

# Check accuracy:
t1 <- round(table(predicted = pred2,
                  actual_author = df_prediction$sender) %>% prop.table(2),2); t1
t2 <- round(table(predicted = pred2,
                  actual_author = df_prediction$sender) ,2); t2


t1_df <-  as.data.frame(t2) %>% group_by(actual_author) %>% mutate(rate = round(Freq/sum(Freq),2))

df_summarizing_results <- rbind.data.frame(df_summarizing_results,data.frame(t1_df,set = q))

correct <- 
  t1_df %>% 
  filter(predicted == actual_author) %>% 
  ungroup() %>% 
  select(Freq)%>% 
  as.data.frame %>% 
  .[,1] 


t1_df <- 
  t1_df %>% group_by(actual_author) %>% 
  mutate(label_pos = rev(cumsum(rev(rate)))-0.5*rate) %>% 
  mutate(col = ifelse(predicted == actual_author,"green","red")) %>% 
  mutate(total = sum(Freq,na.rm = TRUE))

gg <- 
  ggplot(data = t1_df) + 
  geom_bar(aes(x = actual_author,
               y = rate,
               color = predicted,
               fill = col),
           stat = "identity")+
  geom_text(aes(x = actual_author,
                y = label_pos,
                label =   paste0(format((round(rate,3)*100),digits = 2,decimal.mark = ",")," %")))+
  geom_text(aes(x = actual_author,
                y = 1.07,
                label =   paste0(total," Observ.")))+
  scale_fill_manual(values=c("green", "red")) +
  labs(x = "Actual Author",
       y = "Predicted Author Frequency",
       title = paste0("Author Sample ",q),
       subtitle = paste0("Correct predictions in ",format((round(correct[1],3)*100),digits = 2,decimal.mark = ",")," %, ",format((round(correct[2],3)*100),digits = 2,decimal.mark = ",")," %, and ",format((round(correct[1],3)*100),digits = 2,decimal.mark = ",")," % of cases"))+
  theme_ipsum(grid = "")


ggsave(filename=paste0("Author_Set_",q,".pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)

}


saveRDS(df_summarizing_results,
        "../data/df_summarizing_results.rds")

#####################################################


df_summarizing_results <- readRDS("../data/df_summarizing_results.rds")

# Correct 
df_summary <- 
  df_summarizing_results %>% 
  group_by(set,actual_author) %>% 
  mutate(obs = sum(Freq,na.rm = TRUE)) %>% 
  group_by(set) %>% 
  mutate(inequality = ((((obs)-mean(unique(obs)))))) %>% 
  mutate(inequality_log = ifelse(inequality > 0, log(inequality),-log(abs(inequality)))) %>% 
  filter(actual_author == predicted) %>% 
  group_by(actual_author,inequality_log) %>% 
  mutate(total = sum(Freq)) %>% 
  summarise (correct = mean(rate))


gg <- 
  ggplot(data = df_summary) + 
  geom_point(aes(x = inequality_log,
                 y = correct,
                 color = actual_author)) + 
  geom_line(aes(x = inequality_log,
                 y = correct,
                 color = actual_author)) + 
  labs(x = "Observation inequality: Log squared difference from Mean n()",
       y = "Authorship correctly predicted",
       title = paste0("Observation Inequality - A Decisive Predictor"),
       subtitle = paste0("Correct Authorship Attribution regressed on relative observation superiority"))+
  theme_ipsum(grid = "Y");gg


ggsave(filename=paste0("Observation_Inequality.pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)
  

