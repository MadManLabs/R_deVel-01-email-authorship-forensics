## Loop for diferent author sets: 


df_summarizing_results <- NULL

svm_tune2 <- 
  tune(svm, factor(sender)~ ., data = df_training,
       kernel="radial", type = "C-classification",
       ranges=list(gamma.range = (2^seq(-5, 0, 1)),
                   cost.range = c(1, 4, 8, 16, 24, 32)))

svm_tune2$best.parameters$cost <- 1
svm_tune2$best.parameters$gamma <- 0.03


for (q in c(1:50)) {
  
  # Pick 3
  filter <- df$sender %>% table() %>% sort() %>% .[.>50 ] %>% names() %>% as.vector() 
  
  #filter <- filter [c(q,q+1,q+2)]
  filter <- sample(filter,3)
  
df_all <- df %>% filter(sender %in% filter)

# mail_summary(df_all)

# Splitting in training and test data
sample_training <- sample(1:nrow(df_all),round(nrow(df_all)*0.75),replace = FALSE)
df_training <- df_all[sample_training,]

 sample_training1 <- sample_training[df_training$sender == sort(unique(df_training$sender))[1]]
 sample_training2 <- sample_training[df_training$sender == sort(unique(df_training$sender))[2]]
 sample_training3 <- sample_training[df_training$sender == sort(unique(df_training$sender))[3]]

 m <- min (length(sample_training1),length(sample_training2),length(sample_training3))

df_training <- 
  rbind.data.frame(
    df_training %>% filter(sender == sort(unique(df_training$sender))[1]) %>% sample_n(m),
    df_training %>% filter(sender == sort(unique(df_training$sender))[2]) %>% sample_n(m),
    df_training %>% filter(sender == sort(unique(df_training$sender))[3]) %>% sample_n(m)
    )


# sample_training_all <- c(sample(sample_training1,m),sample(sample_training2,m),sample(sample_training3,m))
# 
# training <- c(1:nrow(df_all))[c(1:nrow(df_all)) %in% sample_training_all]
# prediction <- c(1:nrow(df_all))[(!c(1:nrow(df_all)) %in% training)]
#prediction <- sample(prediction,100)

df_prediction <- 
  df_all [c(1:nrow(df_all)) != sample_training,]


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


correct <- 
  t1_df %>% 
  filter(predicted == actual_author) %>% 
  ungroup() %>% 
  select(Freq)%>% 
  as.data.frame %>% 
  .[,1] 


abc <- t1_df$actual_author %>% unique()



a <- abc[1]

a1 <- 
  t1_df 

tp1 <- 
  a1$Freq [a1$actual_author== a & a1$predicted ==a]

fp1 <- 
  sum(a1$Freq [a1$actual_author!= a & a1$predicted ==a])

fn1 <- 
  sum(a1$Freq [a1$actual_author== a & a1$predicted !=a])


recall1 <- tp1/(tp1+fn1)

precision1 <- tp1/(tp1+fp1)

f1 = (2*recall1 * precision1) / (recall1 + precision1)


a <- abc[2]

a2 <- 
  t1_df 

tp2 <- 
  a2$Freq [a2$actual_author== a & a2$predicted ==a]

fp2 <- 
  sum(a2$Freq [a2$actual_author!= a & a2$predicted ==a])

fn2 <- 
  sum(a2$Freq [a2$actual_author== a & a2$predicted !=a])


recall2 <- tp2/(tp2+fn2)

precision2 <- tp2/(tp2+fp2)

f2 = (2*recall2 * precision2) / (recall2 + precision2)



a <- abc[3]

a3 <- 
  t1_df 

tp3 <- 
  a3$Freq [a3$actual_author== a & a3$predicted ==a]

fp3 <- 
  sum(a3$Freq [a3$actual_author!= a & a3$predicted ==a])

fn3 <- 
  sum(a3$Freq [a3$actual_author== a & a3$predicted !=a])


recall3 <- tp3/(tp3+fn3)

precision3 <- tp3/(tp3+fp3)

f3 = (2*recall3 * precision3) / (recall3 + precision3)



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
                label =   paste0(m," Train. Observ.")))+
  geom_text(aes(x = 1,
                y = 1.4,
                label =   paste0("Preci.: ",round(ifelse(!is.nan(precision1),precision1,0),2))))+
  geom_text(aes(x = 1,
                y = 1.30,
                label =   paste0("Recall: ",round(ifelse(!is.nan(recall1),recall1,0),2))))+
  geom_text(aes(x = 1,
                y = 1.20,
                label =   paste0("F: ",round(ifelse(!is.nan(f1),f1,0),2))))+
  geom_text(aes(x = 2,
                y = 1.4,
                label =   paste0("Preci.: ",round(ifelse(!is.nan(precision2),precision2,0),2))))+
  geom_text(aes(x = 2,
                y = 1.30,
                label =   paste0("Recall: ",round(ifelse(!is.nan(recall2),recall2,0),2))))+
  geom_text(aes(x = 2,
                y = 1.20,
                label =   paste0("F: ",round(ifelse(!is.nan(f2),f2,0),2))))+
  geom_text(aes(x = 3,
                y = 1.4,
                label =   paste0("Preci.: ",round(ifelse(!is.nan(precision3),precision3,0),2))))+
  geom_text(aes(x = 3,
                y = 1.30,
                label =   paste0("Recall: ",round(ifelse(!is.nan(recall3),recall3,0),2))))+
  geom_text(aes(x = 3,
                y = 1.20,
                label =   paste0("F: ",round(ifelse(!is.nan(f3),f3,0),2))))+
  scale_fill_manual(values=c("green", "red")) +
  labs(x = "Actual Author",
       y = "Predicted Author Frequency",
       title = paste0("Author Sample ",q))+
  theme_ipsum(grid = "");gg


ggsave(filename=paste0("Author_Set_",q,".pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


df_summarizing_results <- rbind.data.frame(df_summarizing_results,data.frame(t1_df,set = q,
                                           precision = c(precision1,precision2,precision3),
                                           recall = c(recall1,recall2,recall3),
                                           f = c(f1,f3,f3)))



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
  summarise (f = mean(f,na.rm = TRUE),
             precision = mean(precision,na.rm = TRUE),
             recall = mean(recall,na.rm = TRUE))


gg <- 
  ggplot(data = df_summary) + 
  geom_point(aes(x = inequality_log,
                 y = f,
                 color = actual_author)) + 
  geom_line(aes(x = inequality_log,
                 y = f,
                 color = actual_author)) + 
  labs(x = "Observation inequality: Log squared difference from Mean n()",
       y = "Authorship correctly predicted",
       title = paste0("F-Score depending of Sample inequality"),
       subtitle = paste0("Correct Authorship Attribution regressed on relative observation superiority"))+
  theme_ipsum(grid = "Y");gg


ggsave(filename=paste0("f.pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)
  


gg <- 
  ggplot(data = df_summary) + 
  geom_point(aes(x = inequality_log,
                 y = precision,
                 color = actual_author)) + 
  geom_line(aes(x = inequality_log,
                y = precision,
                color = actual_author)) + 
  labs(x = "Observation inequality: Log squared difference from Mean n()",
       y = "Authorship correctly predicted",
       title = paste0("Precision depending of Sample inequality"),
       subtitle = paste0("Correct Authorship Attribution regressed on relative observation superiority"))+
  theme_ipsum(grid = "Y");gg


ggsave(filename=paste0("precision.pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



gg <- 
  ggplot(data = df_summary) + 
  geom_point(aes(x = inequality_log,
                 y = recall,
                 color = actual_author)) + 
  geom_line(aes(x = inequality_log,
                y = recall,
                color = actual_author)) + 
  labs(x = "Observation inequality: Log squared difference from Mean n()",
       y = "Authorship correctly predicted",
       title = paste0("Recall depending of Sample inequality"),
       subtitle = paste0("Correct Authorship Attribution regressed on relative observation superiority"))+
  theme_ipsum(grid = "Y");gg


ggsave(filename=paste0("recall.pdf"),
       plot=gg,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


