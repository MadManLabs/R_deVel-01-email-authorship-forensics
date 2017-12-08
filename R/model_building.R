#######################################
##One-dimensional case: number of words
#######################################

# Read data

data_all <- readRDS(file = "data/processed_data1.rds")
names(data_all)


# Look at authors/dep.variable
data_all$candidate %>% table() %>% sort()

data_sub <- 
  data_all %>% 
  group_by(candidate) %>% 
  filter(n() >= 50) %>% 
  as.data.frame()

data_sub$candidate %>% table() %>% sort()


data <- 
  data_sub %>% 
  select(-t,
         -blank_lines,
         -total_lines,
         -number_stopwords,
         -number_shortwords) 

names(data)

# Z-standardize
data_z <- 
  data %>% 
  mutate_at(vars(-one_of("candidate","text")),
            funs( (.-mean(.,na.rm = T))/sd(.,na.rm = T))) 


model_data <- data_z %<>% na.omit()


# Get frequencies
ggplot(data = model_data) +
  geom_bar(aes(x = candidate),stat = "count")+
  geom_text(aes(x = candidate,label = ..count..),nudge_y = 20,stat = "count")
  theme(axis.text = element_text(angle = 90, hjust = 1,vjust = 1)) +
  labs(title = "Author Frequency")
  

# For model data, create 
  

# Splitting in training and test data

sample_training <- sample(1:nrow(model_data),round(nrow(model_data)*0.75),replace = FALSE)

training <- c(1:nrow(model_data)) %in% sample_training
prediction <- !c(1:nrow(model_data)) %in% training

training_data <- 
  model_data [training,]

testing_data <- 
  model_data [prediction,]

names(training_data)

# Tune 1
svm_tune1 <- 
    tune(svm, factor(candidate)~ -text, data = training_data,
         kernel="radial", type = "C-classification",
         ranges=list(cost=10^(-1:2), 
                     gamma=c(.5,1,2)))


## classification mode
# default with factor response:
model <- svm(candidate ~ 
             -text,
             data = training_data,
             gamma = svm_tune1$best.parameters$gamma,
             cost = svm_tune1$best.parameters$gamma,
             kernel = "radial",
             type = "C-classification")

print(model)
summary(model)


# Calculate Decision Values and Probabilities
predictions <- predict(model,
                 testing_data,
                 decision.values = TRUE)
str(predictions)
attr(predictions, "decision.values")

# Check accuracy:
t1 <- round(table(predicted = predictions,
                  actual_author = testing_data$candidate) %>% prop.table(2),2); t1

names(predictions)
