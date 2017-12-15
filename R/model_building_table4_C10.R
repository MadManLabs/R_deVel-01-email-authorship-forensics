source("R/packages.R")


# Read ground truth 

dir("data/offline/C10")

json_file <- "data/offline/C10/ground-truth.json"


# Read data

data_all <- readRDS(file = "data/C10/processed_data.rds")

data_all <- 
  data_all %>% 
  select(-t,
         -blank_lines,
         -total_lines,
         -number_stopwords,
         -number_shortwords,
         -M,
         -V,
         -C) 


testing_data <- readRDS(file = "data/C10/unknown.rds")
names(testing_data)

testing_data <- 
  testing_data %>% 
  select(-t,
         -blank_lines,
         -total_lines,
         -number_stopwords,
         -number_shortwords,
         -M,
         -V,
         -C) 


names(data_all)

# Look at authors/dep.variable
data_all$candidate %>% table() %>% sort()


author_limit <- 50

# with 50
data_sub <- 
  data_all %>% 
  group_by(candidate) %>% 
  filter(n() >= author_limit) %>% 
  as.data.frame()

data_sub$candidate %>% table() %>% sort()

data <- data_sub

names(data)


# Z-standardize
data_z <- 
  data %>% 
  mutate_at(vars(-one_of("candidate","text")),
            funs(as.vector(scale(.))))

data_z[data_z == "NaN"] <- 0

model_data_all <- 
  data_z %>% 
  select(-text)%>% # this has to be fixed
  na.omit()

# Z-standardize testing
testing_data_z <- 
  testing_data %>% 
  mutate_at(vars(-one_of("candidate","text")),
            funs(as.vector(scale(.))))

testing_data_z[testing_data_z == "NaN"] <- 0

testing_data_z %<>% na.omit()

# Model 
model_data_all <- 
  data_z %>% 
  select(-text)%>% # this has to be fixed
  na.omit()


model_data <- 
  model_data_all %>% 
  group_by(candidate) %>% 
  sample_n(round(author_limit))

names(model_data)


# Splitting in training and test data
results <- data.frame()

training_data <- 
  model_data 

testing_data <- 
  testing_data_z



true <- fromJSON(txt = json_file, flatten=TRUE)  [[1]]

true$text <- true$`unknown-text`
true$`unknown-text` <- NULL

testing_data <- 
  left_join(testing_data,true,by = "text")

names(training_data)


# Tune 1
svm_tune1 <- 
    tune(svm, factor(candidate)~ ., data = training_data,
         kernel="radial", type = "C-classification",
         ranges=list(gamma = 10^(-5:1),
                     cost = 2^(-2:4))
         )
summary(svm_tune1)

## classification mode
# default with factor response:
model <- svm(candidate ~ 
             .,
             data = training_data,
             gamma = svm_tune1$best.parameters$gamma,
             cost = svm_tune1$best.parameters$cost,
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

t1 <- table(predicted = predictions,
                  actual_author = testing_data$`true-author`) ; t1

t2 <- round(table(predicted = predictions,
                  actual_author = testing_data$`true-author`) %>% prop.table(2),2); t2

(precision <- diag(t1) / rowSums(t1))
(recall <- (diag(t1) / colSums(t1)))

(f <- (2*(precision*recall))/(precision+recall))

accuracy <- length(which(predictions == testing_data$`true-author`))/length(predictions);accuracy

out_pre <- sum(precision,na.rm = TRUE)/3
out_rec <- sum(recall,na.rm = TRUE)/3
out_f <- f

results  <- 
  bind_rows(results,
                      data.frame(loop = q,
                                 precision = out_pre,
                                 recall = out_rec,
                                 f_score = f,
                                 accuracy = accuracy)
)

data.frame(predictions,testing_data$`true-author`)
data.frame(predictions)

