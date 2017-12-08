# Install Packages

#install.packages("install.load")

install.load::install_load("plyr",
                           "tidyverse",
                           "magrittr",
                           "stringr",
                           "ggplot2",
                           "tm",
                           "e1071", # collection of ML algorithms, here we get the SVM algorithm from
                           "tuneR",  #cross validation and model optimizaton
                           #"openNLP", #Natural Language Processing
                           #"openNLPmodels.en" # English model files
                           "hrbrthemes",
                           "Rcpp",
                           "RcppArmadillo",
                           "BH",
                           "DT",
                           "formattable",
                           "extrafont",
                           "ggrepel",
                           "wordcloud2",
                           "reshape2") #

loadfonts(device="pdf")  
