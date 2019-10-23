# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("class")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(class)

setwd("c:/temp/cpsc375hw3")

# 2
# a. Load the data. Column 1 ("Code") is the anonymized subject code and will not be used here. 
# Columns 2-10 are the 9 features. Column 11 is the diagnosis: [B]enign or [M]alignant.

data <- read_csv("breast-cancer-wisconsin.csv")

# 2ai. How many total cases are there in the data?: ___ 
nrow(data)

# 2aii. How many [B]enign cases are there in the data?: ___ 
nrow(data %>% filter(Class == "B"))

# 2aiii. How many [M]alignant cases are there in the data?: ___ 
nrow(data %>% filter(Class == "M"))

# b. Run the k-means clustering algorithm using all the rows and all the 9 features. 
# Use k=2, nstart=10. 
# i. What should be the value of k? k = ___ (already given) 
# ii. Give R code: 
km <- kmeans(data[,2:10], centers = 2, nstart = 10)
data$cluster <- km$cluster
data$cluster <- as.factor(data$cluster)

# c. Evaluation: Compare the resulting clusters with the known diagnosis . 
# i. What is the contingency table of your clustering? 
# (Note: you can arbitrarily assign cluster 1/2 to Benign/Malignant) 

# cluster = 1, class == "B"
k1M <- nrow(data %>% filter(cluster == 1 & Class == "M"))
k1B <- nrow(data %>% filter(cluster == 1 & Class == "B"))

# cluster = 1, class == "M"
k2M <- nrow(data %>% filter(cluster == 2 & Class == "M"))
k2B <- nrow(data %>% filter(cluster == 2 & Class == "B"))

contingencyTable <- matrix(c(c(k1M, k1B), c(k2M, k2B)), nrow = 2, ncol = 2)
colnames(contingencyTable) <- c("M", "B")
rownames(contingencyTable) <- paste("Cluster", c(1, 2))