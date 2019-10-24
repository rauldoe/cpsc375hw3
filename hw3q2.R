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

# class == "B", cluster = 1
kM1 <- nrow(data %>% filter(Class == "M" & cluster == 1))
kB1 <- nrow(data %>% filter(Class == "B" & cluster == 1))

# class == "M", cluster = 2
kM2 <- nrow(data %>% filter(Class == "M" & cluster == 2))
kB2 <- nrow(data %>% filter(Class == "B" & cluster == 2))

contingencyTable <- matrix(c(c(kM1, kB1), c(kM2, kB2)), nrow = 2, ncol = 2)
colnames(contingencyTable) <- paste("Predicted/Cluster", sep = "", c(1, 2))
rownames(contingencyTable) <- paste("Actual/", sep = "", c("M", "B"))
