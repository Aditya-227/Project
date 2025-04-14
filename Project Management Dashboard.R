install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("readr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
data <- read.csv(file.choose())
head(data)
sum(is.na(data))
data$Genre <- as.factor(data$Genre)
str(data)
summary(data)
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = 'blue', color = 'black', alpha = 0.7) +
  ggtitle("Age Distribution of Customers") +
  theme_minimal()
ggplot(data, aes(x = Spending.Score..1.100.)) +
  geom_histogram(bins = 30, fill = 'green', color = 'black', alpha = 0.7) +
  ggtitle("Spending Score Distribution") +
  theme_minimal()
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(aes(color = Genre), alpha = 0.7) +
  ggtitle("Annual Income vs Spending Score") +
  theme_minimal()
data_cluster <- data %>% select(Annual.Income..k.., Spending.Score..1.100.)
data_cluster_scaled <- scale(data_cluster)
set.seed(123)
kmeans_result <- kmeans(data_cluster_scaled, centers = 4, nstart = 25)
data$Cluster <- as.factor(kmeans_result$cluster)
head(data)
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) +
  geom_point(alpha = 0.7) +
  ggtitle("Customer Segments") +
  theme_minimal()