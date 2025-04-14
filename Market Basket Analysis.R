install.packages("arules")
install.packages("arulesViz")
install.packages("dplyr")
library(arules)
library(arulesViz)
library(dplyr)
setwd("C:/Users/aarus/Downloads")
getwd()
retail_data <- read.csv("OnlineRetail.csv", stringsAsFactors = FALSE)
head(retail_data)
str(retail_data)
retail_clean <- retail_data %>%
  filter(!is.na(InvoiceNo) & !is.na(Description)) %>%              
  filter(!grepl("^C", InvoiceNo)) %>%                              
  filter(Quantity > 0) %>%                                         
  filter(Description != "", !is.na(Description)) %>%               
  filter(Country == "United Kingdom")                              
basket_data <- split(retail_clean$Description, retail_clean$InvoiceNo)
basket_data <- split(retail_clean$Description, retail_clean$InvoiceNo)
basket_data <- lapply(basket_data, function(x) {
  x <- iconv(x, to = "UTF-8", sub = "byte")
  x <- unique(trimws(x))
  x <- x[x != ""]
  return(x)
})
basket_data <- basket_data[lengths(basket_data) > 0]
basket_list <- as(basket_data, "transactions")
summary(basket_list)
rules <- apriori(basket_list, parameter = list(supp = 0.002, conf = 0.2, minlen = 2))
summary(rules)
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))
coffee_rules <- subset(rules_sorted, lhs %pin% "COFFEE")
inspect(coffee_rules)
plot(rules_sorted[1:20], method = "graph", engine = "htmlwidget")
plot(rules_sorted[1:20], method = "grouped")
write(rules_sorted, file = "final_market_basket_rules.csv", sep = ",", quote = TRUE)