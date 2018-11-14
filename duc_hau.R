library(ggplot2)
library(plyr)
library(stringr)

source(file="preprocessing.R")
str(olympic_dataset)

correlation_matrix = cor(as.matrix(olympic_dataset), method = c("pearson", "kendall", "spearman"))

# Count medal each country got
medal_per_country <- ddply(olympic_dataset, 
                   c("NOC", "Medal"), 
                   summarise, 
                   Gold=sum(olympic_dataset$Medal=="Gold"), 
                   Bronze=sum(olympic_dataset$Medal=="Bronze") )
