library(ggplot2)
library(plyr)
library(stringr)

library(dplyr)      # NB: place after plyr for being sure it will override some methods
library(tidyr)

source(file="preprocessing_tibble.R")
str(olympic_dataset)

# TODO may not need
# correlation_matrix = cor(as.matrix(olympic_dataset), method = c("pearson", "kendall", "spearman"))

# =========== By Country ===========
# Count medal each country got
medal_per_country <- olympic_dataset %>% 
    count(NOC, Medal) %>%
    spread(Medal, n, fill=0)

# Get top n countries that got most medal
medal_per_country$Total = medal_per_country$Bronze + medal_per_country$Gold + medal_per_country$Silver
top_medal = medal_per_country %>%
    arrange(desc(Total)) %>%
    top_n(20)

ggplot(top_medal, aes(x = reorder(NOC, -Total), y = Total)) + geom_bar(stat="identity", aes(fill=NOC)) + coord_flip()

# Show top country with the medal quality
top_medal2 = olympic_dataset %>% 
    count(NOC, Medal) %>%
    subset(Medal != "None") %>%
    filter(NOC %in% top_medal$NOC)

ggplot(top_medal2, aes(x = NOC, y = n, fill = Medal)) + geom_bar(stat="identity") + coord_flip()

#Investigate USA:
usa = olympic_dataset %>%
    filter(NOC == "USA", Medal != 'None') %>%
    ddply(c("Year"), summarise, Total_medal = n()) %>%
    ggplot(aes(x = Year, y = Total_medal)) + geom_bar(stat = "identity")

# =========== By Age ===========
# Show medal by age
olympic_dataset %>%
    count(Age, Medal) %>%
    spread(Medal, n, fill=0) %>%
    mutate(Total = Bronze + Gold + Silver) %>%
    ggplot(aes(x = Age, y = Total)) + geom_bar(stat = "identity")

# Medal quality by age
olympic_dataset %>% 
    count(Age, Medal) %>%
    subset(Medal != "None") %>%
    ggplot(aes(x = Age, y = n, fill = Medal)) + geom_bar(stat = "identity")

# =========== Performance ===========
# Maybe the number of medal says nothing, may be it's simply many participant figuring that category
# Performance should measure #Medal/#Participant = (#Gold + #Silver + #Bronze)/(#G + #S + #B + #None)
# **Problem**: category X has only 1 participant but is winner => 100% performance.
## =========== Age ===========
medal_per_age <- olympic_dataset %>%
    count(Age, Medal) %>%
    spread(Medal, n, fill=0) %>%
    mutate(
        Nb_participant = Bronze + Gold + Silver + None,
        Nb_medal = Bronze + Gold + Silver
    )

# We can test with various twist alpha:
alpha = mean(medal_per_age$Nb_participant)
# alpha = max(medal_per_age$Nb_participant)
alpha = medal_per_age$Nb_participant

medal_per_age = medal_per_age %>%
    mutate(
        Performance = (Nb_participant/alpha) * (Nb_medal / Nb_participant)
    )

ggplot(medal_per_age, aes(x = Age, y = Performance)) + geom_bar(stat = "identity")

## =========== Country ===========
medal_per_country <- olympic_dataset %>% 
    count(NOC, Medal) %>%
    spread(Medal, n, fill=0)