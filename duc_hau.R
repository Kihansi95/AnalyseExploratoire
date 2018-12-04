library(ggplot2)
library(plyr)
library(stringr)

library(dplyr)      # NB: place after plyr for being sure it will override some methods
library(tidyr)

source(file="preprocessing_tibble.R")
str(olympic_dataset)

# =========== Explore medal gained ===========
# Testing out medal number (Bronze + Gold + Silver)
## =========== By Country ===========
# Count medal each country got
medal_per_country <- olympic_dataset %>% 
    count(NOC, Medal) %>%
    spread(Medal, n, fill=0)

# Get top n countries that got most medal
medal_per_country$Total = medal_per_country$Bronze + medal_per_country$Gold + medal_per_country$Silver
top_medal = medal_per_country %>%
    arrange(desc(Total)) %>%
    top_n(20)

ggplot(top_medal, aes(x = reorder(NOC, -Total), y = Total)) + 
    geom_bar(stat="identity", aes(fill=NOC)) + 
    coord_flip() +
    labs(x="Country", y="Total medal", title = "Top 20 countries that have won the most medal since 1890")

# Show top country with the medal quality
top_medal2 = olympic_dataset %>% 
    count(NOC, Medal) %>%
    subset(Medal != "None") %>%
    filter(NOC %in% top_medal$NOC) 

ggplot(top_medal2, aes(x = NOC, y = n, fill = Medal)) + 
    geom_bar(stat="identity") + 
    coord_flip() +
    labs(x="Country", y="Medal Number", title = "Top 20 countries with medal qualities")

#Investigate USA:
olympic_dataset %>%
    filter(NOC == "USA", Medal != 'None') %>%
    ddply(c("Year"), summarise, Total_medal = n()) %>%
    ggplot(aes(x = Year, y = Total_medal)) + 
    geom_bar(stat = "identity") +
    geom_smooth() +
    labs(x="Year", y="Medal Number", title = "Investigate in USA's performance")

## =========== By Age ===========
# Show medal by age
olympic_dataset %>%
    count(Age, Medal) %>%
    spread(Medal, n, fill=0) %>%
    mutate(Total = Bronze + Gold + Silver) %>%
    ggplot(aes(x = Age, y = Total)) + 
    geom_bar(stat = "identity", fill="steelblue") + 
    #geom_smooth() +
    labs(x="Âge", y="Nomre de médaille", title = "Nombre de médaille selon l'âge de participants", subtitle='L\'âge le plus jeune est à 10')

# Medal quality by age
olympic_dataset %>% 
    count(Age, Medal) %>%
    subset(Medal != "None") %>%
    ggplot(aes(x = Age, y = n, fill = Medal)) + geom_bar(stat = "identity") +
    labs(x="Âge", y="Nomre de médaille", title = "Qualité de médaille selon l'âge")

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
# alpha = max(medal_per_age$Nb_participant) # This one is not really useful
alpha = medal_per_age$Nb_participant

medal_per_age = medal_per_age %>%
    mutate(
        Performance = (Nb_participant/alpha) * (Nb_medal / Nb_participant)
    )

min_age = min(olympic_dataset$Age, na.rm=TRUE)
max_age = max(olympic_dataset$Age, na.rm=TRUE)
ggplot(medal_per_age, aes(x = Age, y = Performance)) + 
    geom_bar(stat = "identity") + 
    coord_cartesian(xlim=c(10, 75))     # Filter some noise

## =========== Country ===========
medal_per_country <- olympic_dataset %>% 
    count(NOC, Medal) %>%
    spread(Medal, n, fill=0) %>%
    mutate(
        Nb_participant = Bronze + Gold + Silver + None,
        Nb_medal = Bronze + Gold + Silver
    )

# Parameters
alpha = mean(medal_per_country$Nb_participant)  # coef of performance
n = 20 # top n countries

# Compute the performance and choose only the top n
medal_per_country = medal_per_country %>%
    mutate(
        Performance = (Nb_participant/alpha) * (Nb_medal / Nb_participant)
    )%>%
    arrange(desc(Performance)) %>%
    top_n(n)

ggplot(medal_per_country, aes(x = NOC, y = Performance)) + 
    geom_bar(stat = "identity") +
    labs(x="Pays", y="Performance", title = "Chance d'avoir un médail pour un sportif", subtitle="Le plus haute valeur, le mieux chance qu'on a. Valeur 1 veut dire que 100% le pays gagnera de médaille")

# =========== Evolution ===========
# Check out if countries start to recrute sportman at age 25
age_evolution = olympic_dataset %>%
    subset(Medal != "None")

ggplot(age_evolution) + 
    geom_point( aes(x = Year, y = Age, color = Medal)) + 
    geom_smooth(aes(x = Year, y = Age)) + 
    labs(x="Age", y="Année", title = "Distribution d'age de participants", subtitle="De 1986 à 2016")

ggplot(age_evolution) + 
    geom_boxplot(aes(x = as.factor(Year), y = Age)) +
    labs(x="Age", y="Année", title = "Distribution d'age de participants", subtitle="De 1986 à 2016")
