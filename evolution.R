
age_evolution = olympic_dataset %>%
  subset(Medal != "None")
  
ggplot(age_evolution) + geom_point( aes(x = Year, y = Age, color = Medal))+ geom_smooth(aes(x = as.int(Year), y = Age))

age_evolution$Year = as.factor(age_evolution$Year)
ggplot(age_evolution) + geom_boxplot(aes(x = Year, y = Age)) + geom_smooth()
