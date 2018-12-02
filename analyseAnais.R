library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)

source(file="preprocessing.R")
#source(file="preprocessing_tibble.R")
str(olympic_dataset)

#keep only data from 1980 till now (10 olympic games)
olympic_dataset = olympic_dataset[olympic_dataset$'Year' >= 1980,]
olympic_dataset = left_join(olympic_dataset, noc_dataset, by='NOC')
olympic_dataset['Year'] <- lapply(olympic_dataset['Year'], factor)

#************************************************************************
# GOAL : Count medal each country got
medal_per_country <- ddply(olympic_dataset, 
                           .(region), 
                           function(x){
                             gold<- sum(x$Medal=="Gold")
                             silver<- sum(x$Medal =="Silver")
                             bronze<- sum(x$Medal =="Bronze")
                             data.frame(Gold=gold, Bronze=bronze)
                             }  )


#filter medals godl != 0
medal_per_country <- medal_per_country[medal_per_country$Gold!=0,]
top_gold = top_n(arrange(medal_per_country,desc(Gold)), 20, Gold)

#display NOC with gold medals over the past 10 olympics games
t <- ggplot(data=top_gold, aes(reorder(region, -Gold), Gold)) + geom_col(aes(fill=region)) + guides(fill="none")
t+labs(x="Regions", y="Gold medals", title = "Top 20 regions that won Gold medals in the past 10 Olympics Games ")
#x= reorder(NOC,-Gold), y = Gold)
#ggplot(top_medal, aes(x = reorder(NOC, -Total), y = Total)) + geom_bar(stat="identity", aes(fill=NOC)) + coord_flip()


#*************************************************************

# GOALS : see if the height of athletes has an influence of winning medals
# ranger par ordre d?croissant de taille les athletes
olympic_dataset <- drop_na(olympic_dataset, Height)
medals_per_height <- ddply(olympic_dataset,
                           .(Height, Sex),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )

# display medals depending of height.                         
t<- ggplot(data=medals_per_height,aes(Height, Medals))+geom_col(position="dodge", aes(fill=Sex))
#+geom_line(data = medals_per_height,aes(x=Height, y=Medals))
t+labs(x="Height", y="Total Medals (Gold, Silver, Bronze)", title = "Total number of medals won by Athletes depending on their Height", subtitle="over the past 10 Olympic Games")


# it is gaussian. 

#*****************************************************************
# GOAL : idem but we will see if there is an evolution over the years

# GOALS : see if the height of athletes has an influence of winning medals
# ranger par ordre d?croissant de taille les athletes
#filter(olympic_dataset,Height>=160 & Height <=190)
medals_per_height_year <- ddply(filter(olympic_dataset,Height>=160 & Height <=200),
                           .(Height, Year, Sex),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )
str(medals_per_height_year)

# display medals depending of height and year.                         
ggplot(data=medals_per_height_year,aes(Height, Medals, group=Year))+geom_col(position="dodge",aes(fill=Year))
#+stat_ydensity()
#+geom_boxplot()
#ggplot(data=medals_mean_height,aes(mean_height, Medals, group=Year))+geom_col(position="dodge",aes(fill=Year))


