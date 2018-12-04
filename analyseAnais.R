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
olympic_dataset = left_join(olympic_dataset, noc_dataset, by= c('NOC'))

# filter PIB dataset 
PIBdata<-PIB_dataset[,c("region","Year","PIB")]
#CHANGE REGION NAME BY HAND ...
# Remarq : we must change factors to be able to change a Region value !!!
levels <- levels(PIBdata$region)
levels[length(levels)+1]<- 'Antigua'
levels[length(levels)+1]<- 'Brunei'
levels[length(levels)+1]<- 'Cape Verde'
levels[length(levels)+1]<- 'Democratic Republic of the Congo'
levels[length(levels)+1]<- 'Russia'
levels[length(levels)+1]<- 'AR Syria'
levels[length(levels)+1]<- 'Trinidad'
levels[length(levels)+1]<- 'UK'


PIBdata$region<- factor(PIBdata$region, levels = levels)
PIBdata$region[PIBdata$region=='Antigua and Barbuda']<- 'Antigua'
PIBdata$region[PIBdata$region=='Brunei Darussalam']<- 'Brunei'
PIBdata$region[PIBdata$region=='Cabo Verde']<- 'Cape Verde'
PIBdata$region[PIBdata$region=='DR Congo']<- 'Democratic Republic of the Congo'
PIBdata$region[PIBdata$region=='Russian Federation']<- 'Russia'
PIBdata$region[PIBdata$region=='Syrian Arab Republic']<- 'AR Syria'
PIBdata$region[PIBdata$region=='Trinidad and Tobago']<- 'Trinidad'
PIBdata$region[PIBdata$region=='United Kingdom']<- 'UK'




# Add PIB Value (Gross Domestic Product GDP )
# ATTENTION, les noms des pays (Team) dans PIBdata sont un peu différent que ceux dans olympic_dataset 
#Par exemple : dans PIBdata, on a USA alors qu'on a Unighted States dans olympic dataset 
#olympic_dataset = left_join(olympic_dataset, PIBdata, by=c('region','Year'))

# Factor for Year column
olympic_dataset['Year'] <- lapply(olympic_dataset['Year'], factor)


#************************************************************************
# GOAL : Count medal each country got
medal_per_country <- ddply(olympic_dataset, 
                           .(region), 
                           function(x){
                             gold <- sum(x$Medal=="Gold")
                             silver<- sum(x$Medal =="Silver")
                             bronze<- sum(x$Medal =="Bronze")
                             data.frame(Gold=gold, Bronze=bronze, Silver=silver)
                             }  )


#filter medals godl != 0
medal_per_country <- medal_per_country[medal_per_country$Gold!=0,]
top_gold = top_n(arrange(medal_per_country,desc(Gold)), 20, Gold)

top_gold['region'] <- lapply(top_gold['region'], factor)

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

#************************************************************************
# GOAL : See if ther is correlation between PIB and maw medals 
medal_per_country_80 <- ddply(filter(olympic_dataset,olympic_dataset$Year=="1980"), 
                           .(region), 
                           function(x){
                                gold<- sum(x$Medal=="Gold")
                                silver<- sum(x$Medal =="Silver")
                                bronze<- sum(x$Medal =="Bronze")
                             data.frame(Medals=gold+bronze+silver)
                           }  )


medal_per_country_80 <- arrange(medal_per_country_80, desc(Medals))
medal_per_country_80 = left_join(medal_per_country_80, filter(PIBdata,PIBdata$Year=="1980"), by=c('region'))
top3 <- top_n(medal_per_country_80,3, Medals)

top3['region'] <- lapply(top3['region'], factor)

#display NOC with gold medals over the past 10 olympics games
t <- ggplot(data=top3, aes(region, PIB)) + geom_col(aes(fill=region)) + guides(fill="none")
t+labs(x="Regions", y="Sum Medals", title = "Top 3 regions that won the most medals 1980")
#x= reorder(NOC,-Gold), y = Gold)
#ggplot(top_medal, aes(x = reorder(NOC, -Total), y = Total)) + geom_bar(stat="identity", aes(fill=NOC)) + coord_flip()
# TODO : faire un graphe avec chaque top 3 en fonction du PIB de 1980 à 2016


#******************************************************************
# GOAL : see PIB of top 10 country in 2016
PIBdata2016 <- arrange(filter(PIBdata,PIBdata$Year=="2016"), desc(PIB))
top2016PIB <-top_n(PIBdata2016, 10, PIB)
# Reorder factor : 
top2016PIB$region <- factor(top2016PIB$region, levels = top2016PIB$region[order(desc(top2016PIB$PIB))])
#top2016PIB$region <-data.frame(lapply(top2016PIB$region, as.character), stringAsFactors=FALSE)

t <- ggplot(data=top2016PIB, aes(region, PIB)) + geom_col(aes(fill=region)) + guides(fill="none")
t+labs(x="Regions", y="PIB 2016", title = "Top 10 regions that have the best PIB in 2016")

#********************************************************************
# GOAL : see who won the most medals in 2016
medal_per_country2016 <- ddply(filter(olympic_dataset,olympic_dataset$Year=="2016"), 
                              .(region), 
                              function(x){
                                gold<- sum(x$Medal=="Gold")
                                silver<- sum(x$Medal =="Silver")
                                bronze<- sum(x$Medal =="Bronze")
                                data.frame(Medals=gold+bronze+silver)
                              }  )


medal_per_country2016 <- arrange(medal_per_country2016, desc(Medals))
medal_per_country2016 = left_join(medal_per_country2016, PIBdata2016, by=c('region'))
top5_2016 <- top_n(medal_per_country2016,10, Medals)

top5_2016['region'] <- lapply(top5_2016['region'], factor)
top5_2016$region <- factor(top5_2016$region, levels = top5_2016$region[order(desc(top5_2016$PIB))])

t <- ggplot(data=top5_2016, aes(region, PIB)) + geom_col(aes(fill=region)) + guides(fill="none")
t+labs(x="Regions", y="medals", title = "Top 10 regions that won the most medals in 2016")

#********************************************************************
# GOALS : see PIB evolution over 1980 to 2016 of top 10 winners in 2016

PIB_10 <- arrange(filter(PIBdata,PIBdata$region %in% top5_2016$region), Year)
t <- ggplot(data=PIB_10, aes(Year,PIB,group=region))+geom_line(aes(color=region), size =1)
t+labs(x="Year", y="PIB", title = "PIB evolution over 1980 to 2016 of top 10 winners in 2016")


# Rank : 1 USA, 2 Germany, 3 UK, 4 Russia, 5 China, 6 France, 7 Australia, 8 Italy, 9 canada, 10 Japan

#*****************************************************************
#GOAL : set a column of country rank of PIB
# TODO

# Question : quel est le rang PIB de chaque pays remportant le plus de médaille ? 
# est-ce que l'ordre change au fil des ans ? et le PIB ?
# quelle est la place dans les JO des Pays qui ont le PIB le plus important ?

PIBdatasummarise <- ddply(PIB_dataset,  .(region), summarise, MeanPIB= mean(PIB) )


