library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)

source(file="preprocessing.R")
str(olympic_dataset)

#keep only data from 1980 till now (10 olympic games)
olympic_dataset = olympic_dataset[olympic_dataset$'Year' >= 1980,]

#************************************************************************
# GOAL : Count medal each country got
medal_per_country <- ddply(olympic_dataset, 
                           .(NOC), 
                           function(x){
                             gold<- sum(x$Medal=="Gold")
                             silver<- sum(x$Medal =="Silver")
                             bronze<- sum(x$Medal =="Bronze")
                             data.frame(Gold=gold, Bronze=bronze)
                             }  )


#filter medals dols != 0
medal_per_country <- medal_per_country[medal_per_country$Gold!=0,]

#display NOC with gold medals over the past 10 olympics games
ggplot(data=medal_per_country, aes(NOC,Gold))+geom_col()

#*************************************************************

# GOALS : see if the height of athletes has an influence of winning medals
# ranger par ordre décroissant de taille les athletes
medals_per_height <- ddply(filter(olympic_dataset, olympic_dataset$Height!="NA"),
                           .(Height),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )

# display medals depending of height.                         
ggplot(data=medals_per_height,aes(Height, Medals))+geom_col()
# it is gaussian. 

#*****************************************************************
# GOAL : idem but we will see if there is an evolution over the years

# GOALS : see if the height of athletes has an influence of winning medals
# ranger par ordre décroissant de taille les athletes
medals_per_height_year <- ddply(filter(olympic_dataset, olympic_dataset$Height!="NA"),
                           .(Height, Year),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )

# display medals depending of height and year.                         
ggplot(data=medals_per_height_year,aes(Height, Medals))+geom_col(aes(fill=Year))


