library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)

source(file="preprocessing.R")
#source(file="preprocessing_tibble.R")
str(olympic_dataset)

#keep only data from 1980 till now (10 olympic games)
#olympic_dataset = olympic_dataset[olympic_dataset$'Year' >= 1980,]

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
# ranger par ordre décroissant de taille les athletes
olympic_dataset <- drop_na(olympic_dataset, Height)
medals_per_height <- ddply(olympic_dataset,
                           .(Height, Sex),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )

max(medals_per_height$Medals)
filter(medals_per_height, medals_per_height$Medals == 941)

max(filter(medals_per_height, medals_per_height$Sex == "F")$Medals)
filter(medals_per_height, medals_per_height$Medals == 535 & medals_per_height$Sex=="F")

# display medals depending of height.                         
t <-ggplot(data=medals_per_height, aes(Height,Medals))+geom_col(position="dodge", aes(fill=Sex))

#+ stat_function(fun = dnorm, n = 10, args = list(mean = 180, sd = 10))
#+geom_line(data = medals_per_height,aes(x=Height, y=Medals))
t+labs(x="Height", y="Total Medals (Gold, Silver, Bronze)", title = "Total number of medals won by Athletes depending on their Height", subtitle="over the past 10 Olympic Games")


# it is gaussian. 

#*****************************************************************
# GOAL : idem but we will see if there is an evolution over the years

medals_year <- ddply(olympic_dataset,
                           .(Year, Sex),
                           function(x){
                             medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                             data.frame(Medals= medals)
                           }  )

# treat the case of winter and summer that are separate after 1992.
medals_year$Year[medals_year$Year=="1994"]<- "1992"
medals_year$Year[medals_year$Year=="1998"]<- "1996"
medals_year$Year[medals_year$Year=="2002"]<- "2000"
medals_year$Year[medals_year$Year=="2006"]<- "2004"
medals_year$Year[medals_year$Year=="2010"]<- "2008"
medals_year$Year[medals_year$Year=="2014"]<- "2012"

ggplot(medals_year,aes(Year, Medals, group=Sex))+geom_col(position="dodge",aes(fill=Sex))+
  labs(x="Années", y="Total Medailles", title = "Evolution du nombre de médailles gagnées par sexe")

homme_femme <- ddply(olympic_dataset, .(Year), function(x){ data.frame(Proportion = sum(x$Sex=="F")/(sum(x$Sex=="F")+sum(x$Sex=="M")))})

homme_femme$Year<- as.numeric(as.character(homme_femme$Year))
ggplot(homme_femme, aes(Year, Proportion))+geom_line(size=1, color="orchid2")+ labs(x="Années", y="Proportion des femmes", title="Evolution dans les temps de la proportion des femmes parmis les athlètes des Jeux Olympiques")

#**********************************F****************************************************************
#GOAL : top winners in summer/in winter

medals_in_summer <- ddply(filter(olympic_dataset,Season == 'Summer'),
                                .(region),
                                function(x){
                                  medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                                  data.frame(Medals= medals)
                                }  )
str(medals_in_summer)
medals_in_summer <- top_n(arrange(medals_in_summer, desc(Medals)),10,Medals)
medals_in_summer$region <- factor(medals_in_summer$region, levels = medals_in_summer$region[order(desc(medals_in_summer$Medals))])
t<- ggplot(data=medals_in_summer,aes(region, Medals))+geom_col(position="dodge",aes(fill=region))
t+labs(x="Regions", y="Sum Medals", title = "Top 10 regions that won the most medals in Summer")+ guides(fill="none")



medals_in_winter <- ddply(filter(olympic_dataset,Season == 'Winter'),
                          .(region),
                          function(x){
                            medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                            data.frame(Medals= medals)
                          }  )
str(medals_in_winter)
medals_in_winter <- top_n(arrange(medals_in_winter, desc(Medals)),10,Medals)
medals_in_winter$region <- factor(medals_in_winter$region, levels = medals_in_winter$region[order(desc(medals_in_winter$Medals))])
# Seulement 3 pays sont dans le top 10 (top 5) en été ET en hiver : la Russie, l'Allemagne, les USA


# **************************************************************************************************************************
# GOAL : étudier l'évolution PIB des pays gagnant
# DEPRECEATED

# Medailles/region, toutes années confondues
t<- ggplot(data=medals_in_winter,aes(region, Medals))+geom_col(position="dodge",aes(fill=region))
t+labs(x="Regions", y="Sum Medals", title = "Top 10 regions that won the most medals in Winter")+ guides(fill="none")



medals_region<- ddply(olympic_dataset,
                          .(region),
                          function(x){
                            medals <- sum(x$Medal=="Gold" | x$Medal=="Sylver" | x$Medal =="Bronze")
                            data.frame(Medals= medals)
                          }  )
medals_region<- top_n(arrange(medals_region, desc(Medals)),10,Medals)
medals_region$region <- factor(medals_region$region, levels = medals_region$region[order(desc(medals_region$Medals))])
t<- ggplot(data=medals_region,aes(region, Medals))+geom_col(position="dodge",aes(fill=region))
t+labs(x="Regions", y="Sum Medals", title = "Top 10 regions that won the most medals")+ guides(fill="none")

#x= reorder(NOC,-Gold), y = Gold)
#ggplot(top_medal, aes(x = reorder(NOC, -Total), y = Total)) + geom_bar(stat="identity", aes(fill=NOC)) + coord_flip()
# TODO : faire un graphe avec chaque top 3 en fonction du PIB de 1980 ? 2016

# OU ALORS : afficher l'avolution du PIB et donner les rangs des pays chaque année

# REMARQUE : en 1980, c'était surtout des pays de l'europ ede l'est et du nord qui gagnaient. 

PIBtop10medals<-filter(PIBdata,PIBdata$region %in% medals_region$region)
PIBtop10medals <- arrange(filter(PIBtop10medals, PIBtop10medals$Year >= "1968"), Year)
PIBtop10medals$PIB <- as.numeric(as.character(PIBtop10medals$PIB))

ggplot(data=PIBtop10medals,aes(Year,PIB, group=region))+geom_line(aes(color=region), size=1)+labs(x="Regions", y="PIB", title = "PIB evolution of Top 10 regions that won the most medals")

#******************************************************************
# GOAL : see PIB of top 10 country in 2016
# DEPRECEATED
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
# DEPRECEATED

PIB_10 <- arrange(filter(PIBdata,PIBdata$region %in% top5_2016$region), Year)
t <- ggplot(data=PIB_10, aes(Year,PIB,group=region))+geom_line(aes(color=region), size =1)
t+labs(x="Year", y="PIB", title = "PIB evolution over 1980 to 2016 of top 10 winners in 2016")


# Rank : 1 USA, 2 Germany, 3 UK, 4 Russia, 5 China, 6 France, 7 Australia, 8 Italy, 9 canada, 10 Japan

#*****************************************************************
# GOAL : influence des guerres 
# constat : pas de JO pendant les guerres mondiales
#DEPRECEATED


medalsTot <- ddply(olympic_dataset,
                   .(Year, region),
                   function(x){
                     gold<- sum(x$Medal=="Gold")
                     silver<- sum(x$Medal =="Silver")
                     bronze<- sum(x$Medal =="Bronze")
                     data.frame(Medals=gold+bronze+silver)
                   })
medalsMean <-ddply(medalsTot,
                   .(region),
                   summarize, Mean= mean(Medals))

medals1968 <- ddply(filter(olympic_dataset,olympic_dataset$Year=="1968"), 
                              .(region), 
                              function(x){
                                gold<- sum(x$Medal=="Gold")
                                silver<- sum(x$Medal =="Silver")
                                bronze<- sum(x$Medal =="Bronze")
                                data.frame(Medals=gold+bronze+silver)
                              }  )


medals1968 <- arrange(medals1968, desc(Medals))
top10_68 <- top_n(medals1968,10, Medals)
top10_68[nrow(top10_68)+1,]<- filter(medalsMean, medalsMean$region=="France")
levels <- levels(top10_68$region)
levels[length(levels)+1]<- 'France Mean'
top10_68$region<- factor(top10_68$region, levels = levels)
top10_68[nrow(top10_68),]$region <- "France Mean"
#top10_68

t <- ggplot(data=top10_68, aes(region, Medals )) + geom_col(aes(fill=region)) + guides(fill="none")
t+labs(x="Regions", y="Medals", title = "Top 10 regions that won the most medals in 1968")

# On voit ici que France est bien en dessous de sa moyenne.
# TODO : justifier pour d'autres événements et on peut se permettre de ne présenter que france et france Mean.
# Italie 1936
# Algérie 1956
# chine 1952
# Portugal 1972
# Egypte 1980

#******************************************************************
#GOAL : set a column of country rank of PIB

# SUJETS d'OUVERTURE 
# Question : quel est le rang PIB de chaque pays remportant le plus de médaille ? 
# est-ce que l'ordre change au fil des ans ? et le PIB ?
# quelle est la place dans les JO des Pays qui ont le PIB le plus important ?

PIBdatasummarise <- ddply(PIB_dataset,  .(region), summarise, MeanPIB= mean(PIB) )


