library(ggplot2)
library(plyr)
library(stringr)

### Load Table_conso
tab_consommation = read.csv2("dataset/Table_conso.csv")
head(tab_consommation)
str(tab_consommation)

###   Ractorize qualitative column
#     transform all [0:1] to boolean
index_boolean = c("algra", "alsuc", "diete", "enric", "rien") # index column that indice boolean in tab_conso
tab_consommation[, index_boolean] = tab_consommation[, index_boolean] == 1
rm(index_boolean)

#     factor those qualitative
index_factor