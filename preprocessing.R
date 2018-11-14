### Data Preprocessing 

## olympic-history-athletes-and-results
olympic_dataset = read.csv2("data/olympic-history-athletes-and-results/athlete_events.csv",sep=",", dec=".")
str(olympic_dataset)


### Maybe use full in future:
## === turn 0,1 into boolean for multiple columns:
# index_boolean = c("algra", "alsuc", "diete", "enric", "rien") # index column that indice boolean in tab_conso
# tab_consommation[, index_boolean] = tab_consommation[, index_boolean] == 1