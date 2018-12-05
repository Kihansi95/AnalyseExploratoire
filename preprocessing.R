### Data Preprocessing 

### Maybe useful in the future:
## === turn 0,1 into boolean for multiple columns:
# index_boolean = c("algra", "alsuc", "diete", "enric", "rien") # index column that indice boolean in tab_conso
# tab_consommation[, index_boolean] = tab_consommation[, index_boolean] == 1

## ==================================== ##
## olympic-history-athletes-and-results ##
## ==================================== ##
# load dataset
olympic_dataset = read.csv2("data/olympic-history-athletes-and-results/athlete_events.csv",sep=",", dec=".")
noc_dataset =     read.csv2("data/olympic-history-athletes-and-results/noc_regions.csv",sep=",", dec=".")

# For athletes got NA medal, will receive "None" medal
levels                      <- levels(olympic_dataset$Medal)
levels[length(levels) + 1]  <- "None"                                     # Get levels and add "None"
olympic_dataset$Medal <- factor(olympic_dataset$Medal, levels = levels)   # refactor include "None" as a factor level
# Not needed, it will complicate the situation
olympic_dataset$Medal[is.na(olympic_dataset$Medal)] <- "None"             # replace NA with "None"

separate(olympic_dataset, Games, c("Year_Games", "Season"), sep = " ")
olympic_dataset$Year_Games <- NULL

# Transform to factor
factor_cols <- c('Sex', 'NOC', 'Medal', 'Sport')
olympic_dataset[factor_cols] <- lapply(olympic_dataset[factor_cols], factor)




#PIBMetadata_dataset = read.csv2("data/PIB-grossDomesticProduct_1980-2017/WID_Metadata_30112018-130904.csv", sep=",", dec=".")
#PIB_dataset = read.csv2("data/PIB-grossDomesticProduct_1980-2017/WID_Data_30112018-130904.csv", 
                        #header = FALSE,col.names =c("region", "Indicator", "Percentile", "Year", "PIB"),sep=";", dec=".")
PIB_dataset= read.csv2("data/PIB-grossDomesticProduct_1896-2017/WID_Data_30112018-144413.csv", 
                        header = FALSE,col.names =c("region", "Indicator", "Percentile", "Year", "PIB"),sep=";", dec=".")
# delete row that have the value "null" for country :
#PIB_dataset = PIB_dataset[-which(PIB_dataset$region =="null"), ]


