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
forest_dataset = read.csv2("data/forest/API_AG.LND.FRST.ZS_DS2_en_csv_v2_10224801.csv",sep=",", dec=".")

# For athletes got NA medal, will receive "None" medal
levels                      <- levels(olympic_dataset$Medal)
levels[length(levels) + 1]  <- "None"                                     # Get levels and add "None"
olympic_dataset$Medal <- factor(olympic_dataset$Medal, levels = levels)   # refactor include "None" as a factor level
# Not needed, it will complicate the situation
olympic_dataset$Medal[is.na(olympic_dataset$Medal)] <- "None"             # replace NA with "None"

# Transform to factor
factor_cols <- c('Sex', 'NOC', 'Medal', 'Sport')
olympic_dataset[factor_cols] <- lapply(olympic_dataset[factor_cols], factor)

