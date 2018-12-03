### Data Preprocessing with tibble instead of dataframe

## ==================================== ##
## olympic-history-athletes-and-results ##
## ==================================== ##
library(readr)
olympic_dataset <- read_csv("data/olympic-history-athletes-and-results/athlete_events.csv")

# For athletes got NA medal, will receive "None" medal
olympic_dataset$Medal[is.na(olympic_dataset$Medal)] <- "None"             # replace NA with "None"

# Transform to factor
# olympic_dataset$Medal <- as.factor(olympic_dataset$Medal)
factor_cols <- c('Sex', 'NOC', 'Medal', 'Sport')
olympic_dataset[factor_cols] <- lapply(olympic_dataset[factor_cols], factor)

PIB_dataset = read.csv2("data/PIB-grossDomesticProduct_1896-2017/WID_Data_30112018-144413.csv", 
                            header = FALSE,col.names =c("region", "Indicator", "Percentile", "Year", "PIB"),sep=";", dec=".")

PIB_dataset = read_csv("data/PIB-grossDomesticProduct_1896-2017/WID_Data_30112018-144413.csv")
