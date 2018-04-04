# loading the survival library for coxph regression
library(survival)

# setting the working directory
setwd("/Users/SriduttN/Desktop/JP Spring Semester")

# loading in the dataset
analysisDataset <- read.csv("Political Atrocities by Civil War - 1995 to 2007 Cleaned.csv", header = TRUE)
# deleting the first column
analysisDataset <- analysisDataset[, -1]

# creating numerical representations for categorical variables
PerpMilitary <- rep(NA, nrow(analysisDataset))
PerpPolice <- rep(NA, nrow(analysisDataset))
PerpOther <- rep(NA, nrow(analysisDataset))

for (i in 1:nrow(analysisDataset)) {
  if (analysisDataset$Perp.State.Military[i] == '') {
    PerpMilitary[i] <- 0
  } else {
    PerpMilitary[i] <- 1
  }
  if (analysisDataset$Perp.State.Police[i] == '') {
    PerpPolice[i] <- 0
  } else {
    PerpPolice[i] <- 1
  }
  if (analysisDataset$Perp.State.Other[i] == '') {
    PerpOther[i] <- 0
  } else {
    PerpOther[i] <- 1
  }
}

# cbinding the categorical variables to the analysis dataset
analysisDataset <- cbind(analysisDataset, PerpMilitary, PerpPolice, PerpOther)

# creating vectors for the explanatory variables included in the model
gdpPerCapita <- rep(NA, nrow(analysisDataset))
logPopulation <- rep(NA, nrow(analysisDataset))
ethnFrac <- rep(NA, nrow(analysisDataset))
relFrac <- rep(NA, nrow(analysisDataset))
internationalizedAlly <- rep(NA, nrow(analysisDataset))
internationalizedOpponent <- rep(NA, nrow(analysisDataset))
aFricanContinent <- rep(NA, nrow(analysisDataset))
logDeaths <- rep(NA, nrow(analysisDataset))
polity <- rep(NA, nrow(analysisDataset))
logMountainous <- rep(NA, nrow(analysisDataset))

# cbinding the columns above 
analysisDataset <- cbind(analysisDataset,
                         gdpPerCapita,
                         logPopulation,
                         ethnFrac,
                         relFrac,
                         internationalizedAlly,
                         internationalizedOpponent,
                         aFricanContinent, 
                         logDeaths,
                         polity,
                         logMountainous)


# exporting the dataset
write.csv(analysisDataset, file = "Political Atrocities Replication File.csv")
