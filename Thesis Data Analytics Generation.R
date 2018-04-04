# installing the libraries
library(foreign)
library(survival)

# exploration of fearon replication data
getwd()
setwd("/Users/SriduttN/Desktop/JP Spring Semester")

# reading in the replication data file
nimmagadda.replication <- read.csv("Political Atrocities Replication File_Final.csv", header = TRUE)

# doing some manipulation to add variables to the dataset
nimmagadda.replication$logDeaths <- log(nimmagadda.replication$Deaths.Number)
logPopulation <- log(nimmagadda.replication$population)
logGDP.capita <- log(nimmagadda.replication$gdpPerCapita)

# creating a vector of 1s to indicate the civil war eventually ended
DidWarEnd <- rep(1, nrow(nimmagadda.replication))

# month length
month.length <- 365.25/12
months.3 <- month.length*3
months.6 <- month.length*6
year <- 365.25

# did the war end within three months?
termination.3months <- rep(NA, nrow(nimmagadda.replication))
for (i in 1:nrow(nimmagadda.replication)) {
  if (nimmagadda.replication$violDiff[i] <= months.3) {
    termination.3months[i] <- 1
  } else {
    termination.3months[i] <- 0
  }
}


# did the war end within six months?
termination.6months <- rep(NA, nrow(nimmagadda.replication))
for (i in 1:nrow(nimmagadda.replication)) {
  if (nimmagadda.replication$violDiff[i] <= months.6) {
    termination.6months[i] <- 1
  } else {
    termination.6months[i] <- 0
  }
}

# did the war end within a year?
termination.year <- rep(NA, nrow(nimmagadda.replication))
for (i in 1:nrow(nimmagadda.replication)) {
  if (nimmagadda.replication$violDiff[i] <= year) {
    termination.year[i] <- 1
  } else {
    termination.year[i] <- 0
  }
}


# cbinding the values
nimmagadda.replication <- cbind(nimmagadda.replication, logPopulation, logGDP.capita, 
                                DidWarEnd, termination.3months, termination.6months,
                                termination.year)

# exporting as a file
write.csv(nimmagadda.replication, file = "Political Atrocities Replication File_Final.v.1.1.csv")

# testing out with regression
regressTest <- lm(data = nimmagadda.replication, formula = violDiff ~ logGDP.capita + logPopulation +
     logMountainous + logDeaths + aFricanContinent + internationalizedOpponent + 
     internationalizedAlly + relFrac + ethnFrac + PerpPolice + PerpMilitary + PerpOther)

# creating a cox regression
testCox <- coxph(formula = Surv(violDiff, DidWarEnd) ~ logGDP.capita + logPopulation +
        logMountainous + logDeaths + aFricanContinent + internationalizedOpponent + 
        internationalizedAlly + relFrac + ethnFrac + PerpPolice + PerpMilitary +
          PerpOther, data = nimmagadda.replication)


plot(survfit(testCox)) 

# what is the magnitude of human rights violations that happened in wars?
atrocityWars <- as.vector(unique(nimmagadda.replication$Country))
finalCOW_intrastateData$countries[!(finalCOW_intrastateData$countries %in% atrocityWars)]

polityData <- read.csv("polityRawData.csv", header = TRUE)
polityData <- subset(polityData, year >= 1995)

updated_wars.WithoutAtrocities <- read.csv("Civil Wars Without Political Atrocities 1995 to 2007_EDITED.csv",
                                           header = TRUE)

# log vectors
updated_wars.WithoutAtrocities$logDeaths.NA <- rep(0, nrow(updated_wars.WithoutAtrocities))
logPop.NA <- log(updated_wars.WithoutAtrocities$population.NA)
logGDPcap.NA <- log(updated_wars.WithoutAtrocities$gdpPerCapita.NA)
didWarEnd.NA <- rep(1, nrow(updated_wars.WithoutAtrocities))

# cbinding, data editing
final.withoutWars <- cbind(updated_wars.WithoutAtrocities, logPop.NA, logGDPcap.NA, didWarEnd.NA)
internationalizedAlly.NA <- final.withoutWars$internationalizedAlly
internationalizedOpponent.NA <- final.withoutWars$internationalizedOpponent
final.withoutWars <- cbind(final.withoutWars, internationalizedAlly.NA, internationalizedOpponent.NA)


# cox regression for wars without atrocities
withoutAtrocities.cox <- coxph(formula = Surv(warLength, didWarEnd.NA) ~ logGDPcap.NA +
                                 logPop.NA + logMountainous.NA + 
                                 aFricanContinent.NA + internationalizedOpponent.NA +
                                 internationalizedAlly.NA + relFrac.NA + relFrac.NA,
                               data = final.withoutWars)

plot(survfit(withoutAtrocities.cox))


plot(Surv(warLength, didWarEnd.NA))

# linear regression for wars without atrocities
regressTest <- lm(data = final.withoutWars, formula = warLength ~ logGDPcap.NA + logPop.NA +
                    logMountainous.NA + logDeaths.NA + aFricanContinent.NA + internationalizedAlly + 
                    internationalizedOpponent + relFrac.NA + ethnFrac.NA)


# creating a survival object
nimmagadda.replication$SurvObj <- with(nimmagadda.replication, Surv(violDiff, DidWarEnd == 1))
replication.cox <- survfit(SurvObj ~ 1, data = nimmagadda.replication, conf.type = "log-log")
plot(replication.cox)

# editing somalia data (taking from estimate somalia gdp per capita based on growth rate between 2013 to 2017)
# cross validated using inflation calculator and tradingeconomics.com
nimmagadda.replication$gdpPerCapita[152:166] <- 359.0242
nimmagadda.replication$gdpPerCapita[167:175] <- 366.689
nimmagadda.replication$logGDP.capita <- log(nimmagadda.replication$gdpPerCapita)

# creating a dataset at just the war level, 31 subjects
warLevelSet <- read.csv("Finalized Compilation.csv", header = TRUE)
