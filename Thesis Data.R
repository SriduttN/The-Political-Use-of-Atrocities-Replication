# Data pre-processing
# If replicating, please reset path in order to create functional code
setwd("/Users/SriduttN/Desktop/JP Spring Semester")

cow_civilWars <- data.frame(read.csv("Intra-StateWarData_v4.1 (Cleaned Data - wo Internationalization).csv", header = TRUE))
cow_civilWars <- cow_civilWars[1:34, ]

atrocities_1995_2012 <- data.frame(read.csv("Atrocities 1995-2012.csv", header = TRUE))
atrocities_2013_2015 <- data.frame(read.csv("Atrocities 2013-2015.csv", header = TRUE))
atrocities_2016_2017 <- data.frame(read.csv("Atrocities 2016-2017.csv", header = TRUE))

# deleting unnecessary rows from input
deleteRows_95_12 <- atrocities_1995_2012[-c(7776:65535), ]
deleteRows_13_15 <- atrocities_2013_2015[-c(3994:65535), ]
deleteRows_16_17 <- atrocities_2016_2017[-c(2947:2948), ]

# deleting unnecessary columns from input
deleteColumns_95_12 <- deleteRows_95_12[, c(1:73)]
deleteColumns_13_15 <- deleteRows_13_15[, c(1:73)]
deleteColumns_16_17 <- deleteRows_16_17[, c(1:73)]

# Combine to create updated data set
updated_data <- rbind(deleteColumns_95_12, deleteColumns_13_15, deleteColumns_16_17)

# Exporting the updated data table as a file
write.csv(updated_data, file = "Compiled Atrocities Data - 1995 to February 2017.csv")

# Selecting for state perpetration of human rightsviolations
statePerps <- updated_data[which(updated_data$Perp.State.Military != '' |
                                   updated_data$Perp.State.Police != '' | updated_data$Perp.State.Other != ''), ]


# creating a dataset of civil wars that is captured by our dataset
civilWars <- c("Croatia-Krajina War",
               "Third Liberian War",
               "Sixth Iraqi Kurdish War",
               "Fifth Democratic Republic of the Congo War",
               "Third Rwandan War",
               "First Congo (Brazzaville)",
               "Second Sierra Leone",
               "Kosovo Independence",
               "Guinea-Bissau Military",
               "Fourth Chad (Togoimi Revolt)",
               "Third Angolan",
               "Second Congo (Brazzaville)",
               "Second Aceh",
               "Oromo Liberation",
               "Second Chechen Civil War",
               "Second Philippine Moro War",
               "Guinean Civil War",
               "Burundi Civil War",
               "First Nepal Maoist Insurgency",
               "Fourth Liberian Civil War",
               "Cote D'Ivoire Military Civil War",
               "Third Phillippine Moro",
               "Darfur",
               "Third Aceh",
               "Second Nepal Maoist Insurgency",
               "Waziristan",
               "First Yemeni Cleric",
               "Phillippine Joint Offensive",
               "Fifth Chad",
               "Third Somalian Civil War",
               "Second Yemeni Cleric")

countries <- c("HRV",
               "LBR",
               "IRQ",
               "COD",
               "RWA",
               "COG",
               "SLE",
               "SCG",
               "GNB",
               "TCD",
               "AGO",
               "COG",
               "IDN",
               "ETH",
               "RUS",
               "PHL",
               "GIN",
               "BDI",
               "NPL",
               "LBR",
               "CIV",
               "PHL",
               "SDN",
               "IDN",
               "NPL",
               "PAK",
               "YEM",
               "PHL",
               "TCD",
               "SOM",
               "YEM")


# using the PITF data, transforming atrocity beginning dates into date objects
# some more data cleaning - what if the start days are NA?
# replace with the midpoint day of the month - 15
for (i in 1:nrow(statePerps)) {
  if (is.na(statePerps$Start.Day[i])) {
    statePerps$Start.Day[i] <- 15
  } else {
    statePerps$Start.Day[i] <- statePerps$Start.Day[i]
  }
}

# creating a vector to store all date objects into
violationsDates <- paste(statePerps$Start.Year, statePerps$Start.Month, statePerps$Start.Day, sep = "/")
# using the as.date function to convert strings into date objects
dated_violations <- as.Date(violationsDates)

# adding the vector of the dated violations to the statePerps dataset
statePerps <- cbind(statePerps, dated_violations)

# now our goal is to create date vector for the civil war start and end dates
editedCOW_intrastate <- cow_civilWars
# if the start date is indeterminate for the civil war, assume that the civil war begins at the
# middle of the month assuming a uniform distribution for war beginnings across the month - 15th day
# of the month
for (i in 1:nrow(editedCOW_intrastate)) {
  if (editedCOW_intrastate$StartDay1[i]==-9) {
    editedCOW_intrastate$StartDay1[i] <- 15
  } else {
    editedCOW_intrastate$StartDay1[i] <- editedCOW_intrastate$StartDay1[i]
  }
}

# doing the same for the end date as well
for (i in 1:nrow(editedCOW_intrastate)) {
  if (editedCOW_intrastate$EndDay1[i]==-9) {
    editedCOW_intrastate$EndDay1[i] <- 15
  } else {
    editedCOW_intrastate$EndDay1[i] <- editedCOW_intrastate$EndDay1[i]
  }
}

write.csv(statePerps, file = "Atrocities Dataset.csv")

# creating a vector for the start date and the end date for each of the civil wars in question
startWars <- paste(editedCOW_intrastate$StartYear1, editedCOW_intrastate$StartMonth1, 
                         editedCOW_intrastate$StartDay1, sep = "/")
startWars_date <- as.Date(startWars)
# end dates now
endWars <- paste(editedCOW_intrastate$EndYear1, editedCOW_intrastate$EndMonth1, 
                 editedCOW_intrastate$EndDay1, sep = "/")
endWars_date <- as.Date(endWars)

# subset the state perpetrators dataset by country code by war
# then, apply the start date and end date for each country as vector
finalCOW_intrastateData <- cbind(editedCOW_intrastate, startWars_date, endWars_date)

# creating a dataset that whittles down the countries and human rights data by start and end
# not the most dynamic solution, but it worked
finalizedData <- statePerps[which(statePerps$Country==countries[1] |
                                     statePerps$Country==countries[2] |
                                     statePerps$Country==countries[3] |
                                     statePerps$Country==countries[4] |
                                     statePerps$Country==countries[5] |
                                     statePerps$Country==countries[6] |
                                     statePerps$Country==countries[7] |
                                     statePerps$Country==countries[8] |     
                                     statePerps$Country==countries[9] |
                                     statePerps$Country==countries[10] |
                                     statePerps$Country==countries[12] |
                                     statePerps$Country==countries[13] |
                                     statePerps$Country==countries[14] |
                                     statePerps$Country==countries[15] |
                                     statePerps$Country==countries[16] |
                                     statePerps$Country==countries[17] |
                                     statePerps$Country==countries[18] |
                                     statePerps$Country==countries[19] |
                                     statePerps$Country==countries[20] |
                                    statePerps$Country==countries[21] |
                                    statePerps$Country==countries[22] |
                                    statePerps$Country==countries[23] |
                                    statePerps$Country==countries[24] |
                                    statePerps$Country==countries[25] |
                                    statePerps$Country==countries[26] |
                                    statePerps$Country==countries[27] |
                                    statePerps$Country==countries[28] |
                                    statePerps$Country==countries[29] |
                                    statePerps$Country==countries[30]), ]



# implement the dplyr library to group violations by country
library(dplyr)

# cleaning up the COW data to match dimensions of start and end dates
# removing the sri lanka civil war
finalCOW_intrastateData <- finalCOW_intrastateData[-33, ]
# removing the nigerian muslim data point 
finalCOW_intrastateData <- finalCOW_intrastateData[-28, ]
# removing the second guinea bissau war data point 
finalCOW_intrastateData <- finalCOW_intrastateData[-9, ]

# putting the coding together
finalCOW_data <- cbind(finalCOW_intrastateData, countries)
rownames(finalCOW_data) <- seq(length(finalCOW_data))

# beginning the process of categorizing human rights violations by conflict start and end dates
# logic: as we go down each point in our finalized data, we want to check whether the country code
# matches to some country code in the COW data

isDataGood <- rep(NA, nrow(finalizedData))

# grouping finalizedData by country
countryCodes <- match(finalizedData$Country, finalCOW_data$countries)

# appears that the easiest way to do this may be by subsetting data by country
hrv <- finalizedData[which(finalizedData$Country == countries[1]), ]
lbr <- finalizedData[which(finalizedData$Country == countries[2]), ]
irq <- finalizedData[which(finalizedData$Country == countries[3]), ]
cod <- finalizedData[which(finalizedData$Country == countries[4]), ]
rwa <- finalizedData[which(finalizedData$Country == countries[5]), ]
cog <- finalizedData[which(finalizedData$Country == countries[6]), ]
sle <- finalizedData[which(finalizedData$Country == countries[7]), ]
scg <- finalizedData[which(finalizedData$Country == countries[8]), ]
gnb <- finalizedData[which(finalizedData$Country == countries[9]), ]
tcd <- finalizedData[which(finalizedData$Country == countries[10]), ]
ago <- finalizedData[which(finalizedData$Country == countries[11]), ]
cog2 <- finalizedData[which(finalizedData$Country == countries[12]), ]
idn <- finalizedData[which(finalizedData$Country == countries[13]), ]
eth <- finalizedData[which(finalizedData$Country == countries[14]), ]
rus <- finalizedData[which(finalizedData$Country == countries[15]), ]
phl <- finalizedData[which(finalizedData$Country == countries[16]), ]
gin <- finalizedData[which(finalizedData$Country == countries[17]), ]
bdi <- finalizedData[which(finalizedData$Country == countries[18]), ]
npl <- finalizedData[which(finalizedData$Country == countries[19]), ]
lbr2 <- finalizedData[which(finalizedData$Country == countries[20]), ]
civ <- finalizedData[which(finalizedData$Country == countries[21]), ]
phl2 <- finalizedData[which(finalizedData$Country == countries[22]), ]
sdn <- finalizedData[which(finalizedData$Country == countries[23]), ]
idn2 <- finalizedData[which(finalizedData$Country == countries[24]), ]
npl2 <- finalizedData[which(finalizedData$Country == countries[25]), ]
pak <- finalizedData[which(finalizedData$Country == countries[26]), ]
yem <- finalizedData[which(finalizedData$Country == countries[27]), ]
phl3 <- finalizedData[which(finalizedData$Country == countries[28]), ]
tcd2 <- finalizedData[which(finalizedData$Country == countries[29]), ]
som <- finalizedData[which(finalizedData$Country == countries[30]), ]
yem2 <- finalizedData[which(finalizedData$Country == countries[31]), ]

# subsetting by country will allow us to figure out which datapoints are actually
# appropriate for our study
# hrv
hrv <- hrv[which(hrv$dated_violations >= finalCOW_data$startWars_date[1] &
                   hrv$dated_violations <= finalCOW_data$endWars_date[1]), ]
hrv <- cbind(hrv, x = rep(finalCOW_data$endWars_date[1], nrow(hrv)))

# lbr
lbr <- lbr[which(lbr$dated_violations >= finalCOW_data$startWars_date[2] &
                   lbr$dated_violations <= finalCOW_data$endWars_date[2]), ]
lbr <- cbind(lbr, x = rep(finalCOW_data$endWars_date[2], nrow(lbr)))

# irq
irq <- irq[which(irq$dated_violations >= finalCOW_data$startWars_date[3] &
                   irq$dated_violations <= finalCOW_data$endWars_date[3]), ]
irq <- cbind(irq, x = rep(finalCOW_data$endWars_date[3], nrow(irq)))

# cod
cod <- cod[which(cod$dated_violations >= finalCOW_data$startWars_date[4] &
                   cod$dated_violations <= finalCOW_data$endWars_date[4]), ]
cod <- cbind(cod, x = rep(finalCOW_data$endWars_date[4], nrow(cod)))


# rwa
rwa <- rwa[which(rwa$dated_violations >= finalCOW_data$startWars_date[5] &
                   rwa$dated_violations <= finalCOW_data$endWars_date[5]), ]
rwa <- cbind(rwa, x = rep(finalCOW_data$endWars_date[5], nrow(rwa)))

# cog
cog <- cog[which(cog$dated_violations >= finalCOW_data$startWars_date[6] &
                   cog$dated_violations <= finalCOW_data$endWars_date[6]), ]
cog <- cbind(cog, x = rep(finalCOW_data$endWars_date[6], nrow(cog)))

# sle
sle <- sle[which(sle$dated_violations >= finalCOW_data$startWars_date[7] &
                   sle$dated_violations <= finalCOW_data$endWars_date[7]), ]
sle <- cbind(sle, x = rep(finalCOW_data$endWars_date[7], nrow(sle)))

# scg
scg <- scg[which(scg$dated_violations >= finalCOW_data$startWars_date[8] &
                   scg$dated_violations <= finalCOW_data$endWars_date[8]), ]
scg <- cbind(scg, x = rep(finalCOW_data$endWars_date[8], nrow(scg)))

# gnb
gnb <- gnb[which(gnb$dated_violations >= finalCOW_data$startWars_date[9] &
                   gnb$dated_violations <= finalCOW_data$endWars_date[9]), ]
gnb <- cbind(gnb, x = rep(finalCOW_data$endWars_date[9], nrow(gnb)))

# tcd
tcd <- tcd[which(tcd$dated_violations >= finalCOW_data$startWars_date[10] &
                   tcd$dated_violations <= finalCOW_data$endWars_date[10]), ]
tcd <- cbind(tcd, x = rep(finalCOW_data$endWars_date[10], nrow(tcd)))

# ago
ago <- ago[which(ago$dated_violations >= finalCOW_data$startWars_date[11] &
                   ago$dated_violations <= finalCOW_data$endWars_date[11]), ]
ago <- cbind(ago, x = rep(finalCOW_data$endWars_date[11], nrow(ago)))

# cog2
cog2 <- cog2[which(cog2$dated_violations >= finalCOW_data$startWars_date[12] &
                   cog2$dated_violations <= finalCOW_data$endWars_date[12]), ]
cog2 <- cbind(cog2, x = rep(finalCOW_data$endWars_date[12], nrow(cog2)))

# idn
idn <- idn[which(idn$dated_violations >= finalCOW_data$startWars_date[13] &
                   idn$dated_violations <= finalCOW_data$endWars_date[13]), ]
idn <- cbind(idn, x = rep(finalCOW_data$endWars_date[13], nrow(idn)))

# eth
eth <- eth[which(eth$dated_violations >= finalCOW_data$startWars_date[14] &
                   eth$dated_violations <= finalCOW_data$endWars_date[14]), ]
eth <- cbind(eth, x = rep(finalCOW_data$endWars_date[14], nrow(eth)))

# rus
rus <- rus[which(rus$dated_violations >= finalCOW_data$startWars_date[15] &
                   rus$dated_violations <= finalCOW_data$endWars_date[15]), ]
rus <- cbind(rus, x = rep(finalCOW_data$endWars_date[15], nrow(rus)))

# phl
phl <- phl[which(phl$dated_violations >= finalCOW_data$startWars_date[16] &
                   phl$dated_violations <= finalCOW_data$endWars_date[16]), ]
phl <- cbind(phl, x = rep(finalCOW_data$endWars_date[16], nrow(phl)))

# gin
gin <- gin[which(gin$dated_violations >= finalCOW_data$startWars_date[17] &
                   gin$dated_violations <= finalCOW_data$endWars_date[17]), ]
gin <- cbind(gin, x = rep(finalCOW_data$endWars_date[17], nrow(gin)))

# bdi
bdi <- bdi[which(bdi$dated_violations >= finalCOW_data$startWars_date[18] &
                   bdi$dated_violations <= finalCOW_data$endWars_date[18]), ]
bdi <- cbind(bdi, x = rep(finalCOW_data$endWars_date[18], nrow(bdi)))

# npl
npl <- npl[which(npl$dated_violations >= finalCOW_data$startWars_date[19] &
                   npl$dated_violations <= finalCOW_data$endWars_date[19]), ]
npl <- cbind(npl, x = rep(finalCOW_data$endWars_date[19], nrow(npl)))

# lbr2
lbr2 <- lbr2[which(lbr2$dated_violations >= finalCOW_data$startWars_date[20] &
                   lbr2$dated_violations <= finalCOW_data$endWars_date[20]), ]
lbr2 <- cbind(lbr2, x = rep(finalCOW_data$endWars_date[20], nrow(lbr2)))

# civ
civ <- civ[which(civ$dated_violations >= finalCOW_data$startWars_date[21] &
                   civ$dated_violations <= finalCOW_data$endWars_date[21]), ]
civ <- cbind(civ, x = rep(finalCOW_data$endWars_date[21], nrow(civ)))

# phl2
phl2 <- phl2[which(phl2$dated_violations >= finalCOW_data$startWars_date[22] &
                   phl2$dated_violations <= finalCOW_data$endWars_date[22]), ]
phl2 <- cbind(phl2, x = rep(finalCOW_data$endWars_date[22], nrow(phl2)))

# sdn
sdn <- sdn[which(sdn$dated_violations >= finalCOW_data$startWars_date[23] &
                   sdn$dated_violations <= finalCOW_data$endWars_date[23]), ]
sdn <- cbind(sdn, x = rep(finalCOW_data$endWars_date[23], nrow(sdn)))

# idn2
idn2 <- idn2[which(idn2$dated_violations >= finalCOW_data$startWars_date[24] &
                   idn2$dated_violations <= finalCOW_data$endWars_date[24]), ]
idn2 <- cbind(idn2, x = rep(finalCOW_data$endWars_date[24], nrow(idn2)))

# npl2
npl2 <- npl2[which(npl2$dated_violations >= finalCOW_data$startWars_date[25] &
                   npl2$dated_violations <= finalCOW_data$endWars_date[25]), ]
npl2 <- cbind(npl2, x = rep(finalCOW_data$endWars_date[25], nrow(npl2)))

# pak
pak <- pak[which(pak$dated_violations >= finalCOW_data$startWars_date[26] &
                   pak$dated_violations <= finalCOW_data$endWars_date[26]), ]
pak <- cbind(pak, x = rep(finalCOW_data$endWars_date[26], nrow(pak)))

# yem
yem <- yem[which(yem$dated_violations >= finalCOW_data$startWars_date[27] &
                   lbr$dated_violations <= finalCOW_data$endWars_date[27]), ]
yem <- cbind(yem, x = rep(finalCOW_data$endWars_date[27], nrow(yem)))

# phl3
phl3 <- phl3[which(phl3$dated_violations >= finalCOW_data$startWars_date[28] &
                   phl3$dated_violations <= finalCOW_data$endWars_date[28]), ]
phl3 <- cbind(phl3, x = rep(finalCOW_data$endWars_date[28], nrow(phl3)))

# tcd2
tcd2 <- tcd2[which(tcd2$dated_violations >= finalCOW_data$startWars_date[29] &
                   tcd2$dated_violations <= finalCOW_data$endWars_date[29]), ]
tcd2 <- cbind(tcd2, x = rep(finalCOW_data$endWars_date[29], nrow(tcd2)))

# som
som <- som[which(som$dated_violations >= finalCOW_data$startWars_date[30] &
                   som$dated_violations <= finalCOW_data$endWars_date[30]), ]
som <- cbind(som, x = rep(finalCOW_data$endWars_date[30], nrow(som)))

# yem2
yem2 <- yem2[which(yem2$dated_violations >= finalCOW_data$startWars_date[31] &
                   yem2$dated_violations <= finalCOW_data$endWars_date[31]), ]
yem2 <- cbind(yem2, x = rep(finalCOW_data$endWars_date[31], nrow(yem2)))

# dataset of political atrocities with civil war end dates
politicalAtrocities <- rbind(hrv, lbr, irq, cod, rwa, cog, sle, scg, gnb, tcd, ago, cog2, 
                             idn, eth, rus, phl, gin, bdi, npl, lbr2, civ, phl2, sdn, 
                             idn2, npl2, pak, yem, phl3, tcd2, som, yem2)

# war end difference between violation and war end date
violDiff <- politicalAtrocities$x - politicalAtrocities$dated_violations
politicalAtrocities <- cbind(politicalAtrocities, violDiff)

# cleaning up the dataset to transform qualitative data into quantitative data, such as
# death and injury numbers 
politicalAtrocities <- politicalAtrocities[-7, ]
rownames(politicalAtrocities) <- seq(nrow(politicalAtrocities))

# export the political atrocities dataset as a csv file
write.csv(politicalAtrocities, file = "Political Atrocities by Civil War - 1995 to 2007.csv")

# manually cleaned dataset to approximate death count where qualitative answers are shown 
# and remove ambiguous datapoints
atrocitiesData <- read.csv("Political Atrocities by Civil War - 1995 to 2007 Cleaned.csv", header = TRUE)


# creating vectors for control variables in final COW
gdpPerCapita <- rep(NA, nrow(finalCOW_intrastateData))
logPopulation <- rep(NA, nrow(finalCOW_intrastateData))
ethnFrac <- rep(NA, nrow(finalCOW_intrastateData))
relFrac <- rep(NA, nrow(finalCOW_intrastateData))
internationalizedOpponent <- rep(NA, nrow(finalCOW_intrastateData))
internationalizedAlly <- rep(NA, nrow(finalCOW_intrastateData))
aFricanContinent <- rep(NA, nrow(finalCOW_intrastateData))
logDeaths <- rep(NA, nrow(finalCOW_intrastateData))
Polity <- rep(NA, nrow(finalCOW_intrastateData))
logMountainous <- rep(NA, nrow(finalCOW_intrastateData))


# the coding below cbinds whether the war has been internationalized
# in favor of ally or opponent to the finalCOW_intrastateData set
internationalizedOpponent <- rep(NA, nrow(finalCOW_intrastateData))
for (i in 1:nrow(finalCOW_intrastateData)) {
  if (finalCOW_intrastateData$Intnl[i] == 0) {
    internationalizedOpponent[i] <- 0
  } else {
    internationalizedOpponent[i] <- NA
  }
}

internationalizedAlly <- rep(NA, nrow(finalCOW_intrastateData))
for (i in 1:nrow(finalCOW_data)) {
  if (finalCOW_intrastateData$Intnl[i] == 0) {
    internationalizedAlly[i] <- 0
  } else {
    internationalizedAlly[i] <- NA
  }
}

# the somalian civil war had international allies and opponents
# for the somalian state
opponents <- c(4, 6, 7, 30)
allies <- c(9, 21, 26, 30)
internationalizedOpponent[opponents] <- 1
internationalizedOpponent[allies[-4]] <- 0

internationalizedAlly[opponents[-4]] <- 0
internationalizedAlly[allies] <- 1

# cbinding opponent and ally international status to the data
finalCOW_intrastateData <- cbind(finalCOW_intrastateData, internationalizedAlly, 
                                 internationalizedOpponent, countries)

# isolating the countries that do have human rights violations
countriesWith.violations <- as.vector(unique(nimmagadda.replication$Country))

# rownames adjustment
rownames(finalCOW_intrastateData) <- 1:nrow(finalCOW_intrastateData)

# wars without human rights violations
warsWithout.violations <- rbind(finalCOW_intrastateData[11,], 
      finalCOW_intrastateData[6,], 
      finalCOW_intrastateData[14,],
      finalCOW_intrastateData[9,], 
      finalCOW_intrastateData[24, ], 
      finalCOW_intrastateData[3,], 
      finalCOW_intrastateData[2,], 
      finalCOW_intrastateData[19,],
      finalCOW_intrastateData[22,], 
      finalCOW_intrastateData[7,], 
      finalCOW_intrastateData[10,],
      finalCOW_intrastateData[27,], 
      finalCOW_intrastateData[31,])

# finding the length of the wars without atrocities
warLength <- rep(NA, nrow(warsWithout.violations))
for (i in 1:length(warLength)) {
  warLength[i] <- warsWithout.violations$endWars_date[i] - warsWithout.violations$startWars_date[i]
}

# cbinding the length of the wars to the dataset
warsWithout.violations <- cbind(warsWithout.violations, warLength)

# cbinding the following vectors to the non-atrocities dataset
gdpPerCapita.NA <- rep(NA, nrow(warsWithout.violations))
population.NA <- rep(NA, nrow(warsWithout.violations))
ethnFrac.NA <- rep(NA, nrow(warsWithout.violations))
relFrac.NA <- rep(NA, nrow(warsWithout.violations))
internationalizedOpponent.NA <- rep(NA, nrow(warsWithout.violations))
internationalizedAlly.NA <- rep(NA, nrow(warsWithout.violations))
aFricanContinent.NA <- rep(NA, nrow(warsWithout.violations))
logDeaths.NA <- rep(NA, nrow(warsWithout.violations))
Polity.NA <- rep(NA, nrow(warsWithout.violations))
logMountainous.NA <- rep(NA, nrow(warsWithout.violations))

# cbind
warsWithout.violations <- cbind(warsWithout.violations, gdpPerCapita.NA, population.NA, ethnFrac.NA, 
                                relFrac.NA, aFricanContinent.NA, logDeaths.NA, 
                                Polity.NA, logMountainous.NA)

# exporting the file for manual editing
write.csv(warsWithout.violations, "Civil Wars Without Political Atrocities 1995 to 2007.csv")

warTime <- rep(NA, nrow(finalCOW_intrastateData))
warTime <- finalCOW_intrastateData$endWars_date - finalCOW_intrastateData$startWars_date

finalCOW_intrastateData <- cbind(finalCOW_intrastateData, warTime)
                                     
write.csv(finalCOW_intrastateData, "Intrastate COW Data Compilation, 1995 to 2007.csv")