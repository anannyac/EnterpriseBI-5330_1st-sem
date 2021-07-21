# R code for examples in Chapter 2

# Example 2.1
# Import the Gig data file into a data frame (table) and label it myData
dim(myData)
head(myData)
View(myData)

is.na(myData$Industry)
which (is.na(myData$Industry))
myData[24,]

length(which(myData$Industry=='Automotive'))
length(which(myData$HourlyWage > 30))
length(which(myData$Industry=='Automotive' & myData$HourlyWage > 30))

sortedData1 <- myData[order(myData$HourlyWage),]
View(sortedData1)
sortedData1 <- myData[order(myData$HourlyWage, decreasing = TRUE),]

sortedData2 <- myData[order(myData$Industry, myData$Job, myData$HourlyWage),]
View(sortedData2)

sortedData3 <- myData[order(myData$Industry, myData$Job, -myData$HourlyWage),]
View(sortedData3)

sortedData4 <- myData[order(-xtfrm(myData$Industry), myData$Job, myData$HourlyWage),]
View(sortedData4)

sortedData5 <- myData[order(myData$Industry, myData$Job, myData$HourlyWage, decreasing = TRUE),]
View(sortedData5)

write.csv(sortedData5,"sortedData5.csv")
# The output file will be created in the project folder


# Example 2.2
# Import the Restaurant_Reviews data file into a data frame (table) and label it myData
is.na(myData)
is.na(myData$Service)
myData[complete.cases(myData), ]
myData[!complete.cases(myData), ]

omission:
omissionData <- na.omit(myData)
View(omissionData)


Imputation:

ambienceMean <- mean(myData$Ambience, na.rm = TRUE)
serviceMean <- mean(myData$Service, na.rm = TRUE)
myData$Ambience[is.na(myData$Ambience)] <- ambienceMean
myData$Service[is.na(myData$Service)] <- serviceMean


# Example 2.3
# Import the Customers data file into a data frame (table) and label it myData.
college <- myData[myData$College=='Yes', ]
college$BirthDate <- as.Date(college$BirthDate, format = "%m/%d/%Y")
cutoffdate1 <- as.Date("01/01/1982", format = "%m/%d/%Y")
cutoffdate2 <- as.Date("12/31/1999", format = "%m/%d/%Y")
millenials <- college[college$BirthDate >= cutoffdate1 & college$BirthDate <= cutoffdate2, ]
subset1 <- millenials[ , c(2,6,8,10,11,14)]
subset2 <- millenials[ , c("Sex", "HouseholdSize", "Income", "Spending2018", "NumOfOrders", "Channel")]
subset1$Sex <- as.factor(subset1$Sex)
subset1$Channel <- as.factor(subset1$Channel)
is.factor(subset1$Channel)

sex <- split(subset1, subset1$Sex)
sex$Female
sex$Male
View(sex$Female)
View(sex$Male)
dataRanges <- myData[c(1:50, 101:200),]

# Example 2.4
# Import the Customers data file into a data frame (table) and label it myData
myData$DaysSinceLastReverse <- as.numeric(myData$DaysSinceLast * -1)
recencyBins <- quantile(myData$DaysSinceLastReverse, probs=seq(0, 1, by=0.20))
recencyBins
frequencyBins <- quantile(myData$NumOfOrders, probs=seq(0, 1, by=0.20))
monetaryBins <- quantile(myData$Spending2018, probs=seq(0, 1, by=0.20))
myData$Recency <- cut(myData$DaysSinceLastReverse,
                        breaks=recencyBins,labels=c("1", "2", "3", "4", "5"), 
                       include.lowest=TRUE, right=FALSE)
myData$Frequency <- cut(myData$NumOfOrders, 
                          breaks=frequencyBins,labels=c("1", "2", "3", "4", "5"), 
                          include.lowest=TRUE, right=FALSE)
myData$Monetary <- cut(myData$Spending2018, 
                         breaks=monetaryBins, labels=c("1", "2", "3", "4", "5"), 
                         include.lowest=TRUE, right=FALSE)
myData$RFM <- paste(myData$Recency, myData$Frequency, myData$Monetary)
head(myData$RFM)

myData$BinnedIncome <- cut(myData$Income, 
                           breaks=5, labels= c("1", "2", "3", "4", "5"), 
                           include.lowest=TRUE, right=FALSE)
head(myData$BinnedIncome)
levels(cut(myData$Income, breaks=5))
table(myData$BinnedIncome)
myData$MembershipTier <- cut(myData$Spending2018, 
                             breaks = c(0, 250, 1000, Inf),
                             labels = c("Bronze", "Silver", "Gold"))
head(myData$MembershipTier)
View(myData)

# Example 2.5
# Import the Customers data file into a data frame (table) and label it myData
myData$SpendingDiff <- myData$Spending2018 - myData$Spending2017
head(myData$SpendingDiff)
myData$PctSpendingDiff <- round((myData$SpendingDiff / myData$Spending2017)*100, digits = 2)
myData$PctSpendingDiff <- paste(myData$PctSpendingDiff, "%")
head(myData$PctSpendingDiff)
myData$IncomeLn <- log(myData$Income)
head(myData$IncomeLn)
myData$BirthDate <- as.Date(myData$BirthDate, format = "%m/%d/%Y")
endDate <- as.Date("01/01/2019", format = "%m/%d/%Y")
myData$Age <- difftime(endDate, myData$BirthDate)/365.25
myData$Age <- as.numeric(myData$Age)
myData$Age <- floor(myData$Age)
head(myData$Age)

myData$BirthMonth <- months(myData$BirthDate)
myData$BirthMonth <- match(myData$BirthMonth, month.name)
head(myData$BirthMonth)
View(myData)

# Example 2.6
# Import the Customers data file into a data frame (table) and label it myData
table(myData$Race)
myData$NewRace <- ifelse(myData$Race %in% c("American Indian", "Pacific Islander"), "Other", myData$Race)
table(myData$NewRace)
View(myData)


# Example 2.7
# Import the Customers data file into a data frame (table) and label it myData
myData$Channel_Referral <- ifelse(myData$Channel == "Referral", 1, 0)
myData$Channel_SM <- ifelse(myData$Channel == "SM", 1, 0)
myData$Channel_TV <- ifelse(myData$Channel == "TV", 1, 0)
View(myData)

# Example 2.8
# Import the Customers data file into a data frame (table) and label it myData
myData$Satisfaction_Score <- ifelse(myData$Satisfaction == "Very Dissatisfied", 1, 
                             ifelse(myData$Satisfaction == "Somewhat Dissatisfied", 2,
                             ifelse(myData$Satisfaction == "Neutral", 3,
                             ifelse(myData$Satisfaction == "Somewhat Satisfied", 4, 5))))
View(myData)

