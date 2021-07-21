## Example 3.1

## Import the Transit_Survey data file into a data frame (table) and label it myData.

Frequency<-table(myData$'Mode of Transportation')
Frequency
View(Frequency)
barplot(Frequency,main="Bar Chart for Transit Survey",xlab="Number of Respondents", horiz=TRUE, col="blue", xlim=c(0,300), las=1, cex.names=0.5)
abline(v=0)

##Example 3.2

## Import the Growth_Value data file into a data frame (table) and label it myData.

intervals<-seq(-50,100,by=25)
value.cut<-cut(myData$Value, intervals, left=FALSE, right=TRUE)
value.freq<-table(value.cut)
value.freq
View(value.freq)
hist(myData$Value, breaks=intervals, right=TRUE, main="Histogram for annual returns (in %) for the Value Fund", xlab="Annual Returns (in%) for Value", col="blue")

##Example 3.3

## Import the Promotion data file into a data frame (table) and label it myData.

myTable<-table(myData$Location, myData$Purchase)
myTable
prop.table(myTable)
myNewTable<-table(myData$Purchase, myData$Location)
barplot(myNewTable, main="Location and Purchase", col=c('blue','red'),legend=rownames(myNewTable), xlab="Location",ylab="Count", ylim=c(0,200))

##Example 3.4

## Import the Growth_Value data file into a data frame (table) and label it myData.

plot(myData$Value~myData$Growth, main="Scatterplot of Value against Growth", xlab="Growth", ylab="Value", col="chocolate",pch=16)

##Example 3.5

## Import the Birth_Life data file into a data frame (table) and label it myData.

plot(myData$'Birth Rate'~myData$'Life Exp', main="Scatterplot of Birth Rate against Life Expectancy", xlab="Life Expectancy (in years)", ylab="Birth Rate (in %)",pch=16, col=ifelse(myData$Development=="Developing", 20,26))
legend("right", legend=c("Developing","Developed"),pch=16, col=c(20,26))

#Example 3.6

## Import the Birth_Life data file into a data frame (table) and label it myData.

plot(myData$'Birth Rate'~myData$'Life Exp', type="n")
symbols(myData$'Birth Rate'~myData$'Life Exp', circles=myData$GNI,inches=0.5, bg='blue',main="A buble plot of birth rate, life expectancy, and GNI",xlab="Life Expectancy (in years)",ylab="Birth Rate (in %)")

#Example 3.7

## Import the Growth_Value data file into a data frame (table) and label it myData.

plot(myData$Growth~myData$Year, main="A line chart for the Growth and Value mutual funds", xlab="Year", ylab="Annual Returns", col="blue",type="l", ylim=c(-100,100))
lines(myData$Value~myData$Year, col="red",type="l")
legend("bottom",legend=c("Growth","Value"), col=c("blue","red"),lty=1)

#Example 3.8

## Import the Bookstores data file into a data frame (table) and label it myData.

myTable<-table(myData$BookStore, myData$BookType)
myTable<-myTable/rowSums(myTable)
myData.matrix<-as.matrix(myTable)
heatmap(myData.matrix, col=heat.colors(256), scale="none", Rowv=NA, Colv=NA)
myBreaks<-c(0,0.05,0.10,0.15,0.20,0.25,0.30)
myCol<-c("blue", "green","yellow","orange1","red1","red3")
heatmap(myData.matrix, col=myCol, breaks=myBreaks, scale="none",Rowv=NA, Colv=NA)

#Example 3.9

## Import the Growth_Value data file into a data frame (table) and label it myData.

mean(myData$Growth)
summary(myData)

#Example 3.13

## Import the Growth_Value data file into a data frame (table) and label it myData.

cor(myData)

#Example 3.14

## Import the Growth_Value data file into a data frame (table) and label it myData.

boxplot(myData$Growth,myData$Value, main="Boxplots for Growth and Value",xlab="Annual Returns, 1984-2018 (in percent)", names=c("Growth","Value"), horizontal=TRUE, col="gold")
outliersGrowth<-boxplot(myData$Growth)$out
outliersValue<-boxplot(myData$Value)$out
outliersGrowth
outliersValue
myData$newGrowth<-ifelse(myData$Growth %in% outliersGrowth, NA, myData$Growth)
myData$newValue<-ifelse(myData$Value %in% outliersValue, NA, myData$Value)
summary(myData)


