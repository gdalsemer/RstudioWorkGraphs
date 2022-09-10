#Grant Dalsemer
#Assignment1


#Question1

library(XML)
install.packages("RCurl")
install.packages("rlist")
library(rlist)
library(RCurl)

#Code was taken from 
#https://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package
#

theurl <- getURL("https://emergency.cdc.gov/han/han00384.asp",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
DataF <- tables[[which.max(n.rows)]]
DataF

DataF <- data.frame("Rank"=x,"State"=y,"NumDeaths"=z)
DataF
class(DataF)
num1 <- DataF[1, 1:3, ]
num1
numLast <- DataF[10, 1:3,]
numLast

sprintf("The %sth most amount of deaths in %s were recorded at: %s", numLast[1], numLast[2], numLast[3])
sprintf("The number %s amount of deaths were recorded in %s and a total of %s deaths were recorded", num1[1], num1[2], num1[3])


#Question2
canes <- read.csv("https://people.sc.fsu.edu/~jburkardt/data/csv/hurricanes.csv")
canes <- as.data.frame(canes)
early <- canes[, 3:7]
early
later <- canes[, 8:13]
later
class(later)

#Early is the data.frame that holds all the hurricanes before 2010
early$means <- rowMeans(early)
early
class(early)
#Later is the data.frame holds all huricane data after 2010.
later$means <- rowMeans(later)
later
compare <- canes

compare$Before2010Means <- early$means

compare$After2010Means <- later$means
class(compare)
compare
#Create a line plot 
install.packages("ggplot2")
library(ggplot2)

#This variable helps create an ordered list with levels for the bottom of the graph
#Rstudio auto defaults to alphabetical order and this needs to be changed.
monthsGraph <- factor(canes$Month, levels = canes$Month)
monthsGraph

g <- ggplot(compare, aes(x = monthsGraph)) + geom_line(aes(y = Before2010Means, color = "Before 2010" ,group = 1)) + geom_point(aes(y = Before2010Means)) +
  geom_line(aes(y = After2010Means, color = "After 2010" ,group = 1)) + geom_point(aes(y = After2010Means))
g <- g + labs(x = compare$Month, y = "Avg Num of canes", title = "Huricanes Before and After 2010")
g

#Question 3
install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)
install.packages("xlsx")
require(xlsx)
theDATA <- read_excel('Data/overdose_data_1999-2015.xlsx', sheet = "Online", na="---")
head(theDATA)
colnames(theDATA)


#Cleaning up the data from the table
theData2 <- drop_na(theDATA)
head(theData2)
class(theData2)
colnames(theData2)
Data3 <- theData2
Data3
class(Data3)
#Set the Colnames to something more readable
#For some reason the file cuts off some of the columns from the data frame
xray <- c("Name", 1999:2015)
colnames(Data3) <- xray
Data3
colnames(Data3)

#Product Chart from Data
Data3
Data4 <- Data3[c(0:3), c(5:18)]
Data4
colnames(Data4)
class(Data4)
Year <- c(2002:2015)
Data4
colnames(Data4)
#This part of the code was possible with flipping the data frame over so that rows=columns and columns=rows
#After that, I could EASILY edit the data frame with the column commands
#I also had to add the row Year twice because the row index of 0
#I wasnt able to find anything that could easily edit that so I decided to do this instead
#The command t(Data_Frame) is used to flip the data frames.
Data05 <- t(Data4)
Data05
Data05 <- cbind(Data05, Year)
colnames(Data05) <- c("Total", "Male", "Female", "Year")
Data06 <- t(Data05)
Data06 <- as.data.frame(Data06)

rownames(Data06)
Data06
Data07 <- t(Data06)
Data07 <- as.data.frame(Data07)
library(ggthemes)
install.packages("lubridate")
library(lubridate)
Data07
g1 <- ggplot(Data07, aes(x=Year)) + geom_line(aes(y=Male, x=Year, color="Males"))
g1 <- g1 + geom_line(aes(y=Female, x=Year, color="Female"))
g1 <- g1 + labs(title="Overdoes Deaths from all drugs from 2002-2015", x="year", y="Deaths")
g1

#Producing another chart for all Opiod Deaths
library(data.table)
Data08 <- Data3[c(0,8,9), c(5:18)]
Data08
Data09 <- t(Data08)
Data09
Data09 <- cbind(Data09, Year)
colnames(Data09) <- c("Female", "Male", "Year")
Data10 <- t(Data09)
Data10 <- as.data.frame(Data10)
#have to flip it one more time
Data10 <- t(Data10)
Data10
#Making sure its a data.frame
class(Data10)
Data10 <- as.data.frame(Data10)
#Good thing I checked!
class(Data10)

g2 <- ggplot(Data10, aes(x=Year)) + geom_line(aes(y=Male, x=Year, color="Males"))
g2 <- g2 + geom_line(aes(y=Female, x=Year, color="Female"))
g2 <- g2 + labs(title="Overdoes Deaths from Opioid drugs from 2002-2015", x="Year", y="Deaths")
g2

#Produce one more graph

Data11 <- Data3[c(0,20,21), c(5:18)]
Data11 <- t(Data11)
Data11 <- cbind(Data11, Year)
colnames(Data11) <- c("Female", "Male", "Year")
Data11
Data12 <- t(Data11)
Data12 <- as.data.frame(Data12)
#have to flip it one more time
Data12
#Making sure its a data.frame
class(Data12)
Data12 <- as.data.frame(Data12)
class(Data12)
Data12
Data12 <- t(Data12)
Data12 <- as.data.frame(Data12)

g3 <- ggplot(Data12, aes(x=Year)) + geom_line(aes(y=Male, color="Males")) 
g3 <- g3 + geom_line(aes(y=Female, x=Year, color="Female"))
g3 <- g3 + labs(title="Overdoes Deaths from Heroin from 2002-2015", x="Year", y="Deaths")
g3

