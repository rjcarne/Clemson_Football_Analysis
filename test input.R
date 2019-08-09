getwd()
setwd("/Users/RyanCarney/Documents/GitHub/Clemson_Football_Analysis")

data <- read.csv("2008Stats.xlsx")
library(readxl)
data <- read_excel("2008Stats.xlsx", sheet = 1, range = "A1:F112")
data<- read_excel("2008Stats.xlsx", sheet = 3)
head(data)
tail(data)

library(ggplot2)
ggplot(data = data, aes(x=Stars)) + geom_histogram(binwidth = .1, fill = "Orange")
#ggplot(data = data, aes(x = Weight(LBs), y = Recruiting Rank)
       
data <- read_excel("2008Stats.xlsx", sheet = 3)
?read_excel()

setwd("/Users/RyanCarney/Desktop/School/sportsource-data-2005")
test <- read.csv("drive.csv")
test

