#Sets working directory to folder from GitHub
setwd("/Users/RyanCarney/Documents/GitHub/Clemson_Football_Analysis")

#Reads in data from excel files into RStudio
source("ReadData.R")

#Defines Functions
source("FunctionDefinitions.R")

source("RegressionModels.R")

#Combine all entered data into data frames by season and by career
SeasonTotals <- CompileCareerStats()

#Career Totals
CareerPassing <- CareerPassingStats()
CareerRushing <- CareerRushingStats()
CareerRecieving <- CareerRecievingStats()
CareerDefense <- CareerDefenseStats()
CareerKicking <- CareerKickingStats()


#Move this to a different file, narrow it down to starters (greater than 100 attempts probably)

#Convert Depth Charts to Position Based Careers
DepthChart <- DepthCombine()



QBDepth <- DepthChart[DepthChart$Position == "QB",]
RBDepth <- DepthChart[DepthChart$Position == "RB",]
RecDepth <- DepthChart[DepthChart$Position == "WR" 
                       | DepthChart$Position == "TE",]
DefDepth <- DepthChart[DepthChart$Position == "CB"
                       | DepthChart$Position == "DB"
                       | DepthChart$Position == "DE"
                       | DepthChart$Position == "DL"
                       | DepthChart$Position == "DT"
                       | DepthChart$Position == "LB"
                       | DepthChart$Position == "S",]
KickDepth <- DepthChart[DepthChart$Position == "K"
                       | DepthChart$Position == "PK"
                       | DepthChart$Position == "P",]

PassingStats <- merge(QBDepth,CareerPassing,by.x = "Player", by.y = "Player")

PassingTest <- PassingStats[PassingStats$Att > 100,]

Regression <- lm(PassingTest$Yds ~ PassingTest$`Recruiting Rank`)
summary(Regression)
Regression$coefficients[1]

testval <- Regression$coefficients[1] + (Regression$coefficients[2] * 0.9999)
testval


