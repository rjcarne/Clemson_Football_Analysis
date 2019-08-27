#This file drives the program through functions and is meant to be run in increments to break down each
#position type. The purpose of this program is to keep adding data and train the model to be more accurate over time



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

#Convert Depth Charts to Position Based Careers
DepthChart <- DepthCombine()

#Predicting Production using regression models based on position and recruting ranking

#Rankings are from 0.5 to 0.9999
#Example Predictions
PredictProduction("QB",0.9999)
PredictProduction("RB",0.9999)
PredictProduction("WR",0.9999)
PredictProduction("TE",0.9999)
PredictProduction("DB",0.9999)
PredictProduction("DE",0.9999)
PredictProduction("DT",0.9999)
PredictProduction("LB",0.9999)
PredictProduction("S",0.9999)
PredictProduction("K",0.8)
PredictProduction("P",0.8)

