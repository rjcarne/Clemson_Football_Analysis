#This file contains the functions that are used to find regression models for each position.
#The functions can be altered so that the same models can be used with different variables

#Determines which regression function to call 
PredictProduction <- function(Position,RecruitRank) {
  if (Position == "QB") {
    PredictedStats <- PassingRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "RB") {
    PredictedStats <- RushingRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "WR") {
    PredictedStats <- RecievingRegression(RecruitRank)
    return(PredictedStats)                       
  }
  else if (Position == "TE") {
    PredictedStats <- TERegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "CB" | Position == "DB") {
    PredictedStats <- DBRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "DE") {
    DefDepth <- DepthChart[DepthChart$Position == "DE",] 
    PredictedStats <- DERegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "DT" | Position == "DL") {
    PredictedStats <- DTRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "LB") {
    PredictedStats <- LBRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "S") {
    PredictedStats <- SRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "K" | Position == "PK") {
    PredictedStats <- KRegression(RecruitRank)
    return(PredictedStats)
  }
  else if (Position == "P") {
    PredictedStats <- PRegression(RecruitRank)
    return(PredictedStats)
  }
}

#Returns a projected career stat line based on recruiting ranking
PassingRegression <- function(RecruitRank) {
  QBDepth <- DepthChart[DepthChart$Position == "QB",]
  PassingStats <- merge(QBDepth,CareerPassing,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters into QB's with at least 100 attempts to remove players who rarely played
  PassingStats <- PassingStats[PassingStats$Att > 100,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(PassingStats[,i] ~ PassingStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","Cmp","Att","Pct","Yds","Y/A","TD","Int","PssrRate","Rush_Att","Rush_Yds","Rush_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
RushingRegression <- function(RecruitRank) {
  RBDepth <- DepthChart[DepthChart$Position == "RB",]
  RushingStats <- merge(RBDepth,CareerRushing,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:8)
  PredictedStats[1] <- RecruitRank
  
  #Filters into RBs with at least 100 carries
  RushingStats <- RushingStats[RushingStats$Attempts > 100,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:14) {
    PredictionModel <- lm(RushingStats[,i] ~ RushingStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","Attempts","Yds","Avg","TD","Receptions","Yards_Rec","TD_Rec")  
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
RecievingRegression <- function(RecruitRank) {
  WRDepth <- DepthChart[DepthChart$Position == "WR",]
  RecievingStats <- merge(WRDepth,CareerRecieving,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:5)
  PredictedStats[1] <- RecruitRank
  
  #Filters into WR's with at least 30 career catches
  RecievingStats <- RecievingStats[RecievingStats$Receptions > 30,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:11) {
    PredictionModel <- lm(RecievingStats[,i] ~ RecievingStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","Receptions","Yds","Avg","TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
TERegression <- function(RecruitRank) {
  TEDepth <- DepthChart[DepthChart$Position == "TE",]
  RecievingStats <- merge(TEDepth,CareerRecieving,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:5)
  PredictedStats[1] <- RecruitRank
  
  #Filters into TE's with at least 20 catches
  RecievingStats <- RecievingStats[RecievingStats$Receptions > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:11) {
    PredictionModel <- lm(RecievingStats[,i] ~ RecievingStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","Receptions","Yds","Avg","TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
DBRegression <- function(RecruitRank) {
  DefStats <- DepthChart[DepthChart$Position == "CB" | DepthChart$Position == "DB",]
  DefStats <- merge(DefStats,CareerDefense,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters out Defensive players with less than 20 tackles in their career
  DefStats <- DefStats[DefStats$Tot > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(DefStats[,i] ~ DefStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Ranking","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
DERegression <- function(RecruitRank) {
  DefStats <- DepthChart[DepthChart$Position == "DE",] 
  DefStats <- merge(DefStats,CareerDefense,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters out Defensive players with less than 20 tackles in their career
  DefStats <- DefStats[DefStats$Tot > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(DefStats[,i] ~ DefStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Ranking","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
DTRegression <- function(RecruitRank) {
  DefStats <- DepthChart[DepthChart$Position == "DT" | DepthChart$Position == "DL",] 
  DefStats <- merge(DefStats,CareerDefense,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters out Defensive players with less than 20 tackles in their career
  DefStats <- DefStats[DefStats$Tot > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(DefStats[,i] ~ DefStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Ranking","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
LBRegression <- function(RecruitRank) {
  DefStats <- DepthChart[DepthChart$Position == "LB",] 
  DefStats <- merge(DefStats,CareerDefense,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters out Defensive players with less than 20 tackles in their career
  DefStats <- DefStats[DefStats$Tot > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(DefStats[,i] ~ DefStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Ranking","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
SRegression <- function(RecruitRank) {
  DefStats <- DepthChart[DepthChart$Position == "S",] 
  DefStats <- merge(DefStats,CareerDefense,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:12)
  PredictedStats[1] <- RecruitRank
  
  #Filters out Defensive players with less than 20 tackles in their career
  DefStats <- DefStats[DefStats$Tot > 20,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:18) {
    PredictionModel <- lm(DefStats[,i] ~ DefStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Ranking","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
KRegression <- function(RecruitRank) {
  KStats <- DepthChart[DepthChart$Position == "PK" | DepthChart$Position == "K",] 
  KStats <- merge(KStats,CareerKicking,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:6)
  PredictedStats[1] <- RecruitRank
  
  #Filters Kickers who attempted at least 30 PATs
  KStats <- KStats[KStats$XPA > 30,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 8:12) {
    PredictionModel <- lm(KStats[,i] ~ KStats$`Recruiting Rank`)
    PredictedStats[i-6] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","XPM","XPA","FGM","FGA","Points")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}

#Returns a projected career stat line based on recruiting ranking
PRegression <- function(RecruitRank) {
  
  KStats <- DepthChart[DepthChart$Position == "P",] 
  KStats <- merge(KStats,CareerKicking,by.x = "Player", by.y = "Player")
  PredictedStats <- c(1:4)
  PredictedStats[1] <- RecruitRank
  
  #Filters into Punters with at least 15 career punts
  KStats <- KStats[KStats$Punts > 15,]
  
  #Creates an vector with each type of stat projection to return. Each iteration finds a new regression model for the stat
  for (i in 13:15) {
    PredictionModel <- lm(KStats[,i] ~ KStats$`Recruiting Rank`)
    PredictedStats[i-11] <- PredictionModel$coefficients[1] + (RecruitRank * PredictionModel$coefficients[2])
  }
  statnames <- c("Recruiting Rank","Punts","Punt_Yards","Avg_Punt_Yds")
  names(PredictedStats) <-statnames
  return(PredictedStats)
}
