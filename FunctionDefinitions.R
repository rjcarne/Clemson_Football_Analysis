#Returns an individual stat value from a single game
PullStat <- function(Year,GameNum,StatType,Player,Value) {
  #Year = Season year in quotes
  Season_Break <- Clemson[[Year]]
  
  #GameNum = Game number that year
  Game_Break <- Season_Break[[GameNum]]
  
  #StatType = "Passing","Rushing","Recieving","Defense","Kicking"
  State_Break <- Game_Break[[StatType]]
  
  #Player = Player name(Row)
  #Value = Stat Desired(Col)
  ReturnValue <- State_Break[Player,Value]  
  return(ReturnValue)
}

#Returns a value from a depth chart. Used for recruiting rank analysis
PullDepthStat <- function(Year,Player,Value) {
  #Year in quotes
  #Player = Player name in quotes
  Season_Chart <- DepthCharts[[Year]]
  PlayerNum <- NULL
  for (i in 1:nrow(Season_Chart)) {
    if (Season_Chart[i,2] == Player) {
      PlayerNum <- i
      break;
    }
  }
  ReturnValue <- Season_Chart[PlayerNum,Value]
  return(ReturnValue)
}

#returns entire game's worth of data in a data frame for a specific position
PullGameStatDF <- function(Year,GameNum, StatType) {
  #Year = Season year in quotes
  Season_Break <- Clemson[[Year]]
  
  #GameNum = Game number that year
  Game_Break <- Season_Break[[GameNum]]
  
  #StatType = "Passing","Rushing","Recieving","Defense","Kicking"
  Stat_Break <- Game_Break[[StatType]]
  
  return(Stat_Break)
}

#Returns a data frame with the stats of an entire season
GetSeasonStatsPass <- function(Year) {
  statnames <- c("Player","Position","Cmp","Att","Pct","Yds","Y/A","TD","Int","PssrRate","Rush_Att","Rush_Yds","Rush_TD")
  StatYear <- PullGameStatDF(Year,1,"Passing")
  for (j in 2:length(Clemson[[Year]])) {
    StatYear <- merge(StatYear,PullGameStatDF(Year,j,"Passing"),by.x = "Player", by.y = "Player")
    for (i in 3:13) {
      StatYear[,i] <- StatYear[,i] + StatYear[,15]
      StatYear[,15] <- NULL
    }
    StatYear[,14] <- NULL
  }
  StatYear[,5] <- StatYear[,3] / StatYear[,4]
  StatYear[,7] <- StatYear[,6] / StatYear[,4]
  StatYear[,10] <- ((8.4 * StatYear[,6]) + (330 * StatYear[,8]) - (200 * StatYear[,9]) + (100 * StatYear[,3])) / StatYear[,4]
  #8.4 * Yds + 330 * TD - 200 * Int + 100 * Cmp) / Att
  colnames(StatYear) <- statnames
  return(StatYear)
}

#Returns a data frame with the stats of an entire season
GetSeasonStatsRush <- function(Year) {
  statnames <- c("Player","Position","Attempts","Yds","Avg","TD","Receptions","Yards_Rec","TD_Rec")
  StatYear <- PullGameStatDF(Year,1,"Rushing")
  for (j in 2:length(Clemson[[Year]])) {
    StatYear <- merge(StatYear,PullGameStatDF(Year,j,"Rushing"),by.x = "Player", by.y = "Player")
    for (i in 3:9) {
      StatYear[,i] <- StatYear[,i] + StatYear[,11]
      StatYear[,11] <- NULL
    }
    StatYear[,10] <- NULL
  }
  StatYear[,5] <- StatYear[,4] / StatYear[,3]
  colnames(StatYear) <- statnames
  return(StatYear)
}

#Returns a data frame with the stats of an entire season
GetSeasonStatsRec <- function(Year) {
  statnames <- c("Player","Position","Receptions","Yds","Avg","TD")
  StatYear <- PullGameStatDF(Year,1,"Recieving")
  for (j in 2:length(Clemson[[Year]])) {
    StatYear <- merge(StatYear,PullGameStatDF(Year,j,"Recieving"),by.x = "Player", by.y = "Player")
    for (i in 3:6) {
      StatYear[,i] <- StatYear[,i] + StatYear[,8]
      StatYear[,8] <- NULL
    }
    StatYear[,7] <- NULL
  }
  StatYear[,5] <- StatYear[,4] / StatYear[,3]
  colnames(StatYear) <- statnames
  return(StatYear)
}

#Returns a data frame with the stats of an entire season
GetSeasonStatsDefense <- function(Year) {
  statnames <- c("Player","Position","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  StatYear <- PullGameStatDF(Year,1,"Defense")
  for (j in 2:length(Clemson[[Year]])) {
    StatYear <- merge(StatYear,PullGameStatDF(Year,j,"Defense"),by.x = "Player", by.y = "Player")
    for (i in 3:13) {
      StatYear[,i] <- StatYear[,i] + StatYear[,15]
      StatYear[,15] <- NULL
    }
    StatYear[,14] <- NULL
  }
  colnames(StatYear) <- statnames
  return(StatYear)
}

#Returns a data frame with the stats of an entire season
GetSeasonStatsKicking <- function(Year) {
  statnames <- c("Player","Position","XPM","XPA","FGM","FGA","Points","Punts","Punt_Yards","Avg_Punt_Yds")
  StatYear <- PullGameStatDF(Year,1,"Kicking")
  for (j in 2:length(Clemson[[Year]])) {
    StatYear <- merge(StatYear,PullGameStatDF(Year,j,"Kicking"),by.x = "Player", by.y = "Player")
    for (i in 3:10) {
      StatYear[,i] <- StatYear[,i] + StatYear[,12]
      StatYear[,12] <- NULL
    }
    StatYear[,11] <- NULL
  }
  StatYear[,10] <- StatYear[,9] / StatYear[,8]
  colnames(StatYear) <- statnames
  return(StatYear)
}

#Creates a tiered list of all season data for over time analysis
CompileCareerStats <- function() {
  SeasonsPassing <- list()
  SeasonsRushing <- list()
  SeasonsRecieving <- list()
  SeasonsDefense <- list()
  SeasonsKicking <- list()
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  StatTypes <- c("Passing","Rushing","Recieving","Defense","Kicking")
  for (i in 1:length(Years)) {
    
    SeasonsPassing[[i]] <- GetSeasonStatsPass(Years[i])
    SeasonsRushing[[i]] <- GetSeasonStatsRush(Years[i])
    SeasonsRecieving[[i]] <- GetSeasonStatsRec(Years[i])
    SeasonsDefense[[i]] <- GetSeasonStatsDefense(Years[i])
    SeasonsKicking[[i]] <- GetSeasonStatsKicking(Years[i])
  }
  names(SeasonsPassing) <- Years
  names(SeasonsRushing) <- Years
  names(SeasonsRecieving) <- Years
  names(SeasonsDefense) <- Years
  names(SeasonsKicking) <- Years
  
  Masterlist <- list(SeasonsPassing,SeasonsRushing,SeasonsRecieving,SeasonsDefense,SeasonsKicking)
  names(Masterlist) <- StatTypes
  
  return(Masterlist)
  
}

#Adds all seasons together by merging and adding values
CareerPassingStats <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  season <- SeasonTotals[["Passing"]]
  statnames <- c("Player","Position","Cmp","Att","Pct","Yds","Y/A","TD","Int","PssrRate","Rush_Att","Rush_Yds","Rush_TD")
  StatYear <- season[["2008"]]

  for (p in 2:length(Years)) {
    StatYear <- merge.data.frame(StatYear,season[[Years[p]]],by.x = "Player", by.y = "Player",all.x = TRUE,all.y = TRUE)
    StatYear[,2] <- "QB"
    
    for (i in 1:nrow(StatYear)) {
      for (j in 1:ncol(StatYear)) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- 0
        }
      }
    }
    for (i in 3:13) {
      StatYear[,i] <- StatYear[,i] + StatYear[,15]
      StatYear[,15] <- NULL
    }
    StatYear[,5] <- StatYear[,3] / StatYear[,4]
    StatYear[,7] <- StatYear[,6] / StatYear[,4]
    StatYear[,10] <- ((8.4 * StatYear[,6]) + (330 * StatYear[,8]) - (200 * StatYear[,9]) + (100 * StatYear[,3])) / StatYear[,4]
    
    StatYear[,14] <- NULL
  }
  
  colnames(StatYear) <- statnames
  rm(i,j)
  return(StatYear)

}

#Adds all seasons together by merging and adding values
CareerRushingStats <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  season <- SeasonTotals[["Rushing"]]
  statnames <- c("Player","Position","Attempts","Yds","Avg","TD","Receptions","Yards_Rec","TD_Rec")
  StatYear <- season[["2008"]]
  
  for (p in 2:length(Years)) {
    StatYear <- merge.data.frame(StatYear,season[[Years[p]]],by.x = "Player", by.y = "Player",all.x = TRUE,all.y = TRUE)
    StatYear[,2] <- "RB"
    
    for (i in 1:nrow(StatYear)) {
      for (j in 1:ncol(StatYear)) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- 0
        }
      }
    }
    for (i in 3:9) {
      StatYear[,i] <- StatYear[,i] + StatYear[,11]
      StatYear[,11] <- NULL
    }
    StatYear[,5] <- StatYear[,4] / StatYear[,3]
    
    StatYear[,10] <- NULL
  }
  
  colnames(StatYear) <- statnames
  rm(i,j)
  return(StatYear)
}

#Adds all seasons together by merging and adding values
CareerRecievingStats <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  season <- SeasonTotals[["Recieving"]]
  statnames <- c("Player","Position","Receptions","Yds","Avg","TD")
  StatYear <- season[["2008"]]
  
  for (p in 2:length(Years)) {
    StatYear <- merge.data.frame(StatYear,season[[Years[p]]],by.x = "Player", by.y = "Player",all.x = TRUE,all.y = TRUE)
    for (q in 1:nrow(StatYear)) {
      if (is.na(StatYear[q,2])) {
        StatYear[q,2] <- StatYear[q,7]
      }
    }

    for (i in 1:nrow(StatYear)) {
      for (j in 1:ncol(StatYear)) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- 0
        }
      }
    }
    for (i in 3:6) {
      StatYear[,i] <- StatYear[,i] + StatYear[,8]
      StatYear[,8] <- NULL
    }

    StatYear[,7] <- NULL
  }
  StatYear[,5] <- StatYear[,4] / StatYear[,3]
  colnames(StatYear) <- statnames
  rm(i,j)
  return(StatYear)
}

#Adds all seasons together by merging and adding values
CareerDefenseStats <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  season <- SeasonTotals[["Defense"]]
  statnames <- c("Player","Position","Solo","Ast","Tot","TFL","Sack","Int","PBU","Pick_6","FR","FF","FR_TD")
  StatYear <- season[["2008"]]
  
  for (p in 2:length(Years)) {
    StatYear <- merge.data.frame(StatYear,season[[Years[p]]],by.x = "Player", by.y = "Player",all.x = TRUE,all.y = TRUE)
    for (q in 1:nrow(StatYear)) {
      if (is.na(StatYear[q,2])) {
        StatYear[q,2] <- StatYear[q,14]
      }
    }
    
    for (i in 1:nrow(StatYear)) {
      for (j in 1:ncol(StatYear)) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- 0
        }
      }
    }
    for (i in 3:13) {
      StatYear[,i] <- StatYear[,i] + StatYear[,15]
      StatYear[,15] <- NULL
    }
    
    StatYear[,14] <- NULL
  }
  colnames(StatYear) <- statnames
  rm(i,j)
  return(StatYear)
}

CareerKickingStats <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  season <- SeasonTotals[["Kicking"]]
  statnames <- c("Player","Position","XPM","XPA","FGM","FGA","Points","Punts","Punt_Yards","Avg_Punt_Yds")
  StatYear <- season[["2008"]]
  
  for (p in 2:length(Years)) {
    StatYear <- merge.data.frame(StatYear,season[[Years[p]]],by.x = "Player", by.y = "Player",all.x = TRUE,all.y = TRUE)
    for (q in 1:nrow(StatYear)) {
      if (is.na(StatYear[q,2])) {
        StatYear[q,2] <- StatYear[q,11]
      }
    }
    
    for (i in 1:nrow(StatYear)) {
      for (j in 1:ncol(StatYear)) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- 0
        }
      }
    }
    for (i in 3:10) {
      StatYear[,i] <- StatYear[,i] + StatYear[,12]
      StatYear[,12] <- NULL
    }
    
    StatYear[,11] <- NULL
  }
  colnames(StatYear) <- statnames
  StatYear[,10] <- StatYear[,9] / StatYear[,8]
  rm(i,j)
  return(StatYear)
}

DepthCombine <- function() {
  Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  StatYear <- DepthCharts[["2008"]]
  statnames <- c("Player","Position","Height(Inches)","Weight(LBs)","Recruiting Rank","Stars")
  colnames(StatYear) <- statnames
  for (p in 2:length(Years)) {  
    StatYear <- merge(StatYear,DepthCharts[[p]], by.x = "Player", by.y = "Player", all.x = TRUE,all.y = TRUE)
    for (i in 1:nrow(StatYear)) {
      for (j in 2:6) {
        if (is.na(StatYear[i,j])) {
          StatYear[i,j] <- StatYear[i,j+5]
        }
      }
    }
    StatYear[,7:11] <- NULL
    colnames(StatYear) <- statnames
  }
  return(StatYear)
}
