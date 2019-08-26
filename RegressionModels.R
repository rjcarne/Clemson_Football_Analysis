PredictProduction <- function(Position,RecruitRank) {
  if (Position == "QB") {
    QBDepth <- DepthChart[DepthChart$Position == "QB",]
  
  }
  else if (Position == "RB") {
    RBDepth <- DepthChart[DepthChart$Position == "RB",]
    
  }
  else if (Position == "WR") {
    WRDepth <- DepthChart[DepthChart$Position == "WR",] 
                           
  }
  else if (Position == "TE") {
    TEDepth <- DepthChart[DepthChart$Position == "TE",] 
  }
  else if (Position == "CB" | Position == "DB") {
    CBDepth <- DepthChart[DepthChart$Position == "CB" | DepthChart$Position == "DB",] 
  }
  else if (Position == "DE") {
    DEDepth <- DepthChart[DepthChart$Position == "DE",] 
  }
  else if (Position == "DT" | Position == "DL") {
    DTDepth <- DepthChart[DepthChart$Position == "DT" | DepthChart$Position == "DL",] 
  }
  else if (Position == "LB") {
    LBDepth <- DepthChart[DepthChart$Position == "LB",] 
  }
  else if (Position == "S") {
    SDepth <- DepthChart[DepthChart$Position == "S",] 
  }
  else if (Position == "K" | Position == "PK") {
    KDepth <- DepthChart[DepthChart$Position == "PK" | DepthChart$Position == "K",] 
  }
  else if (Position == "P") {
    PDepth <- DepthChart[DepthChart$Position == "P",] 
  }
  
  
  PassingForever <- merge(QBDepth,CareerPassing,by.x = "Player", by.y = "Player")
  
  PassingTest <- PassingForever[PassingForever$Att > 100,]
  
  Regression <- lm(PassingTest$Yds ~ PassingTest$`Recruiting Rank`)
  summary(Regression)
  Regression$coefficients[1]
  
  
  
  
  
  
}