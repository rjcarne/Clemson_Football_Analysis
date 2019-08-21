PullData <- function(Year,GameNum,StatType,Player,Value) {
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