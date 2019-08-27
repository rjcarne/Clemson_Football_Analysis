#Create vectors of file names, sheets in each file, etc
setwd("/Users/RyanCarney/Documents/GitHub/Clemson_Football_Analysis/BoxScores")

inFileNames <- c("2008.xlsx","2009.xlsx","2010.xlsx","2011.xlsx","2012.xlsx",
                 "2013.xlsx","2014.xlsx","2015.xlsx","2016.xlsx","2017.xlsx",
                 "2018.xlsx")

num_GamesinSeason <- c(8,15,14,15,14,14,14,16,16,15,16)
StatNames <- c("Passing","Rushing","Recieving","Defense","Kicking")
Years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")



Clemson <- list()
DepthCharts <- list()


#Read Depth Chart
#depth2008 <- readxl::read_excel(inFileNames[1], sheet = 1, range = "$A$1:$F$112")

#Loop through all seasons
for (g in 1:11) {
    
    #Read Depth Chart
    depth <- readxl::read_excel(inFileNames[g], sheet = 1, range = "$A$1:$F$122")
    statnames <- c("Player","Position","Height(Inches)","Weight(LBs)","Recruiting Rank","Stars")
    #Set All NA's to 0
    for (i in 1:120) {
        for (j in 1:6) {
            if (is.na(depth[i,j])) {
                depth[i,j] <- 0
            }
        }
    }
    depth <- subset(depth,Position != 0)
    swap <- depth[,1]
    depth[,1] <- depth[,2]
    depth[,2] <- swap
    colnames(depth) <- statnames
    DepthCharts[[g]] <- depth
    rm(depth,swap)
    
    #Create list for season
    Season <- list()
    
    #Loop through each game
    for (k in 2:num_GamesinSeason[g]) {

        #Loop for games with k
        Passing <- readxl::read_excel(inFileNames[g], sheet = k, range = "$A$2:$M$9")

        #Passing

        #Set All NA's to 0
        for (i in 1:7) {
            for (j in 1:13) {
                if (is.na(Passing[i,j])) {
                    Passing[i,j] <- 0
                }
            }
        }
        #trim Null Rows
        Passing <- subset(Passing,Position != 0)
        
        #Rushing
        Rushing <- readxl::read_excel(inFileNames[g], sheet = k, range = "$A$11:$I$20")
        #Set All NA's to 0
        for (i in 1:8) {
            for (j in 1:9) {
                if (is.na(Rushing[i,j])) {
                    Rushing[i,j] <- 0
                    }
            }
        }
        #trim Null Rows
        Rushing <- subset(Rushing,Position != 0)
           
        
        #Recieving
        Recieving <- readxl::read_excel(inFileNames[g], sheet = k, range = "$A$23:$F$49")
        #Set All NA's to 0
        for (i in 1:22) {
            for (j in 1:6) {
                if (is.na(Recieving[i,j])) {
                    Recieving[i,j] <- 0
                }
            }
        }
        #trim Null Rows
        Recieving <- subset(Recieving,Position != 0)
        
        
        #Defense
        Defense <- readxl::read_excel(inFileNames[g], sheet = k, range = "$A$52:$M$109")
        #Set All NA's to 0
        for (i in 1:51) {
            for (j in 1:13) {
                if (is.na(Defense[i,j])) {
                    Defense[i,j] <- 0
                }
            }
            }
        #trim Null Rows
        Defense <- subset(Defense,Position != 0)
        

        #Kicking

        Kicking <- readxl::read_excel(inFileNames[g], sheet = k, range = "$A$112:$J$118")

        #Set All NA's to 0
        for (i in 1:7) {
            for (j in 1:10) {
                if (is.na(Kicking[i,j])) {
                    Kicking[i,j] <- 0
                }
            }
        }
        #trim Null Rows
        Kicking <- subset(Kicking,Position != 0)
        
        
        #Compile Into One List

        Game <- list(Passing,Rushing,Recieving,Defense,Kicking)
        names(Game) <- StatNames



        #Add Game to Season
        Season[[k-1]] <- Game

        rm(Passing,Rushing,Recieving,Defense,Kicking,i,j,Game)

        }

    #Add Season to All Stats
    
    Clemson[[g]] <- Season
    rm(Season)
}
names(DepthCharts) <- Years
names(Clemson) <- Years
rm (g,k,inFileNames,num_GamesinSeason,StatNames,Years,statnames)

#Change working directory back to master for R scripts
setwd("/Users/RyanCarney/Documents/GitHub/Clemson_Football_Analysis")




