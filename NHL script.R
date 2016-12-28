source('NHL Helper Functions.R')
library(XML)

nhl.playoffs <- function(year){
  # Import and rename data
  reg.season <- link.reader(year)
  names(reg.season) <- c("Date", "Away Team", "Goals Away", "Home Team", "Goals Home", "OT/SO", "Att.", "LOG", "Notes")
  
  # Get teams
  teams <- sort(unique(reg.season$`Away Team`))
  
  # Divide teams into Eastern and Western Conferences
  east.teams <- c("Washington Capitals", "Pittsburgh Penguins", "Florida Panthers", "New York Rangers", "New York Islanders", "Tampa Bay Lightning", "Philadelphia Flyers", "Detroit Red Wings", "Boston Bruins", "Carolina Hurricanes", "Ottawa Senators", "New Jersey Devils", "Montreal Canadiens", "Buffalo Sabres", "Columbus Blue Jackets", "Toronto Maple Leafs")
  west.teams <- teams[!(teams %in% east.teams)]
  
  # Divide East into Metropolitan and Atlantic Divisions
  metropolitan.teams <- c("Washington Capitals", "Pittsburgh Penguins", "New York Rangers", "New York Islanders", "Philadelphia Flyers", "Carolina Hurricanes", "New Jersey Devils", "Columbus Blue Jackets")
  atlantic.teams <- east.teams[!(east.teams %in% metropolitan.teams)]
  
  # Divide West into Central and Pacific Divisions
  central.teams <- c("Dallas Stars", "St. Louis Blues", "Chicago Blackhawks", "Nashville Predators", "Minnesota Wild", "Colorado Avalanche", "Winnipeg Jets")
  pacific.teams <- west.teams[!(west.teams %in% central.teams)]
  
  # Cleans data
  for(i in 1:nrow(reg.season)){
    # Gets winners and losers for each game
    if(reg.season$`Goals Away`[i] > reg.season$`Goals Home`[i]){
      reg.season[["Winner"]][i] <- reg.season$`Away Team`[i]
      reg.season[["Loser"]][i] <- reg.season$`Home Team`[i]
    }
    else{
      reg.season[["Winner"]][i] <- reg.season$`Home Team`[i]
      reg.season[["Loser"]][i] <- reg.season$`Away Team`[i]
    }
    
    # Assigns points according to 3-2-1-0 point system
    if(reg.season$`OT/SO`[i] == ""){
      reg.season[["Winner Points Awarded"]][i] <- 3
      reg.season[["Loser Points Awarded"]][i] <- 0
    }
    else{
      reg.season[["Winner Points Awarded"]][i] <- 2
      reg.season[["Loser Points Awarded"]][i] <- 1
    }
  }
  
  pts2.1 <- unlist(lapply(teams, point.totaler.2.1, df = reg.season)) # Assigns point totals for each team under 2.1 system
  pts.3.2.1.0 <- unlist(lapply(teams, point.totaler.3.2.1.0, df = reg.season)) # Assigns point totals for each team under 3.2.1.0 system
  
  points <- as.data.frame(cbind(teams, pts2.1, pts.3.2.1.0))
  names(points) <- c("Team", "Points in 2-1 System", "Points in 3-2-1-0 System")
  points$`Points in 2-1 System` <- as.integer(as.character(points$`Points in 2-1 System`))
  points$`Points in 3-2-1-0 System` <- as.integer(as.character(points$`Points in 3-2-1-0 System`))
  
  # Make divisions within the conference
  central <- as.data.frame(cbind(central.teams, rep(0, length(central.teams)), rep(0, length(central.teams))))
  metropolitan <- as.data.frame(cbind(metropolitan.teams, rep(0, length(metropolitan.teams)), rep(0, length(metropolitan.teams))))
  pacific <- as.data.frame(cbind(pacific.teams, rep(0, length(pacific.teams)), rep(0, length(pacific.teams))))
  atlantic <- as.data.frame(cbind(atlantic.teams, rep(0, length(atlantic.teams)), rep(0, length(atlantic.teams))))
  
  names(central) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  central$Team <- as.character(central$Team)
  central$`Points in 2-1-0` <- as.numeric(as.character(central$`Points in 2-1-0`))
  central$`Points in 3-2-1-0` <- as.numeric(as.character(central$`Points in 3-2-1-0`))
  
  names(atlantic) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  atlantic$Team <- as.character(atlantic$Team)
  atlantic$`Points in 2-1-0` <- as.numeric(as.character(atlantic$`Points in 2-1-0`))
  atlantic$`Points in 3-2-1-0` <- as.numeric(as.character(atlantic$`Points in 3-2-1-0`))
  
  names(metropolitan) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  metropolitan$Team <- as.character(metropolitan$Team)
  metropolitan$`Points in 2-1-0` <- as.numeric(as.character(metropolitan$`Points in 2-1-0`))
  metropolitan$`Points in 3-2-1-0` <- as.numeric(as.character(metropolitan$`Points in 3-2-1-0`))
  
  names(pacific) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  pacific$Team <- as.character(pacific$Team)
  pacific$`Points in 2-1-0` <- as.numeric(as.character(pacific$`Points in 2-1-0`))
  pacific$`Points in 3-2-1-0` <- as.numeric(as.character(pacific$`Points in 3-2-1-0`))
  
  # Fill in divisions 
  for(i in 1:nrow(points)){
    team <- points$Team[i]
    if(team %in% central$Team){
      central$`Points in 2-1-0`[central$Team == team] <- points$`Points in 2-1 System`[points$Team == team]
      central$`Points in 3-2-1-0`[central$Team == team] <- points$`Points in 3-2-1-0 System`[points$Team == team]
    }
    else{
      if(team %in% pacific$Team){
        pacific$`Points in 2-1-0`[pacific$Team == team] <- points$`Points in 2-1 System`[points$Team == team]
        pacific$`Points in 3-2-1-0`[pacific$Team == team] <- points$`Points in 3-2-1-0 System`[points$Team == team]
      }
      else{
        if(team %in% atlantic$Team){
          atlantic$`Points in 2-1-0`[atlantic$Team == team] <- points$`Points in 2-1 System`[points$Team == team]
          atlantic$`Points in 3-2-1-0`[atlantic$Team == team] <- points$`Points in 3-2-1-0 System`[points$Team == team]
        }
        else{
          metropolitan$`Points in 2-1-0`[metropolitan$Team == team] <- points$`Points in 2-1 System`[points$Team == team]
          metropolitan$`Points in 3-2-1-0`[metropolitan$Team == team] <- points$`Points in 3-2-1-0 System`[points$Team == team]
        }
      }
    }
  }
  
  east.conf <- rbind(atlantic, metropolitan)
  west.conf <- rbind(central, pacific)
  
  west.in <- data.frame(
    Team = character(),
    'Points in 2-1-0' = integer(),
    'Points in 3-2-1-0' = integer(),
    stringsAsFactors = F
  )
  east.in <- data.frame(
    Team = character(),
    V2 = integer(),
    V3 = integer(),
    stringsAsFactors = F
  )
  
  # Create playoff picture under 2-1-0
  central.index <- order(central$`Points in 2-1-0`, decreasing = T)
  central.ordered <- central[central.index, ]
  west.in[1:3, ] <- central.ordered[1:3, ]
  
  pacific.index <- order(pacific$`Points in 2-1-0`, decreasing = T)
  pacific.ordered <- pacific[pacific.index, ]
  west.in[4:6, ] <-  pacific.ordered[1:3, ]
  
  atlantic.index <- order(atlantic$`Points in 2-1-0`, decreasing = T)
  atlantic.ordered <- atlantic[atlantic.index, ]
  east.in[1:3, ] <- atlantic.ordered[1:3, ]
  
  metropolitan.index <- order(metropolitan$`Points in 2-1-0`, decreasing = T)
  metropolitan.ordered <- metropolitan[metropolitan.index, ]
  east.in[4:6, ] <- metropolitan.ordered[1:3, ]
  
  west.conf.index <- order(west.conf$`Points in 2-1-0`, decreasing = T)
  west.ordered <- west.conf[west.conf.index, ]
  
  east.conf.index <- order(east.conf$`Points in 2-1-0`, decreasing = T)
  east.ordered <- east.conf[east.conf.index, ]
  
  names(east.in) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  names(west.in) <- names(east.in)
  
  # Get wildcards
  east.teams.left <- east.ordered[!(east.ordered$Team %in% east.in$Team), ]
  east.field <- rbind(east.in, east.teams.left[1:2, ])
  
  west.teams.left <- west.ordered[!(west.ordered$Team %in% west.in$Team), ]
  west.field <- rbind(west.in, west.teams.left[1:2, ])
  
  west.in1 <- data.frame(
    Team = character(),
    'Points in 2-1-0' = integer(),
    'Points in 3-2-1-0' = integer(),
    stringsAsFactors = F
  )
  east.in1 <- data.frame(
    Team = character(),
    V2 = integer(),
    V3 = integer(),
    stringsAsFactors = F
  )
  
  # Create playoff picture under 3-2-1-0
  central.index1 <- order(central$`Points in 3-2-1-0`, decreasing = T)
  central.ordered1 <- central[central.index1, ]
  west.in1[1:3, ] <- central.ordered1[1:3, ]
  
  pacific.index1 <- order(pacific$`Points in 3-2-1-0`, decreasing = T)
  pacific.ordered1 <- pacific[pacific.index1, ]
  west.in1[4:6, ] <-  pacific.ordered1[1:3, ]
  
  atlantic.index1 <- order(atlantic$`Points in 3-2-1-0`, decreasing = T)
  atlantic.ordered1 <- atlantic[atlantic.index1, ]
  east.in1[1:3, ] <- atlantic.ordered1[1:3, ]
  
  metropolitan.index1 <- order(metropolitan$`Points in 3-2-1-0`, decreasing = T)
  metropolitan.ordered1 <- metropolitan[metropolitan.index1, ]
  east.in1[4:6, ] <- metropolitan.ordered1[1:3, ]
  
  west.conf.index1 <- order(west.conf$`Points in 3-2-1-0`, decreasing = T)
  west.ordered1 <- west.conf[west.conf.index1, ]
  
  east.conf.index1 <- order(east.conf$`Points in 3-2-1-0`, decreasing = T)
  east.ordered1 <- east.conf[east.conf.index1, ]
  
  names(east.in1) <- c("Team", "Points in 2-1-0", "Points in 3-2-1-0")
  names(west.in1) <- names(east.in1)
  
  # Get wildcards
  east.teams.left1 <- east.ordered1[!(east.ordered1$Team %in% east.in1$Team), ]
  east.field1 <- rbind(east.in1, east.teams.left1[1:2, ])
  
  west.teams.left1 <- west.ordered1[!(west.ordered1$Team %in% west.in1$Team), ]
  west.field1 <- rbind(west.in1, west.teams.left1[1:2, ])
  
  return(list(east.field, east.field1, west.field, west.field1))
}
