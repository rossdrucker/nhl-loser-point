cat('\014')
library(XML)
source('NHL Helper Functions 2.R')

# Current format and alignment
if(year >= 2014){
  reg.season <- link.reader(year)
  names(reg.season) <- c("Date", "Away Team", "Goals Away", "Home Team", "Goals Home", "OT/SO", "Attendance", "LOG", "Notes")
  
  teams <- sort(unique(reg.season$`Away Team`))
  
  # Divide teams into Eastern and Western Conferences
  east.teams <- c("Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Columbus Blue Jackets", 
                  "Detroit Red Wings", "Florida Panthers", "Montreal Canadiens", "New Jersey Devils", 
                  "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", 
                  "Pittsburgh Penguins", "Tampa Bay Lightning", "Toronto Maple Leafs", 
                  "Washington Capitals")
  west.teams <- teams[!(teams %in% east.teams)]
  
  # Divide East into Metropolitan and Atlantic Divisions
  metropolitan.teams <- c("Carolina Hurricanes", "Columbus Blue Jackets", "New Jersey Devils", 
                          "New York Islanders", "New York Rangers", "Philadelphia Flyers", 
                          "Pittsburgh Penguins", "Washington Capitals")
  atlantic.teams <- east.teams[!(east.teams %in% metropolitan.teams)]
  
  # Divide West into Central and Pacific Divisions
  pacific.teams <- c("Anaheim Ducks", "Arizona Coyotes", "Calgary Flames", "Edmonton Oilers", 
                     "Los Angeles Kings", "San Jose Sharks", "Vancouver Canucks")
  central.teams <- west.teams[!(west.teams %in% pacific.teams)]
  
  # Gets data to be of right types
  reg.season$`Goals Away` <- as.numeric(as.character(reg.season$`Goals Away`))
  reg.season$`Goals Home` <- as.numeric(as.character(reg.season$`Goals Home`))
  
  reg.season <- na.omit(reg.season) # THIS LINE REMOVES ANY POSTPONED GAMES
  
  # Cleans data
  for(i in 1:nrow(reg.season)){
    # Gets winners and losers for each game
    if(reg.season$`Goals Away`[i] >= reg.season$`Goals Home`[i]){
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
  
  points <- data.frame(
    teams,
    Points = as.numeric(as.character(unlist(lapply(teams, point.totaler, df = reg.season)))), # Assigns point totals for each team under 2.1 system
    reg.wins = as.numeric(as.character(unlist(lapply(teams, reg.win.counter, df = reg.season)))), # Regulation wins
    ot.wins = as.numeric(as.character(unlist(lapply(teams, ot.win.counter, df = reg.season)))), # Overtime wins
    so.wins = as.numeric(as.character(unlist(lapply(teams, so.win.counter, df = reg.season)))), # Shootout wins
    reg.loss = as.numeric(as.character(unlist(lapply(teams, reg.loss.counter, df = reg.season)))), # Regulation losses
    ot.loss = as.numeric(as.character(unlist(lapply(teams, ot.loss.counter, df = reg.season)))), # Overtime losses
    so.loss = as.numeric(as.character(unlist(lapply(teams, so.loss.counter, df = reg.season)))), # Shootout losses
    gf = as.numeric(as.character(unlist(lapply(teams, goals.for, df = reg.season)))), # Goals for
    ga = as.numeric(as.character(unlist(lapply(teams, goals.against, df = reg.season)))),
    gp = as.numeric(as.character(unlist(lapply(teams, gp, df = reg.season)))),
    stringsAsFactors = F
  )
  
  points[["ROW"]] <- points$reg.wins + points$ot.wins
  points[["DIFF"]] <- points$gf - points$ga
  
  # Make within the conference divisions
  atlantic <- div.builder("atlantic")
  central <- div.builder("central")
  metropolitan <- div.builder("metropolitan")
  pacific <- div.builder("pacific")
  
  # Fill in division
  for(i in 1:nrow(points)){
    team <- points$teams[i]
    if(team %in% central$Team){
      central$Points[central$Team == team] <- points$Points[points$teams == team]
      central$reg.wins[central$Team == team] <- points$reg.wins[points$teams == team]
      central$ot.wins[central$Team == team] <- points$ot.wins[points$teams == team]
      central$so.wins[central$Team == team] <- points$so.wins[points$teams == team]
      central$reg.loss[central$Team == team] <- points$reg.loss[points$teams == team]
      central$ot.loss[central$Team == team] <- points$ot.loss[points$teams == team]
      central$so.loss[central$Team == team] <- points$so.loss[points$teams == team]
      central$gf[central$Team == team] <- points$gf[points$teams == team]
      central$ga[central$Team == team] <- points$ga[points$teams == team]
      central$gp[central$Team == team] <- points$gp[points$teams == team]
      central$ROW[central$Team == team] <- points$ROW[points$teams == team]
      central$DIFF[central$Team == team] <- points$DIFF[points$teams == team]
    }
    else{
      if(team %in% metropolitan$Team){
        metropolitan$Points[metropolitan$Team == team] <- points$Points[points$teams == team]
        metropolitan$reg.wins[metropolitan$Team == team] <- points$reg.wins[points$teams == team]
        metropolitan$ot.wins[metropolitan$Team == team] <- points$ot.wins[points$teams == team]
        metropolitan$so.wins[metropolitan$Team == team] <- points$so.wins[points$teams == team]
        metropolitan$reg.loss[metropolitan$Team == team] <- points$reg.loss[points$teams == team]
        metropolitan$ot.loss[metropolitan$Team == team] <- points$ot.loss[points$teams == team]
        metropolitan$so.loss[metropolitan$Team == team] <- points$so.loss[points$teams == team]
        metropolitan$gf[metropolitan$Team == team] <- points$gf[points$teams == team]
        metropolitan$ga[metropolitan$Team == team] <- points$ga[points$teams == team]
        metropolitan$gp[metropolitan$Team == team] <- points$gp[points$teams == team]
        metropolitan$ROW[metropolitan$Team == team] <- points$ROW[points$teams == team]
        metropolitan$DIFF[metropolitan$Team == team] <- points$DIFF[points$teams == team]
      }
      else{
        if(team %in% atlantic$Team){
          atlantic$Points[atlantic$Team == team] <- points$Points[points$teams == team]
          atlantic$reg.wins[atlantic$Team == team] <- points$reg.wins[points$teams == team]
          atlantic$ot.wins[atlantic$Team == team] <- points$ot.wins[points$teams == team]
          atlantic$so.wins[atlantic$Team == team] <- points$so.wins[points$teams == team]
          atlantic$reg.loss[atlantic$Team == team] <- points$reg.loss[points$teams == team]
          atlantic$ot.loss[atlantic$Team == team] <- points$ot.loss[points$teams == team]
          atlantic$so.loss[atlantic$Team == team] <- points$so.loss[points$teams == team]
          atlantic$gf[atlantic$Team == team] <- points$gf[points$teams == team]
          atlantic$ga[atlantic$Team == team] <- points$ga[points$teams == team]
          atlantic$gp[atlantic$Team == team] <- points$gp[points$teams == team]
          atlantic$ROW[atlantic$Team == team] <- points$ROW[points$teams == team]
          atlantic$DIFF[atlantic$Team == team] <- points$DIFF[points$teams == team]
        }
        else{
          pacific$Points[pacific$Team == team] <- points$Points[points$teams == team]
          pacific$reg.wins[pacific$Team == team] <- points$reg.wins[points$teams == team]
          pacific$ot.wins[pacific$Team == team] <- points$ot.wins[points$teams == team]
          pacific$so.wins[pacific$Team == team] <- points$so.wins[points$teams == team]
          pacific$reg.loss[pacific$Team == team] <- points$reg.loss[points$teams == team]
          pacific$ot.loss[pacific$Team == team] <- points$ot.loss[points$teams == team]
          pacific$so.loss[pacific$Team == team] <- points$so.loss[points$teams == team]
          pacific$gf[pacific$Team == team] <- points$gf[points$teams == team]
          pacific$ga[pacific$Team == team] <- points$ga[points$teams == team]
          pacific$gp[pacific$Team == team] <- points$gp[points$teams == team]
          pacific$ROW[pacific$Team == team] <- points$ROW[points$teams == team]
          pacific$DIFF[pacific$Team == team] <- points$DIFF[points$teams == team]
        }
      }
    }
  }
  
  a <- div.orderer(atlantic, df1 = reg.season, df2 = atlantic, df3 = reg.season, df4 = points)
  c <- div.orderer(central, df1 = reg.season, df2 = central, df3 = reg.season, df4 = points)
  m <- div.orderer(metropolitan, df1 = reg.season, df2 = metropolitan, df3 = reg.season, df4 = points)
  p <- div.orderer(pacific, df1 = reg.season, df2 = pacific, df3 = reg.season, df4 = points)
  
  east.playoffs <- playoff.picture.builder(div1 = a, div2 = m, df1 = reg.season, df2 = points, df3 = reg.season, df4 = points)
  west.playoffs <- playoff.picture.builder(div1 = c, div2 = p, df1 = reg.season, df2 = points, df3 = reg.season, df4 = points)
  
  list(east.playoffs, west.playoffs)
}

# Atlanta moved to Winnipeg in 2011-2012 (year = 2012)

# ROW tiebreaker instituted 2010-2011, old alignment
if(year > 2010 & year < 2014){
  reg.season <- link.reader(year)
  names(reg.season) <- c("Date", "Away Team", "Goals Away", "Home Team", "Goals Home", "OT/SO", "Attendance", "LOG", "Notes")
  
  teams <- sort(unique(reg.season$`Away Team`))
  
  # Divide teams into Eastern and Western Conferences
  if(year < 2012){
    east.teams <- c("Atlanta Thrashers", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Florida Panthers", "Montreal Canadiens", "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "Tampa Bay Lightning", "Toronto Maple Leafs", "Washington Capitals")
  }
  if(year >= 2012){
    east.teams <- c("Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Florida Panthers", "Montreal Canadiens", "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "Tampa Bay Lightning", "Toronto Maple Leafs", "Washington Capitals", "Winnipeg Jets")
    
  }
  west.teams <- teams[!(teams %in% east.teams)]
  
  atlantic.teams <- c("New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers", "Pittsburgh Penguins")
  northeast.teams <- c("Boston Bruins", "Buffalo Sabres", "Montreal Canadiens", "Ottawa Senators", "Toronto Maple Leafs")
  if(year < 2012){
    southeast.teams <- c("Atlanta Thrashers", "Carolina Hurricanes", "Florida Panthers", "Tampa Bay Lightning", "Washington Capitals")
  }
  if(year >= 2012){
    southeast.teams <- c("Carolina Hurricanes", "Florida Panthers", "Tampa Bay Lightning", "Washington Capitals", "Winnipeg Jets")
  }
  central.teams <- c("Chicago Blackhawks", "Columbus Blue Jackets", "Detroit Red Wings", "Nashville Predators", "St. Louis Blues")
  northwest.teams <- c("Calgary Flames", "Colorado Avalanche", "Edmonton Oilers", "Minnesota Wild", "Vancouver Canucks")
  pacific.teams <- c("Dallas Stars", "Los Angeles Kings", "Anaheim Ducks", "Phoenix Coyotes", "San Jose Sharks")
  
  # Gets data to be of right types
  reg.season$`Goals Away` <- as.numeric(as.character(reg.season$`Goals Away`))
  reg.season$`Goals Home` <- as.numeric(as.character(reg.season$`Goals Home`))
  
  reg.season <- na.omit(reg.season) # THIS LINE REMOVES ANY POSTPONED GAMES
  
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
    # Assigns points: 3 for a regulation win, 2 for an OT/SO win, 1 for an OT/SO loss, and 0 for regulation loss
    if(reg.season$`OT/SO`[i] == ""){
      reg.season[["Winner Points Awarded"]][i] <- 3
      reg.season[["Loser Points Awarded"]][i] <- 0
    }
    else{
      if(reg.season$`OT/SO`[i] != "" & (reg.season$`Goals Away`[i] != reg.season$`Goals Home`[i])){
        reg.season[["Winner Points Awarded"]][i] <- 2
        reg.season[["Loser Points Awarded"]][i] <- 1
      }
    }
  }
  
  points <- data.frame(
    teams,
    Points = as.numeric(as.character(unlist(lapply(teams, point.totaler, df = reg.season)))), # Assigns point totals for each team under 2.1 system
    reg.wins = as.numeric(as.character(unlist(lapply(teams, reg.win.counter, df = reg.season)))), # Regulation wins
    ot.wins = as.numeric(as.character(unlist(lapply(teams, ot.win.counter, df = reg.season)))), # Overtime wins
    so.wins = as.numeric(as.character(unlist(lapply(teams, so.win.counter, df = reg.season)))), # Shootout wins
    reg.loss = as.numeric(as.character(unlist(lapply(teams, reg.loss.counter, df = reg.season)))), # Regulation losses
    ot.loss = as.numeric(as.character(unlist(lapply(teams, ot.loss.counter, df = reg.season)))), # Overtime losses
    so.loss = as.numeric(as.character(unlist(lapply(teams, so.loss.counter, df = reg.season)))), # Shootout losses
    gf = as.numeric(as.character(unlist(lapply(teams, goals.for, df = reg.season)))), # Goals for
    ga = as.numeric(as.character(unlist(lapply(teams, goals.against, df = reg.season)))),
    gp = as.numeric(as.character(unlist(lapply(teams, gp, df = reg.season)))),
    stringsAsFactors = F
  )
  
  points[["ROW"]] <- points$reg.wins + points$ot.wins
  points[["DIFF"]] <- points$gf - points$ga
  
  # Builds divisions
  atlantic <- div.builder("atlantic")
  northeast <- div.builder("northeast")
  southeast <- div.builder("southeast")
  central <- div.builder("central")
  northwest <- div.builder("northwest")
  pacific <- div.builder("pacific")
  
  # Fill in division
  for(i in 1:nrow(points)){
    team <- points$teams[i]
    if(team %in% atlantic$Team){
      atlantic$Points[atlantic$Team == team] <- points$Points[points$teams == team]
      atlantic$reg.wins[atlantic$Team == team] <- points$reg.wins[points$teams == team]
      atlantic$ot.wins[atlantic$Team == team] <- points$ot.wins[points$teams == team]
      atlantic$so.wins[atlantic$Team == team] <- points$so.wins[points$teams == team]
      atlantic$reg.loss[atlantic$Team == team] <- points$reg.loss[points$teams == team]
      atlantic$ot.loss[atlantic$Team == team] <- points$ot.loss[points$teams == team]
      atlantic$so.loss[atlantic$Team == team] <- points$so.loss[points$teams == team]
      atlantic$gf[atlantic$Team == team] <- points$gf[points$teams == team]
      atlantic$ga[atlantic$Team == team] <- points$ga[points$teams == team]
      atlantic$gp[atlantic$Team == team] <- points$gp[points$teams == team]
      atlantic$ROW[atlantic$Team == team] <- points$ROW[points$teams == team]
      atlantic$DIFF[atlantic$Team == team] <- points$DIFF[points$teams == team]
    }
    else{
      if(team %in% northeast$Team){
        northeast$Points[northeast$Team == team] <- points$Points[points$teams == team]
        northeast$reg.wins[northeast$Team == team] <- points$reg.wins[points$teams == team]
        northeast$ot.wins[northeast$Team == team] <- points$ot.wins[points$teams == team]
        northeast$so.wins[northeast$Team == team] <- points$so.wins[points$teams == team]
        northeast$reg.loss[northeast$Team == team] <- points$reg.loss[points$teams == team]
        northeast$ot.loss[northeast$Team == team] <- points$ot.loss[points$teams == team]
        northeast$so.loss[northeast$Team == team] <- points$so.loss[points$teams == team]
        northeast$gf[northeast$Team == team] <- points$gf[points$teams == team]
        northeast$ga[northeast$Team == team] <- points$ga[points$teams == team]
        northeast$gp[northeast$Team == team] <- points$gp[points$teams == team]
        northeast$ROW[northeast$Team == team] <- points$ROW[points$teams == team]
        northeast$DIFF[northeast$Team == team] <- points$DIFF[points$teams == team]
      }
      else{
        if(team %in% southeast$Team){
          southeast$Points[southeast$Team == team] <- points$Points[points$teams == team]
          southeast$reg.wins[southeast$Team == team] <- points$reg.wins[points$teams == team]
          southeast$ot.wins[southeast$Team == team] <- points$ot.wins[points$teams == team]
          southeast$so.wins[southeast$Team == team] <- points$so.wins[points$teams == team]
          southeast$reg.loss[southeast$Team == team] <- points$reg.loss[points$teams == team]
          southeast$ot.loss[southeast$Team == team] <- points$ot.loss[points$teams == team]
          southeast$so.loss[southeast$Team == team] <- points$so.loss[points$teams == team]
          southeast$gf[southeast$Team == team] <- points$gf[points$teams == team]
          southeast$ga[southeast$Team == team] <- points$ga[points$teams == team]
          southeast$gp[southeast$Team == team] <- points$gp[points$teams == team]
          southeast$ROW[southeast$Team == team] <- points$ROW[points$teams == team]
          southeast$DIFF[southeast$Team == team] <- points$DIFF[points$teams == team]
        }
        else{
          if(team %in% central$Team){
            central$Points[central$Team == team] <- points$Points[points$teams == team]
            central$reg.wins[central$Team == team] <- points$reg.wins[points$teams == team]
            central$ot.wins[central$Team == team] <- points$ot.wins[points$teams == team]
            central$so.wins[central$Team == team] <- points$so.wins[points$teams == team]
            central$reg.loss[central$Team == team] <- points$reg.loss[points$teams == team]
            central$ot.loss[central$Team == team] <- points$ot.loss[points$teams == team]
            central$so.loss[central$Team == team] <- points$so.loss[points$teams == team]
            central$gf[central$Team == team] <- points$gf[points$teams == team]
            central$ga[central$Team == team] <- points$ga[points$teams == team]
            central$gp[central$Team == team] <- points$gp[points$teams == team]
            central$ROW[central$Team == team] <- points$ROW[points$teams == team]
            central$DIFF[central$Team == team] <- points$DIFF[points$teams == team]
          }
          else{
            if(team %in% northwest$Team){
              northwest$Points[northwest$Team == team] <- points$Points[points$teams == team]
              northwest$reg.wins[northwest$Team == team] <- points$reg.wins[points$teams == team]
              northwest$ot.wins[northwest$Team == team] <- points$ot.wins[points$teams == team]
              northwest$so.wins[northwest$Team == team] <- points$so.wins[points$teams == team]
              northwest$reg.loss[northwest$Team == team] <- points$reg.loss[points$teams == team]
              northwest$ot.loss[northwest$Team == team] <- points$ot.loss[points$teams == team]
              northwest$so.loss[northwest$Team == team] <- points$so.loss[points$teams == team]
              northwest$gf[northwest$Team == team] <- points$gf[points$teams == team]
              northwest$ga[northwest$Team == team] <- points$ga[points$teams == team]
              northwest$gp[northwest$Team == team] <- points$gp[points$teams == team]
              northwest$ROW[northwest$Team == team] <- points$ROW[points$teams == team]
              northwest$DIFF[northwest$Team == team] <- points$DIFF[points$teams == team]
            }
            else{
              if(team %in% pacific$Team){
                pacific$Points[pacific$Team == team] <- points$Points[points$teams == team]
                pacific$reg.wins[pacific$Team == team] <- points$reg.wins[points$teams == team]
                pacific$ot.wins[pacific$Team == team] <- points$ot.wins[points$teams == team]
                pacific$so.wins[pacific$Team == team] <- points$so.wins[points$teams == team]
                pacific$reg.loss[pacific$Team == team] <- points$reg.loss[points$teams == team]
                pacific$ot.loss[pacific$Team == team] <- points$ot.loss[points$teams == team]
                pacific$so.loss[pacific$Team == team] <- points$so.loss[points$teams == team]
                pacific$gf[pacific$Team == team] <- points$gf[points$teams == team]
                pacific$ga[pacific$Team == team] <- points$ga[points$teams == team]
                pacific$gp[pacific$Team == team] <- points$gp[points$teams == team]
                pacific$ROW[pacific$Team == team] <- points$ROW[points$teams == team]
                pacific$DIFF[pacific$Team == team] <- points$DIFF[points$teams == team]
              }
            }
          }
        }
      }
    }
  }
  
  a <- div.orderer(atlantic, df1 = reg.season, df2 = atlantic, df3 = reg.season, df4 = points)
  ne <- div.orderer(northeast, df1 = reg.season, df2 = northeast, df3 = reg.season, df4 = points)
  se <- div.orderer(southeast, df1 = reg.season, df2 = southeast, df3 = reg.season, df4 = points)
  c <- div.orderer(central, df1 = reg.season, df2 = central, df3 = reg.season, df4 = points)
  nw <- div.orderer(northwest, df1 = reg.season, df2 = northwest, df3 = reg.season, df4 = points)
  p <- div.orderer(pacific, df1 = reg.season, df2 = pacific, df3 = reg.season, df4 = points)
  
  east.playoffs <- playoff.picture.builder2(a, ne, se)
  west.playoffs <- playoff.picture.builder2(c, nw, p)
  
  list(east.playoffs, west.playoffs)
}

if(year >= 2006 & year <= 2010){
  reg.season <- link.reader(year)
  names(reg.season) <- c("Date", "Away Team", "Goals Away", "Home Team", "Goals Home", "OT/SO", "Attendance", "LOG", "Notes")
  
  teams <- sort(unique(reg.season$`Away Team`))
  
  # Divide teams into Eastern and Western Conferences
  east.teams <- c("Atlanta Thrashers", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Florida Panthers", "Montreal Canadiens", "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "Tampa Bay Lightning", "Toronto Maple Leafs", "Washington Capitals")
  west.teams <- teams[!(teams %in% east.teams)]
  
  atlantic.teams <- c("New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers", "Pittsburgh Penguins")
  northeast.teams <- c("Boston Bruins", "Buffalo Sabres", "Montreal Canadiens", "Ottawa Senators", "Toronto Maple Leafs")
  southeast.teams <- c("Atlanta Thrashers", "Carolina Hurricanes", "Florida Panthers", "Tampa Bay Lightning", "Washington Capitals")
  
  central.teams <- c("Chicago Blackhawks", "Columbus Blue Jackets", "Detroit Red Wings", "Nashville Predators", "St. Louis Blues")
  northwest.teams <- c("Calgary Flames", "Colorado Avalanche", "Edmonton Oilers", "Minnesota Wild", "Vancouver Canucks")
  if(year == 2006){
    pacific.teams <- c("Dallas Stars", "Los Angeles Kings", "Mighty Ducks of Anaheim", "Phoenix Coyotes", "San Jose Sharks")
  }
  if(year > 2006){
    pacific.teams <- c("Anaheim Ducks", "Dallas Stars", "Los Angeles Kings", "Phoenix Coyotes", "San Jose Sharks")
  }
  
  # Gets data to be of right types
  reg.season$`Goals Away` <- as.numeric(as.character(reg.season$`Goals Away`))
  reg.season$`Goals Home` <- as.numeric(as.character(reg.season$`Goals Home`))
  
  reg.season <- na.omit(reg.season) # THIS LINE REMOVES ANY POSTPONED GAMES
  
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
    # Assigns points: 3 for a regulation win, 2 for an OT/SO win, 1 for an OT/SO loss, and 0 for regulation loss
    if(reg.season$`OT/SO`[i] == ""){
      reg.season[["Winner Points Awarded"]][i] <- 3
      reg.season[["Loser Points Awarded"]][i] <- 0
    }
    else{
      if(reg.season$`OT/SO`[i] != "" & (reg.season$`Goals Away`[i] != reg.season$`Goals Home`[i])){
        reg.season[["Winner Points Awarded"]][i] <- 2
        reg.season[["Loser Points Awarded"]][i] <- 1
      }
    }
  }
  
  points <- data.frame(
    teams,
    Points = as.numeric(as.character(unlist(lapply(teams, point.totaler, df = reg.season)))), # Assigns point totals for each team under 2.1 system
    reg.wins = as.numeric(as.character(unlist(lapply(teams, reg.win.counter, df = reg.season)))), # Regulation wins
    ot.wins = as.numeric(as.character(unlist(lapply(teams, ot.win.counter, df = reg.season)))), # Overtime wins
    so.wins = as.numeric(as.character(unlist(lapply(teams, so.win.counter, df = reg.season)))), # Shootout wins
    reg.loss = as.numeric(as.character(unlist(lapply(teams, reg.loss.counter, df = reg.season)))), # Regulation losses
    ot.loss = as.numeric(as.character(unlist(lapply(teams, ot.loss.counter, df = reg.season)))), # Overtime losses
    so.loss = as.numeric(as.character(unlist(lapply(teams, so.loss.counter, df = reg.season)))),
    gf = as.numeric(as.character(unlist(lapply(teams, goals.for, df = reg.season)))), # Goals for
    ga = as.numeric(as.character(unlist(lapply(teams, goals.against, df = reg.season)))),
    gp = as.numeric(as.character(unlist(lapply(teams, gp, df = reg.season)))),
    stringsAsFactors = F
  )
  
  points[["DIFF"]] <- points$gf - points$ga
  points[["Wins"]] <- points$reg.wins + points$ot.wins + points$so.wins
  points[["Losses"]] <- points$reg.loss
  points[["OTL"]] <- points$ot.loss + points$so.loss
  
  # Builds divisions
  atlantic <- div.builder2("atlantic")
  northeast <- div.builder2("northeast")
  southeast <- div.builder2("southeast")
  central <- div.builder2("central")
  northwest <- div.builder2("northwest")
  pacific <- div.builder2("pacific")
  
  # Fill in division
  for(i in 1:nrow(points)){
    team <- points$teams[i]
    if(team %in% atlantic$Team){
      atlantic$Points[atlantic$Team == team] <- points$Points[points$teams == team]
      atlantic$gp[atlantic$Team == team] <- points$gp[points$teams == team]
      atlantic$Wins[atlantic$Team == team] <- points$Wins[points$teams == team]
      atlantic$Losses[atlantic$Team == team] <- points$Losses[points$teams == team]
      atlantic$OTL[atlantic$Team == team] <- points$OTL[points$teams == team]
      atlantic$DIFF[atlantic$Team == team] <- points$DIFF[points$teams == team]
    }
    else{
      if(team %in% northeast$Team){
        northeast$Points[northeast$Team == team] <- points$Points[points$teams == team]
        northeast$gp[northeast$Team == team] <- points$gp[points$teams == team]
        northeast$Wins[northeast$Team == team] <- points$Wins[points$teams == team]
        northeast$Losses[northeast$Team == team] <- points$Losses[points$teams == team]
        northeast$OTL[northeast$Team == team] <- points$OTL[points$teams == team]
        northeast$DIFF[northeast$Team == team] <- points$DIFF[points$teams == team]
      }
      else{
        if(team %in% southeast$Team){
          southeast$Points[southeast$Team == team] <- points$Points[points$teams == team]
          southeast$gp[southeast$Team == team] <- points$gp[points$teams == team]
          southeast$Wins[southeast$Team == team] <- points$Wins[points$teams == team]
          southeast$Losses[southeast$Team == team] <- points$Losses[points$teams == team]
          southeast$OTL[southeast$Team == team] <- points$OTL[points$teams == team]
          southeast$DIFF[southeast$Team == team] <- points$DIFF[points$teams == team]
        }
        else{
          if(team %in% central$Team){
            central$Points[central$Team == team] <- points$Points[points$teams == team]
            central$gp[central$Team == team] <- points$gp[points$teams == team]
            central$Wins[central$Team == team] <- points$Wins[points$teams == team]
            central$Losses[central$Team == team] <- points$Losses[points$teams == team]
            central$OTL[central$Team == team] <- points$OTL[points$teams == team]
            central$DIFF[central$Team == team] <- points$DIFF[points$teams == team]
          }
          else{
            if(team %in% northwest$Team){
              northwest$Points[northwest$Team == team] <- points$Points[points$teams == team]
              northwest$gp[northwest$Team == team] <- points$gp[points$teams == team]
              northwest$Wins[northwest$Team == team] <- points$Wins[points$teams == team]
              northwest$Losses[northwest$Team == team] <- points$Losses[points$teams == team]
              northwest$OTL[northwest$Team == team] <- points$OTL[points$teams == team]
              northwest$DIFF[northwest$Team == team] <- points$DIFF[points$teams == team]
            }
            else{
              if(team %in% pacific$Team){
                pacific$Points[pacific$Team == team] <- points$Points[points$teams == team]
                pacific$gp[pacific$Team == team] <- points$gp[points$teams == team]
                pacific$Wins[pacific$Team == team] <- points$Wins[points$teams == team]
                pacific$Losses[pacific$Team == team] <- points$Losses[points$teams == team]
                pacific$OTL[pacific$Team == team] <- points$OTL[points$teams == team]
                pacific$DIFF[pacific$Team == team] <- points$DIFF[points$teams == team]
              }
            }
          }
        }
      }
    }
  }
  
  a <- div.orderer2(atlantic, df5 = points, df2 = atlantic, df3 = reg.season, df4 = points)
  ne <- div.orderer2(northeast, df5 = points, df2 = northeast, df3 = reg.season, df4 = points)
  se <- div.orderer2(southeast, df5 = points, df2 = southeast, df3 = reg.season, df4 = points)
  c <- div.orderer2(central, df5 = points, df2 = central, df3 = reg.season, df4 = points)
  nw <- div.orderer2(northwest, df5 = points, df2 = northwest, df3 = reg.season, df4 = points)
  p <- div.orderer2(pacific, df5 = points, df2 = pacific, df3 = reg.season, df4 = points)
  
  east.playoffs <- playoff.picture.builder1(a, ne, se)
  west.playoffs <- playoff.picture.builder1(c, nw, p)
  
  list(east.playoffs, west.playoffs)
}

if(year == 2005){
  print('There was a lockout in 2005.')
}

##########################
# LEAGUE LOCKOUT IN 2005 #
##########################

if(year >= 2000 & year < 2005){
  reg.season <- link.reader(year)
  names(reg.season) <- c("Date", "Away Team", "Goals Away", "Home Team", "Goals Home", "OT/SO", "Attendance", "LOG", "Notes")
  
  teams <- sort(unique(reg.season$`Away Team`))
  
  # Divide teams into Eastern and Western Conferences
  east.teams <- c("Atlanta Thrashers", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Florida Panthers", "Montreal Canadiens", "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "Tampa Bay Lightning", "Toronto Maple Leafs", "Washington Capitals")
  west.teams <- teams[!(teams %in% east.teams)]
  
  atlantic.teams <- c("New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers", "Pittsburgh Penguins")
  northeast.teams <- c("Boston Bruins", "Buffalo Sabres", "Montreal Canadiens", "Ottawa Senators", "Toronto Maple Leafs")
  southeast.teams <- c("Atlanta Thrashers", "Carolina Hurricanes", "Florida Panthers", "Tampa Bay Lightning", "Washington Capitals")
  
  central.teams <- c("Chicago Blackhawks", "Columbus Blue Jackets", "Detroit Red Wings", "Nashville Predators", "St. Louis Blues")
  northwest.teams <- c("Calgary Flames", "Colorado Avalanche", "Edmonton Oilers", "Minnesota Wild", "Vancouver Canucks")
  pacific.teams <- c("Dallas Stars", "Los Angeles Kings", "Mighty Ducks of Anaheim", "Phoenix Coyotes", "San Jose Sharks")
  
  # Gets data to be of right types
  reg.season$`Goals Away` <- as.numeric(as.character(reg.season$`Goals Away`))
  reg.season$`Goals Home` <- as.numeric(as.character(reg.season$`Goals Home`))
  
  reg.season <- na.omit(reg.season) # THIS LINE REMOVES ANY POSTPONED GAMES
  
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
    # Assigns points: 3 for a regulation win, 2 for an overtime win, 1 for an OT
    # loss, 1.5 for a tie, and 0 for regulation loss
    if(reg.season$`OT/SO`[i] == ""){
      reg.season[["Winner Points Awarded"]][i] <- 3
      reg.season[["Loser Points Awarded"]][i] <- 0
    }
    else{
      if(reg.season$`OT/SO`[i] != "" & (reg.season$`Goals Away`[i] != reg.season$`Goals Home`[i])){
        reg.season[["Winner Points Awarded"]][i] <- 2
        reg.season[["Loser Points Awarded"]][i] <- 1
      }
      else{
        reg.season[["Winner Points Awarded"]][i] <- 1.5
        reg.season[["Loser Points Awarded"]][i] <- 1.5
      }
    }
  }
  
  points <- data.frame(
    teams,
    Points = as.numeric(as.character(unlist(lapply(teams, point.totaler, df = reg.season)))), # Assigns point totals for each team under 2.1 system
    reg.wins = as.numeric(as.character(unlist(lapply(teams, reg.win.counter, df = reg.season)))), # Regulation wins
    ot.wins = as.numeric(as.character(unlist(lapply(teams, ot.win.counter, df = reg.season)))), # Overtime wins
    reg.loss = as.numeric(as.character(unlist(lapply(teams, reg.loss.counter, df = reg.season)))), # Regulation losses
    ot.loss = as.numeric(as.character(unlist(lapply(teams, ot.loss.counter, df = reg.season)))), # Overtime losses
    ties = as.numeric(as.character(unlist(lapply(teams, tie.counter, df = reg.season)))),
    gf = as.numeric(as.character(unlist(lapply(teams, goals.for, df = reg.season)))), # Goals for
    ga = as.numeric(as.character(unlist(lapply(teams, goals.against, df = reg.season)))),
    gp = as.numeric(as.character(unlist(lapply(teams, gp, df = reg.season)))),
    stringsAsFactors = F
  )
  
  points[["DIFF"]] <- points$gf - points$ga
  points[["Wins"]] <- points$reg.wins + points$ot.wins
  points[["Losses"]] <- points$reg.loss
  points[["OTL"]] <- points$ot.loss
  points[["Ties"]] <- points$ties
  
  points <- points[, c(1, 2, 10, 12, 13, 15, 14, 11)]
  
  # Builds divisions
  atlantic <- div.builder1("atlantic")
  northeast <- div.builder1("northeast")
  southeast <- div.builder1("southeast")
  central <- div.builder1("central")
  northwest <- div.builder1("northwest")
  pacific <- div.builder1("pacific")
  
  # Fill in division
  for(i in 1:nrow(points)){
    team <- points$teams[i]
    if(team %in% atlantic$Team){
      atlantic$Points[atlantic$Team == team] <- points$Points[points$teams == team]
      atlantic$gp[atlantic$Team == team] <- points$gp[points$teams == team]
      atlantic$Wins[atlantic$Team == team] <- points$Wins[points$teams == team]
      atlantic$Losses[atlantic$Team == team] <- points$Losses[points$teams == team]
      atlantic$Ties[atlantic$Team == team] <- points$Ties[points$teams == team]
      atlantic$OTL[atlantic$Team == team] <- points$OTL[points$teams == team]
      atlantic$DIFF[atlantic$Team == team] <- points$DIFF[points$teams == team]
    }
    else{
      if(team %in% northeast$Team){
        northeast$Points[northeast$Team == team] <- points$Points[points$teams == team]
        northeast$gp[northeast$Team == team] <- points$gp[points$teams == team]
        northeast$Wins[northeast$Team == team] <- points$Wins[points$teams == team]
        northeast$Losses[northeast$Team == team] <- points$Losses[points$teams == team]
        northeast$Ties[northeast$Team == team] <- points$Ties[points$teams == team]
        northeast$OTL[northeast$Team == team] <- points$OTL[points$teams == team]
        northeast$DIFF[northeast$Team == team] <- points$DIFF[points$teams == team]
      }
      else{
        if(team %in% southeast$Team){
          southeast$Points[southeast$Team == team] <- points$Points[points$teams == team]
          southeast$gp[southeast$Team == team] <- points$gp[points$teams == team]
          southeast$Wins[southeast$Team == team] <- points$Wins[points$teams == team]
          southeast$Losses[southeast$Team == team] <- points$Losses[points$teams == team]
          southeast$Ties[southeast$Team == team] <- points$Ties[points$teams == team]
          southeast$OTL[southeast$Team == team] <- points$OTL[points$teams == team]
          southeast$DIFF[southeast$Team == team] <- points$DIFF[points$teams == team]
        }
        else{
          if(team %in% central$Team){
            central$Points[central$Team == team] <- points$Points[points$teams == team]
            central$gp[central$Team == team] <- points$gp[points$teams == team]
            central$Wins[central$Team == team] <- points$Wins[points$teams == team]
            central$Losses[central$Team == team] <- points$Losses[points$teams == team]
            central$Ties[central$Team == team] <- points$Ties[points$teams == team]
            central$OTL[central$Team == team] <- points$OTL[points$teams == team]
            central$DIFF[central$Team == team] <- points$DIFF[points$teams == team]
          }
          else{
            if(team %in% northwest$Team){
              northwest$Points[northwest$Team == team] <- points$Points[points$teams == team]
              northwest$gp[northwest$Team == team] <- points$gp[points$teams == team]
              northwest$Wins[northwest$Team == team] <- points$Wins[points$teams == team]
              northwest$Losses[northwest$Team == team] <- points$Losses[points$teams == team]
              northwest$Ties[northwest$Team == team] <- points$Ties[points$teams == team]
              northwest$OTL[northwest$Team == team] <- points$OTL[points$teams == team]
              northwest$DIFF[northwest$Team == team] <- points$DIFF[points$teams == team]
            }
            else{
              if(team %in% pacific$Team){
                pacific$Points[pacific$Team == team] <- points$Points[points$teams == team]
                pacific$gp[pacific$Team == team] <- points$gp[points$teams == team]
                pacific$Wins[pacific$Team == team] <- points$Wins[points$teams == team]
                pacific$Losses[pacific$Team == team] <- points$Losses[points$teams == team]
                pacific$Ties[pacific$Team == team] <- points$Ties[points$teams == team]
                pacific$OTL[pacific$Team == team] <- points$OTL[points$teams == team]
                pacific$DIFF[pacific$Team == team] <- points$DIFF[points$teams == team]
              }
            }
          }
        }
      }
    }
  }
  
  a <- div.orderer1(atlantic, df5 = points, df2 = atlantic, df3 = reg.season, df4 = points)
  ne <- div.orderer1(northeast, df5 = points, df2 = northeast, df3 = reg.season, df4 = points)
  se <- div.orderer1(southeast, df5 = points, df2 = southeast, df3 = reg.season, df4 = points)
  c <- div.orderer1(central, df5 = points, df2 = central, df3 = reg.season, df4 = points)
  nw <- div.orderer1(northwest, df5 = points, df2 = northwest, df3 = reg.season, df4 = points)
  p <- div.orderer1(pacific, df5 = points, df2 = pacific, df3 = reg.season, df4 = points)
  
  east.playoffs <- playoff.picture.builder1(a, ne, se)
  west.playoffs <- playoff.picture.builder1(c, nw, p)
  
  list(east.playoffs, west.playoffs)
}
