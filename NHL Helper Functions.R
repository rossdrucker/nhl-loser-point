# Reads in game-by-game schedule and results for given year (where year is of cup, not beginning of season)
link.reader <- function(year){
  link.part.1 <- paste("http://www.hockey-reference.com/leagues/NHL_", year, sep = "")
  full.link <- paste(link.part.1, "_games.html#games::none", sep = "")
  table <- readHTMLTable(full.link, header = T, stringsAsFactors = F)
  return(table$games)
}

# Reads in standings, home/away/month-by-month breakdown and head-to-head matrix
head.to.head.reader <- function(year){
  link.part.1 <- paste("http://www.hockey-reference.com/leagues/NHL_", year, sep = "")
  full.link <- paste(link.part.1, "_standings.html#team_vs_team::none", sep = "")
  table <- readHTMLTable(full.link, header = T, stringsAsFactors = F)
  head.to.head <- table$team_vs_team
  head.to.head <- head.to.head[, 2:length(head.to.head)]
  names(head.to.head) <- c("Teams", head.to.head[, 1])
  return(head.to.head)
}

# Gets number of games played
gp <- function(team, df){
  away.games <- sum(df$`Away Team` == team)
  home.games <- sum(df$`Home Team` == team)
  return(away.games + home.games)
}

# Gets total points according to system
point.totaler <- function(team, df){
  tot <- 0
  for(i in 1:nrow(df)){
    if(df$Winner[i] == team){
      tot <- tot + df$`Winner Points Awarded`[i]
    }
    else{
      if(df$Loser[i] == team){
        tot <- tot + df$`Loser Points Awarded`[i] 
      }
    }
  }
  return(tot)
}

# Returns how many games were won in regulation
reg.win.counter <- function(team, df){
  return(length(which(df$Winner == team & df$`OT/SO` == "" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were won in overtime
ot.win.counter <- function(team, df){
  return(length(which(df$Winner == team & df$`OT/SO` == "OT" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were won in shooutout
so.win.counter <- function(team, df){
  return(length(which(df$Winner == team & df$`OT/SO` == "SO" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were lost in regulation
reg.loss.counter <- function(team, df){
  return(length(which(df$Loser == team & df$`OT/SO` == "" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were lost in overtime
ot.loss.counter <- function(team, df){
  return(length(which(df$Loser == team & df$`OT/SO` == "OT" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were lost in shooutout
so.loss.counter <- function(team, df){
  return(length(which(df$Loser == team & df$`OT/SO` == "SO" & df$`Goals Away` != df$`Goals Home`)))
}

# Returns how many games were tied (valid for seasons before 2005)
tie.counter <- function(team, df){
  return(length(which((df$Winner == team & df$`OT/SO` != "" & df$`Goals Away` == df$`Goals Home`) | (df$Loser == team & df$`OT/SO` != "" & df$`Goals Away` == df$`Goals Home`))))
}

# Additive goals for
goals.for <- function(team, df){
  gf <- 0
  games.played.in <- which(df$`Away Team` == team | df$`Home Team` == team)
  for(i in 1:length(games.played.in)){
    row <- games.played.in[i]
    if(df$`Away Team`[row] == team){
      gf <- gf + df$`Goals Away`[row]
    }
    if(df$`Home Team`[row] == team){
      gf <- gf + df$`Goals Home`[row]
    }
  }
  return(gf)
}

# Additive goals against
goals.against <- function(team, df){
  ga <- 0
  games.played.in <- which(df$`Away Team` == team | df$`Home Team` == team)
  for(i in 1:length(games.played.in)){
    row <- games.played.in[i]
    if(df$`Away Team`[row] == team){
      ga <- ga + df$`Goals Home`[row]
    }
    if(df$`Home Team`[row] == team){
      ga <- ga + df$`Goals Away`[row]
    }
  }
  return(ga)
}

# Builds the divisions
div.builder <- function(division, ...){
  div.teams <- paste(division, ".teams", sep = "")
  div <- data.frame(
    Team = eval(parse(text = div.teams)),
    Points = integer(length = length(eval(parse(text = div.teams)))),
    reg.wins = integer(length = length(eval(parse(text = div.teams)))),
    ot.wins = integer(length = length(eval(parse(text = div.teams)))),
    so.wins = integer(length = length(eval(parse(text = div.teams)))),
    reg.loss = integer(length = length(eval(parse(text = div.teams)))),
    ot.loss = integer(length = length(eval(parse(text = div.teams)))),
    so.loss = integer(length = length(eval(parse(text = div.teams)))),
    gf = integer(length = length(eval(parse(text = div.teams)))),
    ga = integer(length = length(eval(parse(text = div.teams)))),
    gp = integer(length = length(eval(parse(text = div.teams)))),
    ROW = integer(length = length(eval(parse(text = div.teams)))),
    DIFF = integer(length = length(eval(parse(text = div.teams)))),
    stringsAsFactors = F
  )
  return(div)
}

###################################################################################
#                                                                                 #
#    First tie breaker: Games played. Team with less games played wins the tie    #
#    breaker.                                                                     #
#                                                                                 #
###################################################################################
tie.breaker.1 <- function(team1, team2, df1){
  team1.gp <- gp(team1, df1)
  team2.gp <- gp(team2, df1)
  if(team1.gp > team2.gp){
    return(team2)
  }
  if(team1.gp < team2.gp){
    return(team1)
  }
  if(team1.gp == team2.gp){
    return(NA)
  }
}

###################################################################################
#                                                                                 #
#    Second tie breaker: Regulation and Overtime wins. The team with the higher   #
#    ROW wins the tiebreaker.                                                     #
#                                                                                 #
###################################################################################

# Returns which team had the higher ROW. Returns NA if they have the same ROW
tie.breaker.2 <- function(team1, team2, df2){
  ROW1 <- df2$ROW[df2$Team == team1]
  ROW2 <- df2$ROW[df2$Team == team2]
  if(ROW1 > ROW2){
    return(team1)
  }
  else{
    if(ROW1 < ROW2){
      return(team2)
    }
    else{
      return(NA)
    }
  }
}


###################################################################################
#                                                                                 #
#    Third tie breaker: head to head. Return team that scored more points in      #
#    head-to-head matchups. If an odd number of games was played, remove points   #
#    from the first game in the city with the extra home game                     #
#                                                                                 #
###################################################################################

# Returns which team won the head-to-head matchup of an even number of home games
tie.breaker.3 <- function(team1, team2, df3){
  # Step 1: Get games of the two tied teams
  games <- which((df3$Winner == team1 & df3$Loser == team2) | (df3$Winner == team2 & df3$Loser == team1))
  if(length(games) == 0){
    return(NA)
  }
  else{
    r1 <- df3[games, ]
    
    # Step 2: Get number of home and away games to see if any games need to be removed
    team1h <- sum(r1$`Home Team` == team1)
    team1a <- sum(r1$`Away Team` == team1)
    
    # Step 3: Remove games if necessary
    if(team1h > team1a){
      for(i in 1:nrow(r1)){
        if(r1$`Home Team`[i] == team2){
          next
        }
        else{
          r1[i, ] <- NA
          r1 <- na.omit(r1)
          break
        }
      }
    }
    
    if(team1h < team1a){
      for(i in 1:nrow(r1)){
        if(r1$`Away Team`[i] == team2){
          next
        }
        else{
          r1[i, ] <- NA
          r1 <- na.omit(r1)
          break
        }
      }
    }
    
    # Step 4: Get number of points earned in respective games
    t1.points <- point.totaler(team1, r1)
    t2.points <- point.totaler(team2, r1)
    
    # Step 5: Determine a winner, or return NA if equal points were earned
    if(t1.points > t2.points){
      return(team1)
    }
    else{
      if(t1.points < t2.points){
        return(team2)
      }
      else{
        return(NA)
      }
    }
  }
}

###################################################################################
#                                                                                 #
#    Fourth tie breaker: Goal differentials. Team with better goal differential   #
#    in regular season wins the tie-breaker.                                      #
#                                                                                 #
###################################################################################

tie.breaker.4 <- function(team1, team2, df4){
  diff1 <- df4$DIFF[df4$teams == team1]
  diff2 <- df4$DIFF[df4$teams == team2]
  if(diff1 > diff2){
    return(team1)
  }
  if(diff1 < diff2){
    return(team2)
  }
  if(diff1 == diff2){
    return(NA)
  }
}

###################################################################################
#                                                                                 #
#    Fifth tie breaker: Most wins (Only used in seasons prior to 2005-2006).      #
#    Team with most wins in regular season wins the tie-breaker.                  #
#                                                                                 #
###################################################################################

tie.breaker.5 <- function(team1, team2, df5){
  wins1 <- df5$Wins[df5$teams == team1]
  wins2 <- df5$Wins[df5$teams == team2]
  if(wins1 > wins2){
    return(team1)
  }
  if(wins1 < wins2){
    return(team2)
  }
  if(wins1 == wins2){
    return(NA)
  }
}

div.orderer <- function(div, df1, df2, df3, df4){
  div.copy <- div
  out <- data.frame(
    Team = character(),
    Points = integer(),
    reg.wins = integer(),
    ot.wins = integer(),
    so.wins = integer(),
    reg.loss = integer(),
    ot.loss = integer(),
    so.loss = integer(),
    gf = integer(),
    ga = integer(),
    gp = integer(),
    ROW = integer(),
    DIFF = integer(),
    stringsAsFactors = F
  )
  for(i in 1:nrow(div)){
    most.points.indexes <- which(div.copy$Points == max(div.copy$Points, na.rm = T))
    
    if(length(most.points.indexes) == 1){
      out[i, ] <- div[most.points.indexes, ]
      div.copy[most.points.indexes, ] <- NA
    }
    else{
      team1 <- div.copy$Team[most.points.indexes[1]]
      team2 <- div.copy$Team[most.points.indexes[2]]
      if(is.na(tie.breaker.1(team1, team2, df1)) == F){
        out[i, ] <- div[div$Team == tie.breaker.1(team1, team2, df1), ]
        div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.1(team1, team2, df1), ] <- NA
      }
      else{
        if(is.na(tie.breaker.2(team1, team2, df2)) == F){
          out[i, ] <- div[div$Team == tie.breaker.2(team1, team2, df2), ]
          div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.2(team1, team2, df2), ] <- NA
        }
        else{
          if(is.na(tie.breaker.3(team1, team2, df3)) == F){
            out[i, ] <- div[div$Team == tie.breaker.3(team1, team2, df3), ]
            div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.3(team1, team2, df3), ] <- NA
          }
          else{
            if(is.na(tie.breaker.4(team1, team2, df4)) == F){
              out[i, ] <- div[div$Team == tie.breaker.4(team1, team2, df4), ]
              div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.4(team1, team2, df4), ] <- NA
            }
          }
        }
      }
    }
  }
  
  return(out)
}

# Get the wild card teams
wild.cards <- function(div1, div2, ...){
  div1.copy <- div1
  div2.copy <- div2
  
  div1.copy[1:3, ] <- NA
  div2.copy[1:3, ] <- NA
  div1.copy <- na.omit(div1.copy)
  div2.copy <- na.omit(div2.copy)
  
  t <- rbind(div1.copy, div2.copy)
  
  return(div.orderer(div = t, df1 = reg.season, df2 = t, df3 = reg.season, df4 = points)[1:2, ])
}

# Get playoff picture
playoff.picture.builder <- function(div1, div2, ...){
  
  out = character(length = 8)
  
  div1.copy <- div1
  div2.copy <- div2
  
  div1.copy[4:length(div1.copy), ] <- NA
  div2.copy[4:length(div2.copy), ] <- NA
  
  div1.copy <- na.omit(div1.copy)
  div2.copy <- na.omit(div2.copy)
  
  top.seed.1 <- div1.copy[1, ]
  top.seed.2 <- div2.copy[1, ]
  
  if(top.seed.1$Points > top.seed.2$Points){
    out[1] <- top.seed.1$Team
    out[2] <- wild.cards(div1, div2)[2, 1]
    out[3] <- div1.copy$Team[2]
    out[4] <- div1.copy$Team[3]
    out[5] <- top.seed.2$Team
    out[6] <- wild.cards(div1, div2)[1, 1]
    out[7] <- div2.copy$Team[2]
    out[8] <- div2.copy$Team[3]
  }
  else{
    if(top.seed.1$Points < top.seed.2$Points){
      out[1] <- top.seed.2$Team
      out[2] <- wild.cards(div1, div2)[2, 1]
      out[3] <- div2.copy$Team[2]
      out[4] <- div2.copy$Team[3]
      out[5] <- top.seed.1$Team
      out[6] <- wild.cards(div1, div2)[1, 1]
      out[7] <- div1.copy$Team[2]
      out[8] <- div1.copy$Team[3]
    }
    else{
      t <- rbind(top.seed.1, top.seed.2)
      ordered <- div.orderer(div = t, df1 = reg.season, df2 = t, df3 = reg.season, df4 = points)
      if(ordered[1, 1] %in% div1.copy$Team){
        out[1] <- ordered[1, 1]
        out[2] <- wild.cards(div1, div2)[2, 1]
        out[3] <- div1.copy$Team[2]
        out[4] <- div1.copy$Team[3]
        out[5] <- ordered[2, 1]
        out[6] <- wild.cards(div1, div2)[1, 1]
        out[7] <- div2.copy$Team[2]
        out[8] <- div2.copy$Team[3]
      }
      else{
        if(ordered[1, 1] %in% div2.copy$Team){
        out[1] <- ordered[1, 1]
        out[2] <- wild.cards(div1, div2)[2, 1]
        out[3] <- div2.copy$Team[2]
        out[4] <- div2.copy$Team[3]
        out[5] <- ordered[2, 1]
        out[6] <- wild.cards(div1, div2)[1, 1]
        out[7] <- div1.copy$Team[2]
        out[8] <- div1.copy$Team[3]
        }
      }
    }
  }
  
  return(out)
}

# Builds divisions prior to 2013 realignment
div.builder1 <- function(division, ...){
  div.teams <- paste(division, ".teams", sep = "")
  div <- data.frame(
    Team = eval(parse(text = div.teams)),
    Points = integer(length = length(eval(parse(text = div.teams)))),
    gp = integer(length = length(eval(parse(text = div.teams)))),
    Wins = integer(length = length(eval(parse(text = div.teams)))),
    Losses = integer(length = length(eval(parse(text = div.teams)))),
    Ties = integer(length = length(eval(parse(text = div.teams)))),
    OTL = integer(length = length(eval(parse(text = div.teams)))),
    DIFF = integer(length = length(eval(parse(text = div.teams)))),
    stringsAsFactors = F
  )
  return(div)
}

# Builds orders divisions
div.orderer1 <- function(div, df5, df2, df3, df4){
  div.copy <- div
  out <- data.frame(
    Team = character(),
    Points = integer(),
    gp = integer(),
    Wins = integer(),
    Losses = integer(),
    Ties = integer(),
    OTL = integer(),
    DIFF = integer(),
    stringsAsFactors = F
  )
  for(i in 1:nrow(div)){
    #div.copy <- na.omit(div.copy)
    most.points.indexes <- which(div.copy$Points == max(div.copy$Points, na.rm = T))
    
    if(length(most.points.indexes) == 1){
      out[i, ] <- div[most.points.indexes, ]
      div.copy[most.points.indexes, ] <- NA
    }
    else{
      team1 <- div.copy$Team[most.points.indexes[1]]
      team2 <- div.copy$Team[most.points.indexes[2]]
      if(is.na(tie.breaker.5(team1, team2, df5)) == F){
        out[i, ] <- div[div$Team == tie.breaker.5(team1, team2, df5), ]
        div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.5(team1, team2, df5), ] <- NA
      }
      else{
        if(is.na(tie.breaker.3(team1, team2, df3)) == F){
          out[i, ] <- div[div$Team == tie.breaker.3(team1, team2, df3), ]
          div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.3(team1, team2, df3), ] <- NA
        }
        else{
          if(is.na(tie.breaker.4(team1, team2, df4)) == F){
            out[i, ] <- div[div$Team == tie.breaker.4(team1, team2, df4), ]
            div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.4(team1, team2, df4), ] <- NA
          }
        }
      }
    }
  }
  return(out)
}

# Builds playoff picture for years between 2000 and 2005
playoff.picture.builder1 <- function(div1, div2, div3, ...){
  
  out = character(length = 8)
  
  div1.copy <- div1
  div2.copy <- div2
  div3.copy <- div3
  
  div1.winner <- div1[1, ]
  div2.winner <- div2[1, ]
  div3.winner <- div3[1, ]
  
  div.champs <- rbind(div1.winner, div2.winner, div3.winner)
  
  div.champs <- div.orderer1(div.champs, df5 = points, df2 = div.champs, df3 = reg.season, df4 = points)
  
  out[1] <- div.champs[1, 1]
  out[3] <- div.champs[2, 1]
  out[5] <- div.champs[3, 1]
  
  div1.copy[1, ] <- NA
  div2.copy[1, ] <- NA
  div3.copy[1, ] <- NA
  
  div1.copy <- na.omit(div1.copy)
  div2.copy <- na.omit(div2.copy)
  div3.copy <- na.omit(div3.copy)
  
  remaining <- rbind(div1.copy, div2.copy, div3.copy)
  
  r1 <- div.orderer1(remaining, df5 = points, df2 = remaining, df3 = reg.season, df4 = points)
  
  out[7] <- r1[1, 1]
  out[8] <- r1[2, 1]
  out[6] <- r1[3, 1]
  out[4] <- r1[4, 1]
  out[2] <- r1[5, 1]
  
  return(out)
}

playoff.picture.builder2 <- function(div1, div2, div3, ...){
  
  out = character(length = 8)
  
  div1.copy <- div1
  div2.copy <- div2
  div3.copy <- div3
  
  div1.winner <- div1[1, ]
  div2.winner <- div2[1, ]
  div3.winner <- div3[1, ]
  
  div.champs <- rbind(div1.winner, div2.winner, div3.winner)
  
  div.champs <- div.orderer(div.champs, df1 = reg.season, df2 = div.champs, df3 = reg.season, df4 = points)
  
  out[1] <- div.champs[1, 1]
  out[3] <- div.champs[2, 1]
  out[5] <- div.champs[3, 1]
  
  div1.copy[1, ] <- NA
  div2.copy[1, ] <- NA
  div3.copy[1, ] <- NA
  
  div1.copy <- na.omit(div1.copy)
  div2.copy <- na.omit(div2.copy)
  div3.copy <- na.omit(div3.copy)
  
  remaining <- rbind(div1.copy, div2.copy, div3.copy)
  
  r1 <- div.orderer(remaining, df1 = reg.season, df2 = remaining, df3 = reg.season, df4 = points)
  
  out[7] <- r1[1, 1]
  out[8] <- r1[2, 1]
  out[6] <- r1[3, 1]
  out[4] <- r1[4, 1]
  out[2] <- r1[5, 1]
  
  return(out)
}

div.builder2 <- function(division, ...){
  div.teams <- paste(division, ".teams", sep = "")
  div <- data.frame(
    Team = eval(parse(text = div.teams)),
    Points = integer(length = length(eval(parse(text = div.teams)))),
    gp = integer(length = length(eval(parse(text = div.teams)))),
    Wins = integer(length = length(eval(parse(text = div.teams)))),
    Losses = integer(length = length(eval(parse(text = div.teams)))),
    OTL = integer(length = length(eval(parse(text = div.teams)))),
    DIFF = integer(length = length(eval(parse(text = div.teams)))),
    stringsAsFactors = F
  )
  return(div)
}

div.orderer2 <- function(div, df5, df2, df3, df4){
  div.copy <- div
  out <- data.frame(
    Team = character(),
    Points = integer(),
    gp = integer(),
    Wins = integer(),
    Losses = integer(),
    OTL = integer(),
    DIFF = integer(),
    stringsAsFactors = F
  )
  for(i in 1:nrow(div)){
    #div.copy <- na.omit(div.copy)
    most.points.indexes <- which(div.copy$Points == max(div.copy$Points, na.rm = T))
    
    if(length(most.points.indexes) == 1){
      out[i, ] <- div[most.points.indexes, ]
      div.copy[most.points.indexes, ] <- NA
    }
    else{
      team1 <- div.copy$Team[most.points.indexes[1]]
      team2 <- div.copy$Team[most.points.indexes[2]]
      if(is.na(tie.breaker.5(team1, team2, df5)) == F){
        out[i, ] <- div[div$Team == tie.breaker.5(team1, team2, df5), ]
        div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.5(team1, team2, df5), ] <- NA
      }
      else{
        if(is.na(tie.breaker.3(team1, team2, df3)) == F){
          out[i, ] <- div[div$Team == tie.breaker.3(team1, team2, df3), ]
          div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.3(team1, team2, df3), ] <- NA
        }
        else{
          if(is.na(tie.breaker.4(team1, team2, df4)) == F){
            out[i, ] <- div[div$Team == tie.breaker.4(team1, team2, df4), ]
            div.copy[!is.na(div.copy$Team) & div.copy$Team == tie.breaker.4(team1, team2, df4), ] <- NA
          }
        }
      }
    }
  }
  return(out)
}
