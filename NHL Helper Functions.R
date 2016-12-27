link.reader <- function(year){
  link.part.1 <- paste("http://www.hockey-reference.com/leagues/NHL_", year, sep = "")
  full.link <- paste(link.part.1, "_games.html#games::none", sep = "")
  table <- readHTMLTable(full.link, header = T, stringsAsFactors = F)
  return(table$games)
}

# Gets indexes of games won
games.won <- function(df, team){
  return(which(df$Winner == team))
}

# Gets indexes of games lost in Shootout or Overtime
games.lost <- function(df, team){
  return(which(df$Loser == team & df$`OT/SO` != ""))
}

# Gets point total under current 2-1-0 system (2 points for any win, 1 for OT/SO loss, 0 for regulation loss)
point.totaler.2.1 <- function(df, team){
  tot <- 0
  wins <- games.won(df, team)
  losses <- games.lost(df, team)
  tot <- (2*length(wins)) + length(losses)
  return(tot)
}

# Gets point total under 3-2-1-0 system (3 points for regulation win, 2 for OT/SO win, 1 for OT/SO loss, 0 for regulation loss)
point.totaler.3.2.1.0 <- function(df, team){
  tot <- 0
  wins <- games.won(df, team)
  losses <- games.lost(df, team)
  for(i in 1:length(wins)){
    row <- wins[i]
    tot <- tot + df$`Winner Points Awarded`[row]
  }
  for(j in 1:length(losses)){
    row <- losses[j]
    tot <- tot + df$`Loser Points Awarded`[row]
  }
  return(tot)
}
