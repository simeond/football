# Table Positions
library(plyr)
library(dplyr)

Table <- function(games,season) {
  games <- games[games$Season==season, ] 
  teams <- sort(unique(c(as.character(games$HomeTeam), as.character(games$AwayTeam)), decreasing = FALSE))
  n <- length(teams)
  g <- nrow(games)
  zero <- matrix(0,n,1)
  T <- data.frame(Team=teams, P=zero, HW=zero, HD=zero, HL=zero, HF=zero, HA=zero,
                  AW=zero, AD=zero, AL=zero, AF=zero, AA=zero, GD=zero, Points=zero)
  for (i in 1:g) {
    if (games[i,"FTHG"] > games[i,"FTAG"]) {
      T[which(teams == games[i,"HomeTeam"]),"Points"] <-
        T[which(teams == games[i,"HomeTeam"]),"Points"] + 3
      T[which(teams == games[i,"HomeTeam"]),"HW"] <-
        T[which(teams == games[i,"HomeTeam"]),"HW"] + 1
      T[which(teams == games[i,"AwayTeam"]),"AL"] <-
        T[which(teams == games[i,"AwayTeam"]),"AL"] + 1
    } else {
      if (games[i,"FTHG"] == games[i,"FTAG"]) {
        T[which(teams == games[i,"HomeTeam"]),"Points"] <-
          T[which(teams == games[i,"HomeTeam"]),"Points"] + 1
        T[which(teams == games[i,"AwayTeam"]),"Points"] <-
          T[which(teams == games[i,"AwayTeam"]),"Points"] + 1
        T[which(teams == games[i,"HomeTeam"]),"HD"] <-
          T[which(teams == games[i,"HomeTeam"]),"HD"] + 1
        T[which(teams == games[i,"AwayTeam"]),"AD"] <-
          T[which(teams == games[i,"AwayTeam"]),"AD"] + 1
      } else {
        T[which(teams == games[i,"AwayTeam"]),"Points"] <-
          T[which(teams == games[i,"AwayTeam"]),"Points"] + 3
        T[which(teams == games[i,"AwayTeam"]),"AW"] <-
          T[which(teams == games[i,"AwayTeam"]),"AW"] + 1
        T[which(teams == games[i,"AwayTeam"]),"HL"] <-
          T[which(teams == games[i,"AwayTeam"]),"HL"] + 1
      }
    }
    T[which(teams == games[i,"HomeTeam"]),"P"] <- T[which(teams == games[i,"HomeTeam"]),"P"] + 1
    T[which(teams == games[i,"AwayTeam"]),"P"] <- T[which(teams == games[i,"AwayTeam"]),"P"] + 1
    T[which(teams == games[i,"HomeTeam"]),"HF"] <- T[which(teams == games[i,"HomeTeam"]),"HF"] +
      games[i,"FTHG"]
    T[which(teams == games[i,"HomeTeam"]),"HA"] <- T[which(teams == games[i,"HomeTeam"]),"HA"] +
      games[i,"FTAG"]
    T[which(teams == games[i,"AwayTeam"]),"AF"] <- T[which(teams == games[i,"AwayTeam"]),"AF"] +
      games[i,"FTAG"]
    T[which(teams == games[i,"AwayTeam"]),"AA"] <- T[which(teams == games[i,"AwayTeam"]),"AA"] +
      games[i,"FTHG"]
    T[which(teams == games[i,"HomeTeam"]),"GD"] <- T[which(teams == games[i,"HomeTeam"]),"GD"] +
      (games[i,"FTHG"] - games[i,"FTAG"])
    T[which(teams == games[i,"AwayTeam"]),"GD"] <- T[which(teams == games[i,"AwayTeam"]),"GD"] +
      (games[i,"FTAG"] - games[i,"FTHG"])
  }
  S <-  data.frame(row.names=c(1:n), T[with(T, order(-Points, -GD)), ])
  return(S)
}


EPL_tablehistory <- llply(seasons,Table, games=EPL_data)
names(EPL_tablehistory) <- seasons
EPL_tablehistory <- ldply(EPL_tablehistory)
EPL_tablehistory <- rename(EPL_tablehistory,replace=c(".id"="Season"))
EPL_tablehistory$Season <- factor(EPL_tablehistory$Season,levels=dates,ordered=TRUE)
EPL_tablehistory <- EPL_tablehistory %.%
  group_by(Season) %.%
  mutate(Position=rank(-Points))

# Average position of EPL by team
EPL_tablehistory %.%
  group_by(Team) %.%
  summarise(median(Position))


