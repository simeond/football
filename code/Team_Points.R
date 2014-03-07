# POINTS HISTORY

library(dplyr)
library(zoo)
# EPL_data <- read.csv("data/EPL_data_test.csv") # read in data

# Points per match####
pointsPerMatch <- function(games,team) { games %.%                                         
  select(Div:Season) %.%
  filter(HomeTeam==team| AwayTeam==team) %.%
  mutate(Team=team) %.%
  mutate(Opposition = factor(ifelse(HomeTeam==team,as.character(AwayTeam),
                             as.character(HomeTeam)))) %.%
  mutate(playHome = ifelse(HomeTeam==Team,1,0)) %.%  
  mutate(Goals = ifelse(HomeTeam==team,FTHG,FTAG)) %.%  
  mutate(GD=0) %.%
  mutate(GD = ifelse(HomeTeam==team,FTHG-FTAG,FTAG-FTHG)) %.%
  mutate(Points = ifelse(GD>0,3,ifelse(GD==0,1,0)))
}

EPL_points <- ldply(teams,pointsPerMatch,games=EPL_data)


# Add rolling averages, sd & cumulative points
rollmean5 <- function(x){ # zoo based 5 week rolling mean
  require(zoo)
  rollmean(x, 5,  fill=NA, align="right")
}

rollsd5 <- function(x){ # zoo based 5 week rolling sd
  require(zoo)
  rollapply(x, 5, sd, fill=NA, align="right")
}

EPL_points <- EPL_points %.%
  group_by(Team,Season) %.%
  mutate(mean5Points=rollmean5(zoo(Points,Date)),
         sd5Points=rollsd5(zoo(Points,Date)),
         cumPoints=cumsum(Points)
         )


# Average points per game over EPL
EPL_points %.%
   group_by(Team) %.%
   summarise(meanPoints=mean(Points)) %.%
   arrange(desc(meanPoints))

# Average goals per game over EPL
EPL_points %.%
  group_by(Team) %.%
  summarise(meanGoals=mean(Goals)) %.%
  arrange(desc(meanGoals))

