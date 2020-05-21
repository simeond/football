# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# DOWNLOAD EPL GAMES  
# 22 April 2018
# 
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# ***********************************************************************************
# NOTES
# 
#
# todo:
#   (1)  
#   (x) 
#   (x) 
# 
# ***********************************************************************************


# ---- packages ----

pacs <-c("tidyverse","readxl","forcats","stringr","haven","skimr")
sapply(pacs, require, character=TRUE)

source("~/Dropbox/HTML_Presentations/assets/templates/ggplot_essence_theme.R")


# ---- load_data ----

years <- substr(as.character(1993:2018),3,4) # extract two digit years
dates <- head(paste(years,years[-1],sep=""),-1)  
url <- paste("http://www.football-data.co.uk/mmz4281/",dates,"/E0.csv",sep="")

EPL <- map_df(url,read_csv,.id="season")

# tidy up 
EPL <- EPL %>% 
  filter(!is.na(FTHG)) %>% 
  select(season,
         division=Div,
         date=Date,
         home_team=HomeTeam,
         away_team=AwayTeam,
         home_goals=FTHG,
         away_goals=FTAG,
         referee=Referee,
         bet_home=B365H,
         bet_draw=B365D,
         bet_away=B365A) %>% 
  group_by(season) %>% 
  mutate(score_diff=home_goals-away_goals,
         nteams = n_distinct(home_team),
         ngames = n())

skim_to_list(EPL)


