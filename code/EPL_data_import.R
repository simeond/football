# IMPORT EPL DATA

# scrape data####
library(plyr)
library(stringr)
library(lubridate)

# ("http://www.football-data.co.uk/mmz4281/1213/E0.csv")

years <- substr(as.character(1993:2014),3,4)
dates <- head(paste(years,years[-1],sep=""),-1)
url <- paste("http://www.football-data.co.uk/mmz4281/",dates,"/E0.csv",sep="")


read_csv_filename <- function(filename){
    ret <- read.csv(filename)
    ret$Season <- str_sub(filename,40,43)
    ret <- ret[ret$HomeTeam!="", ]
    ret
  }

EPL_data <-ldply(url,read_csv_filename)

# tidy up data & create variables ####
EPL_data <- EPL_data[EPL_data$HomeTeam!="", ]
EPL_data <- EPL_data[ ,colSums(is.na(EPL_data))<nrow(EPL_data)]
EPL_data$Season <- factor(EPL_data$Season,levels=dates,ordered=TRUE)

# convenience vectors
teams <- unique(c(as.character(EPL_data$HomeTeam), 
                  as.character(EPL_data$AwayTeam)))
seasons <- unique(EPL_data$Season)

convertDates <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

EPL_data$Date <- convertDates(dmy(EPL_data$Date),1992) # to convert 93 to 1993 not 2093
EPL_data$HomeTeam <- factor(EPL_data$HomeTeam)
EPL_data$AwayTeam <- factor(EPL_data$AwayTeam)

# -1 = Away win, 0 = Draw, 1 = Home win
EPL_data$MatchResult <- sign(EPL_data$FTHG - EPL_data$FTAG)


# A list for JAGS with the data from d where the strings are coded as
# integers
data_list <- list(HomeGoals = EPL_data$FTHG,
                  AwayGoals = EPL_data$FTAG,
                  HomeTeam  = as.numeric(factor(EPL_data$HomeTeam,levels = teams)),
                  AwayTeam  = as.numeric(factor(EPL_data$AwayTeam, levels = teams)),
                  Season    = as.numeric(factor(EPL_data$Season, levels = seasons, order=TRUE)),
                  n_teams   = length(teams),
                  n_games   = nrow(EPL_data), n_seasons = length(seasons))

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep = ","), "]")
}


# graph####
library(ggplot2)

m <- ggplot(EPL_data,aes(x=Date,y=FTHG))
m + geom_path() + facet_wrap(~HomeTeam)

qplot(Season, HomeTeam, data = EPL_data, ylab = "Team", xlab = "Particicipation by Season")

