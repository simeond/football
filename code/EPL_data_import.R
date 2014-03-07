# IMPORT EPL DATA

# scrape data####
library(plyr)
library(stringr)
library(lubridate)

# ("http://www.football-data.co.uk/mmz4281/1213/E0.csv")

years <- substr(as.character(1993:2014),3,4) # extract two digit years
dates <- head(paste(years,years[-1],sep=""),-1)  
url <- paste("http://www.football-data.co.uk/mmz4281/",dates,"/E0.csv",sep="")


read_csv_filename <- function(filename){
    ret <- read.csv(filename)
    ret$Season <- str_sub(filename,40,43) # extract 4 digit season
    ret <- ret[ret$HomeTeam!="", ] # delete blanks
    ret
  }

EPL_data <-ldply(url,read_csv_filename) # read in files

# tidy up data & create variables ####
EPL_data <- EPL_data[EPL_data$HomeTeam!="", ] # delete blanks
EPL_data <- EPL_data[ ,colSums(is.na(EPL_data))<nrow(EPL_data)]  
EPL_data$Season <- factor(EPL_data$Season,levels=dates,ordered=TRUE) # ordered factor

# convenience vectors
teams <- unique(c(as.character(EPL_data$HomeTeam), 
                  as.character(EPL_data$AwayTeam)))
seasons <- unique(EPL_data$Season) # required?

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




