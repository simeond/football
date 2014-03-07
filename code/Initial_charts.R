# INITIAL CHARTS

library(ggplot)
library(zoo)
# EPL_data <- read.csv("data/EPL_data_test.csv") # read in data

# Points per match####
m <- ggplot(t3,aes(x=Date,y=we5Points,group=Season))
m + geom_line(aes(colour=Season)) + facet_wrap(~Team)

t3 <- EPL_points %.%
  group_by(Team) %.%
  summarise(meanPoints=mean(Points)) %.%
  arrange(desc(meanPoints))



qplot(Season, HomeTeam, data = EPL_data, ylab = "Team", xlab = "Particicipation by Season")
