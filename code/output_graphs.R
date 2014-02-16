# GRAPH UP OUTPUT

# Plots histograms over home_goals, away_goals, the difference in goals
# and a barplot over match results.
plot_goals <- function(home_goals, away_goals) {
  n_matches <- length(home_goals)
  goal_diff <- home_goals - away_goals
  match_result <- ifelse(goal_diff < 0, 
                    "away_win", ifelse(goal_diff > 0,"home_win", "equal"))
  hist(home_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  hist(away_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
  hist(goal_diff, xlim = c(-6, 6), breaks = (-100:100) - 0.5)
  barplot(table(match_result)/n_matches, ylim = c(0, 1))
}


plot_pred_comp1 <- function(home_team, away_team, ms) {
  # Simulates and plots game goals scores using the MCMC samples from the m1
  # model.
  par(mfrow = c(2, 4))
  home_baseline <- ms[, "home_baseline"]
  away_baseline <- ms[, "away_baseline"]
  home_skill <- ms[, col_name("skill", which(teams == home_team))]
  away_skill <- ms[, col_name("skill", which(teams == away_team))]
  home_goals <- rpois(nrow(ms), exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(nrow(ms), exp(away_baseline + away_skill - home_skill))
  plot_goals(home_goals, away_goals)
  # Plots the actual distribution of goals between the two teams
  home_goals <- d$HomeGoals[EPL_data$HomeTeam == home_team & EPL_data$AwayTeam == away_team]
  away_goals <- d$AwayGoals[EPL_data$HomeTeam == home_team & EPL_data$AwayTeam == away_team]
  plot_goals(home_goals, away_goals)
}

plot_pred_comp1("Aston Villa", "Arsenal", ms3)

# The ranking of the teams for the 2012/13 season.
team_skill <- ms3[, str_detect(string = colnames(ms3), "skill\\[21,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_baseline[21]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
caterplot(team_skill, labels.loc = "above", val.lim = c(0.7, 3.8))

plotPost(team_skill[, "Arsenal"] - team_skill[, "Aston Villa"], compVal = 0,
         xlab = "← Arsenal   vs     BAV →")