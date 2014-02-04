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
  baseline <- ms[, "baseline"]
  home_skill <- ms[, col_name("skill", which(teams == home_team))]
  away_skill <- ms[, col_name("skill", which(teams == away_team))]
  home_goals <- rpois(nrow(ms), exp(baseline + home_skill - away_skill))
  away_goals <- rpois(nrow(ms), exp(baseline + away_skill - home_skill))
  plot_goals(home_goals, away_goals)
  # Plots the actual distribution of goals between the two teams
  home_goals <- d$HomeGoals[d$HomeTeam == home_team & d$AwayTeam == away_team]
  away_goals <- d$AwayGoals[d$HomeTeam == home_team & d$AwayTeam == away_team]
  plot_goals(home_goals, away_goals)
}

plot_pred_comp1("Aston Villa", "Arsenal", ms1)