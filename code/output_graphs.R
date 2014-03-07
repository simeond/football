# GRAPH UP OUTPUT

# PLOT HOME GOALS, AWAY GOALS & DIFFERENCES
Plots histograms over home_goals, away_goals, the difference in goals####
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

# PLOT PREDICTIONS BASED ON MODEL####
plot_pred_comp <- function(home_team, away_team, ms, season) {
  # Simulates and plots game goals scores using the MCMC samples from the m3
  # model.
  par(mfrow = c(2, 4))
  home_baseline <- ms[, "home_baseline[21]"]
  away_baseline <- ms[, "away_baseline[21]"]
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

# RANKING OF TEAMS BY SKILL BY SEASON####
team_skill <- ms3[, str_detect(string = colnames(ms3), "skill\\[21,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_baseline[21]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
caterplot(team_skill, labels.loc = "above", val.lim = c(0.7, 3.8))

plotPost(team_skill[, "Arsenal"] - team_skill[, "Aston Villa"], compVal = 0,
         xlab = "← Arsenal   vs     BAV →")

# MATCH PREDICTIONS ####
n <- nrow(ms3)
m3_pred <- sapply(1:nrow(EPL_data), function(i) {
  home_team <- which(teams == EPL_data$HomeTeam[i])
  away_team <- which(teams == EPL_data$AwayTeam[i])
  season <- which(seasons == EPL_data$Season[i])
  home_skill <- ms3[, col_name("skill", season, home_team)]
  away_skill <- ms3[, col_name("skill", season, away_team)]
  home_baseline <- ms3[, col_name("home_baseline", season)]
  away_baseline <- ms3[, col_name("away_baseline", season)]  
  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <-  as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)  
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
m3_pred <- t(m3_pred)
mean(EPL_data$FTHG == m3_pred[, "mode_home_goal"], na.rm = T)

