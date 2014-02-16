# JAGS Model
library(rjags)
library(coda)
library(mcmcplots)

m3_string <- "model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i],AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i],AwayTeam[i]])
}

for(season_i in 1:n_seasons) {
for(home_i in 1:n_teams) {
for(away_i in 1:n_teams) {
lambda_home[season_i, home_i, away_i] <- exp( home_baseline[season_i] + skill[season_i, home_i] - skill[season_i, away_i])
lambda_away[season_i, home_i, away_i] <- exp( away_baseline[season_i] + skill[season_i, away_i] - skill[season_i, home_i])
}
}
}

skill[1, 1] <- 0 
for(j in 2:n_teams) {
skill[1, j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_baseline[1] ~ dnorm(0, 0.0625)
away_baseline[1] ~ dnorm(0, 0.0625)

for(season_i in 2:n_seasons) {
skill[season_i, 1] <- 0 
for(j in 2:n_teams) {
skill[season_i, j] ~ dnorm(skill[season_i - 1, j], season_tau)
}
home_baseline[season_i] ~ dnorm(home_baseline[season_i - 1], season_tau)
away_baseline[season_i] ~ dnorm(away_baseline[season_i - 1], season_tau)
}

season_tau <- 1/pow(season_sigma, 2) 
season_sigma ~ dunif(0, 3) 
}"


# Compiling model 1
m3 <- jags.model(textConnection(m3_string),
                 data = data_list, n.chains = 3,
                 n.adapt = 10000)
update(m3, 10000)

s3 <- coda.samples(m3, 
                   variable.names = c("home_baseline", "away_baseline",
                                      "skill", "season_sigma", "group_sigma", "group_skill"),
                   n.iter = 40000, thin = 8)

# Merging the three MCMC chains into one matrix
ms3 <- as.matrix(s3)
Sys.time()

summary(s3)
plot(s3[, "home_baseline"])
plotPost(exp(ms3[, "home_baseline"]) - exp(ms3[, "away_baseline"]), compVal = 0,
         xlab = "Home advantage in number of goals")