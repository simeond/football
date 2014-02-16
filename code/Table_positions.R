# Table Positions

Table <- function(games) {
  teams <- sort(unique(c(games[,1], games[,2])), decreasing = FALSE)
  n <- length(teams)
  g <- nrow(games)
  zero <- matrix(0,n,1)
  T <- data.frame(Team=teams, P=zero, HW=zero, HD=zero, HL=zero, HF=zero, HA=zero,
                  AW=zero, AD=zero, AL=zero, AF=zero, AA=zero, GD=zero, Points=zero)
  for (i in 1:g) {
    if (games[i,3] > games[i,4]) {
      T[which(teams == games[i,1]),"Points"] <-
        T[which(teams == games[i,1]),"Points"] + 3
      T[which(teams == games[i,1]),"HW"] <-
        T[which(teams == games[i,1]),"HW"] + 1
      T[which(teams == games[i,2]),"AL"] <-
        T[which(teams == games[i,2]),"AL"] + 1
    } else {
      if (games[i,3] == games[i,4]) {
        T[which(teams == games[i,1]),"Points"] <-
          T[which(teams == games[i,1]),"Points"] + 1
        T[which(teams == games[i,2]),"Points"] <-
          T[which(teams == games[i,2]),"Points"] + 1
        T[which(teams == games[i,1]),"HD"] <-
          T[which(teams == games[i,1]),"HD"] + 1
        T[which(teams == games[i,2]),"AD"] <-
          T[which(teams == games[i,2]),"AD"] + 1
      } else {
        T[which(teams == games[i,2]),"Points"] <-
          T[which(teams == games[i,2]),"Points"] + 3
        T[which(teams == games[i,2]),"AW"] <-
          T[which(teams == games[i,2]),"AW"] + 1
        T[which(teams == games[i,1]),"HL"] <-
          T[which(teams == games[i,1]),"HL"] + 1
      }
    }
    T[which(teams == games[i,1]),"P"] <- T[which(teams == games[i,1]),"P"] + 1
    T[which(teams == games[i,2]),"P"] <- T[which(teams == games[i,2]),"P"] + 1
    T[which(teams == games[i,1]),"HF"] <- T[which(teams == games[i,1]),"HF"] +
      games[i,3]
    T[which(teams == games[i,1]),"HA"] <- T[which(teams == games[i,1]),"HA"] +
      games[i,4]
    T[which(teams == games[i,2]),"AF"] <- T[which(teams == games[i,2]),"AF"] +
      games[i,4]
    T[which(teams == games[i,2]),"AA"] <- T[which(teams == games[i,2]),"AA"] +
      games[i,3]
    T[which(teams == games[i,1]),"GD"] <- T[which(teams == games[i,1]),"GD"] +
      (games[i,3] - games[i,4])
    T[which(teams == games[i,2]),"GD"] <- T[which(teams == games[i,2]),"GD"] +
      (games[i,4] - games[i,3])
  }
  S <-  data.frame(row.names=c(1:n), T[with(T, order(-Points, -GD)), ])
  return(S)
}