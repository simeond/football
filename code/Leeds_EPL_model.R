# Football -- an R file to model and simulate football results.

# Copyright (C) 2011  James Gardner, Jochen Voss
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# version 1 (2011-05-06)

# Newer versions may possibly be found at
# http://www1.maths.leeds.ac.uk/~voss/projects/2010-sports/

Parameters <- function(games) {
  teams <- sort(unique(c(games[,1], games[,2])), decreasing = FALSE)
  n <- length(teams)
  g <- nrow(games)
  Y <- matrix(0,2*g,1)
  for (i in 1:g) {
    Y[((2*i)-1)] <- games[i,3]
    Y[(2*i)] <- games[i,4]
  }
  X <- matrix(0,2*g,((2*n)+1))
  for (i in 1:g) {
    M <- which(teams == games[i,1])
    N <- which(teams == games[i,2])
    X[((2*i)-1),M] <- 1
    X[((2*i)-1),N+n] <- -1
    X[(2*i),N] <- 1
    X[(2*i),M+n] <- -1
    X[((2*i)-1),((2*n)+1)] <- 1
  }
  XX <- X[,-1]
  parameters <- glm(formula = Y ~ 0 + XX, family = poisson)
  Z <- c(0, coefficients(parameters))
  P <- data.frame(row.names=teams, Attack=Z[1:n], Defence=Z[(n+1):(2*n)])
  return(list(teams=P,home=as.numeric(Z[2*n+1])))
}

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

Games <- function(parameters) {
  rownames(parameters$teams)
  P <- parameters$teams
  home <- parameters$home
  n <- length(teams)
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- rpois(1, exp(P[i,]$Attack - P[j,]$Defence + home))
        C[row,4] <- rpois(1, exp(P[j,]$Attack - P[i,]$Defence))
        row <- row + 1
      }
    }
  }
  return(C)
}

Sim <- function(parameters, k) {
  teams <- rownames(parameters$teams)
  n <- length(teams)
  A <- data.frame(row.names=teams)
  for(i in 1:k) {
    T <- Table(Games(parameters))
    for(j in 1:n) {
      A[teams[j],i] <- which(T == teams[j])
    }
  }
  return(A)
}

SimStats <- function(Sim) {
  teams <- rownames(Sim)
  n <- length(teams)
  zero <- matrix(0,n,1)
  M <- data.frame(Team=teams, Average=rowMeans(Sim), StDev=zero, Mode=zero, Attack=zero,
                  Defence=zero)
  for(i in 1:n) {
    a <- as.numeric(Sim[i,])
    M[i,"StDev"] <- sd(a)
    M[i,"Mode"] <- names(sort(-table(a)))[1] 
  }
  for(i in 1:n) {
    M[i,"Attack"] <- TeamParameters$teams[i,"Attack"]
    M[i,"Defence"] <- TeamParameters$teams[i,"Defence"]
  }
  N <- data.frame(row.names=c(1:n), M[with(M, order(Average)), ])
  return(N)
}

SimTeam <- function(Predictions,k,Team) {
  D <- numeric(k)
  teams <- rownames(Predictions)
  for(i in 1:k) {
    D[i] <- Predictions[which(teams == Team),i]
  }
  return(D)
}

MultiPara <- function(realpara,k) {
  teams <- rownames(realpara$teams)
  n <- length(teams)
  zero <- matrix(0,n,1)
  Q <- data.frame(row.names=teams, Attack=zero, ASQ=zero, Defence=zero, DSQ=zero)
  homepara <- 0
  homeSQ <- 0
  for(i in 1:k) {
    G <- Games(realpara)
    P <- Parameters(G)
    for(j in 1:n) {
      Q[j,1] <- Q[j,1] + P$teams[j,1]
      Q[j,2] <- Q[j,2] + P$teams[j,1]^2
      Q[j,3] <- Q[j,3] + P$teams[j,2]
      Q[j,4] <- Q[j,4] + P$teams[j,2]^2	
    }
    homepara <- homepara + P$home
    homeSQ <- homeSQ + P$home^2
  }
  R <- data.frame(row.names=teams,
                  Attack.bias=realpara$teams[,1] - Q[,1]/k, 
                  Attack.sd=(k/(k-1))*(sqrt(Q[,2]/k - (Q[,1]/k)^2)),
                  Defence.bias=realpara$teams[,2] - Q[,3]/k, 
                  Defence.sd=(k/(k-1))*(sqrt(Q[,4]/k - (Q[,3]/k)^2)))
  return(list(teams=R, home.bias=realpara$home - homepara/k, 
              home.sd=(k/(k-1))*(sqrt(homeSQ/k - (homepara/k)^2))))
}

ProbTable <- function(parameters,hometeam,awayteam) {
  teams <- rownames(parameters$teams)
  P <- parameters$teams
  home <- parameters$home
  a <- which(teams == hometeam)
  b <- which(teams == awayteam)
  lambdaa <- exp(P[a,]$Attack - P[b,]$Defence + home)
  lambdab <- exp(P[b,]$Attack - P[a,]$Defence)
  A <- as.numeric()
  B <- as.numeric()
  for(i in 0:6) {
    A[(i+1)] <- dpois(i,lambdaa)
    B[(i+1)] <- dpois(i,lambdab)
  }
  A[8] <- 1 - sum(A[1:7])
  B[8] <- 1 - sum(B[1:7])
  name <- c("0","1","2","3","4","5","6","7+")
  zero <- mat.or.vec(8,1)
  C <- data.frame(row.names=name, "0"=zero, "1"=zero, "2"=zero, "3"=zero, "4"=zero,
                  "5"=zero, "6"=zero, "7+"=zero)
  for(j in 1:8) {
    for(k in 1:8) {
      C[j,k] <- A[k]*B[j]
    }
  }
  colnames(C) <- name
  return(round(C*100,2))
}

ResultProbs <- function(probs) {
  R <- matrix(0,3,1)
  n <- length(probs)
  for(i in 1:n) {
    for(j in 1:n) {
      if(i > j) {
        R[3] <- R[3] + probs[i,j]
      } else {
        if(i == j) {
          R[2] <- R[2] + probs[i,j]
        } else {
          R[1] <- R[1] + probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1], Draw=R[2], AwayWin=R[3]))
}