# LINEAR MODEL OF EPL

games <- EPL_data[EPL_data$Season=='1112', ]
teams

model0 <- glm(Goals ~ 1,data=EPL_points,family='poisson')
model1 <- glm(Goals ~ 0 + Team,data=EPL_points,family='poisson')
model2 <- glm(Goals ~ 0 + Team + Opposition ,data=EPL_points,family='poisson')
model3 <- glm(Goals ~ 0 + Team + Opposition + HomeTeam, data=EPL_points,family='poisson')
anova (model0,model1,model2,model3,test='Chisq')

t <- model.frame(Goals ~ 0 + Team + Opposition + HomeTeam[HomeTeam==Team], data=EPL_points)

fbpredict <- function(object,HomeTeam,AwayTeam) {
  top <- data.frame(Team=c(HomeTeam,AwayTeam),Opposition=c(AwayTeam,HomeTeam),HomeTeam=HomeTeam)
  prepred <- predict(object,top,type='response')
  dp1 <- dpois(0:9,prepred[1])
  dp2 <- dpois(0:9,prepred[2])
  oo <- outer(dp2,dp1)
  rownames(oo) <- 0:9
  colnames(oo) <- 0:9
  class(oo) <- c('fboo',class(oo))
  attr(oo,'row') <- HomeTeam
  attr(oo,'col') <- AwayTeam
  wel <- c(sum(oo[upper.tri(oo)]),sum(diag(oo)),sum(oo[lower.tri(oo)]))
  names(wel) <- c(HomeTeam,'equal',AwayTeam)
  return(list(details=oo,'summary chances'=wel))
}


print.fboo <- function(x,...) {
  cat(attr(x,'row'),'in rows against',attr(x,'col'),'in columns \n')
  class(x) <- class(x)[-1]
  attr(x,'row') <- NULL
  attr(x,'col') <- NULL
  oo <- formatC(x,format='f',width=4) # fixed format
  oo <- gsub('\\.0+$','       ',oo)   # replace trailing 0 by ' '
  oo <- substr(oo,1,6)                # and fix the width
  print(oo,quote=FALSE,justify='left')
}

fbpredict(model3,"Aston Villa","Tottenham")
fbpredict(model3,"Tottenham","Aston Villa")

fbpredict(model3,"Arsenal","Man United")
fbpredict(model3,"Man United","Arsenal")


##### leeds model


Parameters <- function(games,Season) {
  teams <- sort(unique(c(games[,HomeTeam], games[,AwayTeam])), decreasing = FALSE)
  n <- length(teams)
  g <- nrow(games)
  Y <- matrix(0,2*g,1)
  for (i in 1:g) {
    Y[((2*i)-1)] <- games[1,5]
    Y[(2*i)] <- games[i,6]
  }
  X <- matrix(0,2*g,((2*n)+1))
  for (i in 1:g) {
    M <- which(teams == games[i,3])
    N <- which(teams == games[i,4])
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

M <- which(teams == games[2,3])
N <- which(teams == games[2,4])
X[((2*1)-1),M] <- 1
X[((2*1)-1),N+n] <- -1
X[(2*1),N] <- 1
X[(2*1),M+n] <- -1
X[((2*1)-1),((2*n)+1)] <- 1


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