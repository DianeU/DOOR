library(tidyverse)
library(DOOR)

set.seed(123)
#Confidence Level 1- α= 0.95
alpha <- 0.05

#Number of simulation repetitions r = 10,000 (for a start)
r <- 1000

# Two Treatment Arms A,B
tx <- c("A", "B")

#Increasing number of DOOR levels K = 3,4,5
K <- 3

#Increasing “Treatment effect”, i.e. increasing true DOOR probability p= P (Y_A≥Y_B).
trueP <- c(0.5, 0.66, 0.74)

#Sample size N = 100,200,500.
N <- c(100,200,500)

# Confidence-Interval Method "\"bootstrap\",\"multinom\" }"
# For "method == bootstrap" , number of bootstrap samples B= 1500
 method <- c("bootstrap", "multinom", "halperin")
#method <- c("multinom", "halperin")
B <- 500

par <- expand.grid(alpha = alpha, r=r, K = K, N = N, method = method, B = B, trueP = trueP, stringsAsFactors = FALSE)

D <- data.frame(alpha = numeric(0), r = numeric(0), K = numeric(0), N = numeric(0), method = character(0), B = numeric(0), trueP = numeric(0),
                xi = numeric(0), LL = numeric(0), UL = numeric(0), se = numeric(0), time = numeric(0))

getData <- function(x){
  N <- x$N
  myData <- data.frame(seq = rep(tx, each = N/2))

  if(x$trueP == 0.5){
    pA <- c(0.2, 0.2, 0.6)
    pB <- c(0.2, 0.2, 0.6)
  } else if (x$trueP == 0.66) {
    pA <- c(0.2, 0.2, 0.6)
    pB <- c(0.2, 0.6, 0.2)
  } else if (x$trueP == 0.74) {
    pA <- c(0.2, 0.2, 0.6)
    pB <- c(0.6, 0.6, 0.2)
  }

  myData <- myData %>%
    mutate(DOOR = (seq==tx[1])* sample(1:(x$K), size = N, replace = TRUE, prob = pA) +
             (seq==tx[2])* sample(1:(x$K), size = N, replace = TRUE, prob = pB))
  res <- get_door_summary(myData, "seq", "DOOR")
  xi <- get_door_probability(res)
  t1 <- Sys.time()
  out <- get_door_prob_CI(res, tx = c("A", "B"), method = x$method)
  t2 <- Sys.time()
  time <- t2 - t1
  c(x, xi = xi, LL = out$ci[1], UL = out$ci[2], se = out$se, time = as.numeric(time))
}

T1 <- Sys.time()
for (i in 1:nrow(par)) {
  x <- par[i, , drop = FALSE]
  tmp <- t(replicate(x$r, getData(x)))
  D <- rbind(D, tmp)
}
T2<- Sys.time()

write_rds(D, path = paste0("../simulations/03_results/", Sys.Date(), "_confidence-intervals.Rds"))
