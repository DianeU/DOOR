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

par <- expand.grid(r = r, N = N, K = K, trueP = trueP, stringsAsFactors = FALSE)


getData <- function(x, tx = tx){
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
    pB <- c(0.6, 0.2, 0.2)
  } else {
    warning("Not a valid option")
  }

  myData <- myData %>%
    mutate(DOOR = (seq==tx[1])* sample(1:(x$K), size = N, replace = TRUE, prob = pA) +
             (seq==tx[2])* sample(1:(x$K), size = N, replace = TRUE, prob = pB))
  res <- get_door_summary(myData, "seq", "DOOR")
  xi <- get_door_probability(res)
  unlist(c(x, xi = xi, pull(res, tx[1])/sum(pull(res, tx[1])), pull(res, tx[2])/sum(pull(res, tx[2]))))
}

D <- data.frame(r = numeric(0),  N = numeric(0), K = numeric(0), trueP = numeric(0),
                xi = numeric(0), p11 = numeric(0), p12 = numeric(0), p13 = numeric(0), p21 = numeric(0), p22 = numeric(0), p23 = numeric(0))

T1 <- Sys.time()
for (i in 1:nrow(par)) {
  x <- par[i, , drop = FALSE]
  tmp <- t(replicate(x$r, getData(x, tx = tx)))
  D <- rbind(D, tmp)
}
T2<- Sys.time()
colnames(D)[6:11] <- c("p11", "p12", "p13", "p21", "p22", "p23")
T2-T1

write_rds(D, path = paste0("../simulations/03_results/", Sys.Date(), "_bias.Rds"))
