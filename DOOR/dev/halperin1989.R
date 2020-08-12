library(DOOR)

set.seed(123)
N <- 26
tx <- c("A", "B")

seq <- rep(tx, each = N/2)
DOOR <- sample(1:3, size = N, replace = TRUE)
data <- data.frame(seq, DOOR)
res <- get_door_summary(data, "seq", "DOOR")
xi <- get_door_probability(res)

K = nrow(res)

p1 <- c(0.2, 0.2, 0.6)
p2 <- c(0.2, 0.6, 0.2)

P <- res %>% dplyr::mutate_at(tx, function(x) x/sum(x))
p1 <- pull(P, tx[1])
p2 <- pull(P, tx[2])

q1 <- 1 - p1
q2 <- 1 - p2

m <- sum(res[,tx[1]])
n <- sum(res[,tx[2]])


P2 <- apply(sapply(1:K, function(k) lead(p2, n = k, default = 0)), 2, sum)
P1 <- apply(sapply(1:K, function(k) dplyr::lag(p1, n = K-k+1, default = 0)), 2, sum)

A <- sum((p1*(P2 + (p2/2))^2)[-K]) + 0.25 * p1[K]*(p2[K])^2
B <- sum((p2*(P1 + (p1/2))^2)[-1]) + 0.25 * p2[1]*(p1[1])^2

# A <- 0
# for(i in 1:(K-1)){
#   innerSum <- 0
#   for (j in i:K) {
#     innerSum <- innerSum + p2[j] + p2[i]/2
#     print(innerSum)
#   }
#   A <- A + p1[i] * innerSum^2
# }
# A <- A + p1[K]*(p2[K])^2/4

#
# B <- 0
# for(j in 2:K){
#   innerSum <- 0
#   for (i in 1:(j-1)) {
#   innerSum <- innerSum +p1[i] + p1[j]/2
#   }
#   B <- B + p2[j] * innerSum^2
# }
# B <- B + (p1[1])^2*p2[1]/4

# Maximum Likelihood estimator
V1 <- 1/(m*n)*(xi - (m + n -1) * xi^2 + (n-1) * A + (m-1)*B - 0.25* sum(p1*p2))

# Unbiased estimators

## A hat hat
#P2 <- apply(sapply(1:K, function(k) lead(p2, n = k, default = 0)), 2, sum)
s2 <- sum(p1*(q2*P2 - P2^2))
corrA <- 1/(n-1) * s2 -  1/(4* (n-1)) * sum(p1*p2*q2)
Ahh <- A - corrA

## B hat hat
#P1 <- apply(sapply(1:K, function(k) lag(p1, n = K-k+1, default = 0)), 2, sum)
s2 <- sum(p2 * (q1 * P1 - P1^2))
corrB <- 1/(m-1) * s2 -  1/(4* (m-1)) * sum(p2*p1*q1)
Bhh <- B - corrB

## Unbiased estimator of xi* (1 - xi)
xi2 <- ((m*n - m - n +2) * xi - m*n*xi^2)/((m-1)*(n-1)) + Ahh/(m-1) + Bhh/(n-1)

theta <- ((m+n-2) * xi - (n-1)*Ahh - (m-1) * Bhh)/((m + n - 2)* xi2)

theta <- max(min(theta, 1), 0)

gamma <- (m + n - 1) - (m + n - 2) * theta

C <- gamma * qchisq(1-0.05, 1)/(m*n)

b <- sqrt(C^2 + 4 * C * xi*(1-xi))
UL <- (C + 2 * xi + b)/(2 * (C+1))
LL <- (C + 2 * xi - b)/(2 * (C+1))


set.seed(123)
N <- 26
seq <- rep(c("A", "B"), each = N/2)
DOOR <- sample(1:4, size = N, replace = TRUE)
data <- data.frame(seq, DOOR)
res <- get_door_summary(data, "seq", "DOOR")
get_door_probability(res)
get_door_prob_CI(res, tx = c("A", "B"), method = "bootstrap")
get_door_prob_CI(res, tx = c("A", "B"), method = "halperin")
get_door_prob_CI(res, tx = c("A", "B"), method = "multinom")
