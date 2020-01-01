# Sample data

# Simplest manual example
seq <- rep(c("A", "B"), each = 4)
DOOR <- c(4,4,2,3,1,2,3,4)
data <- data.frame(DOOR, seq)
res <- get_door_summary(data, seq, DOOR)


# Medium example
N <- 500
seq <- rep(c("A", "B"), each = N/2)
#DOOR <- sample(1:4, size = N, replace = T)
DOOR <- c(
  sample(1:4, size = N/2, replace = T, prob = c(0.05, 0.15, 0.25, 0.55)),
  sample(1:4, size = N/2, replace = T)
)
data <- data.frame(seq, DOOR)
res <- get_door_summary(data, seq, DOOR)



get_door_probability(res)
get_door_probability_1(data)

rbenchmark::benchmark(
 "vectorized" = get_door_probability(res),
 "loop" = get_door_probability_1(data),
 replications = 1000
)


t1 <- Sys.time()
get_door_prob_CI(res, tx = c("A", "B"), B = 1000)
t2 <- Sys.time()
t2 - t1
