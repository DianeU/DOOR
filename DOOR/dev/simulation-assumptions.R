library(DOOR)

# Scenario 1
M <- matrix(c(1:3, 0.2, 0.2, 0.6,0.2, 0.2, 0.6), ncol = 3)
colnames(M) <- c("DOOR", "A", "B")
get_door_probability(as.data.frame(M))

# Scenario 2
M <- matrix(c(1:3, 0.2, 0.2, 0.6,0.2, 0.6, 0.2), ncol = 3)
colnames(M) <- c("DOOR", "A", "B")
get_door_probability(as.data.frame(M))


# Scenario 3
M <- matrix(c(1:3, 0.2, 0.2, 0.6, 0.6, 0.2, 0.2), ncol = 3)
colnames(M) <- c("DOOR", "A", "B")
get_door_probability(as.data.frame(M))


set.seed(1234)
N <- 100
seq <- rep(c("A", "B"), each = N/2)
DOOR <- c(sample(1:3, size = sum(seq == "A"), replace = TRUE, prob = M[,2]),
          sample(1:3, size = sum(seq == "B"), replace = TRUE, prob = M[,3]))
data <- data.frame(seq, DOOR)
res <- get_door_summary(data, "seq", "DOOR")
get_door_probability(res)
get_door_prob_CI(res, tx = c("A", "B"), B = 1000)
get_door_prob_CI(res, tx = c("A", "B"), method = "multinom")

