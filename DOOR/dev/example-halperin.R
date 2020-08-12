
tx <- c("A", "B")

#Increasing number of DOOR levels K = 3,4,5
K <- 3; N <- 100

#Increasing “Treatment effect”, i.e. increasing true DOOR probability p= P (Y_A≥Y_B).
pA <- c(0.2, 0.2, 0.6)
pB <- c(0.2, 0.6, 0.2)


myData <- data.frame(seq = rep(c("A", "B"), each = N))
myData <- myData %>%
  mutate(DOOR = (seq==tx[1])* sample(1:(K), size = N, replace = TRUE, prob = pA) +
           (seq==tx[2])* sample(1:(K), size = N, replace = TRUE, prob = pB))
res <- get_door_summary(myData, "seq", "DOOR")
xi <- get_door_probability(res)
ci <- get_door_prob_CI(res, tx = c("A", "B"), method = "halperin")
