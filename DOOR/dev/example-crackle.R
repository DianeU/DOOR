library(tidyverse)
library(DOOR)

myData <- read.csv("./dev/DOOR_tst.csv")

myData2 <- myData %>% rename(tx = trt, DOOR = doorcat4)

myData2 %>% select(-patnum)

res <- get_door_summary(myData, tx = "trt", outcome = "doorcat4")
get_door_prob_CI(res, tx = c("Non-CP CRE", "CPE"), method = "halperin")
get_door_prob_CI(res, tx = c("Non-CP CRE", "CPE"), method = "multinom")
get_door_prob_CI(res, tx = c("Non-CP CRE", "CPE"))

x <- pull(res, 1)
N <- apply(res[-1], 2, sum)
p <- apply(res[-1], 2, function(n) n/sum(N))
data <- data.frame(
  seq = rep(tx, times = N),
  DOOR = unlist(lapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p[,txi])))
)
res <- get_door_summary(data, "seq", "DOOR")
get_door_probability(res, tx = tx)
