get_door_prob_CI_H0 <- function(res, tx, alpha = 0.05, method = "bootstrap", B = 100){
  if (method == "bootstrap"){
    p <- replicate(B,
                   {
                     x <- res$DOOR
                     N <- apply(res[-1], 2, sum)
                     p <- apply(res[2] + res[3], 2, function(n) n/ N)
                     data <- data.frame(
                       seq = rep(tx, times = N),
                       #DOOR = as.vector(sapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p[,txi])))
                       DOOR = as.vector(sapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p)))
                     )
                     res <- get_door_summary(data, "seq", "DOOR")
                     get_door_probability(res)
                   })
  }
  quantile(p, probs = c(alpha/2, 1-alpha/2))
}
