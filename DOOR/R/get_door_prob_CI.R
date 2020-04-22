#' Calculate confidence interval for the DOOR probability
#'
#' @param res summary table of the sample size by treatment and door outcome, result of get_door_summary. Treatment in columns, DOOR outcomes in rows.
#' @param tx character, levels of the treatment indicator variable that should also be column names res
#' @param alpha numeric in (0,1) significance level, so that 1-alpha is the coverage probability.
#' @param method character, to indicate whether bootstrap ("bootstrap") or asymptotic formulae ("exact") should be used.
#' @param B number of bootstrap samples
#'
#' @return dataframe with the door summary
#'
#' @description This function calculates the DOOR probability from the door summary.
#'
#' @references Evans, S. R., Rubin, D., Follmann, et. al. (2015). Desirability of outcome ranking (DOOR) and response adjusted for duration of antibiotic risk (RADAR). Clinical Infectious Diseases, 61(5), 800-806.
#'
#' @examples
#' set.seed(123)
#' N <- 26
#' seq <- rep(c("A", "B"), each = N/2)
#' DOOR <- sample(1:4, size = N, replace = TRUE)
#' data <- data.frame(seq, DOOR)
#' res <- get_door_summary(data, "seq", "DOOR")
#' get_door_probability(res)
#' #get_door_prob_CI(res, tx = c("A", "B"))
#'
#'@importFrom stats quantile
#'
#' @export
get_door_prob_CI <- function(res, tx, alpha = 0.05, method = "bootstrap", B = 100){
  if (method == "bootstrap"){
    p <- replicate(B,
                   {
                     x <- res$DOOR
                     N <- apply(res[-1], 2, sum)
                     p <- apply(res[-1], 2, function(n) n/sum(N))
                     data <- data.frame(
                       seq = rep(tx, times = N),
                       DOOR = as.vector(sapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p[,txi])))
                     )
                     res <- get_door_summary(data, "seq", "DOOR")
                     get_door_probability(res)
                   })
  quantile(p, probs = c(alpha/2, 1-alpha/2), names = FALSE)
  } else if (method == "multinom"){
    DOOR_pr <- get_door_probability(res)

    # CONSTRUCT COVARIANCE MATRIX
    K <- nrow(res) #Number of door levels
    N <- apply(res[-1], 2, sum)
    pr <- apply(res[-1], 2, function(x) x/sum(N))

    V <- apply(pr, 2, function(x){
      tmp <- -(x %*% t(x))/(sum(x)^2)
      diag(tmp) <- (x * (1-x))/(sum(x)^2)
      tmp
    })

    V <- t(t(V)/N)

    V_p <- matrix(0, nrow = 2*K, ncol = 2*K)
    V_p[1:K, 1:K] <- V[,1]
    V_p[(K+1):(2*K), (K+1):(2*K)] <- V[,2]

    # # CONSTRUCT THE DELTA
    nA <- pull(res, tx[1])
    nB <- pull(res, tx[2])

    MA <- sapply(1:K, function(k) lead(nA, n = k, default = 0))
    MB <- sapply(1:K, function(k) lead(nB, n = k, default = 0))
    J <- rep(1, K)

    Delta_A <- (MB %*% J + 0.5 * nB)/ sum(nB)
    Delta_B <- (MA %*% J + 0.5 * nA)/ sum(nA)
    Delta <- c(Delta_A, Delta_B)

    se <- sqrt(Delta %*% V_p %*% Delta)

    c(max(DOOR_pr + qnorm(alpha/2)*se, 0), min(DOOR_pr - qnorm(alpha/2)*se, 1)) # Interval should lie in [0,1]
  } else {
    print("Method not recognized")
  }
}
