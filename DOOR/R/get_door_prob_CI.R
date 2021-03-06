#' Calculate confidence interval for the DOOR probability
#'
#' @param res summary table of the sample size by treatment and door outcome, result of get_door_summary. Treatment in columns, DOOR outcomes in rows.
#' @param tx character, levels of the treatment indicator variable that should also be column names res
#' @param alpha numeric in (0,1) significance level, so that 1-alpha is the coverage probability.
#' @param method character, to indicate whether bootstrap ("bootstrap"), asymptotic formulae ("multinom"), or the method proposed by Halperin et al (1989) ("halperin") should be used.
#' @param B number of bootstrap samples
#'
#' @return list that contains two elements: the two element vector ci contains the lower and upper bound of the confidence interval. se contains the standard error of the DOOR probability estimator. For the
#' bootstrap method that is the sample variance of the generated bootstrap samples. For multinomial method, the standard error is computed using the Delta method. For the Halperin method,
#' se actually denotes V tilde of equation 3.6 from the paper.
#'
#' @description This function calculates the DOOR probability from the door summary.
#'
#' @references Evans, S. R., Rubin, D., Follmann, et. al. (2015). Desirability of outcome ranking (DOOR) and response adjusted for duration of antibiotic risk (RADAR). Clinical Infectious Diseases, 61(5), 800-806.
#' Halperin, M, Hamdy MI, Thall PF (1989). Distribution-free confidence intervals for a parameter of Wilcoxon-Mann-Whitney type for ordered categories and progressive censoring.
#'
#' @examples
#' set.seed(123)
#' N <- 26
#' seq <- rep(c("A", "B"), each = N/2)
#' DOOR <- sample(1:4, size = N, replace = TRUE)
#' data <- data.frame(seq, DOOR)
#' res <- get_door_summary(data, "seq", "DOOR")
#' get_door_prob_CI(res, tx = c("A", "B"), method = "halperin")
#'
#'@importFrom stats quantile lag qchisq qnorm
#'
#' @export
get_door_prob_CI <- function(res, tx, alpha = 0.05, method = "bootstrap", B = 100){
  if (method == "bootstrap"){
    N <- apply(res[-1], 2, sum)
    x <- pull(res, 1)
    p <- apply(res[-1], 2, function(n) n/sum(N))
    p <- replicate(B,
                   {
                     data <- data.frame(
                       seq = rep(tx, times = N),
                       DOOR = unlist(lapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p[,txi])))
                     )
                     res <- get_door_summary(data, "seq", "DOOR")
                     get_door_probability(res, tx = tx)
                   })
  list(ci = quantile(p, probs = c(alpha/2, 1-alpha/2), names = FALSE),
       se = sqrt(var(p)/sum(N)))
  } else if (method == "multinom"){
    DOOR_pr <- get_door_probability(res, tx = tx)

    # CONSTRUCT COVARIANCE MATRIX
    K <- nrow(res) #Number of door levels
    N <- apply(res[-1], 2, sum)
    #pr <- apply(res[-1], 2, function(x) x/sum(N))
    pr <- apply(res[-1], 2, function(x) x/sum(x))

    # Create variance matrix
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

    MA <- sapply(1:K, function(k) lead(nA, n = k, default = 0)) # Diagonal matrix with the frequencies
    MB <- sapply(1:K, function(k) lead(nB, n = k, default = 0))
    J <- rep(1, K)

    Delta_A <- (MB %*% J + 0.5 * nB)/ sum(nB)
    Delta_B <- (MA %*% J + 0.5 * nA)/ sum(nA)
    Delta <- c(Delta_A, Delta_B)

    se <- sqrt(Delta %*% V_p %*% Delta)


    list(ci = c(max(DOOR_pr + qnorm(alpha/2)*se, 0), min(DOOR_pr - qnorm(alpha/2)*se, 1)), # Interval should lie in [0,1]
         se = se)

  } else if (method == "halperin"){
    xi <- get_door_probability(res, tx = tx)

    K = nrow(res)
    P <- res %>% dplyr::mutate_at(tx, function(x) x/sum(x))
    p1 <- pull(P, tx[1]) # Group A
    p2 <- pull(P, tx[2]) # Group B

    q1 <- 1 - p1
    q2 <- 1 - p2

    m <- sum(res[,tx[1]]) # Group A
    n <- sum(res[,tx[2]]) # Group B

    P1 <- apply(sapply(1:K, function(k) dplyr::lag(p1, n = K-k+1, default = 0)), 2, sum)
    P2 <- apply(sapply(1:K, function(k) lead(p2, n = k, default = 0)), 2, sum)

    A <- sum((p1*(P2 + (p2/2))^2)[-K]) + 0.25 * p1[K]*(p2[K])^2
    B <- sum((p2*(P1 + (p1/2))^2)[-1]) + 0.25 * p2[1]*(p1[1])^2

    # Maximum Likelihood estimator
    V1 <- 1/(m*n)*(xi - (m + n -1) * xi^2 + (n-1) * A + (m-1)*B - 0.25* sum(p1*p2))

    # Unbiased estimators

    ## A hat hat
    s1 <- sum(p1*(q2*P2 - P2^2))
    corrA <- 1/(n-1) * s1 -  1/(4* (n-1)) * sum(p1*p2*q2)
    Ahh <- A - corrA

    ## B hat hat
    s2 <- sum(p2 * (q1 * P1 - P1^2))
    corrB <- 1/(m-1) * s2 -  1/(4* (m-1)) * sum(p2*p1*q1)
    Bhh <- B - corrB

    ## Unbiased estimator of xi* (1 - xi)
    xi2 <- ((m*n - m - n +2) * xi - m *n * xi^2)/((m-1)*(n-1)) + Ahh/(m-1) + Bhh/(n-1)

    theta <- ((m+n-2) * xi - (n-1)*Ahh - (m-1) * Bhh)/((m + n - 2)* xi2)
    theta <- max(min(theta, 1), 0)

    gamma <- (m + n - 1) - (m + n - 2) * theta

    V_tilde = gamma*xi2/(m*n)

    C <- gamma * qchisq(1-alpha, 1)/(m*n)

    b <- sqrt(C^2 + 4 * C * xi*(1-xi))
    UL <- (C + 2 * xi + b)/(2 * (C+1))
    LL <- (C + 2 * xi - b)/(2 * (C+1))


    list(ci = c(LL, UL), # Interval should lie in [0,1]
         se = V_tilde)
  }  else if (method == "halperin2"){
    xi <- get_door_probability(res, tx = tx)

    K = nrow(res)
    P <- res %>% dplyr::mutate_at(tx, function(x) x/sum(x))
    p1 <- pull(P, tx[1]) # Group A
    p2 <- pull(P, tx[2]) # Group B

    q1 <- 1 - p1
    q2 <- 1 - p2

    m <- sum(res[,tx[1]]) # Group A
    n <- sum(res[,tx[2]]) # Group B

    # Cumulative sum of Probabilities
    P1 <- apply(sapply(1:K, function(k) dplyr::lag(p1, n = K-k+1, default = 0)), 2, sum)
    P2 <- apply(sapply(1:K, function(k) lead(p2, n = k, default = 0)), 2, sum)

    # Coefficients in the variance estimation
    A <- sum((p1*(P2 + (p2/2))^2)[-K]) + 0.25 * p1[K]*(p2[K])^2
    B <- sum((p2*(P1 + (p1/2))^2)[-1]) + 0.25 * p2[1]*(p1[1])^2

    # Maximum Likelihood estimator
    V1 <- 1/(m*n)*(xi - (m + n -1) * xi^2 + (n-1) * A + (m-1)*B - 0.25* sum(p1*p2))

    # Unbiased estimators

    ## A hat hat
    s1 <- sum(p1*(q2*P2 - P2^2))
    corrA <- 1/(n-1) * s1 -  1/(4* (n-1)) * sum(p1*p2*q2)
    Ahh <- A - corrA

    ## B hat hat
    s2 <- sum(p2 * (q1 * P1 - P1^2))
    corrB <- 1/(m-1) * s2 -  1/(4* (m-1)) * sum(p2*p1*q1)
    Bhh <- B - corrB

    ## Unbiased estimator of xi* (1 - xi)
    xi2 <- ((m*n - m - n +2) * xi - m *n * xi^2)/((m-1)*(n-1)) + Ahh/(m-1) + Bhh/(n-1)

    theta <- ((m+n-2) * xi - (n-1)*Ahh - (m-1) * Bhh)/((m + n - 2)* xi2)
    theta <- max(min(theta, 1), 0)

    gamma <- (m + n - 1) - (m + n - 2) * theta

    V_tilde = gamma*xi2/(m*n)

    C <- gamma * qchisq(1-alpha, 1)/(m*n)

    b <- sqrt(C^2 + 4 * C * xi2) # just changed xi2 here
    UL <- (C + 2 * xi + b)/(2 * (C+1))
    LL <- (C + 2 * xi - b)/(2 * (C+1))

    list(ci = c(LL, UL), # Interval should lie in [0,1]
         se = V_tilde)
  } else {
    print("Method not recognized")
  }
}
