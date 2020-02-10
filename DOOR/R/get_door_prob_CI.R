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
                p <- apply(res[2] + res[3], 2, function(n) n/ N)/2
                data <- data.frame(
                  seq = rep(tx, times = N),
                  DOOR = as.vector(sapply(tx, function(txi) sample(x, size = N[txi], replace = TRUE, prob = p)))
                )
                res <- get_door_summary(data, "seq", "DOOR")
                get_door_probability(res)
              })
  }
  quantile(p, probs = c(alpha/2, 1-alpha/2))
}
