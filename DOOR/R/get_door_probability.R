#' @include get_door_summary.R
NULL

#' Get DOOR Probability
#'
#' @param res summary table of the sample size by treatment and door outcome, result of get_door_summary. Treatment in columns, DOOR outcomes in rows.
#' @param tx character, levels of the treatment indicator variable that should also be column names res
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
#'
#'
#' @export
get_door_probability <- function(res, tx = c("A", "B")){
  nA <- pull(res, tx[1])
  nB <- pull(res, tx[2])
  K = nrow(res)
  M <- sapply(1:K, function(k) lead(nA, n = k, default = 0))
  (sum(M%*%nB) + 0.5 * sum(nA*nB))/ (sum(nA) * sum(nB))
}


#' Get DOOR probability the conventional way+
#'
#' @param data dataframe, data set that contains the raw data
#'
#' @return summary table with the number of participants by door outcome and treatment group
#'
#' @description This function calculates the DOOR summary
#'
#' @references Evans, S. R., Rubin, D., Follmann, et. al. (2015). Desirability of outcome ranking (DOOR) and response adjusted for duration of antibiotic risk (RADAR). Clinical Infectious Diseases, 61(5), 800-806.
#'
#' @examples
#' set.seed(123)
#' N <- 26
#' seq <- rep(c("A", "B"), each = N/2)
#' DOOR <- sample(1:4, size = N, replace = TRUE)
#' data <- data.frame(seq, DOOR)
#' get_door_summary(data, "seq", "DOOR")
#'
#' @export
get_door_probability_1 <- function(data){
  dataA <- data %>% filter(seq == "A")
  dataB <- data %>% filter(seq == "B")
  count <- 0

  for(i in 1:nrow(dataA)){
    for(j in 1: nrow(dataB)){
      inc <- as.numeric((dataA[i, "DOOR"] > dataB[j, "DOOR"])) + 0.5 * as.numeric((dataA[i, "DOOR"] == dataB[j, "DOOR"]))
      count <- count + inc
    }
  }
  count / (nrow(dataA)*nrow(dataB))
}



