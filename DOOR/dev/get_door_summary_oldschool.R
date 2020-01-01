#' Get DOOR summary
#'
#' @param data dataframe, data set that contains the raw data
#' @param tx the treatment assignment variable (unquoted)
#' @param outcome the DOOR outcome variable (unquoted)
#'
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
#' DOOR <- sample(1:4, size = N, replace = T)
#' data <- data.frame(seq, DOOR)
#' get_door_summary(D, tx = "seq", outcome = "DOOR")
get_door_summary_OS <- function(data, tx, outcome){

  outcome <- data[[outcome]]
  tx <- data[[tx]]
  table(outcome, tx)
}
get_door_summary(data, seq, DOOR)

