#' Get DOOR summary
#'
#' @param data dataframe, data set that contains the raw data
#' @param tx character, name of the treatment assignment indicator variable
#' @param outcome character, name of the categorical DOOR variable
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
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr group_by summarise arrange enquo n
#'
#' @export
get_door_summary <- function(data, tx, outcome){
  #.group_vars <- enquos(...)
  #data %>% group_by(!!!.group_vars) %>% summarise(count = n())

  group_vars <- c(tx, outcome)

  data %>%
    group_by(.dots = group_vars) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = tx, values_from = "count", values_fill = list(count = 0L)) %>%
    arrange(!!sym(outcome))
}
