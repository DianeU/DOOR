#' Get DOOR summary
#'
#' @param data dataframe, data set that contains the raw data
#' @param tx name of the treatment assignment indicator variable
#' @param outcome the categorical DOOR variable
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
#' get_door_summary(data, seq, DOOR)
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr group_by summarise arrange enquo
#'
#' @export
get_door_summary <- function(data, tx, outcome){
  #.group_vars <- enquos(...)
  #data %>% group_by(!!!.group_vars) %>% summarise(count = n())

  .tx <- dplyr::enquo(tx)
  .outcome <- dplyr::enquo(outcome)

  data %>%
    group_by(!!.tx, !!.outcome) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = !!.tx, values_from = "count", values_fill = list(count = 0L)) %>%
    arrange(!!.outcome)
}

