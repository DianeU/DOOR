#' Plot DOOR comparison
#'
#' @param res door summary
#'
#' @return barplot with the DOOR comparison
#'
#' @description This function plots the DOOR comparison
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
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr group_by summarise arrange enquo
#' @import ggplot2
#' @importFrom scales percent_format
#'
#' @export
plot_door_comparison <- function(res){
  tx <- names(res)[-1]
  DOOR <- names(res)[1]

  res %>% pivot_longer(!!enquo(tx)) %>%
    group_by(name) %>%
    mutate(perc = value/ sum(value)) %>%
    ungroup() %>%
    ggplot(aes(name, perc, fill = factor(DOOR))) +
    geom_bar(position="fill", stat="identity") +
    geom_text(aes(label=ifelse(perc >= 0.07, paste0(sprintf("%.0f", perc*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="white") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "top") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y="", x="", fill = "DOOR")

}
