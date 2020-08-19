library(tidyverse)
library(ggsci)

D <- read_rds("../simulations/03_results/2020-08-14_confidence-intervals.Rds")
D <- as_tibble(lapply(D, unlist))

D <- D %>%
  mutate(bias = trueP - xi,
         width = UL - LL,
         inside = trueP >= LL & trueP <= UL
  )

results <- D %>%
  group_by(K, N, method, trueP) %>%
  summarize(
    bias_exp = mean(bias),
    bias_sd = sd(bias),
    xi_est = mean(xi),
    xi_sd = sd(xi),
    width_exp = mean(width),
    width_sd = sd(width),
    cov_prob = sum(inside)/n()*100,
    time_exp = mean(time),
    time_sd = sd(time)
  )


# Estimation of the bias does not depend on the CI
D %>%
  filter(method == "bootstrap") %>%
  ggplot(aes(as.factor(trueP), bias, fill = as.factor(N))) + geom_boxplot() + theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "True DOOR probability", fill = "Sample size", y = "Bias (true - estimated)")
ggsave(paste0("../simulations/03_results/", Sys.Date(), "_bias.png"))

results %>%
  #filter(N == 100) %>%
  ggplot(aes(trueP, cov_prob, color = as.factor(method))) +
  geom_hline(yintercept = 95, color = "darkgrey", linetype = "dashed") +
  geom_point(size = 1.3) + geom_line(size = 1.05) + theme_minimal() +
  scale_color_jco() +
  theme(legend.position = "top") + facet_wrap(~N) +
  labs(x = "True DOOR probability", color = "Method", y = "Coverage Probability") +
  scale_x_continuous(breaks = c(0.5, 0.66, 0.74), limits=c(0.5, 0.8))
ggsave(paste0("../simulations/03_results/", Sys.Date(), "_coverage-probability.png"))


D %>%
  filter(trueP == 0.5) %>%
  ggplot(aes(as.factor(N), width, fill = method)) + geom_boxplot() + theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_jco() +
  labs(x = "Sample size", fill = "Method", y = "Interval Width")
ggsave(paste0("../simulations/03_results/", Sys.Date(), "_interval-width-by-sample-size_P05.png"))


D %>%
  filter(N == 200) %>%
  ggplot(aes(as.factor(trueP), width, fill = method)) + geom_boxplot() + theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_jco() +
  labs(x = "True DOOR Probability", fill = "Method", y = "Interval Width")
ggsave(paste0("../simulations/03_results/", Sys.Date(), "_interval-width-by-true-prob_N200.png"))



timings <- results %>%
  ungroup() %>%
  mutate(time_exp = round(time_exp, 4)) %>%
  select(N, trueP, method, time_exp) %>%
  pivot_wider(names_from = "method", values_from = "time_exp")
write.csv(timings, file = paste0("../simulations/03_results/", Sys.Date(), "_simulation-timing.csv"))

xtable::xtable(timings, digits = 4)
