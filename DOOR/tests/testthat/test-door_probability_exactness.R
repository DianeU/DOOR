test_that("Minimal example gives correct probability", {
  seq <- rep(c("A", "B"), each = 4)
  DOOR <- c(4,4,2,3,1,2,3,4)
  data <- data.frame(DOOR, seq)
  res <- get_door_summary(data, "seq", "DOOR")
  expect_equal(get_door_probability(res), 0.6875)
})
testthat::test_that("CI contains the DOOR probability", {
  set.seed(123)
  N <- 26
  seq <- rep(c("A", "B"), each = N/2)
  DOOR <- sample(1:4, size = N, replace = T)
  data <- data.frame(seq, DOOR)
  res <- get_door_summary(data, "seq", "DOOR")
  p <- get_door_probability(res)
  ci <- get_door_prob_CI(res, tx = c("A", "B"))
  testthat::expect_true(ci[1] <= p)
  testthat::expect_true(ci[2] >= p)
})
