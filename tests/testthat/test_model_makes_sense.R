context("model makes sense?")



test_that("example", {
  expect_equal(1 + 1, 2, tolerance = 1e-6)
  expect_true(1 + 1 == 2)
  expect_error(sqrt(lm), "non-numeric")
   # expect_identical(3, sqrt(3)^2)
})


test_that("growth rate", {

  ## relationship between growth rate (epsilon) and N
  ## if growth rate (epsilon) is 0, then N should not change. If growth rate is positive, then N should increase.

  # defining the parameter to vary
  time <- seq(1985, 2015, length.out = 101)

  v <- c("epsilon")
  parameters <- generate_parameters(epsilon=0.002)
  pp <- unlist(parameters[v])
  pp <- as.data.frame(rbind(pp * 0, pp))
  scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, main_model, time))

  # remove this shit
  # run it with epsilon = 0, then run if with epsilon is positive

  # running the model with the variations of the parameter
  result <- list()
  for(j in 1:nrow(pp))
  {
    x <- scenarios[[j]]
    xx <- x[grep("^[SI]", names(scenarios[[j]]))]
    result[[j]] <- rowSums(do.call(cbind, xx))
  }

  # test 1: are all increments in N equal to 0?
  expect_true(all(abs(diff(result[[1]])) < 10^-10))

  # test 2: are all increments in N positive AND are the increments getting bigger?
  expect_true(all(diff(result[[2]]) > 0) && all(diff(diff(result[[2]])) > 0))
})
