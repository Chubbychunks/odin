context("model makes sense?")

# not sure if want lhs_parameters or generate_parameters

test_that("example", {
  expect_equal(1 + 1, 2, tolerance = 1e-6)
  expect_true(1 + 1 == 2)
  expect_error(sqrt(lm), "non-numeric")
   # expect_identical(3, sqrt(3)^2)
})

#result = run_model(parameters, main_model, time, output_vars = c("Ntot", "prev_client"))



# GROWTH RATE AND DEMOGRAPHY
###################################################################################################################################
###################################################################################################################################

test_that("omega adds to 1", {
  parameters <- lhs_parameters(1)
  expect_true(sum(parameters[[1]]$omega) == 1)
})


test_that("growth rate zero", {
  parameters <- generate_parameters(epsilon = 0)
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))

  # are all increments in N equal to 0?
  expect_true(all(abs(diff(N)) < 10^-10))
})

test_that("growth rate increases", {
  parameters <- generate_parameters(epsilon = 0.1)
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))

  # test 2: are all increments in N positive AND are the increments getting bigger?
  expect_true(all(diff(N) > 0) && all(diff(diff(N)) > 0))
})

test_that("growth rate decreases", {
  parameters <- generate_parameters(epsilon = -0.1)
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))

  # test 2: are all increments in N positive AND are the increments getting bigger?
  expect_true(all(diff(N) < 0) && all(diff(diff(N)) > 0))
})


# test_that("growth rate", {
#   parameters <- generate_parameters(epsilon = 0.01, omega = 0.15)
#   result = run_model(parameters, main_model, time)
#   xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
#   N <- rowSums(do.call(cbind, xx))
#
#   # test 2: are all increments in N positive AND are the increments getting bigger?
#   expect_true(all(diff(N) < 0) && all(diff(diff(N)) > 0))
# })
# ^ why doesn't N * epsilon equal the differences in N at each timestep?
# Because the timesteps are infintessimally small?!?!?!?

# PREP
###################################################################################################################################
###################################################################################################################################


test_that("no prep", {
  parameters <- generate_parameters(zetaa = c(0,0), zetab = c(0,0), zetac = c(0,0), I11_init = c(0,0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I11", names(result)), grep("S1", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})

test_that("prep increases", {
  parameters <- generate_parameters(zetaa = c(0.1,0.1), zetab = c(0.1,0.1), zetac = c(0.1,0.1), I11_init = c(0,0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I11", names(result)), grep("S1", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(!all(diff(N) == 0))
})



# FORCE OF INFECTION SET TO ZERO
###################################################################################################################################
###################################################################################################################################

#
# # only group 1 infected, does group 2 get infected?
# parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0))
# pp <- unlist(parameters[v])
# pp <- as.data.frame(rbind(c(0, pp[2]), pp))
# scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))
#
# # running the model with the variations of the parameter
# result <- list()
# for(j in 1:nrow(pp))
# {
#   x <- scenarios[[j]]
#   xx <- x[c(grep("I01", names(scenarios[[j]])), grep("I11", names(scenarios[[j]])))]
#   result[[j]] <- sum(c(xx$I01[,2], xx$I11[,2]))
# }
#
# # test 1: is there anyone on PrEP?
# if(!all(result[[1]]==0))
# {
#   print("There are still people being infected!")
# }
#
# # beta and 2->1 infection
#
#
# # only group 2 infected, does group 1 get infected?
# parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000))
# pp <- unlist(parameters[v])
# pp <- as.data.frame(rbind(c(pp[1], 0), pp))
# scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))
#
# # running the model with the variations of the parameter
# result <- list()
# for(j in 1:nrow(pp))
# {
#   x <- scenarios[[j]]
#   xx <- x[c(grep("I01", names(scenarios[[j]])), grep("I11", names(scenarios[[j]])))]
#   result[[j]] <- sum(c(xx$I01[,1], xx$I11[,1]))
# }
#
# # test 1: is there anyone on PrEP?
# if(!all(result[[1]]==0))
# {
#   print("There are still people being infected!")
# }

