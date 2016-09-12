context("model makes sense?")

# not sure if want lhs_parameters or generate_parameters

test_that("example", {
  expect_equal(1 + 1, 2, tolerance = 1e-6)
  expect_true(1 + 1 == 2)
  expect_error(sqrt(lm), "non-numeric")
  # expect_identical(3, sqrt(3)^2)
})

#result = run_model(parameters, main_model, time, output_vars = c("Ntot", "prev_client"))



# NO SEEDING OF EPIDEMIC
###################################################################################################################################
###################################################################################################################################

# no infected, no incidence?
test_that("no incidence", {
  for (Ncat in c(2, 5))
  {
    parameters <- generate_parameters(Ncat = Ncat, I11_init = rep(0, Ncat), I01_init = rep(0,Ncat), S1a_init = rep(100,Ncat), S1b_init = rep(100,Ncat), S1c_init = rep(100,Ncat))
    result = run_model(parameters, main_model, time)
    xx <- result[c(grep("I[0-9]", names(result)))]
    expect_true(all(unlist(xx) == 0))
    expect_equal(ncol(result$S0), Ncat)
  }
})

# no infected, then population sizes remain equal between groups?
test_that("risk group sizes equal", {
  parameters <- generate_parameters(I11_init = c(0,0), I01_init = c(0,0), S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))
  result = run_model(parameters, main_model, time)
  N_client <- result[c(grep("N_client", names(result)))]
  N_FSW <- result[c(grep("N_FSW", names(result)))]
  expect_equal(N_client[[1]], N_FSW[[1]], tolerance = 1e-6)
})

# no infected, then 0 cumulative infections
test_that("cumulative infections", {
  parameters <- generate_parameters(I11_init = c(0,0), I01_init = c(0,0), S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  expect_true(all(unlist(xx) == 0))
})


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


# ALL COMPARTMENTS ARE POSITIVE

test_that("all compartments positive", {
  parameters <- generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("S[0-9]|I[0-9]", names(result)))]
  expect_true(all(unlist(xx) >= 0))
})

# CUMULATIVE INFECTIONS ALWAYS POSITIVE
test_that("cumulative infections", {
  parameters <- generate_parameters(S1b_init = c(100,100), S1c_init = c(100,100))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  expect_true(all(diff(xx[[1]][,1]) >= 0))
  expect_true(all(diff(xx[[1]][,2]) >= 0))
})

# FORCE OF INFECTION SET TO ZERO
###################################################################################################################################
###################################################################################################################################

# BETA
# only group 1 infected, does group 2 get infected?
test_that("beta 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), beta = c(0, 0.01))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("beta 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), beta = c(0.01, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})


# R
# only group 1 infected, does group 2 get infected?
test_that("R 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), R = c(0, 0.01))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("R 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), R = c(0.01, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})


# n
# only group 1 infected, does group 2 get infected?
test_that("n 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), n = c(0, 0.01))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("n 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), n = c(0.01, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

# c is a bit more complicated, with the balancing sex acts

# # c
# # only group 1 infected, does group 2 get infected?
# test_that("c 1", {
#   parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), c = c(0, 1))
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
#   expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
# })
#
# # only group 2 infected, does group 1 get infected?
# test_that("c 2", {
#   parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), c = c(1, 0))
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
#   expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
# })

# fc and ec
# only group 1 infected, does group 2 get infected?
# condom efficacy is 1 and frequency of condom use is 1 - no infections
test_that("fc ec 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is NOT 1 and frequency of condom use is 1 - some infections
test_that("fc ec 1b", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0.9, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fc ec 1c", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})


# only group 2 infected, does group 1 get infected?
test_that("fc ec 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2b", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 0.9), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2c", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 0.99)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})



# fP and ec
# only group 1 infected, does group 2 get infected?
# condom efficacy is 1 and frequency of condom use is 1 - no infections
test_that("fP ec 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is NOT 1 and frequency of condom use is 1 - some infections
test_that("fP ec 1b", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0.9, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fP ec 1c", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})


# only group 2 infected, does group 1 get infected?
test_that("fP ec 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP ec 2b", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 0.9), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP ec 2c", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 0.99)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})


# ALL FORCES OF INFECTION POSITIVE?!



# DISEASE PROGRESSION
###################################################################################################################################
###################################################################################################################################

# Setting progression rate from acute to CD4>500 to zero
# done by setting tau[0-9]1 and gamma[0-9]1 to 0

test_that("acute to CD4>500 zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]1", parameter_names), grep("tau[0-9]1", parameter_names))]
  parameters <- generate_parameters(I11_init = c(100,100), set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]2|I[0-9]3|I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# Setting progression rate from CD4>500 to CD4 350-500 to zero
# done by setting gamma[0-9]2 to 0

test_that("CD4>500 to CD4 350-500 zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]2", parameter_names))]
  parameters <- generate_parameters(I11_init = c(100,100), set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]3|I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# Setting progression rate from CD4 350-500 to CD4 200-349 to zero
# done by setting gamma[0-9]3 to 0

test_that("CD4 350-500 to CD4 200-349 zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]3", parameter_names))]
  parameters <- generate_parameters(I11_init = c(100,100), set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})


# Setting progression rate from CD4 200-349 to CD4 <200 to zero
# done by setting gamma[0-9]4 to 0

test_that("CD4 200-349 to CD4 <200 to zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]4", parameter_names))]
  parameters <- generate_parameters(I11_init = c(100,100), set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# CARE CASCADE PROGRESSION
###################################################################################################################################
###################################################################################################################################


# PREP

test_that("no prep", {
  relevant_parameters = parameter_names[c(grep("zeta", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters, I11_init = c(0,0))
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

# NO TESTING

test_that("no testing", {
  relevant_parameters = parameter_names[c(grep("tau", parameter_names))]
  parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I2[0-9]|I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO ART

test_that("no ART", {
  relevant_parameters = parameter_names[c(grep("rho", parameter_names))]
  parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO DROP OUT

test_that("no drop out", {
  relevant_parameters = parameter_names[c(grep("phi", parameter_names))]
  parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})

# BALANCING
###################################################################################################################################
###################################################################################################################################

# DUNNO!
test_that("B check 0", {
  parameters <- generate_parameters(theta = 0)
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})
test_that("B check 0.5", {
  parameters <- generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})
test_that("B check 1", {
  parameters <- generate_parameters(theta = 1)
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})


# CALCULATING PREVALENCE
###################################################################################################################################
###################################################################################################################################
test_that("prevalence", {
  parameters = generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]", names(result)))]
  all = result[c(grep("I[0-9]", names(result)), grep("S[0-9]", names(result)))]

  expect_equal(result$prev_FSW, 100 * rowSums(do.call(cbind, lapply(all_infected, function(x) x <- x[,1]))) / rowSums(do.call(cbind, lapply(all, function(x) x <- x[,1]))), tolerance = 1e-6)
  expect_equal(result$prev_client, 100 * rowSums(do.call(cbind, lapply(all_infected, function(x) x <- x[,2]))) / rowSums(do.call(cbind, lapply(all, function(x) x <- x[,2]))), tolerance = 1e-6)

  # this will need to be tested against overall prevalence
  #over_prevalence = rowSums(do.call(cbind, all_infected)) / rowSums(do.call(cbind, all))

  # result$prev
})

#  OVERALL PREVALENCE IS EQUAL TO WEIGHTED AVERAGE OF ALL PREVALENCES

# CALCULATING INCIDENCE
###################################################################################################################################
###################################################################################################################################

# set all mortality to zero, set births to zero
# incidence can be calculated by:
# lambda * S

test_that("comparing incidence", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]4", parameter_names))]
  parameters <- generate_parameters(I11_init = c(100,100), set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# CALCULATING FORCE OF INFECTION
###################################################################################################################################
###################################################################################################################################



# LOGICAL CHECKS FOR MODEL
###################################################################################################################################
###################################################################################################################################

# increase beta, increase overall prevalence

test_that("beta vs prevalence", {
  parameters <- generate_parameters(beta = c(0.001, 0.001))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(beta = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase R, increase overall prevalence

test_that("R vs prevalence", {
  parameters <- generate_parameters(R = c(0.001, 0.001))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(R = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase n, increase overall prevalence

test_that("n vs prevalence", {
  parameters <- generate_parameters(n = c(1, 1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(n = c(15, 15))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase c, increase overall prevalence

test_that("c vs prevalence", {
  parameters <- generate_parameters(c = c(1, 1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(c = c(15, 15))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase fc, decrease overall prevalence

test_that("fc vs prevalence", {
  parameters <- generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(theta = 0.5, set_null = "fc_y")
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase ec, decrease overall prevalence

test_that("ec vs prevalence", {
  parameters <- generate_parameters(ec = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(ec = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})



# increase fP, decrease overall prevalence

test_that("fP vs prevalence", {
  parameters <- generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(theta = 0.5, set_null = "fP_y")
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase eP, decrease overall prevalence

test_that("eP vs prevalence", {
  parameters <- generate_parameters(eP = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(eP = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# # increase prep uptake, decrease overall prevalence
#
# test_that("zeta vs prevalence", {
#
#   parameters <- generate_parameters(theta = 0.5)
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("S[0-9]", names(result)))]
#   N1 <- rowSums(do.call(cbind, xx))
#
#   relevant_parameters = parameter_names[c(grep("zeta", parameter_names))]
#   parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("S[0-9]", names(result)))]
#   N2 <- rowSums(do.call(cbind, xx))
#
#
#
#
#
#   parameters <- generate_parameters(theta = 0.5)
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I[0-9][0-9]", names(result)))]
#   N1 <- rowSums(do.call(cbind, xx))
#
#   relevant_parameters = parameter_names[c(grep("zeta", parameter_names))]
#   parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I[0-9][0-9]", names(result)))]
#   N2 <- rowSums(do.call(cbind, xx))
#
#   expect_true(sum(N2) > sum(N1))
# })





# increase ART uptake, decrease overall prevalence

test_that("ART vs prevalence", {
  skip("WIP")
  parameters <- generate_parameters(theta = 0.5)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  relevant_parameters = parameter_names[c(grep("rho", parameter_names))]
  parameters <- generate_parameters(theta = 0.5, set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# testing

# prep adherence

# dropout
