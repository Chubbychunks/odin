context("model makes sense?")

# not sure if want lhs_parameters or generate_parameters

test_that("example", {
  expect_equal(1 + 1, 2, tolerance = 1e-6)
  expect_true(1 + 1 == 2)
  expect_error(sqrt(lm), "non-numeric")
  # expect_identical(3, sqrt(3)^2)
})

#result = run_model(parameters, main_model, time, output_vars = c("Ntot", "prev_client"))

#GENERAL TESTS

#Ncat works?
test_that("Ncat", {
  for (Ncat in c(2, 8))
  {
    parameters <- generate_parameters(Ncat = Ncat)
    result = run_model(parameters, main_model, time)
    expect_equal(ncol(result$S0), Ncat)
  }
})

# ALL COMPARTMENTS ARE POSITIVE

test_that("all compartments positive", {
  parameters <- generate_parameters()
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

# NO SEEDING OF EPIDEMIC
###################################################################################################################################
###################################################################################################################################

# no infected, no incidence?
test_that("no incidence", {
  for (Ncat in c(2, 7))
  {
    parameters <- generate_parameters(Ncat = Ncat, I11_init = rep(0, Ncat), I01_init = rep(0,Ncat), S1a_init = rep(100,Ncat), S1b_init = rep(100,Ncat), S1c_init = rep(100,Ncat))
    result = run_model(parameters, main_model, time)
    xx <- result[c(grep("I[0-9]", names(result)))]
    expect_true(all(unlist(xx) == 0))
    expect_equal(ncol(result$S0), Ncat)
  }
})

# no infected, then 0 cumulative infections
test_that("cumulative infections", {
  parameters <- generate_parameters(I11_init = c(0,0), I01_init = c(0,0), S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  expect_true(all(unlist(xx) == 0))
})

# no infected, then population sizes remain equal between groups?
test_that("risk group sizes equal", {
  parameters <- generate_parameters(I11_init = c(0,0), I01_init = c(0,0), S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))
  result = run_model(parameters, main_model, time)
  N_client <- result[c(grep("N_client", names(result)))]
  N_FSW <- result[c(grep("N_FSW", names(result)))]
  expect_equal(N_client[[1]], N_FSW[[1]], tolerance = 1e-6)
})




# GROWTH RATE AND DEMOGRAPHY
###################################################################################################################################
###################################################################################################################################


# add test that there are the correct amounts of omega, mu etc


test_that("omega adds to 1", {
  parameters <- lhs_parameters(1)
  expect_equal(sum(parameters[[1]]$omega), 1)
})

test_that("omega keeps consistent population?", {
  parameters <- generate_parameters(omega = c(0.01, 0.02, 0.5, 0.1, 0.12, 0.03, 0.22), Ncat = 7, beta = c(0,0,0,0,0,0,0),
                                    S0_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22),
                                    I01_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22))
  result = run_model(parameters, main_model, time)
  xx <- result[grep("frac_N", names(result))] # grepping all the Ss and Is


  expect_true(all(abs(diff(xx$frac_N))<10^-12))
  expect_equal(as.numeric(xx$frac_N[1,]), as.numeric(xx$frac_N[2,]))
})


test_that("omega keeps consistent population even with HIV?", {
  parameters <- generate_parameters(omega = c(0.01, 0.02, 0.5, 0.1, 0.12, 0.03, 0.22), Ncat = 7,
                                    S0_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22),
                                    I01_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22))
  result = run_model(parameters, main_model, time)
  xx <- result[grep("frac_N", names(result))] # grepping all the Ss and Is


  expect_true(all(abs(diff(xx$frac_N))<10^-12))
  expect_equal(as.numeric(xx$frac_N[1,]), as.numeric(xx$frac_N[2,]))
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






# LAMBDAS
# if prep is useless, then cumulative infections should be equal no matter what prep adherence is
test_that("useless prep", {
  parameters <- generate_parameters(I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa = c(1, 1), zetab = c(1, 1), zetac = c(1, 1),
                                    eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa = c(0, 0), zetab = c(0, 0), zetac = c(0, 0),
                                    eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(all(abs(N1 - N2) < 10^-2))
})


test_that("useful prep", {
  parameters <- generate_parameters(I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa = c(1, 1), zetab = c(1, 1), zetac = c(1, 1),
                                    eP0 = c(0, 0), eP1a = c(0.1, 0.1), eP1b = c(0.1, 0.1), eP1c = c(0.1, 0.1))
  result1 = run_model(parameters, main_model, time)
  parameters <- generate_parameters(I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa = c(0, 0), zetab = c(0, 0), zetac = c(0, 0),
                                    eP0 = c(0, 0), eP1a = c(0.1, 0.1), eP1b = c(0.1, 0.1), eP1c = c(0.1, 0.1))
  result2 = run_model(parameters, main_model, time)
  expect_true(result1$cumuInf[length(time)] < result2$cumuInf[length(time)])
})

# FORCE OF INFECTION SET TO ZERO
###################################################################################################################################
###################################################################################################################################


# BETA
# only group 1 infected, does group 2 get infected?
test_that("beta 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), beta = c(0.01, 0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("beta 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), beta = c(0, 0.01))
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
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), n = matrix(c(0, 1, 0, 0), nrow = 2, ncol = 2, byrow = T))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("n 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), n = matrix(c(0, 0, 1, 0), nrow = 2, ncol = 2, byrow = T))
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
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is NOT 1 and frequency of condom use is 1 - some infections
test_that("fc ec 1b", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 0.9), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fc ec 1c", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 1), fc_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 0.99)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})


# only group 2 infected, does group 1 get infected?
test_that("fc ec 2", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2b", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0.9, 0), fc_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2c", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), ec = c(1, 0), fc_y = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

# not sure if need... was trying to do fP and eP and had copied and pasted above for the condom.
#
# fP and eP
# only group 1 infected, does group 2 get infected?
# prep efficacy is 1 and frequency of prep use is 1 - no infections
test_that("fP eP 1", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})
#
# condom efficacy is NOT 1 and frequency of condom use is 1 - some infections
test_that("fP eP 1b", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 0.99), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 0.99), eP1b = c(0, 1), eP1c = c(0, 1), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 0.99), eP1c = c(0, 1), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 0.99), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fP eP 1c", {
  parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y = cbind(c(0, 0, 0, 0), c(1, 1, 0.99, 1)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})
#
#
# only group 2 infected, does group 1 get infected?
test_that("fP eP 2", {
  parameters <- generate_parameters(Ncat = 2, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), eP1d = c(1, 0), fP_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP eP 2b", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(0.99, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(0.99, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(0.99, 0), eP1c = c(1, 0), fP_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(0.99, 0), fP_y = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP eP 2c", {
  parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(0.99, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)), zetaa = c(0.1, 0.1))
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
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I2[0-9]|I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO ART

test_that("no ART", {
  relevant_parameters = parameter_names[c(grep("rho", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO DROP OUT

test_that("no drop out", {
  relevant_parameters = parameter_names[c(grep("phi", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
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
  parameters <- generate_parameters(theta = matrix(0, ncol = 2, nrow = 2))
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})
test_that("B check 0.5", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})
test_that("B check 1", {
  parameters <- generate_parameters(theta = matrix(1, ncol = 2, nrow = 2))
  result = run_model(parameters, main_model, time)
  expect_equal(as.numeric(result[["B_check"]]), rep(1, length(result$B_check)))
})

# test_that("cstar", {
#   # this is to check diagonal of matrix is 0
#   parameters <- generate_parameters()
#   result = run_model(parameters, main_model, time)
#   for(i in 1:length(result$cstar[,1,1]))
#   {
#     for(j in 1:length(result$cstar[1,1,]))
#     {
#       expect_equal(result$cstar[i,,][j,j], 0)
#     }
#   }
# 
#   # this is to check that sex acts balance with 2 groups
#   parameters <- generate_parameters(theta = matrix(c(0.5, 0.9, 0.1, 0.5), ncol = 2, nrow = 2, byrow = T),
#                                     omega = c(0.2, 0.8),
#                                     S0_init = c(100*0.2, 100*0.8),
#                                     I01_init = c(100*0.2, 100*0.8))
#   result = run_model(parameters, main_model, time)
#   for(i in 1:length(result$cstar[,1,1]))
#   {
#     expect_equal(result$cstar[i,,][1,2] * result$N[i,1], result$cstar[i,,][2,1] * result$N[i,2])
#   }
# 
#   # this is to check that sex acts balance with 3 groups
#   parameters <- generate_parameters(theta = matrix(c(0.5, 0.9, 0.6, 0.1, 0.5, 0.2, 0.4, 0.8, 0.5), ncol = 3, nrow = 3, byrow = T),
#                                     omega = c(0.1, 0.4, 0.5),
#                                     S0_init = c(100*0.1, 100*0.4, 100*0.5),
#                                     I01_init = c(100*0.1, 100*0.4, 100*0.5),
#                                     Ncat = 3)
#   result = run_model(parameters, main_model, time)
#   for(i in 1:length(result$cstar[,1,1]))
#   {
#     expect_equal(result$cstar[i,,][1,2] * result$N[i,1], result$cstar[i,,][2,1] * result$N[i,2])
#     expect_equal(result$cstar[i,,][1,3] * result$N[i,1], result$cstar[i,,][3,1] * result$N[i,3])
#     expect_equal(result$cstar[i,,][2,3] * result$N[i,2], result$cstar[i,,][3,2] * result$N[i,3])
#   }
# 
# 
# }
# )


# CALCULATING PREVALENCE
###################################################################################################################################
###################################################################################################################################
test_that("prevalence", {
  parameters = generate_parameters()
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]", names(result)))]
  all = result[c(grep("I[0-9]", names(result)), grep("^S[0-9]", names(result)))]

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
  parameters <- generate_parameters(n = matrix(1, ncol = 2, nrow = 2))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(n = matrix(10, ncol = 2, nrow = 2))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase c, increase overall prevalence

test_that("c_comm vs prevalence", {
  parameters <- generate_parameters(c_comm = rep_len(2, 2))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(c_comm = rep_len(23, 2))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# test_that("c_noncomm vs prevalence", {
#   parameters <- generate_parameters(c_noncomm = rep_len(2, 2))
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I[0-9][0-9]", names(result)))]
#   N1 <- rowSums(do.call(cbind, xx))
#   
#   parameters <- generate_parameters(c_noncomm = rep_len(23, 2))
#   result = run_model(parameters, main_model, time)
#   xx <- result[c(grep("I[0-9][0-9]", names(result)))]
#   N2 <- rowSums(do.call(cbind, xx))
#   
#   expect_true(sum(N2) > sum(N1))
# })

# increase fc, decrease overall prevalence

test_that("fc vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(set_null = "fc_y")
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
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(set_null = "fP_y")
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase eP, decrease overall prevalence

test_that("eP vs prevalence", {
  parameters <- generate_parameters(eP0 = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(eP0 = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))

  parameters <- generate_parameters(eP1a = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(eP1a = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))

  parameters <- generate_parameters(eP1b = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(eP1b = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))

  parameters <- generate_parameters(eP1c = c(0.9, 0.9))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(eP1c = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase prep uptake, decrease overall prevalence

test_that("zeta vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  relevant_parameters = parameter_names[c(grep("zeta", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))

  parameters <- generate_parameters(zetaa = c(0.1, 0.1))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(zetaa = c(0.09, 0.09))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))


  # in this test, i made gammas and taus 0 to make sure prep doesn't advtange by going to ART quicker
  parameters <- generate_parameters(zetaa = c(0.1, 0.1), eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0), gamma01 = c(0, 0), gamma11 = c(0, 0), tau01 = c(0, 0), tau11 = c(0, 0))
  result1 = run_model(parameters, main_model, time)
  parameters <- generate_parameters(zetaa = c(0.09, 0.09), eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0), gamma01 = c(0, 0), gamma11 = c(0, 0), tau01 = c(0, 0), tau11 = c(0, 0))
  result2 = run_model(parameters, main_model, time)
  expect_equal(result1$cumuInf[length(time)], result2$cumuInf[length(time)])

})





# increase ART uptake, decrease overall prevalence

test_that("ART vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  # this test won't work until R is a matrix with r,s (I think)

  relevant_parameters = parameter_names[c(grep("rho", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase testing, decrease overall prevalence

test_that("testing vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  relevant_parameters = parameter_names[c(grep("tau", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})




# increase prep adherence, decrease overall prevalence


test_that("prep adherence vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  relevant_parameters = parameter_names[c(grep("zeta", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))

  # on the basis that higher adherence is better
  parameters <- generate_parameters(zetaa = c(0.1,0.1), zetab = c(0,0), zetac = c(0,0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- generate_parameters(zetaa = c(0,0), zetab = c(0.1,0.1), zetac = c(0,0))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})

# increase dropout, increase overall prevalence

test_that("dropout vs prevalence", {
  parameters <- generate_parameters()
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  relevant_parameters = parameter_names[c(grep("psi", parameter_names))]
  parameters <- generate_parameters(set_null = relevant_parameters)
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N1) > sum(N2))
})

