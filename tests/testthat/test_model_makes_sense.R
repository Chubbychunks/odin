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
  for (Ncat in c(2, 10))
  {
    parameters <- lhs_parameters(1, Ncat = Ncat)[[1]]
    result = run_model(parameters, main_model, time)
    expect_equal(ncol(result$S0), Ncat)
  }
})

# ALL COMPARTMENTS ARE POSITIVE

test_that("all compartments positive", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("S[0-9]|I[0-9]", names(result)))]
  expect_true(all(unlist(xx) >= 0))
})

# CUMULATIVE INFECTIONS ALWAYS POSITIVE

test_that("cumulative infections", {
  for(Ncat in c(2,10)){
    parameters <- lhs_parameters(1, Ncat = Ncat, S1b_init = rep_len(100, Ncat), S1c_init = rep_len(100, Ncat))[[1]]
    result = run_model(parameters, main_model, time)
    xx <- result[c(grep("cumuInf", names(result)))]
    expect_true(all(diff(xx[[1]][,1]) >= 0))
    expect_true(all(diff(xx[[1]][,2]) >= 0))
  }
})

# NO SEEDING OF EPIDEMIC
###################################################################################################################################
###################################################################################################################################

# no infected, no incidence?
test_that("no incidence", {
  for (Ncat in c(2, 10))
  {
    parameters <- lhs_parameters(1, Ncat = Ncat, prev_init_FSW = 0, prev_init_rest = 0, S1a_init = rep(100,Ncat), S1b_init = rep(100,Ncat), S1c_init = rep(100,Ncat))[[1]]
    result = run_model(parameters, main_model, time)
    xx <- result[c(grep("I[0-9]", names(result)))]
    expect_true(all(unlist(xx) == 0))
    expect_equal(ncol(result$S0), Ncat)
  }
})

# no infected, then 0 cumulative infections
test_that("cumulative infections", {
  parameters <- lhs_parameters(1, prev_init_FSW = 0, prev_init_rest = 0, S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  expect_true(all(unlist(xx) == 0))
})

# # no infected, then population sizes remain equal between groups?
# test_that("risk group sizes equal", {
#   parameters <- lhs_parameters(1, I11_init = c(0,0), I01_init = c(0,0), S1a_init = c(100,100), S1b_init = c(100,100), S1c_init = c(100,100))[[1]]
#   result = run_model(parameters, main_model, time)
#   expect_equal(result$N[,1], result$N[,2], tolerance = 1e-6)
# })




# GROWTH RATE AND DEMOGRAPHY
###################################################################################################################################
###################################################################################################################################


# add test that there are the correct amounts of omega, mu etc


test_that("omega adds to 1", {
  parameters <- lhs_parameters(1)
  expect_equal(sum(parameters[[1]]$omega), 1)
})

test_that("omega keeps consistent population?", {
  parameters <- lhs_parameters(1, replaceDeaths = 1, Ncat = 9, movement = 0, forced_pars = list(omega = c(0.01, 0.02, 0.3, 0.1, 0.12, 0.25, 0.1, 0.1, 0), beta = c(0,0,0,0,0,0,0,0,0),
                               S0_init = c(100*0.01, 100*0.02, 100*0.3, 100*0.1, 100*0.12, 100*0.25, 100*0.1, 100*0.1, 100*0),
                               I01_init = c(100*0.01, 100*0.02, 100*0.3, 100*0.1, 100*0.12, 100*0.25, 100*0.1, 100*0.1, 100*0)))[[1]]
  result = run_model(parameters, main_model, time)
  

  xx <- result[grep("frac_N", names(result))] # grepping all the Ss and Is
  
  
  expect_true(all(abs(diff(xx$frac_N))<10^-12))
  expect_equal(as.numeric(xx$frac_N[1,]), as.numeric(xx$frac_N[2,]))
})


test_that("omega keeps consistent population even with HIV?", {
  parameters <- lhs_parameters(1, Ncat = 9, movement = 0, replaceDeaths = 1, forced_pars = list(omega = c(0.01, 0.02, 0.5, 0.1, 0.12, 0.03, 0.22, 0, 0),
                               S0_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22, 0, 0),
                               I01_init = c(100*0.01, 100*0.02, 100*0.5, 100*0.1, 100*0.12, 100*0.03, 100*0.22,0 , 0)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("frac_N", names(result))] # grepping all the Ss and Is
  
  
  expect_true(all(abs(diff(xx$frac_N))<10^-4))
  expect_equal(as.numeric(xx$frac_N[1,]), as.numeric(xx$frac_N[2,]))
})

test_that("growth rate zero", {
  parameters <- lhs_parameters(1, replaceDeaths = 1, movement = 0, epsilon_y = c(0,0,0,0,0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))
  
  # are all increments in N equal to 0?
  expect_true(all(abs(diff(N)) < 10^-2))
})

test_that("growth rate increases", {
  parameters <- lhs_parameters(1, epsilon_y = c(0.1,0.1,0.1,0.1,0.1))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))
  
  # test 2: are all increments in N positive AND are the increments getting bigger?
  expect_true(all(diff(N) > 0) && all(diff(diff(N)) > 0))
})

test_that("growth rate decreases", {
  parameters <- lhs_parameters(1, epsilon_y = c(-0.1,-0.1,-0.1,-0.1,-0.1))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("^[SI]", names(result))] # grepping all the Ss and Is
  N <- rowSums(do.call(cbind, xx))
  
  # test 2: are all increments in N positive AND are the increments getting bigger?
  expect_true(all(diff(N) < 0) && all(diff(diff(N)) > 0))
})


# test_that("growth rate", {
#   parameters <- lhs_parameters(1, epsilon = 0.01, omega = 0.15)
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
  parameters <- lhs_parameters(1, I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa_y = matrix(rep(0, 8), ncol=2), zetab_y = matrix(rep(0, 8), ncol=2), zetac_y = matrix(rep(0, 8), ncol=2),
                               eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0), eP1d = c(0, 0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("cumuInf", names(result))] # grepping all the Ss
  N1 <- rowSums(do.call(cbind, xx))

  
  parameters2 <- modifyList(parameters, list(zetaa_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2), zetab_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2), zetac_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2)))
  
  result2 = run_model(parameters2, main_model, time)
  xx2 <- result2[grep("cumuInf", names(result2))] # grepping all the Ss
  N2 <- rowSums(do.call(cbind, xx2))
  

  # NOTE FOR THIS TEST THAT IT ONLY WORKS IF PREP UPTAKE DOESNT HAPPEN EARLY!!!!! IT AFFECTS HOW THE POPULATION GROWS...
  
#   which(unlist(parameters) - unlist(parameters2) != 0)
  
  expect_true(all(abs(N1 - N2) < 10^-2))
})


test_that("useful prep", {
  parameters <- lhs_parameters(1, I11_init = c(1000,1000), I01_init = c(1000,1000), zetaa_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2), zetab_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2), zetac_y = matrix(c(0, 0, 0, 0, 0.1, 0.1, 0, 0), byrow = T, ncol=2),
                               eP0 = c(0, 0), eP1a = c(0.1, 0.1), eP1b = c(0.1, 0.1), eP1c = c(0.1, 0.1), eP1d = c(0.1, 0.1))[[1]]
  result1 = run_model(parameters, main_model, time)
  parameters <- modifyList(parameters, list(zetaa_y = matrix(c(0, 0, 0, 0, 0, 0, 0, 0), byrow = T, ncol=2), zetab_y = matrix(c(0, 0, 0, 0, 0, 0, 0, 0), byrow = T, ncol=2), zetac_y = matrix(c(0, 0, 0, 0, 0, 0, 0, 0), byrow = T, ncol=2)))
  result2 = run_model(parameters, main_model, time)
  expect_true(result1$cumuInf[length(time)] < result2$cumuInf[length(time)])
})

# FORCE OF INFECTION SET TO ZERO
###################################################################################################################################
###################################################################################################################################


# BETA
# only group 1 infected, does group 2 get infected?
test_that("beta 1", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), beta = c(0.01, 0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("beta 2", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), beta = c(0, 0.01))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})


# R
# if R is 0, no infections
test_that("R 1", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), R = 0, infect_ART = 0, infect_acute = 0, infect_AIDS = 0)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})




# n
# only group 1 infected, does group 2 get infected?
test_that("n 1", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), n_comm = matrix(c(0, 1, 0, 0), nrow = 2, ncol = 2, byrow = T), n_noncomm = matrix(c(0, 1, 0, 0), nrow = 2, ncol = 2, byrow = T))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# only group 2 infected, does group 1 get infected?
test_that("n 2", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), n_comm = matrix(c(0, 0, 1, 0), nrow = 2, ncol = 2, byrow = T), n_noncomm = matrix(c(0, 0, 1, 0), nrow = 2, ncol = 2, byrow = T))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})



# fc and ec
# only group 1 infected, does group 2 get infected?
# condom efficacy is 1 and frequency of condom use is 1 - no infections
test_that("fc ec 1", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 1), fc_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fc_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is NOT 1 and frequency of condom use is 1 - some infections
test_that("fc ec 1b", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 0.9), fc_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fc_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fc ec 1c", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), ec = c(0, 1), fc_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 0.99)), fc_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 0.99)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})


# only group 2 infected, does group 1 get infected?
test_that("fc ec 2", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), ec = c(1, 0), fc_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fc_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2b", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), ec = c(0.9, 0), fc_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fc_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fc ec 2c", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), ec = c(1, 0), fc_y_comm = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)), fc_y_noncomm = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)))[[1]]
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
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})



#
# PREP efficacy is NOT 1 and frequency of PREP use is 1 - some infections
test_that("fP eP 1b", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 0.99), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 0.99), eP1b = c(0, 1), eP1c = c(0, 1), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 0.99), eP1c = c(0, 1), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 0.99), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 1, 1)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})

# condom efficacy is 1 and frequency of condom use is NOT 1 - some infections
test_that("fP eP 1c", {
  parameters <- lhs_parameters(1, I11_init = c(1000,0), I01_init = c(1000,0), eP0 = c(0, 1), eP1a = c(0, 1), eP1b = c(0, 1), eP1c = c(0, 1), fP_y_comm = cbind(c(0, 0, 0, 0), c(1, 1, 0.99, 1)), fP_y_noncomm = cbind(c(0, 0, 0, 0), c(1, 1, 0.99, 1)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,2], xx$I11[,2]))==0)
})
#
#
# only group 2 infected, does group 1 get infected?
test_that("fP eP 2", {
  parameters <- lhs_parameters(1, Ncat = 2, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), eP1d = c(1, 0), fP_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_true(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP eP 2b", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(0.99, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(0.99, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(0.99, 0), eP1c = c(1, 0), fP_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(1, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(0.99, 0), fP_y_comm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 1), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I01", names(result)), grep("I11", names(result)))]
  expect_false(sum(c(xx$I01[,1], xx$I11[,1]))==0)
})

test_that("fP eP 2c", {
  parameters <- lhs_parameters(1, I11_init = c(0,1000), I01_init = c(0,1000), eP0 = c(0.99, 0), eP1a = c(1, 0), eP1b = c(1, 0), eP1c = c(1, 0), fP_y_comm = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)), fP_y_noncomm = cbind(c(1, 1, 1, 0.99), c(0, 0, 0, 0)), zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2))[[1]]
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
  relevant_parameters = parameter_names[c(grep("gamma[0-9]1", parameter_names), grep("testing_prob_y", parameter_names))]
  parameters <- lhs_parameters(1, I11_init = c(100,100), set_null = relevant_parameters)[[1]]
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]2|I[0-9]3|I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# Setting progression rate from CD4>500 to CD4 350-500 to zero
# done by setting gamma[0-9]2 to 0

test_that("CD4>500 to CD4 350-500 zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]2", parameter_names))]
  parameters <- lhs_parameters(1, I11_init = c(100,100), set_null = relevant_parameters)[[1]]
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]3|I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})

# Setting progression rate from CD4 350-500 to CD4 200-349 to zero
# done by setting gamma[0-9]3 to 0

test_that("CD4 350-500 to CD4 200-349 zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]3", parameter_names))]
  parameters <- lhs_parameters(1, I11_init = c(100,100), set_null = relevant_parameters)[[1]]
  result = run_model(parameters, main_model, time)
  all_infected = result[c(grep("I[0-9]4|I[0-9]5", names(result)))]
  expect_true(all(unlist(all_infected) == 0))
})


# Setting progression rate from CD4 200-349 to CD4 <200 to zero
# done by setting gamma[0-9]4 to 0

test_that("CD4 200-349 to CD4 <200 to zero", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]4", parameter_names))]
  parameters <- lhs_parameters(1, I11_init = c(100,100), set_null = relevant_parameters)[[1]]
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
  parameters <- lhs_parameters(1, set_null = relevant_parameters, I11_init = c(0,0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I11", names(result)), grep("S1", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})

test_that("prep increases", {
  parameters <- lhs_parameters(1, zetaa_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2), zetab_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2), zetac_y = matrix(c(0, 0, 0, 0, 1, 1, 0, 0), byrow = T, ncol=2), I11_init = c(0,0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I11", names(result)), grep("S1", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(!all(diff(N) == 0))
})

# NO TESTING

test_that("no testing", {
  relevant_parameters = parameter_names[c(grep("testing_prob_y", parameter_names))]
  parameters <- lhs_parameters(1, set_null = relevant_parameters)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I2[0-9]|I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO ART

test_that("no ART", {
  relevant_parameters = parameter_names[c(grep("ART_prob_y", parameter_names))]
  parameters <- lhs_parameters(1, set_null = relevant_parameters)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I3[0-9]|I4[0-9]", names(result)))]
  N <- rowSums(do.call(cbind, xx))
  expect_true(all(diff(N) == 0))
})


# NO DROP OUT

test_that("no drop out", {
  relevant_parameters = parameter_names[c(grep("phi", parameter_names))]
  parameters <- lhs_parameters(1, set_null = relevant_parameters)[[1]]
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
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  expect_equal(result$B_check_comm, result$B_check_noncomm)
  expect_equal(result$B_check_comm, rep(1, length(time)))
})

# c_comm_balanced? contained in the above test tbh



# CALCULATING PREVALENCE
###################################################################################################################################
###################################################################################################################################
test_that("prevalence", {
  for (Ncat in 2:2){
    parameters = lhs_parameters(1, Ncat = Ncat)[[1]]
    result = run_model(parameters, main_model, time)
    all_infected = result[c(grep("I[0-9]", names(result)))]
    all = result[c(grep("I[0-9]", names(result)), grep("^S[0-9]", names(result)))]
    
    expect_equal(result$prev[,1], 100 * rowSums(do.call(cbind, lapply(all_infected, function(x) x <- x[,1]))) / rowSums(do.call(cbind, lapply(all, function(x) x <- x[,1]))), tolerance = 1e-6)
    expect_equal(result$prev[,2], 100 * rowSums(do.call(cbind, lapply(all_infected, function(x) x <- x[,2]))) / rowSums(do.call(cbind, lapply(all, function(x) x <- x[,2]))), tolerance = 1e-6)
    
    # this will need to be tested against overall prevalence
    #over_prevalence = rowSums(do.call(cbind, all_infected)) / rowSums(do.call(cbind, all))
    
    # result$prev}
  }
})

#  OVERALL PREVALENCE IS EQUAL TO WEIGHTED AVERAGE OF ALL PREVALENCES

# CALCULATING INCIDENCE
###################################################################################################################################
###################################################################################################################################

# set all mortality to zero, set births to zero
# incidence can be calculated by:
# lambda * S

#dont understand this test

test_that("comparing incidence", {
  relevant_parameters = parameter_names[c(grep("gamma[0-9]4", parameter_names))]
  parameters <- lhs_parameters(1, I11_init = c(100,100), set_null = relevant_parameters)[[1]]
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
  parameters <- lhs_parameters(1, beta = c(0.001, 0.001))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(beta = c(0.002, 0.002)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

# increase R, increase overall prevalence

test_that("R vs prevalence", {
  parameters <- lhs_parameters(1, R = 0.001)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(R = 0.1))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

# increase n, increase overall prevalence

test_that("n vs prevalence", {
  parameters <- lhs_parameters(1, n_comm = matrix(1, ncol = 2, nrow = 2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(n_comm = matrix(10, ncol = 2, nrow = 2)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

test_that("n vs prevalence", {
  parameters <- lhs_parameters(1, n_noncomm = matrix(1, ncol = 2, nrow = 2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(n_noncomm = matrix(10, ncol = 2, nrow = 2)))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

# increase c, increase overall prevalence

test_that("c_comm vs prevalence", {
  parameters <- lhs_parameters(1, c_comm = rep_len(2, 2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(c_comm = rep_len(23, 2)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

test_that("c_noncomm vs prevalence", {
  parameters <- lhs_parameters(1, c_noncomm = rep_len(2, 2))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(c_noncomm = rep_len(23, 2)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})


# increase fc, decrease overall prevalence

test_that("fc vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "fc_y_comm")[[1]]$fc_y_comm
  parameters <- modifyList(parameters, list(fc_y_comm = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

test_that("fc vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "fc_y_noncomm")[[1]]$fc_y_noncomm
  parameters <- modifyList(parameters, list(fc_y_noncomm = newpars))
  
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

# increase ec, decrease overall prevalence

test_that("ec vs prevalence", {
  parameters <- lhs_parameters(1, ec = c(0.9, 0.9))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(ec = c(0.1, 0.1)))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})



# increase fP, decrease overall prevalence

test_that("fP vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "fP_y_comm")[[1]]$fP_y_comm
  parameters <- modifyList(parameters, list(fP_y_comm = newpars))
  
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

test_that("fP vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "fP_y_noncomm")[[1]]$fP_y_noncomm
  parameters <- modifyList(parameters, list(fP_y_noncomm = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})

# increase eP, decrease overall prevalence

test_that("eP vs prevalence", {
  parameters <- lhs_parameters(1, eP0 = c(0.9, 0.9))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))

  parameters <- modifyList(parameters, list(eP0 = c(0.1, 0.1)))
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  
  parameters <- lhs_parameters(1, eP1a = c(0.9, 0.9))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(eP1a = c(0.1, 0.1)))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  parameters <- lhs_parameters(1, eP1b = c(0.9, 0.9))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(eP1b = c(0.1, 0.1)))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  parameters <- lhs_parameters(1, eP1c = c(0.9, 0.9))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  parameters <- modifyList(parameters, list(eP1c = c(0.1, 0.1)))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("I[0-9][0-9]", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})


# increase prep uptake, decrease overall prevalence

test_that("zeta vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  
  newpars = lhs_parameters(1, set_null = list("zetaa_y"))[[1]]$zetaa_y
  parameters <- modifyList(parameters, list(zetaa_y = newpars, zetab_y = newpars, zetac_y = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(zetaa_y = parameters$zetaa_y * 0.99))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(zetab_y = parameters$zetab_y * 0.99))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
  
  # c is ineffective prep!
  parameters <- lhs_parameters(1, kappaa = c(0,0), kappab = c(0,0), kappac = c(0,0))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  parameters <- modifyList(parameters, list(zetac_y = parameters$zetac_y * 0.5))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) - sum(N1) < 10^-2)
  
  
  
#   # in this test, i made gammas and taus 0 to make sure prep doesn't advtange by going to ART quicker
#   parameters <- lhs_parameters(1, zetaa = c(0.1, 0.1), eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0), gamma01 = c(0, 0), gamma11 = c(0, 0), tau01 = c(0, 0), tau11 = c(0, 0))[[1]]
#   result1 = run_model(parameters, main_model, time)
#   parameters <- lhs_parameters(1, zetaa = c(0.09, 0.09), eP0 = c(0, 0), eP1a = c(0, 0), eP1b = c(0, 0), eP1c = c(0, 0), gamma01 = c(0, 0), gamma11 = c(0, 0), tau01 = c(0, 0), tau11 = c(0, 0))[[1]]
#   result2 = run_model(parameters, main_model, time)
#   expect_equal(result1$cumuInf[length(time)], result2$cumuInf[length(time)])
  
})





# increase ART uptake, decrease overall prevalence

test_that("ART vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "ART_prob_y")[[1]]$ART_prob_y
  parameters <- modifyList(parameters, list(ART_prob_y = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))

  expect_true(sum(N2) > sum(N1))
})


# increase testing, decrease overall cumuinf

test_that("testing vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  newpars = lhs_parameters(1, set_null = "testing_prob_y")[[1]]$testing_prob_y
  parameters <- modifyList(parameters, list(testing_prob_y = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N2) > sum(N1))
})






# increase prep adherence movement, increase overall infections


test_that("adherence movements vs infections", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  
  newpars = lhs_parameters(1, set_null = "psia")[[1]]$psia
  parameters <- modifyList(parameters, list(psia = newpars, psib = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N1) > sum(N2))
})


# increase prep drop out, increase overall infections


test_that("prep dropout vs infections", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  
  newpars = lhs_parameters(1, set_null = "kappaa")[[1]]$kappaa
  parameters <- modifyList(parameters, list(kappaa = newpars, kappab = newpars, kappac = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N1) > sum(N2))
})


# increase ART drop out, increase overall infections

test_that("ART dropout vs prevalence", {
  parameters <- lhs_parameters(1)[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N1 <- rowSums(do.call(cbind, xx))
  
  
  newpars = lhs_parameters(1, set_null = "phi2")[[1]]$phi2
  parameters <- modifyList(parameters, list(phi2 = newpars, phi3 = newpars, phi4 = newpars, phi5 = newpars))
  
  result = run_model(parameters, main_model, time)
  xx <- result[c(grep("cumuInf", names(result)))]
  N2 <- rowSums(do.call(cbind, xx))
  
  expect_true(sum(N1) > sum(N2))
})


test_that("movement in = out", {
  parameters <- lhs_parameters(1, Ncat = 9)[[1]]
  result = run_model(parameters, main_model, time)
  for (i in 1:result$Ncat[1])
    expect_equal(sum(result$rate_move_in[1,,i]), -result$rate_move_out[1,i])
})


# MODEL TESTS NCAT = 9 WITH BEST SET PARS
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

test_that("omega keeps consistent population even with HIV?", {
  parameters <- lhs_parameters(1, Ncat = 9, set_pars = best_set, forced_pars = list(movement = 0, replaceDeaths = 1))[[1]]
  result = run_model(parameters, main_model, time)
  xx <- result[grep("frac_N", names(result))] # grepping all the Ss and Is
  
  
  expect_true(all(abs(diff(xx$frac_N))<10^-4))
  expect_equal(as.numeric(xx$frac_N[1,]), as.numeric(xx$frac_N[2,]))
})