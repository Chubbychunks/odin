require(ggplot2)
require(reshape2)

#

# to debug:
# open terminal in the folder
# R- d valgrind

# some parameters to be dependent on others which have been sampled
# ART to be defined by nubmers, not rates

odin::odin_package(".") # looks for any models inside inst/odin

devtools::load_all()

devtools::test()

parameters <- generate_parameters(Ncat = 8, c_comm=c(11,22,0,0,2.5,0,0,0))
result = run_model(parameters, main_model, time)
names(result)

result$Ncat
result$c_comm
result$c_comm_balanced
result$c_noncomm
result$c_noncomm_balanced
result$N






parameters <- generate_parameters(Ncat = 7, c_comm=c(1,2,3,4,5,6,7))
result = run_model(parameters, main_model, time)







parameters <- generate_parameters(Ncat = 7, c_comm=c(1244,52,0,0,24,0,0), c_noncomm=c(0.377,0.96,0.96,0.96,2.03,1.34,0),
                                  omega = c(1000, 1127, 143728, 500, 27323, 112436, 10)/(1000+1127+143728+500+27323+112436),
                                  S0_init = c(1000, 1127, 143728, 500, 27323, 112436, 10)*0.99,
                                  I01_init = c(1000, 1127, 143728, 500, 27323, 112436, 10)*0.01)
result = run_model(parameters, main_model, time)
result$N

result$B_check_comm
result$B_check_noncomm




names(result)

result$Ncat
result$c_comm
result$c_comm_balanced
result$c_noncomm
result$c_noncomm_balanced
result$N

result$S0[1,,]

result$sum_in_S0


result$B_check_comm
result$B_check_noncomm


#below to tset for PrEP
parameters <- generate_parameters(zetaa = c(0,0),zetab = c(0,0),zetac = c(0,0), psia = c(1, 1), psib = c(1, 1))
result = run_model(parameters, main_model, time)

result$OnPrEP
result$t

result$PrEP_0
result$PrEP_1a

result$S1a
result$S1b
result$S1c

result$lambda_0[10,,]
result$lambda_1a[10,,]
result$lambda_1b[10,,]
result$lambda_1c[10,,]

result$lambda_sum_0[2,]
result$lambda_sum_1a[2,]
result$lambda_sum_1b[2,]
result$lambda_sum_1c[2,]


# result$N
# result$c[1,,]
# result$cstar[1,,]
# result$B[1,,]
# result$theta[1,,]

# RUNNING MULTIPLE SIMULATIONS

number_simulations = 3

parms = lhs_parameters(number_simulations)


# main_model <- odin::odin("ODEs_odin.R")


time <- seq(1985, 2016, length.out = 32)

groups <- c("FSW", "Clients")

# parameter ranges

f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev_client", "prev_FSW","fc","prev_1")]
}
res = lapply(parms, f, main_model, time)
mu_input <- t(sapply(parms, "[[", "mu"))
fc_y_input <- t(sapply(parms, "[[", "fc_y"))
prev_client_output <- t(sapply(res, "[[", "prev_client"))

# PLOTTING MULTIPLE SIMULATIONS
# fc over time
df = melt(data.frame(time, res[[1]]$fc), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()

# prevalence over time

# mu vs prev
matplot(time, prev_client_output, type = "l", lty = 1, col = "#00000055")
plot(mu_input[, 1], prev_client_output[length(time), ])





# SINGLE RUN WITH PARAMETERS TO CHECK EVERY OUTPUT
##############################################################################

# plot fc
parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
yy <- result[grep("fc", names(result))]
df = melt(data.frame(time, yy$fc), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# plot fP
parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
yy <- result[grep("fP", names(result))]
df = melt(data.frame(time, yy$fP), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# number of people on PrEP
parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
yy <- result[grep("S1[a-z]", names(result))]
yy_plot <- melt(
  data.frame(time,
             FSW = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,1]))),
             clients = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,2])))
  ),id.vars = "time")

ggplot(data = yy_plot, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# plot N
parameters <- generate_parameters()
result = run_model(parameters, main_model, time = seq(1985, 2016, length.out = 310))
yy <- result[grep("N_client|N_FSW", names(result))]
df = melt(data.frame(time = seq(1985, 2016, length.out = 310), FSW = yy$N_FSW, client = yy$N_client), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line(alpha = 0.5) + theme_bw()


# plot prevalence
parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
yy <- result[grep("prev", names(result))]
df = melt(data.frame(time, FSW = yy$prev_FSW, client = yy$prev_client), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# cumulative infections
parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
xx <- result[grep("cumu", names(result))]
N <- rowSums(do.call(cbind, xx))
