require(ggplot2)
require(reshape2)

#

# some parameters to be dependent on others which have been sampled
# ART to be defined by nubmers, not rates

odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()


devtools::test()














number_simulations = 100

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


#plotting
# fc over time
df = melt(data.frame(time, res[[1]]$fc), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()

# prevalence over time

# mu vs prev
matplot(time, prev_client_output, type = "l", lty = 1, col = "#00000055")
plot(mu_input[, 1], prev_client_output[length(time), ])





# SINGLE RUN WITH PARAMETERS TO CHECK EVERY OUTPUT
##############################################################################

# cumulative infections

parameters <- generate_parameters(theta = 0.5)
result = run_model(parameters, main_model, time)
xx <- result[grep("cumu", names(result))] # grepping all the Ss and Is
N <- rowSums(do.call(cbind, xx))
