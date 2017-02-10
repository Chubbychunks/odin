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

time <- seq(1986, 2016, length.out = 31)
parameters <- lhs_parameters(1,Ncat = 7)[[1]]
result = run_model(parameters, main_model, time)


out=data.frame(time=result$t,output=result$Ntot)
out

ggplot(out, aes(x = time, y = output)) + geom_line() + theme_bw()


# no zetas
parameters <- lhs_parameters(1,Ncat = 7, set_null = list("zetaa_y", "zetab_y", "zetac_y"))[[1]]





# test
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()
time <- seq(1986, 2016, length.out = 31)

parameters <- lhs_parameters(1,Ncat = 7)[[1]]
result = run_model(parameters, main_model, time)



# prev of all groups
#### Ncat = 7
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

number_simulations = 25
parms = lhs_parameters(number_simulations, Ncat = 7)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

out = data.frame(time, do.call(rbind, lapply(res, function(x) x$prev)), as.character(sort(rep(seq(1,number_simulations), length(time)))))
names(out) = c("time", "group 1", "group 2", "group 3", "group 4", "group 5", "group 6", "group 7", "replication")
out_melted = melt(out, id.vars = c("time", "replication"))
ggplot(data = out_melted, aes(x = time, y = value, factor = replication, color = variable)) + geom_line() + theme_bw() + facet_wrap(~variable)









#####




result$prev
result$c_comm_balanced
result$frac_N
result$omega

# showing priors are flat and explore space
result$c_comm

parameters <- lhs_parameters(2000, Ncat = 7)
parm_prior = do.call(rbind, lapply(parameters, function(x) x$mu))
hist(parm_prior[,1])

# test prep

odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

time <- seq(1986, 2016, length.out = 31)
parameters <- lhs_parameters(1,Ncat = 7)[[1]]
result = run_model(parameters, main_model, time)

df=melt(data.frame(time, a = result$S1a[,1], b = result$S1b[,1], c = result$S1c[,1], d = result$S1d[,1]), id.vars = "time")
ggplot(df, aes(x = time, y = value, color = variable)) + geom_line() + theme_bw()





# balancing
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
#

# mixing




odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

parameters <- lhs_parameters(1,Ncat = 7)[[1]]
time <- seq(1986, 2016, length.out = 31)
result = run_model(parameters, main_model, time)
result$M_comm[2,,]
result$M_noncomm[2,,]
result$p_comm[2,,]
result$p_noncomm[2,,]


# need to write general test solutions. done! general test below
timestep = 3
all_female_partnerships = 0; all_male_partnerships = 0;
for(i in 1:4)
  for (j in 1:7)
    all_female_partnerships = all_female_partnerships + result$p_comm[timestep, i, j] * result$c_comm_balanced[timestep,i] * result$N[timestep,i]

for(i in 5:6)
  for (j in 1:7)
    all_male_partnerships = all_male_partnerships + result$p_comm[timestep, i, j] * result$c_comm_balanced[timestep,i] * result$N[timestep,i]
all_female_partnerships/all_male_partnerships
#nonnoncomm check
# need to write general test solutions. done! general test below
timestep = 3
all_female_partnerships = 0; all_male_partnerships = 0;
for(i in 1:4)
  for (j in 1:7)
    all_female_partnerships = all_female_partnerships + result$p_noncomm[timestep, i, j] * result$c_noncomm_balanced[timestep,i] * result$N[timestep,i]

for(i in 5:6)
  for (j in 1:7)
    all_male_partnerships = all_male_partnerships + result$p_noncomm[timestep, i, j] * result$c_noncomm_balanced[timestep,i] * result$N[timestep,i]
all_female_partnerships/all_male_partnerships



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

time <- seq(1986, 2016, length.out = 31)

groups <- c("FSW", "Clients")

# parameter ranges

f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev_client", "prev_FSW","fc_comm","prev_1","prev")]
}
res = lapply(parms, f, main_model, time)
mu_input <- t(sapply(parms, "[[", "mu"))
fc_y_input <- t(sapply(parms, "[[", "fc_y_comm"))
prev_client_output <- t(sapply(res, "[[", "prev_client"))

# fc over time
df = melt(data.frame(time, res[[1]]$fc_comm), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()

# prevalence over time
matplot(time, res[[1]]$prev, type = "l")#, lty = 1, col = "#00000055")


# mu vs prev?
plot(mu_input[, 1], prev_client_output[length(time), ])


# PLOTTING MULTIPLE SIMULATIONS


number_simulations = 100
parms = lhs_parameters(number_simulations, Ncat = 2)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

df=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,1])), group = "group 1")
df_2=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,2])), group = "group 2")

df_melted = melt(df, id.vars = c("time","group"))
df_melted_2 = melt(df_2, id.vars = c("time","group"))
df_all = rbind(df_melted, df_melted_2)
ggplot(data = df_all, aes(x = time, y = value, factor = variable, color = group)) + geom_line(alpha = 0.5) + theme_bw()


#### Ncat = 7
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

number_simulations = 25
parms = lhs_parameters(number_simulations, Ncat = 7)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

df=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,1])), group = "group 1")
df_2=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,2])), group = "group 2")
df_3=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,3])), group = "group 3")
df_4=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,4])), group = "group 4")
df_5=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,5])), group = "group 5")
df_6=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,6])), group = "group 6")
df_7=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,7])), group = "group 7")

df_melted = melt(df, id.vars = c("time","group"))
df_melted_2 = melt(df_2, id.vars = c("time","group"))
df_melted_3 = melt(df_3, id.vars = c("time","group"))
df_melted_4 = melt(df_4, id.vars = c("time","group"))
df_melted_5 = melt(df_5, id.vars = c("time","group"))
df_melted_6 = melt(df_6, id.vars = c("time","group"))
df_melted_7 = melt(df_7, id.vars = c("time","group"))

df_all = rbind(df_melted, df_melted_2, df_melted_3, df_melted_4, df_melted_5, df_melted_6, df_melted_7)
ggplot(data = df_all, aes(x = time, y = value, factor = variable, color = group)) + geom_line(alpha = 0.5) + theme_bw()

df=do.call(cbind,lapply(res, function(x) x$prev[,1]))
colnames(df) <- seq(1, number_simulations)
df <- data.frame(time, df)
df_melted <- melt(df, id.vars = "time")
ggplot(data = df_melted, aes(x = time, y = value, factor = variable)) + geom_line() + theme_bw() + labs(x="year",y="prevlance (%) former FSW")


# show priors are flat and well explored
number_simulations = 200
parms = lhs_parameters(number_simulations, Ncat = 7)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("c_comm", "c_noncomm","mu","gamma01")]
}
res = lapply(parms, f, main_model, time)

c_comm_prior = do.call(rbind, lapply(res, function(x) x$c_comm[1,]))
hist(c_comm_prior[,1])

mu_prior = do.call(rbind, lapply(res, function(x) x$mu[1,]))
hist(mu_prior[,1])

c_noncomm_prior = do.call(rbind, lapply(res, function(x) x$c_noncomm[1,]))
hist(c_noncomm_prior[,1])

gamma01_prior = do.call(rbind, lapply(res, function(x) x$gamma01[1,]))
hist(gamma01_prior[,1])



# SINGLE RUN WITH PARAMETERS TO CHECK EVERY OUTPUT
##############################################################################

# plot fc
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("c_comm", names(result))]
df = melt(data.frame(time, yy$c_comm), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# number of people on PrEP
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("S1[a-z]", names(result))]
yy_plot <- melt(
  data.frame(time,
             FSW = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,1]))),
             clients = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,2])))
  ),id.vars = "time")

ggplot(data = yy_plot, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()



# plot prevalence
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("prev", names(result))]
df = melt(data.frame(time, FSW = yy$prev_FSW, client = yy$prev_client), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# cumulative infections
parameters <-lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
xx <- result[grep("cumu", names(result))]
N <- rowSums(do.call(cbind, xx))
df = melt(data.frame(time, N), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()
