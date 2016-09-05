require(ggplot2)
require(reshape2)


# some parameters to be dependent on others which have been sampled
# ART to be defined by nubmers, not rates

odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

number_simulations = 1

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

# y <- mod$run(time)
# HIV_epi = mod$transform_variables(y)
# names(HIV_epi)
# HIV_epi$B_check


# plot
# HIV_epi = cbind(time = time, as.data.frame(mod$transform_variables(y)))
# HIV_epi_melted = melt(HIV_epi, id.vars = c("time"))
# ggplot(HIV_epi_melted, aes(x = time, y = value, colour = variable)) + geom_point() + geom_line()





# OTHER FUNCTIONS
##############################################################################


###### this is just varying one parameter
# vary_S0 <- function(S0, base, gen, time) {
#   base$S0_init <- S0 * base$omega
#   gen(user=base)$run(time)
# }
#
# S0 <- c(10, 100, 1000, 10000)
# lapply(S0, vary_S0, parameters, main_model, time)


#### vary several parameters
# vary_a_bunch <- function(x, base, gen, time) {
#   base[names(x)] <- x
#   gen(user=base)$run(time)
# }

#
#
# # MODEL CHECKS
# ##############################################################################
#
# # v is a vector containing the variables that we are varying
# v <- c("theta")#,"gamma44")
# # cats are the categories we are outputting AND other parameters/things!
# cats <- c("I01",  "Ntot")
#
#
# pp <- unlist(parameters[v])
# pp <- as.data.frame(rbind(pp * 0, pp, pp * 2))
# # pp <- as.data.frame(rbind(pp, c(pp[1] * 0, pp[2]))) # pp 0 only for group 1
# # pp <- as.data.frame(rbind(pp, c(2 * pp[2], pp[1] * 0))) # pp 0 only for group 2
#
# #pp <- as.data.frame(rbind(pp/2, pp, pp * 2))
# rownames(pp) <- NULL
#
#
#
#
# naming_function <- function(xx){
#   if(length(xx) == length(cats)){
#     for(y in 1:(length(cats)-1))
#     {
#       colnames(xx[[y]]) = paste(groups, cats[y], sep=" ")
#     }
#     colnames(xx[[y+1]]) = "Ntot"
#     return(xx)
#   }
#   else
#   {
#     return("ERROR! STATS DOESNT MATCH CATS!")
#   }
# }
#
#
#
#
# vary2 <- function(x, base, v, gen, time) {
#   n <- lengths(base[v])
#   base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
#   mod <- gen(user=base)
#   res <- mod$transform_variables(mod$run(time))
#   # do fitting here
#
#   #parameterise first, then fit
#
#
#   ##OUTPUT
#   ##############################
#
#   stats <- res[names(res) %in% cats]
#   stats = naming_function(stats)
#   stats
#
#
#   # output the likelihood thing here, don't output all the variables and parameres!
#   #sum(res$S0) - 4362182
# }
#
# output <- lapply(seq_len(nrow(pp)), function(i) vary2(pp[i,], parameters, v, main_model, time))
#
#
#
# head(output[[1]]$S0)
# head(output[[1]]$I01)
# head(output[[1]]$Ntot)
#
# head(output[[2]]$S0)
# head(output[[2]]$I01)
# head(output[[2]]$Ntot)
#
#
#
#
#
#
#
#
#
# # PLOTTING
# ###############################################################################################
#
# for(p in 2:2)
# {
#   # taking the second simulation, where the parameter is altered!
#   df_out = c()
#   for(x in 1:length(cats))
#   {
#     df_out = cbind(df_out, output[[p]][[x]])
#   }
#   df_out <- data.frame(time, df_out)
#
#   df_out_melted <- melt(df_out, id.vars = c("time"))
#   df_out_melted <- cbind(df_out_melted, c(rep(c(rep("FSW", length(time)), rep("Client", length(time))), length(cats)-1), rep("Total", length(time))))
#
#   cats_vec = c();
#   for(x in 1:(length(cats)-1))
#   {
#     cats_vec = c(cats_vec, rep(cats[x], length(groups)*length(time)))
#   }
#   cats_vec = c(cats_vec, rep(cats[x+1], length(time)))
#   df_out_melted$cat <- cats_vec
#
#
#   colnames(df_out_melted) <- c("time","variable","value","group","cat")
#   plot <- ggplot(data = df_out_melted, aes(x = time, y = value, colour = cat)) + geom_line() + theme_bw() + facet_wrap(~group, scales = "free") + theme(legend.position = "right", legend.title = element_text(face = "bold"))
#   print(plot)
# }
# #lapply(output, head)
#














# DISCARDED NONSENSE
##############################################################################


#plot + coord_cartesian(ylim = c(0, 3000))
#head(output[[1]])


#lapply(output, head)





#s0 for third set of parameters
#head(output[[3]]$S0)




#
# lapply(output, head)
#
# lengths(parameters[v])
#
#
# lapply(seq_len(nrow(d)), function(i) f(d[i,]))
#
# expand.grid(a=1:5, b=letters[1:3])
#
# mod <- main_model(user = parameters)
#
#

#
#
# v <- names(mod$order)
# head(as.data.frame(lapply(something[v], function(x) x[,1])))
# head(as.data.frame(lapply(something[v], function(x) x[,2])))
#
# by_category <- lapply(seq_len(mod$contents()$Ncat), function(i) as.data.frame(lapply(something[v], function(x) x[, i])))
#
# lapply(by_category, head)
#
# lapply(something, rowSums)
#
# head(HIV_epi)
#



# model checks
# beta 0?

#####
# mod <- main_model()
#
# mod$initial()
#
# time <- seq(0, 10, length.out = 1000)
#
# y <- mod$run(time)
#
# pairs(y[,-1], panel = lines)
#
# head(output[[1]])
# head(output[[2]])
