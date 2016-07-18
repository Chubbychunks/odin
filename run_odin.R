require(ggplot2)
require(reshape2)






# gen <- odin::odin("ODEs_odin.R")
gen <- odin::odin("ODEs_odin_with_2_groups.R")


time <- seq(0, 100, length.out = 100)

groups <- c("FSW", "Clients")

parameters <- list(S0_init = c(10000,10000),
                   S1a_init = c(0,0),
                   S1b_init = c(0,0),
                   S1c_init = c(0,0),
                   I01_init = c(1000,0),
                   I11_init = c(0,0),
                   I02_init = c(0,0),
                   I03_init = c(0,0),
                   I04_init = c(0,0),
                   I05_init = c(0,0),
                   I22_init = c(0,0),
                   I23_init = c(0,0),
                   I24_init = c(0,0),
                   I25_init = c(0,0),
                   I32_init = c(0,0),
                   I33_init = c(0,0),
                   I34_init = c(0,0),
                   I35_init = c(0,0),
                   I42_init = c(0,0),
                   I43_init = c(0,0),
                   I44_init = c(0,0),
                   I45_init = c(0,0),
                   
                   
                   mu = c(0.02,0.02),
                   gamma01 = c(0.2,0.2),
                   gamma02 = c(0.2,0.2),
                   gamma03 = c(0.2,0.2),
                   gamma04 = c(0.2,0.2),
                   
                   gamma11 = c(0.2,0.2),
                   
                   gamma22 = c(0.2,0.2),
                   gamma23 = c(0.2,0.2),
                   gamma24 = c(0.2,0.2),
                   
                   gamma32 = c(0.2,0.2),
                   gamma33 = c(0.2,0.2),
                   gamma34 = c(0.2,0.2),
                   
                   gamma42 = c(0.2,0.2),
                   gamma43 = c(0.2,0.2),
                   gamma44 = c(0.2,0.2),
                   
                   rho2 = c(0.1,0.1),
                   rho3 = c(0.1,0.1),
                   rho4 = c(0.1,0.1),
                   rho5 = c(0.1,0.1),
                   
                   phi2 = c(0.04,0.01),
                   phi3 = c(0.04,0.01),
                   phi4 = c(0.04,0.01),
                   phi5 = c(0.04,0.01),
                   
                   psia = c(1,1),
                   psib = c(1,1),
                   
                   tau01 = c(0.5,0.5),
                   tau11 = c(0.5,0.5),
                   tau2 = c(0.5,0.5),
                   tau3 = c(0.5,0.5),
                   tau4 = c(0.5,0.5),
                   tau5 = c(0.5,0.5),
                   
                   zetaa = c(0.1,0.1),
                   zetab = c(0.1,0.1),
                   zetac = c(0.1,0.1),
                   
                   alpha01 = c(0.01,0.01),
                   alpha02 = c(0.01,0.01),
                   alpha03 = c(0.01,0.01),
                   alpha04 = c(0.01,0.01),
                   alpha05 = c(1,1),
                   
                   alpha11 = c(0.01,0.01),
                   
                   alpha21 = c(0.01,0.01),
                   alpha22 = c(0.01,0.01),
                   alpha23 = c(0.01,0.01),
                   alpha24 = c(0.01,0.01),
                   alpha25 = c(1,1),
                   
                   alpha32 = c(0.01,0.01),
                   alpha33 = c(0.01,0.01),
                   alpha34 = c(0.01,0.01),
                   alpha35 = c(1,1),
                   
                   alpha42 = c(0.01,0.01),
                   alpha43 = c(0.01,0.01),
                   alpha44 = c(0.01,0.01),
                   alpha45 = c(1,1),
                   
                   
                   beta = c(0.0193,0.0182),
                   #beta = 0,
                   c = c(4,6),
                   ec = c(0.85,0.84),
                   # ec = c(1,1),
                   
                   eP = c(0.6,0.5),
                   epsilon = 0.001,
                   # fc = c(1,1),
                   fc = c(0.9,0.9),
                   
                   fP = c(0.5,0.3),
                   n = c(10,3),
                   #n = 0,
                   
                   R = c(1,1),
                   omega = c(0.5, 0.5)
)

mod <- gen(user = parameters)
y <- mod$run(time)

# HIV_epi = cbind(time = time, as.data.frame(mod$transform_variables(y)))
# HIV_epi_melted = melt(HIV_epi, id.vars = c("time"))
# ggplot(HIV_epi_melted, aes(x = time, y = value, colour = variable)) + geom_point() + geom_line()

###### this is just varying one parameter
# vary_S0 <- function(S0, base, gen, time) {
#   base$S0_init <- S0 * base$omega
#   gen(user=base)$run(time)
# }
# 
# S0 <- c(10, 100, 1000, 10000)
# lapply(S0, vary_S0, parameters, gen, time)


#### vary several parameters
# vary_a_bunch <- function(x, base, gen, time) {
#   base[names(x)] <- x
#   gen(user=base)$run(time)
# }



# MODEL CHECKS
############



# v is a vector containing the variables that we are varying
v <- c("epsilon")
# cats are the categories we are outputting AND other parameters/things!
cats <- c("S0", "I01", "Ntot")


pp <- unlist(parameters[v])
pp <- as.data.frame(rbind(pp, pp * 0))
#pp <- as.data.frame(rbind(pp, c(pp[1] * 0, 2 * pp[2]))) # pp is how they vary
#pp <- as.data.frame(rbind(pp/2, pp, pp * 2))
rownames(pp) <- NULL




naming_function <- function(xx){
  if(length(xx) == length(cats)){
    for(y in 1:(length(cats)-1))
    {
      colnames(xx[[y]]) = paste(groups, cats[y], sep=" ")
    }
    colnames(xx[[y+1]]) = "Ntot"
    return(xx)
  }
  else
  {
    return("ERROR! STATS DOESNT MATCH CATS!")
  }
}






vary2 <- function(x, base, v, gen, time) {
  n <- lengths(base[v])
  base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
  mod <- gen(user=base)
  res <- mod$transform_variables(mod$run(time))
  # do fitting here
  
  ###
  
  ##OUTPUT
  ##############################
  #stats <- cbind(res$S0,res$I01)
  
  stats <- res[names(res) %in% cats]
  
  # colnames(stats[[1]]) = paste(groups, " S0", sep="")
  # colnames(stats[[2]]) = paste(groups, " I01", sep="")
  # colnames(stats[[3]]) = "Ntot"
  
  stats = naming_function(stats)
  
  
  
  stats
  
  # groups <- c("FSW", "Clients")
  # stats <- cbind(res$cats)
  # stats
  
  # output the likelihood thing here, don't output all the variables and parameres!
  #sum(res$S0) - 4362182
}

output <- lapply(seq_len(nrow(pp)), function(i) vary2(pp[i,], parameters, v, gen, time))

head(output[[1]]$S0)
head(output[[1]]$I01)
head(output[[1]]$Ntot)

head(output[[2]]$S0)
head(output[[2]]$I01)
head(output[[2]]$Ntot)


# PLOTTING
###############################################################################################

df_out <- data.frame(time, output[[2]][[1]],  output[[2]][[2]],  output[[2]][[3]])
df_out_melted <- melt(df_out, id.vars = c("time"))
df_out_melted <- cbind(df_out_melted, c(rep(c(rep("FSW", length(time)), rep("Client", length(time))), length(cats)-1), rep("Total", length(time))))
colnames(df_out_melted) <- c("time","variable","value","group")
ggplot(data = df_out_melted, aes(x = time, y = value, colour = variable, linetype = variable)) + geom_line() + theme_bw() + facet_wrap(~group) + theme(legend.position = "top")

#lapply(output, head)


############



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
# mod <- gen(user = parameters)
# 
# 
# y <- mod$run(time)
# 
# something = mod$transform_variables(y)
# 
# names(something)
# 
# something$S0
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
# mod <- gen()
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
