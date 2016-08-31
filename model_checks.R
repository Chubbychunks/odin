# this script will be converted to a package

mod <- gen(user = parameters)

time <- seq(0, 100, length.out = 101)

# takes the new varying parameter and it meshes it with the original base parameter set
f <- function(x, base, v, gen, time) {
  n <- lengths(base[v])
  base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
  mod <- gen(user=base)
  mod$transform_variables(mod$run(time))
}








# GROWTH RATE AND DEMOGRAPHY
###################################################################################################################################
###################################################################################################################################

## the parts of omega must add up to 1!

if(sum(parameters$omega) != 1)
{
  print("the sum of omega doesn't equal 1!")
}

##########

## relationship between growth rate (epsilon) and N
## if growth rate (epsilon) is 0, then N should not change. If growth rate is positive, then N should increase.

# defining the parameter to vary
v <- c("epsilon")
parameters <- generate_parameters(epsilon=0.002)
pp <- unlist(parameters[v])
pp <- as.data.frame(rbind(pp * 0, pp))
scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))

# running the model with the variations of the parameter
result <- list()
for(j in 1:nrow(pp))
{
  x <- scenarios[[j]]
  xx <- x[grep("^[SI]", names(scenarios[[j]]))]
  result[[j]] <- rowSums(do.call(cbind, xx))
}

# test 1: are all increments in N equal to 0?
if(!all(abs(diff(result[[1]])) < 10^-10))
{
  print("N is not constant with a zero growth rate!")
}

# test 2: are all increments in N positive AND are the increments getting bigger?
if(!all(diff(result[[2]]) > 0) || !all(diff(diff(result[[2]])) > 0))
{
  print("The population is not growing exponentially!")
}

# test 3: declining population size?!

# test 4: epsilon * N ?? doesn't seem to follow exactly...
  
# 0.002 * result[[2]]
# diff(result[[2]])
# these two should be the same

##########



# CHECKING FOR LEAKS: SETTING ARROWS TO ZERO!
###################################################################################################################################
###################################################################################################################################


## PrEP uptake rates to zero

# if all zetas are zero, then there should be no one on PrEP


# defining the parameter to vary
v <- c("zetaa","zetab","zetac")

parameters <- generate_parameters(zetaa = c(0.1,0.1), zetab = c(0.1,0.1), zetac = c(0.1,0.1), I11_init = c(0,0))
pp <- unlist(parameters[v])
pp <- as.data.frame(rbind(pp * 0, pp))
scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))

# running the model with the variations of the parameter
result <- list()
for(j in 1:nrow(pp))
{
  x <- scenarios[[j]]
  xx <- x[c(grep("I11", names(scenarios[[j]])), grep("S1", names(scenarios[[j]])))]
  result[[j]] <- rowSums(do.call(cbind, xx))
}

# test 1: is there anyone on PrEP?
if(!all(result[[1]]==0))
{
  print("There are some people on PrEP when uptake is zero!")
}

# test 2: are some people going on PrEP if PrEP uptake is positive? if the second time point is higher than the first
if(!(result[[2]][2] > result[[2]][1]))
{
  print("The population is not growing exponentially!")
}

###################################################################################################################################


# Setting force of infection (gamma) to zero

# setting beta to zero


# defining the parameter to vary
v <- c("beta")

# only group 1 infected, does group 2 get infected?
parameters <- generate_parameters(I11_init = c(1000,0), I01_init = c(1000,0))
pp <- unlist(parameters[v])
pp <- as.data.frame(rbind(c(0, pp[2]), pp))
scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))

# running the model with the variations of the parameter
result <- list()
for(j in 1:nrow(pp))
{
  x <- scenarios[[j]]
  xx <- x[c(grep("I01", names(scenarios[[j]])), grep("I11", names(scenarios[[j]])))]
  result[[j]] <- sum(c(xx$I01[,2], xx$I11[,2]))
}

# test 1: is there anyone on PrEP?
if(!all(result[[1]]==0))
{
  print("There are still people being infected!")
}

# beta and 2->1 infection


# only group 2 infected, does group 1 get infected?
parameters <- generate_parameters(I11_init = c(0,1000), I01_init = c(0,1000))
pp <- unlist(parameters[v])
pp <- as.data.frame(rbind(c(pp[1], 0), pp))
scenarios <- lapply(seq_len(nrow(pp)), function(i) f(pp[i,], parameters, v, gen, time))

# running the model with the variations of the parameter
result <- list()
for(j in 1:nrow(pp))
{
  x <- scenarios[[j]]
  xx <- x[c(grep("I01", names(scenarios[[j]])), grep("I11", names(scenarios[[j]])))]
  result[[j]] <- sum(c(xx$I01[,1], xx$I11[,1]))
}

# test 1: is there anyone on PrEP?
if(!all(result[[1]]==0))
{
  print("There are still people being infected!")
}

#!!!! still haven't done the beta from 2->1 going to zero yet.



# setting R to zero

# setting n to zero

# setting condom (fc) and efficacy of condom (eC) to 1

# setting prep use (fP) and prep efficacy (eP) to 1

# CALCULATING INCIDENCE
###################################################################################################################################
###################################################################################################################################


# CALCULATING PREVALENCE
###################################################################################################################################
###################################################################################################################################

## different ways to calculate it

## does prevalence generally increase with beta?



# CALCULATING THE FORCE OF INFECTION
###################################################################################################################################
###################################################################################################################################



# 


print("done")
