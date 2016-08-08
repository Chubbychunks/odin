# this script will be converted to a package

time <- seq(0, 100, length.out = 101)

# takes the new varying parameter and it meshes it with the original base parameter set
f <- function(x, base, v, gen, time) {
  n <- lengths(base[v])
  base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
  mod <- gen(user=base)
  mod$transform_variables(mod$run(time))
}


# GROWTH RATE AND DEMOGRAPHY
########################################################################################

# relationship between growth rate (epsilon) and N
# if growth rate (epsilon) is 0, then N should not change. If growth rate is positive, then N should increase.

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

# CHECKING FOR LEAKS: SETTING ARROWS TO ZERO!
########################################################################################


# calculating incidence in several ways

# calculating prevalence in several ways