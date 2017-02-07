# helper files have functions that will be used a lot in the model tests

time <- seq(1986, 2016, length.out = 310)


# takes the new varying parameter and it meshes it with the original base parameter set
f <- function(x, base, v, gen, time) {
  n <- lengths(base[v])
  base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
  mod <- gen(user=base)
  mod$transform_variables(mod$run(time))
}

run_model <- function(p, gen, time, output_vars) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[output_vars]
}

parameter_names = names(lhs_parameters(1)[[1]])
