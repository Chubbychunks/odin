# takes the new varying parameter and it meshes it with the original base parameter set
f <- function(x, base, v, gen, time) {
  n <- lengths(base[v])
  base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
  mod <- gen(user=base)
  mod$transform_variables(mod$run(time))
}
