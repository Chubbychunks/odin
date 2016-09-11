
# the parameters below will be sampled from an LHS and will replace their respective defaults
# unless I put something in the args of the function, eg sample = mu
lhs_parameters <- function(n, sample = NULL) {
  ranges <- rbind(
    mu = c(1/50, 1/42), # sampling LHS as a rate.....!!!!
    mu = c(1/47, 1/40),

    gamma01 = c(0.16, 0.5), # from Mathieu's parameters  IN YEARS
#     gamma01 = c(2, 6.25), # from Mathieu's parameters

    SC_to_200_349 = c(2.2, 4.6), # seroconversion to CD4 stage 4 IN YEARS
#     SC_to_200_349 = c(1/4.6, 1/2.2), # seroconversion to CD4 stage 4

    gamma04 = c(3.9, 5), # from Mathieu's parameters  IN YEARS
#     gamma04 = c(3.9, 5), # from Mathieu's parameters

    epsilon = c(0.026, 0.028),
    omega = c(0.4, 0.6))
  if (!is.null(sample)) {
    ranges <- ranges[rownames(ranges) %in% sample, , drop=FALSE]
  }
  samples <- tgp::lhs(n, ranges)
  nms <- rownames(ranges)
  i <- split(seq_along(nms), nms)
  f <- function(x) {
    lapply(i, function(j) x[j])
  }
  samples_list <- apply(samples, 1, f)

  # parameters dependent on others
  prog_rate = 2/(samples_list[[1]]$SC_to_200_349 - samples_list[[1]]$gamma01)
  samples_list[[1]]$gamma02 = c(prog_rate, prog_rate)
  samples_list[[1]]$gamma03 = c(prog_rate, prog_rate)

  # converting durations into rates
  samples_list[[1]]$gamma01 = 1/samples_list[[1]]$gamma01
  samples_list[[1]]$SC_to_200_349 = 1/samples_list[[1]]$SC_to_200_349
  samples_list[[1]]$gamma04 = 1/samples_list[[1]]$gamma04

  # filling in other parameters
  samples_list[[1]]$omega <- c(samples_list[[1]]$omega, 1-(samples_list[[1]]$omega))


  lapply(samples_list, function(x) generate_parameters(parameters = x))
}

generate_parameters <- function(..., parameters = list(...), set_null = list(...)) {
  defaults <- list(S0_init = c(2000,2000),
                   S1a_init = c(0,0),
                   S1b_init = c(0,0),
                   S1c_init = c(0,0),
                   I01_init = c(1000,1000),
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

                   cumuInf_init = c(0,0),


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

                   SC_to_200_349 = c(0.3, 0.3),

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


                   beta = c(0.193,0.182),
                   #beta = 0,
                   c = c(4,6),
                   ec = c(0.85,0.84),
                   # ec = c(1,1),

                   eP = c(0.6,0.5),
                   epsilon = 0.001,
                   # fc = c(1,1),
                   fc_t = c(1985, 1990, 1998, 2016),
                   fc_y = cbind(c(0, 0, 0.7, 0.9), c(0, 0, 0.3, 0.5)),

                   fP_t = c(1985, 2014, 2015, 2016),
                   fP_y = cbind(c(0, 0, 0.99, 0), c(0, 0, 0, 0)),
                   n = c(10,3),
                   #n = 0,

                   R = c(1,1),
                   omega = 0.5,
                   theta = 0.5,

                   M = cbind(c(0.2, 0.2, 0.2, 0.2, 0.2),
                             c(0.2, 0.2, 0.2, 0.2, 0.2),
                             c(0.2, 0.2, 0.2, 0.2, 0.2),
                             c(0.2, 0.2, 0.2, 0.2, 0.2),
                             c(0.2, 0.2, 0.2, 0.2, 0.2)),

                   A = cbind(c(0.33, 0.33, 0.33),
                             c(0.33, 0.33, 0.33),
                             c(0.33, 0.33, 0.33))
  )

  if (length(set_null) > 0L) {
    defaults = modifyList(defaults, lapply(defaults[match(set_null, names(defaults))], function(x) x*0))
  }

  if (length(parameters) == 0L) {
    return(defaults)
  }

  if (is.null(names(parameters)) || !all(nzchar(names(parameters)))) {
    stop("All arguments must be named")
  }
  extra <- setdiff(names(parameters), names(defaults))
  if (length(extra) > 0L) {
    stop("Unknown arguments: ", extra)
  }


  # list of parameters that depend on others
  ret <- modifyList(defaults, parameters)

  if(length((ret$omega) == 1))
  {
    ret$omega <- c(ret$omega, 1-(ret$omega))
  }

  ret
}
