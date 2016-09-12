# parameteres which depend on others, etc
fix_parameters <- function(x, Ncat) {
  # parameters dependent on others
  prog_rate = 2/(x$SC_to_200_349 - x$gamma01)
  x$gamma02 = rep_len(prog_rate, Ncat)
  x$gamma03 = rep_len(prog_rate, Ncat)

  # converting durations into rates
  x$gamma01 = 1/x$gamma01
  x$SC_to_200_349 = 1/x$SC_to_200_349
  x$gamma04 = 1/x$gamma04

  # filling in other parameters
  x$gamma01 <- rep_len(x$gamma01, Ncat)
  x$gamma04 <- rep_len(x$gamma04, Ncat)


  x$omega <- x$omega/sum(x$omega)

  x$gamma22 <- x$gamma02
  x$gamma23 <- x$gamma03
  x$gamma24 <- x$gamma04

  x$gamma42 <- x$gamma02
  x$gamma43 <- x$gamma03
  x$gamma44 <- x$gamma04

  # progression is slowed by ART_RR

  x$gamma32 <- (x$gamma02)/x$ART_RR
  x$gamma33 <- (x$gamma03)/x$ART_RR
  x$gamma34 <- (x$gamma04)/x$ART_RR


  return(x)
}


#
# the parameters below will be sampled from an LHS and will replace their respective defaults
# unless I put something in the args of the function, eg sample = mu
lhs_parameters <- function(n, sample = NULL, Ncat = 2) {
  ranges <- rbind(
    mu = c(1/50, 1/42), # sampling LHS as a rate.....!!!!
    mu = c(1/47, 1/40),

    mu = c(1/47, 1/40),
    mu = c(1/47, 1/40),
    mu = c(1/47, 1/40),


    gamma01 = c(0.16, 0.5), # from Mathieu's parameters  IN YEARS
    #     gamma01 = c(2, 6.25), # from Mathieu's parameters

    SC_to_200_349 = c(2.2, 4.6), # seroconversion to CD4 stage 4 IN YEARS
    #     SC_to_200_349 = c(1/4.6, 1/2.2), # seroconversion to CD4 stage 4

    gamma04 = c(3.9, 5), # from Mathieu's parameters  IN YEARS
    #     gamma04 = c(3.9, 5), # from Mathieu's parameters

    ART_RR = c(2, 3), # from Mathieu's parameters  IN YEARS

    epsilon = c(0.026, 0.028),
    omega = c(0.4, 0.6),
    omega = c(0.4, 0.6),
    omega = c(0.4, 0.6),
    omega = c(0.4, 0.6),
    omega = c(0.4, 0.6)
    )
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

  samples_list <- lapply(samples_list, fix_parameters, Ncat)

  lapply(samples_list, function(x) generate_parameters(parameters = x, Ncat = Ncat))
}

generate_parameters <- function(..., parameters = list(...), set_null = list(...), Ncat = 2) {
  defaults <- list(Ncat = Ncat,

                   S0_init = rep_len(2000, Ncat),
                   S1a_init = rep_len(0, Ncat),
                   S1b_init = rep_len(0, Ncat),
                   S1c_init = rep_len(0, Ncat),
                   I01_init = rep_len(1000, Ncat),
                   I11_init = rep_len(0, Ncat),
                   I02_init = rep_len(0, Ncat),
                   I03_init = rep_len(0, Ncat),
                   I04_init = rep_len(0, Ncat),
                   I05_init = rep_len(0, Ncat),
                   I22_init = rep_len(0, Ncat),
                   I23_init = rep_len(0, Ncat),
                   I24_init = rep_len(0, Ncat),
                   I25_init = rep_len(0, Ncat),
                   I32_init = rep_len(0, Ncat),
                   I33_init = rep_len(0, Ncat),
                   I34_init = rep_len(0, Ncat),
                   I35_init = rep_len(0, Ncat),
                   I42_init = rep_len(0, Ncat),
                   I43_init = rep_len(0, Ncat),
                   I44_init = rep_len(0, Ncat),
                   I45_init = rep_len(0, Ncat),

                   cumuInf_init = rep_len(0, Ncat),


                   mu = rep_len(0.02, Ncat),
                   gamma01 = rep_len(0.2, Ncat),
                   gamma02 = rep_len(0.2, Ncat),
                   gamma03 = rep_len(0.2, Ncat),
                   gamma04 = rep_len(0.2, Ncat),

                   gamma11 = rep_len(0.2, Ncat),

                   gamma22 = rep_len(0.2, Ncat),
                   gamma23 = rep_len(0.2, Ncat),
                   gamma24 = rep_len(0.2, Ncat),

                   gamma32 = rep_len(0.2, Ncat),
                   gamma33 = rep_len(0.2, Ncat),
                   gamma34 = rep_len(0.2, Ncat),

                   gamma42 = rep_len(0.2, Ncat),
                   gamma43 = rep_len(0.2, Ncat),
                   gamma44 = rep_len(0.2, Ncat),

                   rho2 = rep_len(0.1,Ncat),
                   rho3 = rep_len(0.1,Ncat),
                   rho4 = rep_len(0.1,Ncat),
                   rho5 = rep_len(0.1,Ncat),

                   phi2 = rep_len(0.04,Ncat), # sort out later
                   phi3 = rep_len(0.04,Ncat),
                   phi4 = rep_len(0.04,Ncat),
                   phi5 = rep_len(0.04,Ncat),

                   psia = rep_len(1,Ncat),
                   psib = rep_len(1,Ncat),

                   tau01 = rep_len(0.5,Ncat),
                   tau11 = rep_len(0.5,Ncat),
                   tau2 = rep_len(0.5,Ncat),
                   tau3 = rep_len(0.5,Ncat),
                   tau4 = rep_len(0.5,Ncat),
                   tau5 = rep_len(0.5,Ncat),

                   zetaa = rep_len(0.1,Ncat),
                   zetab = rep_len(0.1,Ncat),
                   zetac = rep_len(0.1,Ncat),

                   SC_to_200_349 = rep_len(0.3, Ncat),

                   alpha01 = rep_len(0.01,Ncat),
                   alpha02 = rep_len(0.01,Ncat),
                   alpha03 = rep_len(0.01,Ncat),
                   alpha04 = rep_len(0.01,Ncat),
                   alpha05 = rep_len(1, Ncat),

                   alpha11 = rep_len(0.01,Ncat),

                   alpha21 = rep_len(0.01,Ncat),
                   alpha22 = rep_len(0.01,Ncat),
                   alpha23 = rep_len(0.01,Ncat),
                   alpha24 = rep_len(0.01,Ncat),
                   alpha25 = rep_len(1,Ncat),

                   alpha32 = rep_len(0.01,Ncat),
                   alpha33 = rep_len(0.01,Ncat),
                   alpha34 = rep_len(0.01,Ncat),
                   alpha35 = rep_len(1,Ncat),

                   alpha42 = rep_len(0.01,Ncat),
                   alpha43 = rep_len(0.01,Ncat),
                   alpha44 = rep_len(0.01,Ncat),
                   alpha45 = rep_len(1,Ncat),


                   beta = rep_len(0.193,Ncat),
                   #beta = 0,
                   c = rep_len(4,Ncat),
                   ec = rep_len(0.85,Ncat),
                   # ec = rep_len(1,1),

                   eP = rep_len(0.6,Ncat),
                   epsilon = 0.001,
                   # fc = c(1,1),
                   fc_t = c(1985, 1990, 1998, 2016),

                   fc_y = matrix(
                     rep(c(0, 0, 0.7, 0.9), Ncat), ncol = Ncat),

                   fP_t = c(1985, 2014, 2015, 2016),
                   fP_y = matrix(
                     rep(c(0, 0, 0.7, 0.9), Ncat), ncol = Ncat),

                   n = rep_len(10, Ncat),
                   #n = 0,

                   R = rep_len(1, Ncat),
                   omega = rep_len(1/Ncat, Ncat),
                   theta = 0.5,

                   M = matrix(1/Ncat, Ncat, Ncat),

                   # A = matrix(1/NAge, NAge, NAge),

                   ART_RR = 2.5
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


  ret
}
