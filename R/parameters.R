# parameters which depend on others, etc
fix_parameters <- function(y, Ncat, Nage) {
  
  y$epsilon_y = c(y$epsilon_1985, y$epsilon_1992, y$epsilon_2002, y$epsilon_2013, y$epsilon_2016)
  
  
  init_prev = c(y$prev_init_FSW, rep_len(y$prev_init_rest, Ncat-1))
  y$S0_init = (1-init_prev) * y$N_init
  y$I01_init = init_prev * y$N_init
  
  N = y$S0_init + y$I01_init
  
  # BIOLOGICAL
  
  # parameters dependent on others
  prog_rate = 2/(y$SC_to_200_349 - y$gamma01)
  y$gamma02 = rep_len(prog_rate, Ncat)
  y$gamma03 = rep_len(prog_rate, Ncat)
  
  # converting durations into rates
  y$gamma01 = 1/y$gamma01
  y$SC_to_200_349 = 1/y$SC_to_200_349
  y$gamma04 = 1/y$gamma04
  
  # filling in other parameters
  y$gamma01 <- rep_len(y$gamma01, Ncat)
  y$gamma04 <- rep_len(y$gamma04, Ncat)
  
  
  # y$omega <- y$omega/sum(y$omega)
  
  y$gamma22 <- y$gamma02
  y$gamma23 <- y$gamma03
  y$gamma24 <- y$gamma04
  
  y$gamma42 <- y$gamma02
  y$gamma43 <- y$gamma03
  y$gamma44 <- y$gamma04
  
  # progression is slowed by ART_RR
  
  y$gamma32 <- (y$gamma02)/y$ART_RR
  y$gamma33 <- (y$gamma03)/y$ART_RR
  y$gamma34 <- (y$gamma04)/y$ART_RR
  
  
  
  
  # behavioural
  
  # y$c_y_comm <- matrix(rep(y$c_y_comm, 4), ncol = Ncat)
  
  y$omega = N/sum(N)


  
  if (Ncat == 9) {
    
    # BIRTHS
    y$omega = c(
       y$fraction_F * (y$N_init[1]/y$N_init[3]) * y$fraction_FSW_foreign, # some FSW come from outside Cotonou
       y$fraction_F * (y$N_init[2]/y$N_init[3]) * y$fraction_FSW_foreign,
       0,
       0,
       0,
       0,
       y$fraction_F * (1 - (y$N_init[2]/y$N_init[3]) * y$fraction_FSW_foreign - (y$N_init[1]/y$N_init[3]) * y$fraction_FSW_foreign),
       1 - y$fraction_F,
       0)
    
    # MIXING
    ###############################################
    
    y$M_noncomm = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0,
                           0, 0, 0, 0, 1, 0, 0, 0, 0,
                           0, 0, 0, 0, 1, 1, 0, 0, 0,
                           0, 0, 0, 0, 1, 1, 0, 0, 0,
                           1, 1, 1, 1, 0, 0, 0, 0, 0,
                           0, 0, 1, 1, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0),
                         nrow = 9, ncol = 9, byrow = T)
    y$M_comm = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0,
                        0, 0, 0, 0, 1, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        1, 1, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0),
                      nrow = 9, ncol = 9, byrow = T)
    
    # MOVEMENT
    ############################################### 
    
    # virgin movement
    y$rate_move_out[7] = - y$rate_enter_sexual_pop
    y$rate_move_out[8] = - y$rate_enter_sexual_pop
    y$rate_move_in[3,7] = y$rate_enter_sexual_pop
    y$rate_move_in[6,8] = y$rate_enter_sexual_pop
    
    #this is just to show what happens when you increase movement
#     y$rate_leave_pro_FSW = y$rate_leave_pro_FSW * 10
#     y$rate_leave_low_FSW = y$rate_leave_low_FSW * 10
#     y$rate_leave_client = y$rate_leave_client * 10
    
    y$prop_client_GPM = y$N_init[5] / y$N_init[6]
    y$prop_pro_FSW_GPF = y$N_init[1] / y$N_init[3]
    y$prop_low_FSW_GPF = y$N_init[2] / y$N_init[3]
    
    # FEMALE MOVEMENT
    
    y$rate_move_out[1] = - y$rate_leave_pro_FSW
    y$rate_move_out[2] = - y$rate_leave_low_FSW
    y$rate_move_out[3] = - y$rate_leave_pro_FSW * y$prop_pro_FSW_GPF - y$rate_leave_low_FSW * y$prop_low_FSW_GPF
    
    y$rate_move_in[1,3] = y$rate_leave_pro_FSW * y$prop_pro_FSW_GPF # moving from GPF to pro-FSW
    y$rate_move_in[2,3] = y$rate_leave_low_FSW * y$prop_low_FSW_GPF # moving from GPF to low-FSW
    y$rate_move_in[4,1] = y$rate_leave_pro_FSW * (1 - y$FSW_leave_Cotonou_fraction) # moving from pro-FSW to former FSW in Cot
    y$rate_move_in[4,2] = y$rate_leave_low_FSW * (1 - y$FSW_leave_Cotonou_fraction) # moving from low-FSW to former FSW in Cot
    y$rate_move_in[9,1] = y$rate_leave_pro_FSW * y$FSW_leave_Cotonou_fraction # moving from low-FSW to former FSW NOT in Cot
    y$rate_move_in[9,2] = y$rate_leave_low_FSW * y$FSW_leave_Cotonou_fraction # moving from low-FSW to former FSW NOT in Cot
    
    
    # MALE MOVEMENT
    
    # virgin movement
    
    y$rate_move_out[5] = - y$rate_leave_client 
    y$rate_move_out[6] = - y$rate_leave_client * y$prop_client_GPM 
    
    y$rate_move_in[6,5] = y$rate_leave_client # moving from client to GPM
    y$rate_move_in[5,6] = y$rate_leave_client * y$prop_client_GPM # moving from GPM to client
    

    y$beta = c(y$betaMtoF, y$betaMtoF, y$betaMtoF, y$betaMtoF, y$betaFtoM, y$betaFtoM, y$betaMtoF, 0, 0)
      # c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
      # y$c_y_comm = 
    y$mu = rep(y$mu[1], 9)
      
  } else {
    # y$omega = y$omega/sum(y$omega)
  }
  

  
  return(y)
}

# N_Ncat7 = function(y) return S0_init[1] + I01_init[1] + I02_init[1] + I03_init[1] + I04_init[1] + I05_init[1]

#
# the parameters below will be sampled from an LHS and will replace their respective defaults
# unless I put something in the args of the function, eg sample = mu
lhs_parameters <- function(n, sample = NULL, Ncat = 2, Nage = 1, ..., set_pars = list(...), set_null= list(...)) {
  
  #fixed pars list
  fixed_pars = list(
    rate_move_in = matrix(0, nrow = Ncat, ncol = Ncat),
    rate_move_out = rep_len(0, Ncat),
    epsilon_y = 0,
    rate_enter_sexual_pop = 1,
    fraction_F = 0.51,
    fraction_FSW_foreign = 0.5
  )
  
  mu <- matrix(rep(c(1/50, 1/42), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("mu", Ncat), NULL))
  omega <- if(Ncat == 9) matrix(c(0.0017, 0.0067, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("omega", Ncat), NULL)) else 
    matrix(rep(c(0.4, 0.6), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("omega", Ncat), NULL))
  
  # c_y_comm <- if(Ncat == 9) matrix(c(300, 1400, 40, 64, 0, 0, 0, 0, 18.67, 37.5, 0, 0, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_y_comm", Ncat), NULL)) else c(1, 3)
  
  #these parameters need to be here so fix_parameters works? below are fixed...
  S0_init = matrix(rep(c(4000, 4000), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("S0_init", Ncat), NULL))
  I01_init = matrix(rep(c(1000, 1000), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("I01_init", Ncat), NULL))
  

  
  N_init = if(Ncat == 9) matrix(c(672, 672, 757, 757, 130895, 130895, 672, 672, 27091, 27091, 100335, 100335, 14544, 14544, 11148, 11148, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("N_init", Ncat), NULL)) else c(300000, 300000)
  #   c_comm = if(Ncat == 9) matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL)) else 
  #     matrix(rep(c(1,3), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL))
  c_comm = if(Ncat == 9) matrix(c(272, 1439, 40, 64, 0, 0, 0, 0, 18.67, 37.5, 0, 0, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL)) else 
    matrix(rep(c(1,3), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL))
  
  c_noncomm = if(Ncat == 9) matrix(c(0.2729358, 0.4682779, 0.2729358, 0.4682779, 0.90, 1.02, 0.90, 1.02, 1.21, 2.5, 1.28, 1.40, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_noncomm", Ncat), NULL)) else 
    matrix(rep(c(1,3), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_noncomm", Ncat), NULL))
  
  
  ranges <- rbind(
    # c_y_comm,
    
    epsilon_1985 = c(0.059346131*1.5, 0.059346131*1.5),
    epsilon_1992 = c(0.053594832*1.5, 0.053594832*1.5),
    epsilon_2002 = c(0.026936907*1.5, 0.026936907*1.5),
    epsilon_2013 = c(0.026936907*1.5, 0.026936907*1.5),
    epsilon_2016 = c(0.026936907*1.5, 0.026936907*1.5),
    
    
    rate_leave_pro_FSW = c(0.2, 0.2),
    FSW_leave_Cotonou_fraction = c(0.05, 0.15),
    rate_leave_low_FSW = c(0.1, 0.1),
    rate_leave_client = c(0.05, 0.05),
    
    
    betaMtoF = c(0.00086, 0.00433),
    betaFtoM = c(0.00279, 0.02701),
    
    prev_init_FSW = c(0.01318836, 0.06592892),
    prev_init_rest = c(0.0003134459, 0.0029420363),
    c_comm,
    c_noncomm,
    mu,
    
    infect_ART = c(0.1, 0.7), # infectiousness RR when on ART
    infect_acute = c(4, 18), # RR for acute phase
    
    gamma01 = c(0.16, 0.5), # from Mathieu's parameters  IN YEARS
    #     gamma01 = c(2, 6.25), # from Mathieu's parameters
    
    SC_to_200_349 = c(2.2, 4.6), # seroconversion to CD4 stage 4 IN YEARS
    #     SC_to_200_349 = c(1/4.6, 1/2.2), # seroconversion to CD4 stage 4
    
    gamma04 = c(3.9, 5), # from Mathieu's parameters  IN YEARS
    #     gamma04 = c(3.9, 5), # from Mathieu's parameters
    
    ART_RR = c(2, 3), # from Mathieu's parameters  IN YEARS
    
    omega,
    
    #below are fixed...
    S0_init,
    I01_init,
    N_init
 
  )
  if (!is.null(sample)) {
    ranges <- ranges[rownames(ranges) %in% sample,  drop=FALSE]
  }
  samples <- tgp::lhs(n, ranges)
  nms <- rownames(ranges)
  i <- split(seq_along(nms), nms)
  f <- function(x) {
    lapply(i, function(j) x[j])
  }
  samples_list <- apply(samples, 1, f)
  
  samples_list <- lapply(samples_list, function(x) modifyList(x, fixed_pars))
  
  samples_list <- lapply(samples_list, fix_parameters, Ncat = Ncat)
  
  samples_list <- lapply(samples_list, function(x) modifyList(x, set_pars))
  
  lapply(samples_list, function(x) generate_parameters(parameters = x, Ncat = Ncat, set_null = set_null))
}

generate_parameters <- function(..., parameters = list(...), set_null = list(...), Ncat = 2, Nage = 1) {
  defaults <- list(Ncat = Ncat,
                   Nage = Nage,
                   
                   N_init = 300000,
                   
                   #                    epsilon_t_comm = c(1985, 1991.99, 1992, 2001.99, 2002, 2012.99, 2013, 2016),
                   #                    epsilon_y_comm = c(0.059346131, 0.059346131, 0.053594832, 0.053594832, 0.026936907, 0.026936907, 0.026936907, 0.026936907),
                   epsilon_t = c(1985, 1992, 2002, 2013, 2016),
                   epsilon_y = c(0.059346131, 0.053594832, 0.026936907, 0.026936907, 0.026936907),
                   epsilon_1985 = 0.059346131,
                   epsilon_1992 = 0.053594832,
                   epsilon_2002 = 0.026936907,
                   epsilon_2013 = 0.026936907,
                   epsilon_2016 = 0.026936907,
                   
                   S0_init = rep_len(2000, Ncat),
                   S1a_init = rep_len(0, Ncat),
                   S1b_init = rep_len(0, Ncat),
                   S1c_init = rep_len(0, Ncat),
                   S1d_init = rep_len(0, Ncat),
                   
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
                   
                   rho2 = rep_len(0.5,Ncat),
                   rho3 = rep_len(0.5,Ncat),
                   rho4 = rep_len(0.5,Ncat),
                   rho5 = rep_len(0.5,Ncat),
                   
                   phi2 = rep_len(0.004,Ncat), # sort out later
                   phi3 = rep_len(0.004,Ncat),
                   phi4 = rep_len(0.004,Ncat),
                   phi5 = rep_len(0.004,Ncat),
                   
                   psia = rep_len(0.1,Ncat),
                   psib = rep_len(0.1,Ncat),
                   
                   tau01 = rep_len(1,Ncat),
                   tau11 = rep_len(1,Ncat),
                   tau2 = rep_len(1,Ncat),
                   tau3 = rep_len(1,Ncat),
                   tau4 = rep_len(1,Ncat),
                   tau5 = rep_len(1,Ncat),
                   
                   # PREP
                   zetaa_t = c(1985, 2013, 2015, 2016),
                   zetaa_y = matrix(c(rep(0, Ncat), 0.0075, rep(0, Ncat-1), rep(0, Ncat), rep(0, Ncat)), ncol = Ncat, byrow = T),
                   zetab_t = c(1985, 2013, 2015, 2016),
                   zetab_y = matrix(c(rep(0, Ncat), 0.0075, rep(0, Ncat-1), rep(0, Ncat), rep(0, Ncat)), ncol = Ncat, byrow = T),                   
                   zetac_t = c(1985, 2013, 2015, 2016),
                   zetac_y = matrix(c(rep(0, Ncat), 0.0075, rep(0, Ncat-1), rep(0, Ncat), rep(0, Ncat)), ncol = Ncat, byrow = T),   
                   
                   eP = rep_len(0.6,Ncat),
                   
                   eP0 = c(0, rep_len(0, (Ncat-1))),
                   eP1a = c(0.9, rep_len(0, (Ncat-1))),
                   eP1b = c(0.45, rep_len(0, (Ncat-1))),
                   eP1c = c(0, rep_len(0, (Ncat-1))),
                   eP1d = c(0, rep_len(0, (Ncat-1))),
                   
                   # condoms 
                   
                   fc_t_comm = c(1985, 1990, 1998, 2016),
                   fc_y_comm = matrix(
                     rep(c(0.5, 0.5, 0.9, 0.99), Ncat), ncol = Ncat),
                   
                   
                   kappaa = c(0.2, rep_len(0,(Ncat-1))),
                   kappab = c(0.2, rep_len(0,(Ncat-1))),
                   kappac = c(0.2, rep_len(0,(Ncat-1))),
                   
                   
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
                   
                   
                   beta = rep_len(0.005,Ncat),
                   betaMtoF = 0.00193,
                   betaFtoM = 0.00897,
                   #beta = 0,
                   
                   p_comm = matrix(1, ncol = Ncat, nrow = Ncat),
                   p_noncomm = matrix(1, ncol = Ncat, nrow = Ncat),
                   
                   
                   ec = rep_len(0.9,Ncat),
                   # ec = rep_len(1,1),
                   
                   
                   
                   
                   epsilon = 0.001,
                   
                   c_comm = rep_len(2, Ncat),
                   c_noncomm = rep_len(2, Ncat), 
                   #                    c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
                   #                    c_y_comm = matrix(rep(c(1, 2, 3, 1, 3, 4, 2, 1, 2, 4), Ncat), ncol = Ncat),
                   #                    
                   #                    c_t_noncomm = c(1985, 1990, 1998, 2016),
                   #                    c_y_noncomm = matrix(rep(c(0.5, 1, 0.7, 0.9), Ncat), ncol = Ncat),
                   
                   fc_t_comm = c(1985, 1990, 1998, 2016),
                   fc_y_comm = matrix(
                     rep(c(0.5, 0.5, 0.9, 0.99), Ncat), ncol = Ncat),
                   
                   fP_t_comm = c(1985, 2014, 2015, 2016),
                   fP_y_comm = matrix(
                     rep(c(1, 1, 1, 1), Ncat), ncol = Ncat),
                   
                   fc_t_noncomm = c(1985, 1990, 1998, 2016),
                   fc_y_noncomm = matrix(
                     rep(c(0, 0, 0.3, 0.5), Ncat), ncol = Ncat),
                   
                   fP_t_noncomm = c(1985, 2014, 2015, 2016),
                   fP_y_noncomm = matrix(
                     rep(c(1, 1, 1, 1), Ncat), ncol = Ncat),
                   
                   n_comm = matrix(1, ncol = Ncat, nrow = Ncat),
                   n_noncomm = matrix(1, ncol = Ncat, nrow = Ncat),
                   
                   #n = 0,
                   
                   R = 1,
                   omega = rep_len(1/Ncat, Ncat),
                   theta = matrix(0.5, ncol = Ncat, nrow = Ncat),
                   
                   M_comm = matrix(1/Ncat, Ncat, Ncat),
                   M_noncomm = matrix(1/Ncat, Ncat, Ncat),
                   
                   # A_F = matrix(1/NAge, NAge, NAge),
                   # A_M = matrix(1/NAge, NAge, NAge),
                   
                   #                    p = matrix(1, NAge, NAge),
                   
                   ART_RR = 2.5, # survival extension cofactor
                   
                   infect_ART = 0.4, # infectiousness RR when on ART
                   infect_acute = 9, # RR for acute phase
                   
                   dur_FSW = 30,
                   OnPrEP_init = rep_len(0, Ncat),
                   
                   
                   # have to include the follow in the function for it to work just using generate_parameters(), and not lhs_parameters()
                   SC_to_200_349 = rep_len(0.3, Ncat),
                   prev_init_FSW = 0.04,
                   prev_init_rest = 0.0008,
                   rate_leave_pro_FSW = 0.2,
                   FSW_leave_Cotonou_fraction = 0.1,
                   rate_leave_client = 0.05,
                   rate_leave_low_FSW = 0.1,
                   prop_client_GPM = 0.2430057, # 27091/111483
                   prop_pro_FSW_GPF = 0.004620494, # 672 / 145439
                   prop_low_FSW_GPF = 0.005204931, # 757 / 145439
                   rate_move_in = matrix(0, ncol = Ncat, nrow = Ncat),
                   rate_move_out = rep_len(0, Ncat),
                   rate_enter_sexual_pop = 1,
                   fraction_F = 0.51,
                   fraction_FSW_foreign = 0.5
                   
                   
                   
  )
  
  
  
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
  
  if (length(set_null) > 0L) {
    ret = modifyList(ret, lapply(ret[match(set_null, names(ret))], function(x) x*0))
  }
  
  ret
}
