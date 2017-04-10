
# before_LHS <- function(y) {
#   # need to pass in the ranges i set, and also the parameters they affect
#   c_comm_grep = rownames(y)[grep("c_comm", rownames(y))]
#   c_comm_grep[grep("FSW", c_comm_grep)]
# }
# require(plyr)
par_seq = c("c_comm", "c_noncomm")
groups_seq = c("ProFSW", "LowFSW", "GPF", "FormerFSW", "Client", "GPM", "VirginF", "VirginM", "FormerFSWoutside")
years_seq = seq(1985, 2016)

# sapply(years_seq, function(x){
#   if(length(grep(x, names(samples_list)))) {
#     
#   }
# })


# after_LHS <- function(x, set_pars) {
#   # pass in the sampled values and glue them together
#   # c_comm_grep = names(x)[grep("c_comm", names(x))]
#   # if(length(c_comm_grep) > 0) {
#     # c_comm_grep[grep("ProFSW", c_comm_grep)]
#     #find along all the c_comm_grep names the ones which match up and replace 
#     for(year in 1:length(years_seq))
#       for(group in 1:length(groups_seq))
#         for(par in 1:length(par_seq))
#           {if(grepl(years_seq[year], names(x)) && grepl(groups_seq[group], names(x)) && grepl(par_seq[par], names(x))){
#             set_pars[paste0(par_seq[par], "_", years_seq[year])][[paste0(par_seq[par], "_", years_seq[year])]][group] = x[paste0(par_seq[par], "_", years_seq[year], "_", groups_seq[group])][[1]]
#             }}
#     # print(c(years_seq[year], groups_seq[group], par_seq[par]))
#     
#     # not quite!
#   # }
#   return(set_pars)
# }

# samples_list <- lapply(samples_list_test, after_LHS, set_pars = best_set)



# parameters which depend on others, etc
fix_parameters <- function(y, Ncat, Nage) {
  
  #   # interpolating for c_comm
  #   rownames(ranges)
  #   
  #   if(Ncat == 9)
  #   {
  #     
  #   }
  
  #   print("before")
  #   print(y)
  if(Ncat == 9) {
    # what_we_got = data.frame(par = c(), year = c(), group = c())
    what_we_got = c()
    for(year in 1:length(years_seq))
    {for(group in 1:length(groups_seq))
    {for(par in 1:length(par_seq))
      # {if(grepl(years_seq[year], names(y)) && grepl(groups_seq[group], names(y)) && grepl(par_seq[par], names(y))){
    {if(paste0(paste0(par_seq[par], "_", years_seq[year], "_", groups_seq[group])) %in% names(y)){
      
      y[paste0(par_seq[par], "_", years_seq[year])][[paste0(par_seq[par], "_", years_seq[year])]][group] = y[paste0(par_seq[par], "_", years_seq[year], "_", groups_seq[group])][[1]]
      # what_we_got = c(what_we_got, paste0(par_seq[par], "_", years_seq[year], "_", groups_seq[group]))
      what_we_got = rbind(what_we_got, c(par, year, group))
      # print(c(par_seq[par], groups_seq[group], years_seq[year]))
    }}}}
    colnames(what_we_got) = c("par", "year", "group")
    
    # now we have to fill in the rest of the years... IF 2 OR MORE YEARS FOR SAME GROUP AND PARM
    # all years before earliest one is same as earliest estimate, 
    # and all years after last one is same as last...
    # and everything in between is interpolated!
    d <- data.frame(what_we_got) 
    par_counts = plyr::count(d, c('par', 'group')) 
    
    for(i in 1:length(par_counts[,1]))
    {
      if(par_counts[i,"freq"] > 1)
      {
        the_years = subset(d, c(par == par_counts[i, "par"] & group == par_counts[i, "group"]), year)
        
        #before
        if(min(the_years) > 1)
        {
          for(year in 1:(min(the_years)-1))
          {
            if(paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year]) %in% names(y))
            {y[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year])][[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year])]][par_counts[i, "group"]] = 
              y[paste0(par_seq[par_counts[i,"par"]], "_", years_seq[min(the_years)], "_", groups_seq[par_counts[i, "group"]])][[1]]}
          }
        }
        
        #after
        if(max(the_years) < length(years_seq))
        {
          for(year in (max(the_years)+1):length(years_seq))
          {
            if(paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year]) %in% names(y))
            {y[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year])][[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[year])]][par_counts[i, "group"]] = 
              y[paste0(par_seq[par_counts[i,"par"]], "_", years_seq[max(the_years)], "_", groups_seq[par_counts[i, "group"]])][[1]]}
          }
        }
        
        # interoplate between
        # take the years that have values
        # calculate the slopes between them
        # apply the slope and time difference to the pars in between
        
        years_seq[unlist(the_years)]
        
        for(j in 1:(length(unlist(the_years))-1)) {
          slope = (y[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j+1])][[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j+1])]][par_counts[i, "group"]] -
            y[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j])][[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j])]][par_counts[i, "group"]]) /
            (years_seq[unlist(the_years)][j+1] - years_seq[unlist(the_years)][j])
          
          for(k in (years_seq[unlist(the_years)][j]+1):(years_seq[unlist(the_years)][j+1]-1))
          {
            if(paste0(par_seq[par_counts[i, "par"]], "_", k) %in% names(y))
              y[paste0(par_seq[par_counts[i, "par"]], "_", k)][[paste0(par_seq[par_counts[i, "par"]], "_", k)]][par_counts[i, "group"]] = slope * 
                (k - years_seq[unlist(the_years)][j]) + 
                y[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j])][[paste0(par_seq[par_counts[i, "par"]], "_", years_seq[unlist(the_years)][j])]][par_counts[i, "group"]]
          }
        }
        

     
        
        
        
      }
    }
    
    
  }
  #   print("after")
  #   print(y)
  
  
  
  # may want to switch off!
  y$betaFtoM_noncomm = y$betaMtoF_noncomm * y$RR_beta_FtM * 0.44
  y$betaMtoF_comm = y$betaMtoF_noncomm * y$RR_beta_GUD
  y$betaFtoM_comm = y$betaMtoF_noncomm * y$RR_beta_FtM * 0.44 # no GUD effect?
  
  
  
  y$epsilon_y = c(y$epsilon_1985, y$epsilon_1992, y$epsilon_2002, y$epsilon_2013, y$epsilon_2016)
  
  
  init_prev = if(Ncat == 9) c(y$prev_init_FSW, rep_len(y$prev_init_rest, 5), 0, 0, 0) else c(y$prev_init_FSW, rep_len(y$prev_init_rest, Ncat-1))
  y$S0_init = (1-init_prev) * y$N_init
  y$I01_init = init_prev * y$N_init
  
  N = y$S0_init + y$I01_init
  
  # BIOLOGICAL
  
  #gamma01, gamma04 input as a DURATION
  #SC_to_200_349 input as a duration
  
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
  y$alpha35 <- (y$alpha05)/y$ART_RR
  
  
  # print(y$fc_y_comm_1993)
  #   fc_y_comm = array(c(c(y$fc_y_comm_1985), c(y$fc_y_comm_1993), 
  #                       c(y$fc_y_comm_1995), c(y$fc_y_comm_1998),
  #                       c(y$fc_y_comm_2002), c(y$fc_y_comm_2005),
  #                       c(y$fc_y_comm_2008), c(y$fc_y_comm_2012),
  #                       c(y$fc_y_comm_2015)), c(Ncat, Ncat, 9))
  
  # PCR
  
  
  
  y$c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016)
  y$c_t_noncomm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016)
  
  y$c_y_comm = rbind(y$c_comm_1985, y$c_comm_1993, y$c_comm_1995, y$c_comm_1998, y$c_comm_2002, 
                     y$c_comm_2005, y$c_comm_2008, y$c_comm_2012, y$c_comm_2015, y$c_comm_2016)
  
  y$c_y_noncomm = rbind(y$c_noncomm_1985, y$c_noncomm_1993, y$c_noncomm_1995, y$c_noncomm_1998, y$c_noncomm_2002, 
                        y$c_noncomm_2005, y$c_noncomm_2008, y$c_noncomm_2012, y$c_noncomm_2015, y$c_noncomm_2016)
  
  
  
  
  
  
  y$fc_y_comm = array(data = c(y$fc_y_comm_1985, y$fc_y_comm_1993, 
                               y$fc_y_comm_1995, y$fc_y_comm_1998,
                               y$fc_y_comm_2002, y$fc_y_comm_2005,
                               y$fc_y_comm_2008, y$fc_y_comm_2012,
                               y$fc_y_comm_2015, y$fc_y_comm_2015), dim=c(Ncat, Ncat, 10))
  
  y$fc_y_comm = aperm(y$fc_y_comm, c(3, 1, 2))
  
  y$fc_y_noncomm = array(data = c(y$fc_y_noncomm_1985, y$fc_y_noncomm_1998, 
                                  y$fc_y_noncomm_2008, y$fc_y_noncomm_2015,
                                  y$fc_y_noncomm_2015), dim=c(Ncat, Ncat, 5))
  
  y$fc_y_noncomm = aperm(y$fc_y_noncomm, c(3, 1, 2))
  
  
  y$omega = N/sum(N)
  
  
  # when sampling pars, sometimes instead of length Ncat, I only sampled length one
  if(length(y$ec) == 1)
    y$ec = rep_len(y$ec, 9)
  if(length(y$eP0) == 1)
    y$eP0 = rep_len(y$eP0, 9)
  
  
  
  if (Ncat == 9) {
    
    y$who_believe_comm = round(y$who_believe_comm)
    
    # BIRTHS
    y$omega = c(
      y$fraction_F * (y$N_init[1]/y$N_init[3]) * y$fraction_FSW_foreign, # some FSW come from outside Cotonou
      y$fraction_F * (y$N_init[2]/y$N_init[3]) * y$fraction_FSW_foreign, # BUT THIS IS SET TO 0 FOR NOW: ALL NEW PEOPLE ARE ALL BORN VIRGINS
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
    
    y$rate_move_out[5] = - y$rate_leave_client 
    y$rate_move_out[6] = - y$rate_leave_client * y$prop_client_GPM 
    
    y$rate_move_in[6,5] = y$rate_leave_client # moving from client to GPM
    y$rate_move_in[5,6] = y$rate_leave_client * y$prop_client_GPM # moving from GPM to client
    
    
    y$beta_comm = c(y$betaMtoF_comm, y$betaMtoF_comm, y$betaMtoF_comm, y$betaMtoF_comm, y$betaFtoM_comm, y$betaFtoM_comm, 0, 0, 0)
    y$beta_noncomm = c(y$betaMtoF_noncomm, y$betaMtoF_noncomm, y$betaMtoF_noncomm, y$betaMtoF_noncomm, y$betaFtoM_noncomm, y$betaFtoM_noncomm, 0, 0, 0)
    
    
    
    # c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
    # y$c_y_comm = 
    y$mu = c(y$muF, y$muF, y$muF, y$muF, y$muM, y$muM, y$muF, y$muM, y$muF)
    
  } else {
    # y$omega = y$omega/sum(y$omega)
  }
  
  if(y$movement == 0) {
    y$rate_move_in = y$rate_move_in * 0
    y$rate_move_out = y$rate_move_out * 0}
  
  return(y)
}

# N_Ncat7 = function(y) return S0_init[1] + I01_init[1] + I02_init[1] + I03_init[1] + I04_init[1] + I05_init[1]

#
# the parameters below will be sampled from an LHS and will replace their respective defaults
# unless I put something in the args of the function, eg sample = mu
lhs_parameters <- function(n, sample = NULL, Ncat = 2, Nage = 1, ..., set_pars = list(...), forced_pars = list(...), set_null= list(...), ranges = NULL) {
  
  
  set_pars <- modifyList(set_pars, forced_pars)
  
  # parameters that I have defined in ranges will be removed from set_pars and fixed pars (fixed pars below)
  if(length(which(names(set_pars) %in% rownames(ranges))) > 0)
    set_pars <- set_pars[-which(names(set_pars) %in% rownames(ranges))]
  
  
  #fixed pars list i think for the fix parameters function
  fixed_pars = list(
    rate_move_in = matrix(0, nrow = Ncat, ncol = Ncat),
    rate_move_out = rep_len(0, Ncat),
    epsilon_y = 0,
    rate_enter_sexual_pop = 0.4,
    fraction_F = 0.516,
    fraction_FSW_foreign = 0,
    movement = 1,
    alpha05 = rep_len(0.3,Ncat),
    fc_y_comm_1985 = matrix(0.2, Ncat, Ncat),
    fc_y_comm_1993 = matrix(0.5, Ncat, Ncat),
    fc_y_comm_1995 = matrix(0.7, Ncat, Ncat),
    fc_y_comm_1998 = matrix(0.3, Ncat, Ncat),
    fc_y_comm_2002 = matrix(0.4, Ncat, Ncat),
    fc_y_comm_2005 = matrix(0.1, Ncat, Ncat),
    fc_y_comm_2008 = matrix(0.1, Ncat, Ncat),
    fc_y_comm_2012 = matrix(0.6, Ncat, Ncat),
    fc_y_comm_2015 = matrix(0.4, Ncat, Ncat),
    fc_y_noncomm_1985 = matrix(0.2, Ncat, Ncat),
    fc_y_noncomm_1998 = matrix(0.4, Ncat, Ncat),
    fc_y_noncomm_2008 = matrix(0.3, Ncat, Ncat),
    fc_y_noncomm_2015 = matrix(0.5, Ncat, Ncat),
    fc_y_noncomm_2015 = matrix(0.5, Ncat, Ncat),
    c_comm_1985 = rep_len(2, Ncat),
    c_comm_1993 = rep_len(2, Ncat),
    c_comm_1995 = rep_len(2, Ncat),
    c_comm_1998 = rep_len(2, Ncat),
    c_comm_2002 = rep_len(2, Ncat),
    c_comm_2005 = rep_len(2, Ncat),
    c_comm_2008 = rep_len(2, Ncat),
    c_comm_2012 = rep_len(2, Ncat),
    c_comm_2015 = rep_len(2, Ncat),
    c_comm_2016 = rep_len(2, Ncat),
    
    c_noncomm_1985 = rep_len(1, Ncat),
    c_noncomm_1993 = rep_len(1, Ncat),
    c_noncomm_1995 = rep_len(1, Ncat),
    c_noncomm_1998 = rep_len(1, Ncat),
    c_noncomm_2002 = rep_len(1, Ncat),
    c_noncomm_2005 = rep_len(1, Ncat),
    c_noncomm_2008 = rep_len(1, Ncat),
    c_noncomm_2012 = rep_len(1, Ncat),
    c_noncomm_2015 = rep_len(1, Ncat),
    c_noncomm_2016 = rep_len(1, Ncat),
    
    betaMtoF_noncomm = 0.00193,
    betaFtoM_noncomm = 0.00867,
    betaMtoF_comm = 0.00193,
    betaFtoM_comm = 0.00867,
    
    muF = 0.02597403,
    muM = 0.02739726,
    RR_beta_FtM = 1,
    RR_beta_GUD = 1,
    who_believe_comm = 0  
    
    
  )
  
  if(length(which(names(fixed_pars) %in% rownames(ranges))) > 0)
    fixed_pars <- fixed_pars[-which(names(fixed_pars) %in% rownames(ranges))]
  
  
  
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
  #   c_comm = if(Ncat == 9) matrix(c(272, 1439, 40, 64, 0, 0, 0, 0, 18.67, 37.5, 0, 0, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL)) else 
  #     matrix(rep(c(1,3), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_comm", Ncat), NULL))
  #   
  #   c_noncomm = if(Ncat == 9) matrix(c(0.2729358, 0.4682779, 0.2729358, 0.4682779, 0.90, 1.02, 0.90, 1.02, 1.21, 2.5, 1.28, 1.40, 0, 0, 0, 0, 0, 0), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_noncomm", Ncat), NULL)) else 
  #     matrix(rep(c(1,3), Ncat), nrow = Ncat, byrow = TRUE, dimnames = list(rep("c_noncomm", Ncat), NULL))
  #   
  
  old_ranges <- rbind(
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
    #     c_comm,
    #     c_noncomm,
    mu,
    
    infect_ART = c(0.1, 0.7), # infectiousness RR when on ART
    infect_acute = c(4, 18), # RR for acute phase
    
    # gamma01 = c(1/0.5, 1/0.16), # rate
    gamma01 = c(0.16, 0.5), # from Mathieu's parameters  IN YEARS
    #     gamma01 = c(2, 6.25), # from Mathieu's parameters
    
    SC_to_200_349 = c(2.2, 4.6), # seroconversion to CD4 stage 4 IN YEARS
    #     SC_to_200_349 = c(1/4.6, 1/2.2), # seroconversion to CD4 stage 4
    
    gamma04 = c(3.9, 5), # from Mathieu's parameters  IN YEARS
    #     gamma04 = c(3.9, 5), # from Mathieu's parameters
    
    ART_RR = c(2, 3), # from Mathieu's parameters  IN YEARS
    
    # omega,
    
    #below are fixed...
    S0_init,
    I01_init,
    N_init
    
    
  )
  
  if (is.null(ranges)) {
    ranges <- old_ranges
  }
  
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
  
  #   print("ehre:")
  #   print(samples_list)
  
  samples_list <- lapply(samples_list, function(x) modifyList(x, fixed_pars)) # btw earlier, fixed pars is replaced by ranges where they overlap
  
  # HERE!
  # samples_list <- lapply(samples_list, after_LHS, set_pars = set_pars)
  
  
  samples_list <- lapply(samples_list, function(x) modifyList(x, set_pars)) # set pars before fixed pars in order to get right fixed
  
  #   print("ehre2:")
  #   print(samples_list)
  samples_list_test <<- samples_list
  
  samples_list <- lapply(samples_list, fix_parameters, Ncat = Ncat)
  
  # samples_list <- lapply(samples_list, function(x) modifyList(x, set_pars)) # set pars after fixed pars in order to get right set pars
  samples_list <- lapply(samples_list, function(x) modifyList(x, forced_pars)) # set pars after fixed pars in order to get right set pars
  
  
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
                   
                   
                   
                   ART_prob_t = c(1985, 2002, 2005, 2016),
                   ART_prob_y = matrix(
                     rep(c(0, 0, 0.2, 0.4), Ncat), ncol = Ncat),
                   
                   phi2 = rep_len(0.004,Ncat), # sort out later
                   phi3 = rep_len(0.004,Ncat),
                   phi4 = rep_len(0.004,Ncat),
                   phi5 = rep_len(0.004,Ncat),
                   
                   psia = rep_len(0.1,Ncat),
                   psib = rep_len(0.1,Ncat),
                   
                   testing_prob_t = c(1985, 2001, 2005, 2006, 2008, 2012, 2013, 2015, 2016),
                   testing_prob_y = matrix(
                     rep(c(0, 0.1, 0.2, 0.4, 0.5, 0.7, 0.5, 0.8, 0.7), Ncat), ncol = Ncat),
                   
                   RR_test_onPrEP = 2,
                   RR_test_CD4200 = 2,
                   RR_ART_CD4200 = 2,
                   
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
                   
                   
                   # partner change rate
                   c_comm_1985 = rep_len(2, Ncat),
                   c_comm_1993 = rep_len(2, Ncat),
                   c_comm_1995 = rep_len(2, Ncat),
                   c_comm_1998 = rep_len(2, Ncat),
                   c_comm_2002 = rep_len(2, Ncat),
                   c_comm_2005 = rep_len(2, Ncat),
                   c_comm_2008 = rep_len(2, Ncat),
                   c_comm_2012 = rep_len(2, Ncat),
                   c_comm_2015 = rep_len(2, Ncat),
                   c_comm_2016 = rep_len(2, Ncat),
                   
                   c_noncomm_1985 = rep_len(1, Ncat),
                   c_noncomm_1993 = rep_len(1, Ncat),
                   c_noncomm_1995 = rep_len(1, Ncat),
                   c_noncomm_1998 = rep_len(1, Ncat),
                   c_noncomm_2002 = rep_len(1, Ncat),
                   c_noncomm_2005 = rep_len(1, Ncat),
                   c_noncomm_2008 = rep_len(1, Ncat),
                   c_noncomm_2012 = rep_len(1, Ncat),
                   c_noncomm_2015 = rep_len(1, Ncat),
                   c_noncomm_2016 = rep_len(1, Ncat),
                   
                   c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
                   c_t_noncomm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
                   
                   c_y_comm = matrix(2, nrow = 10, ncol = Ncat),
                   c_y_noncomm = matrix(1, nrow = 10, ncol = Ncat),
                   
                   # condoms 
                   #                    fc_t_noncomm = c(1985, 1990, 1998, 2016),
                   #                    fc_y_noncomm = matrix(
                   #                      rep(c(0, 0, 0.3, 0.5), Ncat), ncol = Ncat),
                   
                   fc_y_comm_1985 = matrix(0.2, Ncat, Ncat),
                   fc_y_comm_1993 = matrix(0.5, Ncat, Ncat),
                   fc_y_comm_1995 = matrix(0.7, Ncat, Ncat),
                   fc_y_comm_1998 = matrix(0.3, Ncat, Ncat),
                   fc_y_comm_2002 = matrix(0.4, Ncat, Ncat),
                   fc_y_comm_2005 = matrix(0.1, Ncat, Ncat),
                   fc_y_comm_2008 = matrix(0.1, Ncat, Ncat),
                   fc_y_comm_2012 = matrix(0.6, Ncat, Ncat),
                   fc_y_comm_2015 = matrix(0.4, Ncat, Ncat),
                   
                   fc_y_noncomm_1985 = matrix(0.2, Ncat, Ncat),
                   fc_y_noncomm_1998 = matrix(0.4, Ncat, Ncat),
                   fc_y_noncomm_2008 = matrix(0.3, Ncat, Ncat),
                   fc_y_noncomm_2015 = matrix(0.5, Ncat, Ncat),
                   fc_y_noncomm_2016 = matrix(0.5, Ncat, Ncat),
                   
                   
                   
                   fc_y_comm = array(0.5,dim=c(9,Ncat,Ncat)),
                   fc_y_noncomm = array(0.5,dim=c(4,Ncat,Ncat)),
                   
                   fc_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
                   #                    fc_y_comm = matrix(
                   #                      rep(c(0.5, 0.5, 0.9, 0.99), Ncat), ncol = Ncat),
                   fc_t_noncomm = c(1985, 1998, 2008, 2015, 2016),
                   #                    fc_y_comm = matrix(
                   #                      rep(c(0.5, 0.5, 0.9, 0.99), Ncat), ncol = Ncat),
                   
                   kappaa = c(0.2, rep_len(0,(Ncat-1))),
                   kappab = c(0.2, rep_len(0,(Ncat-1))),
                   kappac = c(0.2, rep_len(0,(Ncat-1))),
                   
                   
                   alpha01 = rep_len(0.01,Ncat),
                   alpha02 = rep_len(0.01,Ncat),
                   alpha03 = rep_len(0.01,Ncat),
                   alpha04 = rep_len(0.01,Ncat),
                   alpha05 = rep_len(0.3, Ncat),
                   
                   alpha11 = rep_len(0.01,Ncat),
                   
                   alpha21 = rep_len(0.01,Ncat),
                   alpha22 = rep_len(0.01,Ncat),
                   alpha23 = rep_len(0.01,Ncat),
                   alpha24 = rep_len(0.01,Ncat),
                   alpha25 = rep_len(0.3,Ncat),
                   
                   alpha32 = rep_len(0.01,Ncat),
                   alpha33 = rep_len(0.01,Ncat),
                   alpha34 = rep_len(0.01,Ncat),
                   alpha35 = rep_len(0.3,Ncat),
                   
                   alpha42 = rep_len(0.01,Ncat),
                   alpha43 = rep_len(0.01,Ncat),
                   alpha44 = rep_len(0.01,Ncat),
                   alpha45 = rep_len(0.3,Ncat),
                   
                   
                   beta = rep_len(0.005,Ncat),
                   betaMtoF = 0.00193,
                   betaFtoM = 0.00867,
                   
                   betaMtoF_noncomm = 0.00193,
                   betaFtoM_noncomm = 0.00867,
                   betaMtoF_comm = 0.00193,
                   betaFtoM_comm = 0.00867,
                   
                   #beta = 0,
                   
                   p_comm = matrix(1, ncol = Ncat, nrow = Ncat),
                   p_noncomm = matrix(1, ncol = Ncat, nrow = Ncat),
                   
                   
                   ec = rep_len(0.8,Ncat),
                   # ec = rep_len(1,1),
                   
                   
                   
                   
                   epsilon = 0.001,
                   
                   c_comm = rep_len(2, Ncat),
                   c_noncomm = rep_len(2, Ncat), 
                   #                    c_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
                   #                    c_y_comm = matrix(rep(c(1, 2, 3, 1, 3, 4, 2, 1, 2, 4), Ncat), ncol = Ncat),
                   #                    
                   #                    c_t_noncomm = c(1985, 1990, 1998, 2016),
                   #                    c_y_noncomm = matrix(rep(c(0.5, 1, 0.7, 0.9), Ncat), ncol = Ncat),
                   
                   
                   
                   fP_t_comm = c(1985, 2014, 2015, 2016),
                   fP_y_comm = matrix(
                     rep(c(1, 1, 1, 1), Ncat), ncol = Ncat),
                   
                   
                   
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
                   infect_AIDS = 7.27, # RR for AIDS phase
                   
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
                   fraction_FSW_foreign = 0.5,
                   replaceDeaths = 0,
                   movement = 1,
                   beta_comm = rep_len(0.002, Ncat),
                   
                   beta_noncomm = rep_len(0.001, Ncat),
                   muF = 0.02597403,
                   muM = 0.02739726,
                   RR_beta_GUD = 1,
                   RR_beta_FtM = 1,
                   who_believe_comm = 0
                   
                   
  )
  
  
  
  if (length(parameters) == 0L) {
    return(defaults)
  }
  
  if (is.null(names(parameters)) || !all(nzchar(names(parameters)))) {
    stop("All arguments must be named")
  }
  #   extra <- setdiff(names(parameters), names(defaults))
  #   if (length(extra) > 0L) {
  #     stop("Unknown arguments: ", extra)
  #   }
  
  # list of parameters that depend on others
  ret <- modifyList(defaults, parameters)
  
  if (length(set_null) > 0L) {
    ret = modifyList(ret, lapply(ret[match(set_null, names(ret))], function(x) x*0))
  }
  
  ret
}
