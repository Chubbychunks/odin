# helper files have functions that will be used a lot in the model tests

time <- seq(1986, 2016, length.out = 31)

# dont know what this is
# takes the new varying parameter and it meshes it with the original base parameter set
# f <- function(x, base, v, gen, time) {
#   n <- lengths(base[v])
#   base[v] <- setNames(split(unlist(x, use.names=FALSE), rep(seq_along(v), n)), v) # base is the original set of pars
#   mod <- gen(user=base)
#   mod$transform_variables(mod$run(time))
# }



run_model <- function(p, gen, time, output_vars) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[output_vars]
}


par_gridplot = function(result, parm) {require(plyr)
  fc_df = aperm(result[parm][[1]], c(2, 3, 1))
  fc_df_list = alply(fc_df, 3)
  
  fc_df_list = lapply(fc_df_list, function(x) {colnames(x) = rownames(x) = c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                                                                             "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
  return(x)})
  
  dat = data.frame(combination = unlist(lapply(fc_df_list, function(x) {
    tmp = expand.grid(colnames(x), rownames(x))
    return(paste(tmp[,2], tmp[,1], sep = "_"))
  })),
  value = unlist(lapply(fc_df_list, c)),
  year = unlist(sort(rep(time, 81))))
  
  dat$combination = factor(dat$combination, levels = 
                             unlist(lapply(fc_df_list, function(x) {
                               tmp = expand.grid(colnames(x), rownames(x))
                               return(paste(tmp[,2], tmp[,1], sep = "_"))
                             }))[1:81])
  
  return(ggplot(dat, aes(x = year, y = value, color = value)) + geom_line(size = 2) + facet_wrap(~combination) + theme_bw())
}


par_gridplot2 = function(result, parm) {
  require(plyr)
  fc_df = aperm(result[parm][[1]], c(2, 3, 1))
  fc_df_list = alply(fc_df, 3)
  
  fc_df_list_applied = lapply(fc_df_list, function(x) {colnames(x) = rownames(x) = c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                                                                             "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
  return(x)})
  
  dat = data.frame(row = 
                     rep(c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                           "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou"), 1, each = 9),
                   col = 
                     rep(c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                           "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou"), 9),
  value = unlist(lapply(fc_df_list_applied, c)),
  year = unlist(sort(rep(time, 81))))
  dat$row = factor(dat$row, levels = c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                                       "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou"))
  dat$col = factor(dat$col, levels = c("Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients",
                                       "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou"))
  
#   dat$combination = factor(dat$combination, levels = 
#                              unlist(lapply(fc_df_list, function(x) {
#                                tmp = expand.grid(colnames(x), rownames(x))
#                                return(paste(tmp[,2], tmp[,1], sep = "_"))
#                              }))[1:81])
  
  return(ggplot(dat, aes(x = year, y = value, color = value)) + geom_line(size = 2) + facet_grid(row~col) + theme_bw())
}



parameter_names = names(lhs_parameters(1)[[1]])


best_set = list(
  prev_init_FSW = 0.0326,
  prev_init_rest = 0.0012,
  N_init = c(672, 757, 130895, 672, 27124, 100305, 14544, 11145, 0),
  fraction_F = 0.515666224,
  epsilon_1985 = 0.059346131 * 1.5,
  epsilon_1992 = 0.053594832 * 1.5,
  epsilon_2002 = 0.026936907 * 1.5,
  epsilon_2013 = 0.026936907 * 1.5,
  epsilon_2016 = 0.026936907 * 1.5,
  # mu = c(0.02597403, 0.02597403, 0.02597403, 0.02597403, 0.02739726, 0.02739726, 0.02597403, 0.02739726, 0.02597403), # women 1/((27 + 50)/2) # men 1/((25 +  48)/2)
  #   c_comm = c(750, 52, 0, 0, 13.5, 0, 0, 0, 0),
  #   c_noncomm = c(0.38, 0.38, 0.88, 0.88, 4, 1.065, 0, 0, 0), # partner change rate lowlevel FSW same as pro, others are approximations from various surveys
  #   
  muF = 0.02597403,
  muM = 0.02739726,
  # PARTNER CHANGE RATE
  c_comm_1985 = c(1229.5, 52, 0, 0, 10.15873, 0, 0, 0, 0), # (1020 + 1439)/2
  c_comm_1993 = c(1229.5, 52, 0, 0, 10.15873, 0, 0, 0, 0), # (1020 + 1439)/2
  c_comm_1995 = c(1280, 52, 0, 0, 10.15873, 0, 0, 0, 0), # (1135 + 1425)/2
  c_comm_1998 = c(881, 52, 0, 0, 10.15873, 0, 0, 0, 0), # (757 + 1005)/2
  c_comm_2002 = c(598.5, 52, 0, 0, 11.08109, 0, 0, 0, 0), # (498 + 699)/2, (13.387-10.15873)/14 * 4 + 10.15873
  c_comm_2005 = c(424, 52, 0, 0, 11.77286, 0, 0, 0, 0), # (366 + 482)/2, (13.387-10.15873)/14 * 7 + 10.15873
  c_comm_2008 = c(371.5, 52, 0, 0, 12.46464, 0, 0, 0, 0), # (272 + 471)/2, (13.387-10.15873)/14 * 10 + 10.15873
  c_comm_2012 = c(541, 52, 0, 0, 13.387, 0, 0, 0, 0), # (459 + 623)/2
  c_comm_2015 = c(400, 52, 0, 0, 17.15294, 0, 0, 0, 0), # (309 + 491)/2
  c_comm_2016 = c(400, 52, 0, 0, 17.15294, 0, 0, 0, 0), # (309 + 491)/2
  
  c_noncomm_1985 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), # (0.4682779 + 0.3886719 + 0.2729358)/3
  c_noncomm_1993 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), 
  c_noncomm_1995 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), 
  c_noncomm_1998 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), 
  c_noncomm_2002 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), 
  c_noncomm_2005 = c(0.3766285, 0.3766285, 0.9610526, 0.9610526, 2.028986, 1.337444, 0, 0, 0), 
  c_noncomm_2008 = c(0.3766285, 0.3766285, 0.7943578, 0.7943578, 2.028986, 0.7878543, 0, 0, 0), 
  c_noncomm_2012 = c(0.3766285, 0.3766285, 0.7943578, 0.7943578, 8.086957, 0.7878543, 0, 0, 0), 
  c_noncomm_2015 = c(0.3766285, 0.3766285, 0.7943578, 0.7943578, 6.258258, 0.7878543, 0, 0, 0), 
  c_noncomm_2016 = c(0.3766285, 0.3766285, 0.7943578, 0.7943578, 6.258258, 0.7878543, 0, 0, 0), 
  
  
  
  n_comm = matrix(c(0, 0, 0, 0, 1.935, 0, 0, 0, 0, # from client sa per partner
                    0, 0, 0, 0, 1.935, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    1.935, 1.935, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0),
                  nrow = 9, ncol = 9, byrow = T),
  n_noncomm = matrix(c(0, 0, 0, 0, 32.7, 0, 0, 0, 0,
                       0, 0, 0, 0, 32.7, 0, 0, 0, 0, # could replace lowlevel with bargirls parameters
                       0, 0, 0, 0, 39, 37.875, 0, 0, 0, #(36.75+39)/2
                       0, 0, 0, 0, 39, 37.875, 0, 0, 0,
                       32.7, 32.7, 39, 39, 0, 0, 0, 0, 0,
                       0, 0, 37.875, 37.875, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0),
                     nrow = 9, ncol = 9, byrow = T),
  #think about transforming to matrix  
  betaMtoF_comm = 0.00051, # RR circumcision = 0.44
  betaFtoM_comm = 0.02442*0.44,
  betaMtoF_noncomm = 0.003,
  betaFtoM_noncomm = 0.0038*0.44,
  
  infect_acute = 9, # RR for acute phase
  infect_AIDS = 2, #7.27, # RR for AIDS phase
  infect_ART = 0.9 * 0.523, # infectiousness RR when on ART (efficacy ART assuimed 90% * % undetectable which is 52.3%)
  ec = rep_len(0.8, 9), # from kate's paper on nigeria SD couples
  eP0 = c(0, rep_len(0, 8)), # assumptions!
  eP1a = c(0.9, rep_len(0, 8)),
  eP1b = c(0.45, rep_len(0, 8)),
  eP1c = c(0, rep_len(0, 8)),
  eP1d = c(0, rep_len(0, 8)),
  gamma01 = 0.4166667, #years
  SC_to_200_349 = 3.4,
  gamma04 = 4.45, #years
  
  
  alpha01 = rep_len(0, 9),
  alpha02 = rep_len(0, 9),
  alpha03 = rep_len(0.05, 9),
  alpha04 = rep_len(0.08, 9),
  alpha05 = rep_len(0.27, 9), #1/2.9
  alpha11 = rep_len(0, 9),
  alpha22 = rep_len(0, 9),
  alpha23 = rep_len(0.05, 9),
  alpha24 = rep_len(0.08, 9),
  alpha25 = rep_len(0.27, 9),
  alpha32 = rep_len(0, 9),
  alpha33 = rep_len(0.05, 9),
  alpha34 = rep_len(0.08, 9),
  alpha35 = rep_len(0.27, 9),
  alpha42 = rep_len(0, 9),
  alpha43 = rep_len(0.05, 9),
  alpha44 = rep_len(0.08, 9),
  alpha45 = rep_len(0.27, 9),
  
  
  #PREP
  zetaa_t = c(1985, 2013, 2015, 2016),
  zetaa_y = matrix(c(rep(0, 9), 0.0075, rep(0, 9-1), rep(0, 9), rep(0, 9)), ncol = 9, byrow = T),
  zetab_t = c(1985, 2013, 2015, 2016),
  zetab_y = matrix(c(rep(0, 9), 0.0075, rep(0, 9-1), rep(0, 9), rep(0, 9)), ncol = 9, byrow = T),                   
  zetac_t = c(1985, 2013, 2015, 2016),
  zetac_y = matrix(c(rep(0, 9), 0.0075, rep(0, 9-1), rep(0, 9), rep(0, 9)), ncol = 9, byrow = T),  
  psia = rep_len(0.1,9),
  psib = rep_len(0.1,9),
  
  #TESTING
  testing_prob_t = c(1985, 2001, 2005, 2006, 2008, 2012, 2013, 2015, 2016),
  testing_prob_y = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, # 1985 columns are the risk groups
                            0, 0, 0, 0, 0, 0, 0, 0, 0, # 2001
                            0, 0, 0, 0, 0, 0, 0, 0, 0, # 2005
                            0.081625, 0.142, 0.142, 0.142, 0.0975, 0.0975, 0, 0, 0, # 2006 0.653/8 slope
                            0.244875, 0.21, 0.21, 0.21, 0.1, 0.1, 0, 0, 0, # 2008 3*0.653/8
                            0.571375, 0.331, 0.331, 0.331, 0.0582, 0.0582, 0, 0, 0, # 2012 7*0.653/8
                            0.653, 0.331, 0.331, 0.331, 0.0582, 0.0582, 0, 0, 0, # 2013
                            0.68, 0.331, 0.331, 0.331, 0.0582, 0.0582, 0, 0, 0, # 2015
                            0.68, 0.331, 0.331, 0.331, 0.0582, 0.0582, 0, 0, 0), # 2016
                          nrow = 9, ncol = 9, byrow = T),
  #ART
  ART_prob_t = c(1985, 2002, 2005, 2016),
  ART_prob_y = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, # 1985
                        0, 0, 0, 0, 0, 0, 0, 0, 0, # 2002
                        0, 0.1448571, 0.1448571, 0.1448571, 0.1448571, 0.1448571, 0, 0, 0, # 2005 0.676/14 * 3
                        0.6739, 0.676, 0.676, 0.676, 0.676, 0.676, 0, 0, 0),
                      nrow = 4, ncol = 9, byrow = T), # 2016 GP: (0.8+0.552)/2
  RR_ART_CD4200 = 5.39,
  phi2 = c(0.105360516, rep_len(0.025,8)), # former sex workers drop out rate??!
  phi3 = c(0.105360516, rep_len(0.025,8)),
  phi4 = c(0.105360516, rep_len(0.025,8)),
  phi5 = c(0.105360516, rep_len(0.025,8)),
  ART_RR = (1.3+3.45)/2,
  
  #CONDOM
  
  fc_y_comm_1985 = matrix(
    c(0, 0, 0, 0, 0.145524, 0, 0, 0, 0, # 0.145524 is using John's FSW condom 1989 as prop of 1993, * our measure of 1993
      0, 0, 0, 0, 0.145524, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.145524, 0.145524, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_1993 = matrix(
    c(0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.536, 0.536, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_1995 = matrix(
    c(0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.536, 0.536, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_1998 = matrix(
    c(0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0.536, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.536, 0.536, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_2002 = matrix(
    c(0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.8, 0.8, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_2005 = matrix(
    c(0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.8, 0.8, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_2008 = matrix(
    c(0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.8, 0.8, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_2012 = matrix(
    c(0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.8, 0.8, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_comm_2015 = matrix(
    c(0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0.8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0.8, 0.8, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_noncomm_1985 = matrix(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  # 1998
  # (0.33 + 0.2705314)/ 2 # average FSW client
  # (0.0326087 + 0.2705314)/ 2 # average client GPF
  # (0.0326087 + 0.04989035) / 2 # average gpm gpf
  
  fc_y_noncomm_1998 = matrix(
    c(0, 0, 0, 0, 0.3002657, 0, 0, 0, 0,
      0, 0, 0, 0, 0.3002657, 0, 0, 0, 0,
      0, 0, 0, 0, 0.15157, 0.04124952, 0, 0, 0,
      0, 0, 0, 0, 0.15157, 0.04124952, 0, 0, 0,
      0.3002657, 0.3002657, 0.15157, 0.15157, 0, 0, 0, 0, 0,
      0, 0, 0.04124952, 0.04124952, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  # 2008
  # (0.33 + 0.4)/ 2 # average FSW client (both approx)
  # ((0.05042017+0.241404781)/2 + 0.4)/ 2 # average client GPF (gpf averaged from 2 estimtes)
  # ((0.05042017+0.241404781)/2 + (0.07103825+0.34838295)/2) / 2 # average gpm gpf
  
  fc_y_noncomm_2008 = matrix(
    c(0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0.365, 0.365, 0.2729562, 0.2729562, 0, 0, 0, 0, 0,
      0, 0, 0.1778115, 0.1778115, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_noncomm_2015 = matrix(
    c(0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0.365, 0.365, 0.2729562, 0.2729562, 0, 0, 0, 0, 0,
      0, 0, 0.1778115, 0.1778115, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  fc_y_noncomm_2016 = matrix(
    c(0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.365, 0, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0, 0, 0, 0, 0.2729562, 0.1778115, 0, 0, 0,
      0.365, 0.365, 0.2729562, 0.2729562, 0, 0, 0, 0, 0,
      0, 0, 0.1778115, 0.1778115, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  
  
  
  fc_t_comm = c(1985, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015, 2016),
  
  fc_t_noncomm = c(1985, 1990, 1998, 2015, 2016),
  
  
  rate_leave_pro_FSW = 0.2,
  FSW_leave_Cotonou_fraction = 0.1,
  rate_leave_low_FSW = 0.1,
  rate_leave_client = 0.05,
  replaceDeaths = 0,
  movement = 1
  
)