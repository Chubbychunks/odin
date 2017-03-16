require(ggplot2)
require(reshape2)

#

# to debug:
# open terminal in the folder
# R- d valgrind

# some parameters to be dependent on others which have been sampled
# ART to be defined by nubmers, not rates

rm(list = ls())
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

devtools::test()

time <- seq(1986, 2016, length.out = 31)
parameters <- lhs_parameters(1,Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)


##### FIXED BEST PARAMETER SET LIST!
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
# parameters <- lhs_parameters(1,Ncat = 2)[[1]]


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
  infect_AIDS = 7.27, # RR for AIDS phase
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
  rate_leave_client = 0.05
  
)


# single run (prev)
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")
# FOI
FOI <- result["lambda_sum_0"][[1]]
df = data.frame(time, FOI)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value)) + labs(y = "force of infection on susceptibles (no PrEP)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")
# mortality
alphaItot <- result["alphaItot"][[1]]
df = data.frame(time, alphaItot)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value)) + labs(y = "Annual AIDS deaths") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")

1/parameters$mu



#try to fit to prevalence data
parameters = lhs_parameters(1, set_pars = best_set, 
                            forced_pars = list(#betaFtoM_comm = 0.00193, betaFtoM_noncomm = 0.00193, # infect_AIDS = 1,
                                               c_comm_1993 = c(1229.5, 52, 0, 0, 20, 0, 0, 0, 0),
                                               c_comm_1995 = c(1280, 52, 0, 0, 10, 0, 0, 0, 0), 
                                               c_comm_1998 = c(881, 52, 0, 0, 8, 0, 0, 0, 0),
                                               c_comm_2002 = c(598.5, 52, 0, 0, 8, 0, 0, 0, 0), 
                                               c_comm_2005 = c(424, 52, 0, 0, 8, 0, 0, 0, 0), 
                                               c_comm_2008 = c(371.5, 52, 0, 0, 8, 0, 0, 0, 0),
                                               c_comm_2012 = c(541, 52, 0, 0, 8, 0, 0, 0, 0),
                                               c_comm_2015 = c(400, 52, 0, 0, 8, 0, 0, 0, 0),
                                               c_comm_2016 = c(400, 52, 0, 0, 8, 0, 0, 0, 0),
                                               testing_prob_y = matrix(0, # 2016
                                                                       nrow = 9, ncol = 9, byrow = T),
                                               rate_enter_sexual_pop = 0.4
#                                                ,
#                                                epsilon_2002 = 0.05 * 1.5,
#                                                epsilon_2013 = 0.05 * 1.5,
#                                                epsilon_2016 = 0.05 * 1.5
                                               ),
                            Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
prev_points = data.frame(time = c(1986, 1987, 1988, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015,1998, 2012, 2015,1998, 2008, 1998, 2008,2012, 2015),variable = c(rep("Pro FSW", 11), rep("Clients", 3), rep("GPF", 2), rep("GPM", 2), rep("Low-level FSW", 2)),value = c(3.3, 8.2, 19.2, 53.3, 48.7, 40.6, 38.9, 34.8, 29.3, 27.4, 18.7,100*0.084, 100*0.028, 100*0.016,100*0.035, 100*0.04,100*0.033, 100*0.02,100*0.167, 100*0.065),upper = c(3.3, 8.2, 19.2, 58.48, 54.42, 44.67, 46.27, 39.38, 33.88, 32.23, 22.01,100*0.11561791, 100*0.051602442, 100*0.035338436,100*0.047726245, 100*0.052817187,100*0.047183668, 100*0.029774338,100*0.268127672, 100*0.130153465),lower = c(3.3, 8.2, 19.2, 48.02, 43.02, 36.58, 31.97, 30.42, 24.93, 23.01, 15.71,100*0.05898524, 100*0.012660836, 100*0.006039259,100*0.024181624, 100*0.030073668,100*0.022857312, 100*0.012427931,100*0.091838441, 100*0.026704897))
ggplot()  + geom_line(data = df, aes(x = time, y = value)) + theme_bw() + labs(x="year",y="prevalance (%)") +
  geom_point(data = prev_points, aes(x = time, y = value))+ geom_errorbar(data = prev_points, aes(x = time, ymin = lower, ymax = upper))+ 
  facet_wrap(~variable, scales = "free") 
data.frame(time,result$c_comm_balanced)
plot(data.frame(year=time, fraction_virgin=result$frac_virgin))
plot(data.frame(year=time, sum_of_weighted_FOI = rowSums(result$lambda_sum_0 * result$frac_N)))


# average life duration
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
1/(parameters$gamma01+parameters$alpha01+parameters$mu) + 
  1/(parameters$gamma02+parameters$alpha02+parameters$mu) + 
  1/(parameters$gamma03+parameters$alpha03+parameters$mu) + 
  1/(parameters$gamma04+parameters$alpha04+parameters$mu) + 
  1/(parameters$alpha05+parameters$mu)

#on ART
1/(parameters$gamma01+parameters$alpha01+parameters$mu) + 
  1/(parameters$gamma32+parameters$alpha32+parameters$mu) + 
  1/(parameters$gamma33+parameters$alpha33+parameters$mu) + 
  1/(parameters$gamma34+parameters$alpha34+parameters$mu) + 
  1/(parameters$alpha35+parameters$mu)



# fitting betas
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, betaMtoF = 0.00193, betaFtoM = 0.00193, infect_AIDS = 1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
prev_points = data.frame(time = c(1986, 1987, 1988, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015,
                                  1998, 2012, 2015,
                                  1998, 2008, 
                                  1998, 2008,
                                  2012, 2015),
                         variable = c(rep("Pro FSW", 11), rep("Clients", 3), rep("GPF", 2), rep("GPM", 2), rep("Low-level FSW", 2)),
                         value = c(3.3, 8.2, 19.2, 53.3, 48.7, 40.6, 38.9, 34.8, 29.3, 27.4, 18.7,
                                   100*0.084, 100*0.028, 100*0.016,
                                   100*0.035, 100*0.04,
                                   100*0.033, 100*0.02,
                                   100*0.167, 100*0.065),
                         upper = c(3.3, 8.2, 19.2, 58.48, 54.42, 44.67, 46.27, 39.38, 33.88, 32.23, 22.01,
                                   100*0.11561791, 100*0.051602442, 100*0.035338436,
                                   100*0.047726245, 100*0.052817187,
                                   100*0.047183668, 100*0.029774338,
                                   100*0.268127672, 100*0.130153465),
                         lower = c(3.3, 8.2, 19.2, 48.02, 43.02, 36.58, 31.97, 30.42, 24.93, 23.01, 15.71,
                                   100*0.05898524, 100*0.012660836, 100*0.006039259,
                                   100*0.024181624, 100*0.030073668,
                                   100*0.022857312, 100*0.012427931,
                                   100*0.091838441, 100*0.026704897))
ggplot()  + geom_line(data = df, aes(x = time, y = value)) + theme_bw() + labs(x="year",y="prevalance (%)") +
  geom_point(data = prev_points, aes(x = time, y = value))+ geom_errorbar(data = prev_points, aes(x = time, ymin = lower, ymax = upper))+ 
  facet_wrap(~variable, scales = "free") 

plot(time, result$c_comm_balanced[,1])
plot(time, result$fc_comm[,1,5])



# rho
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["rho"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "ART rate") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")


# N
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["N"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "N") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")



# tau
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["tau"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Testing rate") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")


# test c_comm
parameters = lhs_parameters(1, c_comm_1985 = rep_len(1.1, 9), Ncat = 9)[[1]]


# new people
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["new_people_in_group"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")



# fc
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["fc_comm"][[1]][,1,5]
df = data.frame(time, yy)
plot(df)
yy <- result["fc_noncomm"][[1]][,1,5]
df = data.frame(time, yy)
lines(df)

par_gridplot2(result = result, "fc_comm")

# frac_F
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
frac_F <- result["frac_F"][[1]][,1]
df = data.frame(time, frac_F)
plot(df) 

parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, betaMtoF = 0, betaFtoM = 0)[[1]]
result = run_model(parameters, main_model, time)
frac_F <- result["frac_F"][[1]][,1]
df = data.frame(time, frac_F)
lines(df) 


# Ntot
parameters = lhs_parameters(1, Ncat = 9, betaMtoF = 0, betaFtoM = 0)[[1]]
result = run_model(parameters, main_model, time)
# result$prev
# result$Ntot
# result$new_people
# result$Ntot_inc_former_FSW_nonCot
result$epsilon
plot(result$Ntot_inc_former_FSW_nonCot)

parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, betaMtoF = 0, betaFtoM = 0)[[1]]
result = run_model(parameters, main_model, time)
# result$prev
# result$Ntot
# result$new_people
# result$Ntot_inc_former_FSW_nonCot
result$epsilon

lines(result$Ntot_inc_former_FSW_nonCot)



# single run (prev)
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")

# no transmission
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, betaMtoF = 0, betaFtoM = 0)[[1]]
result = run_model(parameters, main_model, time)
data.frame(time, result$frac_N)
result$prev
result$Ntot


# frac virgin
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, forced_pars = list(rate_enter_sexual_pop = .4))[[1]]
result = run_model(parameters, main_model, time)
fraction_virgin <- result["frac_virgin"][[1]]
df = data.frame(time, fraction_virgin);plot(df)


# frac N
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["frac_N"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "frac N") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")





# c_noncomm
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, forced_pars = list(c_noncomm = c(0.38, 0.38, 0.88, 0.88, 1, 1.065, 0, 0, 0)))[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")




### variations of beta
betaseq = c("point estimates", "lower bounds", "upper bounds")
betaMtoFseq = c(0.00193, 0.00086, 0.00433)
betaFtoMseq = c(0.00867, 0.00279, 0.0279)

yy = data.frame()
for(i in 1:3)
{
  run = betaseq[i]
  parameters = lhs_parameters(1, set_pars = best_set, forced_pars = list(betaMtoF_comm = betaMtoFseq[i], betaFtoM_comm = betaFtoMseq[i], betaMtoF_noncomm = betaMtoFseq[i], betaFtoM_noncomm = betaFtoMseq[i]
                                                                         #                                                                          , n_comm = matrix(c(0, 0, 0, 0, 1.935, 0, 0, 0, 0, # from client sa per partner
                                                                         #                                                                                                                                                             0, 0, 0, 0, 1.935, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             1.935, 1.935, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                         #                                                                                                                                                             0, 0, 0, 0, 0, 0, 0, 0, 0),
                                                                         #                                                                                                                                                           nrow = 9, ncol = 9, byrow = T)
  ), Ncat = 9)[[1]]
  result = run_model(parameters, main_model, time)
  yy <- rbind(yy, data.frame(time = time, result["prev"][[1]], replication = betaseq[i]))
}
colnames(yy) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou", "replication")
yy_melted = melt(yy, id.vars = c("time", "replication"))

ggplot(data = yy_melted, aes(x = time, y = value, color = replication)) + facet_wrap(~variable, scales = "free") + geom_line() + theme_bw() + labs(x="year",y="prevalance (%)")


prev_points = data.frame(time = c(1986, 1987, 1988, 1993, 1995, 1998, 2002, 2005, 2008, 2012, 2015,
                                  1998, 2012, 2015,
                                  1998, 2008, 
                                  1998, 2008,
                                  2012, 2015),
                         variable = c(rep("Pro FSW", 11), rep("Clients", 3), rep("GPF", 2), rep("GPM", 2), rep("Low-level FSW", 2)),
                         value = c(3.3, 8.2, 19.2, 53.3, 48.7, 40.6, 38.9, 34.8, 29.3, 27.4, 18.7,
                                   100*0.084, 100*0.028, 100*0.016,
                                   100*0.035, 100*0.04,
                                   100*0.033, 100*0.02,
                                   100*0.167, 100*0.065),
                         upper = c(3.3, 8.2, 19.2, 58.48, 54.42, 44.67, 46.27, 39.38, 33.88, 32.23, 22.01,
                                   100*0.11561791, 100*0.051602442, 100*0.035338436,
                                   100*0.047726245, 100*0.052817187,
                                   100*0.047183668, 100*0.029774338,
                                   100*0.268127672, 100*0.130153465),
                         lower = c(3.3, 8.2, 19.2, 48.02, 43.02, 36.58, 31.97, 30.42, 24.93, 23.01, 15.71,
                                   100*0.05898524, 100*0.012660836, 100*0.006039259,
                                   100*0.024181624, 100*0.030073668,
                                   100*0.022857312, 100*0.012427931,
                                   100*0.091838441, 100*0.026704897))
ggplot()  + geom_line(data = yy_melted, aes(x = time, y = value, color = replication)) + theme_bw() + labs(x="year",y="prevalance (%)") +
  geom_point(data = prev_points, aes(x = time, y = value))+ geom_errorbar(data = prev_points, aes(x = time, ymin = lower, ymax = upper))+ 
  facet_wrap(~variable, scales = "free") 


#c_comm
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, forced_pars = list(c_comm = c(750, 52, 0, 0, 5, 0, 0, 0, 0)))[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")+
  geom_point(data = points, aes(x = time, y = value))+ geom_errorbar(data = points, aes(x = time, ymin = lower, ymax = upper))





#beta 0
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9, forced_pars = list(betaMtoF = 0, betaFtoM = 0))[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")




#more condoms run
parameters = lhs_parameters(1, set_pars = best_set, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")

best_set_2 = best_set

best_set_2$fc_y_comm_1993 = best_set$fc_y_comm_1993*1.8
best_set_2$fc_y_comm_1995 = best_set$fc_y_comm_1995*1.8
best_set_2$fc_y_comm_1998 = best_set$fc_y_comm_1998*1.8

best_set_2$fc_y_noncomm_1998 = best_set$fc_y_noncomm_1998*1.8
best_set_2$fc_y_noncomm_2008 = best_set$fc_y_noncomm_2008*1.8
best_set_2$fc_y_noncomm_2015 = best_set$fc_y_noncomm_2015*1.8

parameters = lhs_parameters(1, set_pars = best_set_2, Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)
yy <- result["prev"][[1]]
df = data.frame(time, yy)
names(df) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Virgin female", "Virgin male", "Former FSW outside Cotonou")
df = melt(df, id.vars = "time")
ggplot(data = df, aes(x = time, y = value, color = variable)) + labs(y = "Prevalence (%)") + geom_line() + theme_bw() + facet_wrap(~variable, scales = "free")+ theme(legend.position="none")


# parameters[!(parameters %in% parameters1)]
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################





out=data.frame(time=result$t,output=result$Ntot)
out

ggplot(out, aes(x = time, y = output)) + geom_line() + theme_bw()


# no zetas
parameters <- lhs_parameters(1,Ncat = 9, set_null = list("zetaa_y", "zetab_y", "zetac_y"))[[1]]





# test
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()
time <- seq(1986, 2016, length.out = 31)

parameters <- lhs_parameters(1,Ncat = 9, movement = 0)[[1]]
result = run_model(parameters, main_model, time)




# prev of all groups
#### Ncat = 9
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

number_simulations = 25
parms = lhs_parameters(number_simulations, Ncat = 9)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

out = data.frame(time, do.call(rbind, lapply(res, function(x) x$prev)), as.character(sort(rep(seq(1,number_simulations), length(time)))))
names(out) = c("time", "Pro FSW", "Low-level FSW", "GPF", "Former FSW in Cotonou", "Clients", "GPM", "Former FSW outside Cotonou", "replication")
out_melted = melt(out, id.vars = c("time", "replication"))
ggplot(data = out_melted, aes(x = time, y = value, factor = replication, color = variable)) + geom_line() + theme_bw() + facet_wrap(~variable)+ theme(legend.position="none")



























result$prev
result$c_comm_balanced
result$frac_N
result$omega

# showing priors are flat and explore space
result$c_comm

parameters <- lhs_parameters(2000, Ncat = 9)
parm_prior1 = do.call(rbind, lapply(parameters, function(x) x$mu))
parm_prior2 = do.call(rbind, lapply(parameters, function(x) x$betaMtoF))

hist(parm_prior1[,1])
ggplot(data= data.frame(parm_prior1, parm_prior2), aes (x= parm_prior1[,1], y= parm_prior2)) + geom_point() + theme_bw()

# test prep

odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

time <- seq(1986, 2016, length.out = 31)
parameters <- lhs_parameters(1,Ncat = 9)[[1]]
result = run_model(parameters, main_model, time)

df=melt(data.frame(time, a = result$S1a[,1], b = result$S1b[,1], c = result$S1c[,1], d = result$S1d[,1]), id.vars = "time")
ggplot(df, aes(x = time, y = value, color = variable)) + geom_line() + theme_bw()





# balancing
parameters <- generate_parameters(Ncat = 9, c_comm=c(1244,52,0,0,24,0,0), c_noncomm=c(0.377,0.96,0.96,0.96,2.03,1.34,0),
                                  omega = c(1000, 1127, 143728, 500, 27323, 112436, 10)/(1000+1127+143728+500+27323+112436),
                                  S0_init = c(1000, 1127, 143728, 500, 27323, 112436, 10)*0.99,
                                  I01_init = c(1000, 1127, 143728, 500, 27323, 112436, 10)*0.01)
result = run_model(parameters, main_model, time)
result$N

result$B_check_comm
result$B_check_noncomm




names(result)

result$Ncat
result$c_comm
result$c_comm_balanced
result$c_noncomm
result$c_noncomm_balanced
result$N

result$S0[1,,]

result$sum_in_S0


result$B_check_comm
result$B_check_noncomm
#

# mixing




odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

parameters <- lhs_parameters(1,Ncat = 9)[[1]]
time <- seq(1986, 2016, length.out = 31)
result = run_model(parameters, main_model, time)
result$M_comm[2,,]
result$M_noncomm[2,,]
result$p_comm[2,,]
result$p_noncomm[2,,]


# need to write general test solutions. done! general test below
timestep = 3
all_female_partnerships = 0; all_male_partnerships = 0;
for(i in 1:4)
  for (j in 1:7)
    all_female_partnerships = all_female_partnerships + result$p_comm[timestep, i, j] * result$c_comm_balanced[timestep,i] * result$N[timestep,i]

for(i in 5:6)
  for (j in 1:7)
    all_male_partnerships = all_male_partnerships + result$p_comm[timestep, i, j] * result$c_comm_balanced[timestep,i] * result$N[timestep,i]
all_female_partnerships/all_male_partnerships
#nonnoncomm check
# need to write general test solutions. done! general test below
timestep = 3
all_female_partnerships = 0; all_male_partnerships = 0;
for(i in 1:4)
  for (j in 1:7)
    all_female_partnerships = all_female_partnerships + result$p_noncomm[timestep, i, j] * result$c_noncomm_balanced[timestep,i] * result$N[timestep,i]

for(i in 5:6)
  for (j in 1:7)
    all_male_partnerships = all_male_partnerships + result$p_noncomm[timestep, i, j] * result$c_noncomm_balanced[timestep,i] * result$N[timestep,i]
all_female_partnerships/all_male_partnerships



#below to tset for PrEP
parameters <- generate_parameters(zetaa = c(0,0),zetab = c(0,0),zetac = c(0,0), psia = c(1, 1), psib = c(1, 1))
result = run_model(parameters, main_model, time)

result$OnPrEP
result$t

result$PrEP_0
result$PrEP_1a

result$S1a
result$S1b
result$S1c

result$lambda_0[10,,]
result$lambda_1a[10,,]
result$lambda_1b[10,,]
result$lambda_1c[10,,]

result$lambda_sum_0[2,]
result$lambda_sum_1a[2,]
result$lambda_sum_1b[2,]
result$lambda_sum_1c[2,]


# result$N
# result$c[1,,]
# result$cstar[1,,]
# result$B[1,,]
# result$theta[1,,]

# RUNNING MULTIPLE SIMULATIONS

number_simulations = 2000

parms = lhs_parameters(number_simulations)

time <- seq(1986, 2016, length.out = 31)

# parameter ranges

f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("mu","fc_comm","betaMtoF","prev", "cumuInftot")]
}
res = lapply(parms, f, main_model, time)
mu_input <- t(sapply(parms, "[[", "mu"))
fc_y_input <- t(sapply(parms, "[[", "fc_y_comm"))
prev_client_output <- t(sapply(res, "[[", "prev_client"))
betaMtoF_input <- t(sapply(parms, "[[", "betaMtoF"))[1,]
cumuInf_output <- t(sapply(res, "[[", "cumuInftot"))[,31]

ggplot(data = data.frame(betaMtoF_input, cumuInf_output), aes(x = betaMtoF_input, y = cumuInf_output)) + geom_point() + theme_bw()



# fc over time
df = melt(data.frame(time, res[[1]]$fc_comm), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()

# prevalence over time
matplot(time, res[[1]]$prev, type = "l")#, lty = 1, col = "#00000055")


# mu vs prev?
plot(mu_input[, 1], prev_client_output[length(time), ])


# PLOTTING MULTIPLE SIMULATIONS


number_simulations = 100
parms = lhs_parameters(number_simulations, Ncat = 2)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

df=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,1])), group = "group 1")
df_2=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,2])), group = "group 2")

df_melted = melt(df, id.vars = c("time","group"))
df_melted_2 = melt(df_2, id.vars = c("time","group"))
df_all = rbind(df_melted, df_melted_2)
ggplot(data = df_all, aes(x = time, y = value, factor = variable, color = group)) + geom_line(alpha = 0.5) + theme_bw()


#### Ncat = 9
odin::odin_package(".") # looks for any models inside inst/odin
devtools::load_all()

number_simulations = 50
parms = lhs_parameters(number_simulations, Ncat = 9)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("prev")]
}
res = lapply(parms, f, main_model, time)

df=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,1])), group = "group 1")
df_2=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,2])), group = "group 2")
df_3=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,3])), group = "group 3")
df_4=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,4])), group = "group 4")
df_5=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,5])), group = "group 5")
df_6=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,6])), group = "group 6")
df_7=data.frame(time,do.call(cbind, lapply(res, function(x) x <- x$prev[,7])), group = "group 7")

df_melted = melt(df, id.vars = c("time","group"))
df_melted_2 = melt(df_2, id.vars = c("time","group"))
df_melted_3 = melt(df_3, id.vars = c("time","group"))
df_melted_4 = melt(df_4, id.vars = c("time","group"))
df_melted_5 = melt(df_5, id.vars = c("time","group"))
df_melted_6 = melt(df_6, id.vars = c("time","group"))
df_melted_7 = melt(df_7, id.vars = c("time","group"))

df_all = rbind(df_melted, df_melted_2, df_melted_3, df_melted_4, df_melted_5, df_melted_6, df_melted_7)
ggplot(data = df_all, aes(x = time, y = value, factor = variable, color = group)) + geom_line(alpha = 0.5) + theme_bw()

df=do.call(cbind,lapply(res, function(x) x$prev[,7]))
colnames(df) <- seq(1, number_simulations)
df <- data.frame(time, df)
df_melted <- melt(df, id.vars = "time")
ggplot(data = df_melted, aes(x = time, y = value, factor = variable)) + geom_line() + theme_bw() + labs(x="year",y="prevalance (%) former FSW outside Cotonou")


# show priors are flat and well explored
number_simulations = 200
parms = lhs_parameters(number_simulations, Ncat = 9)
time <- seq(1986, 2016, length.out = 31)
f <- function(p, gen, time) {
  mod <- gen(user = p)
  all_results <- mod$transform_variables(mod$run(time))
  all_results[c("c_comm", "c_noncomm","mu","gamma01")]
}
res = lapply(parms, f, main_model, time)

c_comm_prior = do.call(rbind, lapply(res, function(x) x$c_comm[1,]))
hist(c_comm_prior[,1])

mu_prior = do.call(rbind, lapply(res, function(x) x$mu[1,]))
hist(mu_prior[,1])

c_noncomm_prior = do.call(rbind, lapply(res, function(x) x$c_noncomm[1,]))
hist(c_noncomm_prior[,1])

gamma01_prior = do.call(rbind, lapply(res, function(x) x$gamma01[1,]))
hist(gamma01_prior[,1])



# SINGLE RUN WITH PARAMETERS TO CHECK EVERY OUTPUT
##############################################################################

# plot fc
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("c_comm", names(result))]
df = melt(data.frame(time, yy$c_comm), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# number of people on PrEP
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("S1[a-z]", names(result))]
yy_plot <- melt(
  data.frame(time,
             FSW = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,1]))),
             clients = rowSums(do.call(cbind, lapply(yy, function(x) x <- x[,2])))
  ),id.vars = "time")

ggplot(data = yy_plot, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()



# plot prevalence
parameters <- lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
yy <- result[grep("prev", names(result))]
df = melt(data.frame(time, FSW = yy$prev_FSW, client = yy$prev_client), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()


# cumulative infections
parameters <-lhs_parameters(1)[[1]]
result = run_model(parameters, main_model, time)
xx <- result[grep("cumu", names(result))]
N <- rowSums(do.call(cbind, xx))
df = melt(data.frame(time, N), id.vars = "time")
ggplot(data = df, aes(x = time, y = value, colour = variable)) + geom_line() + theme_bw()
