# NOTES
##############################################################################

# lambdas should be different for each compartment??!?!?!?!
# I05 no gamma5s anywhere!
# calculate E for all, then fraction for each pop
# if sum(omega) != 1, then problem! - how do I hard code this error?

# each variable must have a derivative part and an initial part

# a and k do not exist here yet

config(include) = "FOI.c"

# ORDINARY DIFFERENTIAL EQUATIONS
##############################################################################


# still need to work on lambda
deriv(S0[]) = E0[i] - S0[i] * lambda_sum_0[i] - S0[i] * mu[i]
deriv(S1a[]) = E1a[i] - S1a[i] * lambda_sum_1a[i] - S1a[i] * mu[i]
deriv(S1b[]) = E1b[i] - S1b[i] * lambda_sum_1b[i] - S1b[i] * mu[i]
deriv(S1c[]) = E1c[i] - S1c[i] * lambda_sum_1c[i] - S1c[i] * mu[i]

#primary infection
deriv(I01[]) = S0[i] * lambda_sum[i] - I01[i] * (gamma01[i] + tau01[i] + alpha01[i] + mu[i])
deriv(I11[]) = S1a[i] * lambda_sum[i] + S1b[i] * lambda_sum[i] + S1c[i] * lambda_sum[i] -
  I11[i] * (gamma11[i] + tau11[i] + alpha11[i] + mu[i])

#chronic
deriv(I02[]) = gamma01[i] * I01[i] + gamma11[i] * I11[i] - I02[i] * (gamma02[i] + tau2[i] + alpha02[i] + mu[i])
deriv(I03[]) = gamma02[i] * I02[i] - I03[i] * (gamma03[i] + tau3[i] + alpha03[i] + mu[i])
deriv(I04[]) = gamma03[i] * I03[i] - I04[i] * (gamma04[i] + tau4[i] + alpha04[i] + mu[i])
deriv(I05[]) = gamma04[i] * I04[i] - I05[i] * (tau5[i] + alpha05[i] + mu[i])

deriv(I22[]) = tau01[i] * I01[i] + tau11[i] * I11[i] + tau2[i] * I02[i] - I22[i] * (gamma22[i] + rho2[i] + alpha22[i] + mu[i])
deriv(I23[]) = gamma22[i] * I22[i] + tau3[i] * I03[i] - I23[i] * (gamma23[i] + rho3[i] + alpha23[i] + mu[i])
deriv(I24[]) = gamma23[i] * I23[i] + tau4[i] * I04[i] - I24[i] * (gamma24[i] + rho4[i] + alpha24[i] + mu[i])
deriv(I25[]) = gamma24[i] * I24[i] + tau5[i] * I05[i] - I25[i] * (rho5[i] + alpha25[i] + mu[i])

deriv(I32[]) = rho2[i] * (I22[i] + I42[i]) - I32[i] * (gamma32[i] + phi2[i] + alpha32[i] + mu[i])
deriv(I33[]) = gamma32[i] * I32[i] + rho3[i] * (I23[i] + I43[i]) - I33[i] * (gamma33[i] + phi3[i] + alpha33[i] + mu[i])
deriv(I34[]) = gamma33[i] * I33[i] + rho4[i] * (I24[i] + I44[i]) - I34[i] * (gamma34[i] + phi4[i] + alpha34[i] + mu[i])
deriv(I35[]) = gamma34[i] * I34[i] + rho5[i] * (I25[i] + I45[i]) - I35[i] * (phi5[i] + alpha35[i] + mu[i])

deriv(I42[]) = phi2[i] * I32[i] - I42[i] * (gamma42[i] + rho2[i] + alpha42[i] + mu[i])
deriv(I43[]) = gamma42[i] * I42[i] + phi3[i] * I33[i] - I43[i] * (gamma43[i] + rho3[i] + alpha43[i] + mu[i])
deriv(I44[]) = gamma43[i] * I43[i] + phi4[i] * I34[i] - I44[i] * (gamma44[i] + rho4[i] + alpha44[i] + mu[i])
deriv(I45[]) = gamma44[i] * I44[i] + phi5[i] * I35[i] - I45[i] * (rho5[i] + alpha45[i] + mu[i])

deriv(cumuInf[]) = S0[i] * lambda_sum[i] + S1a[i] * lambda_sum[i] + S1b[i] * lambda_sum[i] + S1c[i] * lambda_sum[i]

# births due to population growth
new_people = epsilon * sum(N)

# births and prep movement
E0[] = mu[i] * N[i] + alphaItot[i] + new_people * omega[i] - S0[i] * (zetaa[i] + zetab[i] + zetac[i])
E1a[] = zetaa[i] * S0[i] - psia[i] * S1a[i]
E1b[] = zetab[i] * S0[i] + psia[i] * S1a[i] - psib[i] * S1b[i]
E1c[] = zetac[i] * S0[i] + psib[i] * S1b[i]


# sum of all compartments
N[] = S0[i] + S1a[i] + S1b[i] + S1c[i] + I01[i] + I11[i] + I02[i] + I03[i] + I04[i] + I05[i] +
  I22[i] + I23[i] + I24[i] + I25[i] + I32[i] + I33[i] + I34[i] + I35[i] +
  I42[i] + I43[i] + I44[i] + I45[i]


# mortality due to HIV infection
alphaItot[] =
  alpha01[i] * I01[i] + alpha11[i] * I11[i] + alpha02[i] * I02[i] + alpha03[i] * I03[i] + alpha04[i] * I04[i] +
  alpha05[i] * I05[i] + alpha22[i] * I22[i] + alpha23[i] * I23[i] + alpha24[i] * I24[i] + alpha25[i] * I25[i] +
  alpha32[i] * I32[i] + alpha33[i] * I33[i] + alpha34[i] * I34[i] + alpha35[i] * I35[i] +
  alpha42[i] * I42[i] + alpha43[i] * I43[i] + alpha44[i] * I44[i] + alpha45[i] * I45[i]

# BALANCING OF PARTNERSHIPS
##############################################################################


B[,] <- if (i < j && c[i,j] > 0) c[j,i] * N[j] / (c[i, j] * N[i]) else 0
cstar[,] <- c[i,j] * (if (i > j) B[j, i]^-(1 - theta[j,i]) else B[i, j]^theta[i,j])


# # when 4d happens
# B[,,,] <- if (i < k && c[i,j,k,l] > 0) c[j,k,i,l] * N[j,l] / (c[i,j,k,l] * N[i,k]) else 0
# cstar[,,,] <- c[i,j,k,l] * (if (i > k) B[k, l, i, j]^(theta - 1) else B[i, j, k, l]^theta)
# dim(B) <- c(Ncat, Nage, Ncat, Nage)
# dim(cstar) <- c(Ncat, Nage, Ncat, Nage)


# B = (c[2,1] * N[2])/(c[1,1] * N[1])
# c1_new = c[1,2] * B^theta
# c2_new = c[2,1] * B^(-(1-theta))

# B_check = (c2_new * N[2])/(c1_new * N[1])
B_check = 1
# functions to balance partnerships




# PROBABILITY OF SEXUAL CONTACT
##############################################################################

# p = 1



# INTERPOLATING FUNCTIONS
##############################################################################


fc[] = interpolate(fc_t, fc_y, "linear")
fP[] = interpolate(fP_t, fP_y, "linear")


# FORCE OF INFECTION
##############################################################################

# as there are only 2 groups so far, we can leave all of the parameters here as vectors, but they will be matrices!
# lambda[2,1] is the force of infection of 1 on 2
# lambda[2,1] = compute_lambda(c1_new, S0[1], S1a[1], S1b[1], S1c[1], I01[1], I11[1], I02[1], I03[1], I04[1], I05[1],
#                           I22[1], I23[1], I24[1], I25[1], I32[1], I33[1], I34[1], I35[1],
#                           I42[1], I43[1], I44[1], I45[1],
#                         N[1], beta[1], R[1], fc[1], fP[1], n[1], eP[1], ec[1])
#
# lambda[1,2] = compute_lambda(c2_new, S0[2], S1a[2], S1b[2], S1c[2], I01[2], I11[2], I02[2], I03[2], I04[2], I05[2],
#                              I22[2], I23[2], I24[2], I25[2], I32[2], I33[2], I34[2], I35[2],
#                              I42[2], I43[2], I44[2], I45[2],
#                              N[2], beta[2], R[2], fc[2], fP[2], n[2], eP[2], ec[2])
#

# k = group of S; i = age of S; j = group of I; l = age of I;

# going into compute_lambda:
# SPECIFIC TO k, i, j, l

# partner change rate (c) of the suseptible k, i and infected j, l (matrix 21x21; doesn't vary by s, a; updates every timestep; FUNCTION?!)
# probability of sexual contact (p) of the suseptible k, i and infected j, l (matrix 21x21; doesn't vary by s, a; fixed at beginning of simulation)
# all care/disease states (s, a) of the infected j, l
# efficacy of PrEP (eP), which depends on PrEP adherence category (vector of length 4)
# transmission rate (beta) of the susceptible k, i (matrix 7x3; doesn't depend on s, a)
# relative risk factors (R) of care state (a) and stage of infection (s) of INFECTED CLASS (matrix 5 x 5)
# number of sex acts per partnership (n) of the suseptible k, i and infected j, l (matrix 21x21; doesn't vary by s, a; fixed at beginning of simulation)
# fraction condom protected (fc) of the suseptible k, i and infected j, l (matrix 21x21; doesn't vary by s, a; updates every timestep with interpolating function)
# fraction PrEP protected (fP) of the susceptible k, i (matrix 7x3; doesn't depend on s, a; set all to 1 for now)

# lambda_0[,] requires kijl... but at the moment only inputs ki?
# lambda_0 will have to be a matrix of size 21x21, and each cell is the sum of all of the FOIs from all the care states

# lambda_0[,] = compute_lambda(c_new, p, S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
#                            I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
#                            I42[j], I43[j], I44[j], I45[j],
#                            N[j], beta[j], R[j], fc[j], fP[j], n[j], eP[j], ec[j])

# 1. H-FSW; 2. L-FSW; 3. GPF; 4. H-Clients; 5. L-Clients; 6. Boyfriends; 7. GPM
# 1. 15-24; 2. 25-34; 3. 35-59

# R is a matrix, because it varies with s and a (which is being summed over in the function)
# first element of vector eP, because lambda 0...

# could expand the lambda function to sum also over j,l... THIS ACTUALLY SEEMS BETTER...


#

#FOI of j on i
lambda[,] = if (i == j) 0 else compute_lambda(cstar[i,j], p[i,j], S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
                           I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
                           I42[j], I43[j], I44[j], I45[j],
                           N[j], beta[j], R[j], fc[j], fP[j], n[i,j], eP[j], ec[j])




#FOI of j on i. PrEP adherence category 0 (off PrEP)
lambda_0[,] = if (i == j) 0 else compute_lambda(cstar[i,j], p[i,j], S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
                                              I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
                                              I42[j], I43[j], I44[j], I45[j],
                                              N[j], beta[j], R[j], fc[j], fP[j], n[i,j], eP0[j], ec[j])
#FOI of j on i. PrEP adherence category 0 (off PrEP)
lambda_1a[,] = if (i == j) 0 else compute_lambda(cstar[i,j], p[i,j], S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
                                                I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
                                                I42[j], I43[j], I44[j], I45[j],
                                                N[j], beta[j], R[j], fc[j], fP[j], n[i,j], eP1a[j], ec[j])
#FOI of j on i. PrEP adherence category 0 (off PrEP)
lambda_1b[,] = if (i == j) 0 else compute_lambda(cstar[i,j], p[i,j], S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
                                                I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
                                                I42[j], I43[j], I44[j], I45[j],
                                                N[j], beta[j], R[j], fc[j], fP[j], n[i,j], eP1b[j], ec[j])
#FOI of j on i. PrEP adherence category 0 (off PrEP)
lambda_1c[,] = if (i == j) 0 else compute_lambda(cstar[i,j], p[i,j], S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
                                                I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
                                                I42[j], I43[j], I44[j], I45[j],
                                                N[j], beta[j], R[j], fc[j], fP[j], n[i,j], eP1c[j], ec[j])


lambda_sum[] = sum(lambda[i,])
lambda_sum_0[] = sum(lambda_0[i,])
lambda_sum_1a[] = sum(lambda_1a[i,])
lambda_sum_1b[] = sum(lambda_1b[i,])
lambda_sum_1c[] = sum(lambda_1c[i,])



# OUTPUTS
##############################################################################

# Calculations
Ntot = sum(N)

N_FSW = N[1]
N_client = N[2]

frac_N[] = N[i] / Ntot

# PREVALENCE
# n.b. prevalence for all ages in each risk group will be useful, so maybe one prev array is too much info in one output
prev_FSW = 100 * (I01[1] + I11[1] + I02[1] + I03[1] + I04[1] + I05[1] +
                    I22[1] + I23[1] + I24[1] + I25[1] + I32[1] + I33[1] + I34[1] + I35[1] +
                    I42[1] + I43[1] + I44[1] + I45[1]) / N[1]

prev_client = 100 * (I01[2] + I11[2] + I02[2] + I03[2] + I04[2] + I05[2] +
                       I22[2] + I23[2] + I24[2] + I25[2] + I32[2] + I33[2] + I34[2] + I35[2] +
                       I42[2] + I43[2] + I44[2] + I45[2]) / N[2]

prev[] = 100 * (I01[i] + I11[i] + I02[i] + I03[i] + I04[i] + I05[i] +
                  I22[i] + I23[i] + I24[i] + I25[i] + I32[i] + I33[i] + I34[i] + I35[i] +
                  I42[i] + I43[i] + I44[i] + I45[i]) / N[i]


output(Ntot) = Ntot
output(new_people) = new_people
output(B_check) = B_check

output(N[]) = N # is it worth outputting N? Once we have ages, it'll be better to have separate Ns for risk groups... but eugene ages can make N a matrix!
output(prev_FSW) = prev_FSW
output(prev_client) = prev_client
output(prev[]) = prev
output(N_FSW) = N_FSW
output(N_client) = N_client
output(frac_N[]) = frac_N
# output(lambda_sum[]) = lambda_sum

# FOI outputs
output(lambda[,]) = lambda
output(fc[]) = fc
output(fP[]) = fP
output(c[,]) = c
output(cstar[,]) = cstar
output(B[,]) = B
output(p[,]) = p
output(n[,]) = n
output(theta[,]) = theta
# INCIDENCE RATE

# = no. disease onsets / sum of "person-time" at risk




# output(prev[]) = prev

# in future nb eP eC constants
# lambda[,] = compute_lambda(S0[j], S1a[j], S1b[j], S1c[j], I01[j], I11[j], I02[j], I03[j], I04[j], I05[j],
#                              I22[j], I23[j], I24[j], I25[j], I32[j], I33[j], I34[j], I35[j],
#                              I42[j], I43[j], I44[j], I45[j],
#                              N[j], beta[i,j], R[i,j], fc[i,j], fP[i,j], n[i,j], eP[j], ec[j])


# do this for all vars!
initial(S0[]) = S0_init[i]
S0_init[] = user()
initial(S1a[]) = S1a_init[i]
S1a_init[] = user()
initial(S1b[]) = S1b_init[i]
S1b_init[] = user()
initial(S1c[]) = S1c_init[i]
S1c_init[] = user()

initial(I01[]) = I01_init[i]
I01_init[] = user()
initial(I11[]) = I11_init[i]
I11_init[] = user()

initial(I02[]) = I02_init[i]
I02_init[] = user()
initial(I03[]) = I03_init[i]
I03_init[] = user()
initial(I04[]) = I04_init[i]
I04_init[] = user()
initial(I05[]) = I05_init[i]
I05_init[] = user()

initial(I22[]) = I22_init[i]
I22_init[] = user()
initial(I23[]) = I23_init[i]
I23_init[] = user()
initial(I24[]) = I24_init[i]
I24_init[] = user()
initial(I25[]) = I25_init[i]
I25_init[] = user()

initial(I32[]) = I32_init[i]
I32_init[] = user()
initial(I33[]) = I33_init[i]
I33_init[] = user()
initial(I34[]) = I34_init[i]
I34_init[] = user()
initial(I35[]) = I35_init[i]
I35_init[] = user()

initial(I42[]) = I42_init[i]
I42_init[] = user()
initial(I43[]) = I43_init[i]
I43_init[] = user()
initial(I44[]) = I44_init[i]
I44_init[] = user()
initial(I45[]) = I45_init[i]
I45_init[] = user()

initial(cumuInf[]) = cumuInf_init[i]
cumuInf_init[] = user()

# initial(S0) = user()
# initial(S1a) = user()
# initial(S1b) = user()
# initial(S1c) = user()
#
# initial(I01) = user()
# initial(I11) = user()
#
# initial(I02) = user()
# initial(I03) = user()
# initial(I04) = user()
# initial(I05) = user()
#
# initial(I22) = user()
# initial(I23) = user()
# initial(I24) = user()
# initial(I25) = user()
#
# initial(I32) = user()
# initial(I33) = user()
# initial(I34) = user()
# initial(I35) = user()
#
# initial(I42) = user()
# initial(I43) = user()
# initial(I44) = user()
# initial(I45) = user()

mu[] = user()
gamma01[] = user()
gamma02[] = user()
gamma03[] = user()
gamma04[] = user()

gamma11[] = user()

gamma22[] = user()
gamma23[] = user()
gamma24[] = user()

gamma32[] = user()
gamma33[] = user()
gamma34[] = user()

gamma42[] = user()
gamma43[] = user()
gamma44[] = user()

rho2[] = user()
rho3[] = user()
rho4[] = user()
rho5[] = user()

phi2[] = user()
phi3[] = user()
phi4[] = user()
phi5[] = user()

psia[] = user()
psib[] = user()

tau01[] = user()
tau11[] = user()
tau2[] = user()
tau3[] = user()
tau4[] = user()
tau5[] = user()

zetaa[] = user()
zetab[] = user()
zetac[] = user()

# note alpha is ordered differently...
alpha01[] = user()
alpha02[] = user()
alpha03[] = user()
alpha04[] = user()
alpha05[] = user()

alpha11[] = user()

alpha21[] = user()
alpha22[] = user()
alpha23[] = user()
alpha24[] = user()
alpha25[] = user()

alpha32[] = user()
alpha33[] = user()
alpha34[] = user()
alpha35[] = user()

alpha42[] = user()
alpha43[] = user()
alpha44[] = user()
alpha45[] = user()

# FOI parameters

beta[] = user()
c[,] = user()
p[,] = user()
ec[] = user()
eP[] = user()
eP0[] = user()
eP1a[] = user()
eP1b[] = user()
eP1c[] = user()
n[,] = user()
R[] = user()

# growth
#epsilon[] = user()
epsilon = user()
omega[] = user()

# balancing
theta[,] = user()

fc_t[] = user()
fc_y[,] = user()
dim(fc_t) = user()
dim(fc_y) = user()

fP_t[] = user()
fP_y[,] = user()
dim(fP_t) = user()
dim(fP_y) = user()
Ncat = user()
Nage = user()



# DIMMING




#parameters

dim(omega) = Ncat
dim(frac_N) = Ncat

# care cascade
dim(mu) = Ncat

dim(gamma01) = Ncat
dim(gamma02) = Ncat
dim(gamma03) = Ncat
dim(gamma04) = Ncat
dim(gamma11) = Ncat
dim(gamma22) = Ncat
dim(gamma23) = Ncat
dim(gamma24) = Ncat
dim(gamma32) = Ncat
dim(gamma33) = Ncat
dim(gamma34) = Ncat
dim(gamma42) = Ncat
dim(gamma43) = Ncat
dim(gamma44) = Ncat

dim(rho2) = Ncat
dim(rho3) = Ncat
dim(rho4) = Ncat
dim(rho5) = Ncat

dim(phi2) = Ncat
dim(phi3) = Ncat
dim(phi4) = Ncat
dim(phi5) = Ncat

dim(psia) = Ncat
dim(psib) = Ncat

dim(tau01) = Ncat
dim(tau11) = Ncat
dim(tau2) = Ncat
dim(tau3) = Ncat
dim(tau4) = Ncat
dim(tau5) = Ncat

dim(zetaa) = Ncat
dim(zetab) = Ncat
dim(zetac) = Ncat

dim(alpha01) = Ncat
dim(alpha02) = Ncat
dim(alpha03) = Ncat
dim(alpha04) = Ncat
dim(alpha05) = Ncat
dim(alpha11) = Ncat
dim(alpha21) = Ncat
dim(alpha22) = Ncat
dim(alpha23) = Ncat
dim(alpha24) = Ncat
dim(alpha25) = Ncat
dim(alpha32) = Ncat
dim(alpha33) = Ncat
dim(alpha34) = Ncat
dim(alpha35) = Ncat
dim(alpha42) = Ncat
dim(alpha43) = Ncat
dim(alpha44) = Ncat
dim(alpha45) = Ncat

# FOI parameters
dim(beta) = Ncat
dim(c) = c(Ncat, Ncat)
dim(p) = c(Ncat, Ncat)
dim(ec) = Ncat
dim(eP) = Ncat
dim(eP0) = Ncat
dim(eP1a) = Ncat
dim(eP1b) = Ncat
dim(eP1c) = Ncat

dim(fc) = Ncat
dim(fP) = Ncat
dim(n) = c(Ncat, Ncat)
dim(R) = Ncat


dim(cumuInf) = Ncat


# states and initial conditions
dim(S0) = Ncat
dim(S1a) = Ncat
dim(S1b) = Ncat
dim(S1c) = Ncat

dim(I01) = Ncat
dim(I11) = Ncat

dim(I02) = Ncat
dim(I03) = Ncat
dim(I04) = Ncat
dim(I05) = Ncat

dim(I22) = Ncat
dim(I23) = Ncat
dim(I24) = Ncat
dim(I25) = Ncat

dim(I32) = Ncat
dim(I33) = Ncat
dim(I34) = Ncat
dim(I35) = Ncat

dim(I42) = Ncat
dim(I43) = Ncat
dim(I44) = Ncat
dim(I45) = Ncat

dim(S0_init) = Ncat
dim(S1a_init) = Ncat
dim(S1b_init) = Ncat
dim(S1c_init) = Ncat

dim(I01_init) = Ncat
dim(I11_init) = Ncat

dim(I02_init) = Ncat
dim(I03_init) = Ncat
dim(I04_init) = Ncat
dim(I05_init) = Ncat

dim(I22_init) = Ncat
dim(I23_init) = Ncat
dim(I24_init) = Ncat
dim(I25_init) = Ncat

dim(I32_init) = Ncat
dim(I33_init) = Ncat
dim(I34_init) = Ncat
dim(I35_init) = Ncat

dim(I42_init) = Ncat
dim(I43_init) = Ncat
dim(I44_init) = Ncat
dim(I45_init) = Ncat




# other variables
dim(N) = Ncat
dim(E0) = Ncat
dim(E1a) = Ncat
dim(E1b) = Ncat
dim(E1c) = Ncat

# other summary stats that are calculated
dim(cumuInf_init) = Ncat
dim(alphaItot) = Ncat
dim(prev) = Ncat
dim(B) <- c(Ncat, Ncat)

# FOI parameters
dim(lambda) = c(Ncat, Ncat)
dim(lambda_0) = c(Ncat, Ncat)
dim(lambda_1a) = c(Ncat, Ncat)
dim(lambda_1b) = c(Ncat, Ncat)
dim(lambda_1c) = c(Ncat, Ncat)


dim(lambda_sum) = Ncat
dim(lambda_sum_0) = Ncat
dim(lambda_sum_1a) = Ncat
dim(lambda_sum_1b) = Ncat
dim(lambda_sum_1c) = Ncat

dim(cstar) <- c(Ncat, Ncat)
dim(theta) <- c(Ncat, Ncat)
