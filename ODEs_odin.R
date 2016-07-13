# note that in eq 1 Sk -> Sak
# eff prep should have superscript
# R is missing subscripts
# lambdas should be separate
# I05 no gamma5s anywhere!

# each variable must have a derivative part and an initial part

# a and k do not exist here yet

config(include) = "FOI.c"

# still need to work on lambda
deriv(S0) = E0 - S0 * lambda - S0 * mu
deriv(S1a) = E1a - S0 * lambda - S1a * mu
deriv(S1b) = E1b - S0 * lambda - S1b * mu
deriv(S1c) = E1c - S0 * lambda - S1c * mu

deriv(I01) = S0 * lambda - I01 * (gamma01 + tau01 + alpha01 + mu)
deriv(I11) = S1a * lambda + S1b * lambda + S1c * lambda -
  I11 * (gamma11 + tau11 + alpha11 + mu)

deriv(I02) = gamma01 * I01 + gamma11 * I11 - I02 * (gamma02 + tau2 + alpha02 + mu)
deriv(I03) = gamma02 * I02 - I03 * (gamma03 + tau3 + alpha03 + mu)
deriv(I04) = gamma03 * I03 - I04 * (gamma04 + tau4 + alpha04 + mu)
deriv(I05) = gamma04 * I04 - I05 * (tau5 + alpha05 + mu)

deriv(I22) = tau01 * I01 + tau11 * I11 + tau2 * I02 - I22 * (gamma22 + rho2 + alpha22 + mu)
deriv(I23) = gamma22 * I22 + tau3 * I03 - I22 * (gamma23 + rho3 + alpha23 + mu)
deriv(I24) = gamma23 * I23 + tau4 * I04 - I24 * (gamma24 + rho4 + alpha24 + mu)
deriv(I25) = gamma24 * I24 + tau5 * I05 - I25 * (rho5 + alpha25 + mu)

deriv(I32) = rho2 * (I22 + I42) - I32 * (gamma32 + phi2 + alpha32 + mu)
deriv(I33) = gamma32 * I32 + rho3 * (I23 + I43) - I33 * (gamma33 + phi3 + alpha33 + mu)
deriv(I34) = gamma33 * I33 + rho4 * (I24 + I44) - I34 * (gamma34 + phi4 + alpha34 + mu)
deriv(I35) = gamma34 * I34 + rho5 * (I25 + I45) - I35 * (phi5 + alpha35 + mu)

deriv(I42) = phi2 * I32 - I42 * (gamma42 + rho2 + alpha42 + mu)
deriv(I43) = gamma42 * I42 + phi3 * I33 - I43 * (gamma43 + rho3 + alpha43 + mu)
deriv(I44) = gamma43 * I43 + phi4 * I34 - I44 * (gamma44 + rho4 + alpha44 + mu)
deriv(I45) = gamma44 * I44 + phi5 * I35 - I45 * (rho5 + alpha45 + mu)



# births and prep movement
E0 = mu * N + epsilon * N + alphaItot - S0 * (zetaa + zetab + zetac)
E1a = zetaa * S0 - psia * S1a
E1b = zetab * S0 + psia * S1a - psib * S1b 
E1c = zetac * S0 + psib * S1b


N = S0 + S1a + S1b + S1c + I01 + I11 + I02 + I03 + I04 + I05 +
  I22 + I23 + I24 + I25 +  I32 + I33 + I34 + I35  +
  I42 + I43 + I44 + I45

alphaItot = 
  alpha01 * I01 + alpha11 * I11 + alpha02 * I02 + alpha03 * I03 + alpha04 * I04 +
  alpha05 * I05 + alpha22 * I22 + alpha23 * I23 + alpha24 * I24 + alpha25 * I25 +
  alpha32 * I32 + alpha33 * I33 + alpha34 * I34 + alpha35 * I35  +
  alpha42 * I42 + alpha43 * I43 + alpha44 * I44 + alpha45 * I45

lambda = c * (FOI_part(I01, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I02, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I03, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I04, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I05, N, beta, R, fc, fP, n, eP, ec) +
                
              FOI_part(I11, N, beta, R, fc, fP, n, eP, ec) +
                
              FOI_part(I22, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I23, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I24, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I25, N, beta, R, fc, fP, n, eP, ec) +
                
              FOI_part(I32, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I33, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I34, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I35, N, beta, R, fc, fP, n, eP, ec) +
                
              FOI_part(I42, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I43, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I44, N, beta, R, fc, fP, n, eP, ec) +
              FOI_part(I45, N, beta, R, fc, fP, n, eP, ec))

# do this for all vars!
initial(S0) = S0_init
S0_init = user()
initial(S1a) = S1a_init
S1a_init = user()
initial(S1b) = S1b_init
S1b_init = user()
initial(S1c) = S1c_init
S1c_init = user()

initial(I01) = I01_init
I01_init = user()
initial(I11) = I11_init
I11_init = user()

initial(I02) = I02_init
I02_init = user()
initial(I03) = I03_init
I03_init = user()
initial(I04) = I04_init
I04_init = user()
initial(I05) = I05_init
I05_init = user()

initial(I22) = I22_init
I22_init = user()
initial(I23) = I23_init
I23_init = user()
initial(I24) = I24_init
I24_init = user()
initial(I25) = I25_init
I25_init = user()

initial(I32) = I32_init
I32_init = user()
initial(I33) = I33_init
I33_init = user()
initial(I34) = I34_init
I34_init = user()
initial(I35) = I35_init
I35_init = user()

initial(I42) = I42_init
I42_init = user()
initial(I43) = I43_init
I43_init = user()
initial(I44) = I44_init
I44_init = user()
initial(I45) = I45_init
I45_init = user()

                
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
              
mu = user()
gamma01 = user()
gamma02 = user()
gamma03 = user()
gamma04 = user()

gamma11 = user()

gamma22 = user()
gamma23 = user()
gamma24 = user()

gamma32 = user()
gamma33 = user()
gamma34 = user()

gamma42 = user()
gamma43 = user()
gamma44 = user()

rho2 = user()
rho3 = user()
rho4 = user()
rho5 = user()

phi2 = user()
phi3 = user()
phi4 = user()
phi5 = user()

psia = user()
psib = user()

tau01 = user()
tau11 = user()
tau2 = user()
tau3 = user()
tau4 = user()
tau5 = user()

zetaa = user()
zetab = user()
zetac = user()

# note alpha is ordered differently...
alpha01 = user()
alpha02 = user()
alpha03 = user()
alpha04 = user()
alpha05 = user()

alpha11 = user()

alpha21 = user()
alpha22 = user()
alpha23 = user()
alpha24 = user()
alpha25 = user()

alpha32 = user()
alpha33 = user()
alpha34 = user()
alpha35 = user()

alpha42 = user()
alpha43 = user()
alpha44 = user()
alpha45 = user()

# FOI parameters

beta = user()
c = user()
ec = user()
eP = user()
epsilon = user()
fc = user()
fP = user()
n = user()
R = user()


