require(ggplot2)
require(reshape2)

gen <- odin::odin("ODEs_odin.R")

time <- seq(0, 100, length.out = 1000)
 
y <- mod$run(time)

mod <- gen(S0_init = 10000,
           S1a_init = 0,
           S1b_init = 0,
           S1c_init = 0,
           I01_init = 100,
           I11_init = 0,
           I02_init = 100,
           I03_init = 100,
           I04_init = 100,
           I05_init = 10,
           I22_init = 100,
           I23_init = 100,
           I24_init = 100,
           I25_init = 10,
           I32_init = 0,
           I33_init = 0,
           I34_init = 0,
           I35_init = 0,
           I42_init = 0,
           I43_init = 0,
           I44_init = 0,
           I45_init = 0,
           
           
           mu = 0.02,
           gamma01 = 0.2,
           gamma02 = 0.2,
           gamma03 = 0.2,
           gamma04 = 0.2,
           
           gamma11 = 0.2,
           
           gamma22 = 0.2,
           gamma23 = 0.2,
           gamma24 = 0.2,
           
           gamma32 = 0.2,
           gamma33 = 0.2,
           gamma34 = 0.2,
           
           gamma42 = 0.2,
           gamma43 = 0.2,
           gamma44 = 0.2,
           
           rho2 = 0.1,
           rho3 = 0.1,
           rho4 = 0.1,
           rho5 = 0.1,
           
           phi2 = 0.04,
           phi3 = 0.04,
           phi4 = 0.04,
           phi5 = 0.04,
           
           psia = 1,
           psib = 1,
           
           tau01 = 0.5,
           tau11 = 0.5,
           tau2 = 0.5,
           tau3 = 0.5,
           tau4 = 0.5,
           tau5 = 0.5,
           
           zetaa = 0.1,
           zetab = 0.1,
           zetac = 0.1,
           
           alpha01 = 0.01,
           alpha02 = 0.01,
           alpha03 = 0.01,
           alpha04 = 0.01,
           alpha05 = 1,
           
           alpha11 = 0.01,
           
           alpha21 = 0.01,
           alpha22 = 0.01,
           alpha23 = 0.01,
           alpha24 = 0.01,
           alpha25 = 1,
           
           alpha32 = 0.01,
           alpha33 = 0.01,
           alpha34 = 0.01,
           alpha35 = 1,
           
           alpha42 = 0.01,
           alpha43 = 0.01,
           alpha44 = 0.01,
           alpha45 = 1,
           
           
           beta = 0.0193,
           c = 4,
           ec = 0.85,
           eP = 0.6,
           epsilon = 0.001,
           fc = 0.8,
           fP = 0.5,
           n = 10,
           R = 1
)

titles = c("time","S0","S1a","S1b","S1c","I01","I11","I02","I03","I04","I05","I22","I23","I24","I25","I32","I33","I34","I35","I42","I43","I44","I45")


HIV_epi <- data.frame(y)

colnames(HIV_epi) = titles

HIV_epi_melted = melt(HIV_epi, id.vars = c("time"))


ggplot(HIV_epi_melted, aes(x = time, y = value, colour = variable)) + geom_point() + geom_line()


#####
# mod <- gen()
# 
# mod$initial()
# 
# time <- seq(0, 10, length.out = 1000)
# 
# y <- mod$run(time)
# 
# pairs(y[,-1], panel = lines)

