# comparing different calculation methods for g
library(metafor)
library(compute.es)

# values from Martin's data file:
# ID# 5, Daoussis & McKelvie 1986
mean_E = 7.6
mean_C = 9.14
sd1i = 2.16  #intervention
sd2i = 3.55  # control
var_E <- sd1i
var_C <- sd2i  
N_Intervention = 24
N_Control = 24
d = -.5241
d_var = .086195
g = -.51551     # value in the data file for comparison to the output below
gvar = .083392  # value in the data file for comparison to the output below
r = 0.74

metafor::escalc (measure = "SMD", m1i = mean_E, m2i = mean_C, 
                 sd1i = sd1i, sd2i = sd2i, n1i = N_Intervention, n2i = N_Control)

metafor::escalc (measure = "SMD", di = d,  n1i = N_Intervention, n2i = N_Control)
metafor::escalc (measure = "SMD", di = d,  n1i = N_Intervention +N_Control -3, n2i = 2)

compute.es::des(d= d, n.1 = N_Intervention, n.2 = N_Control, dig = 6)


# metafor::escalc seems to return d_var where it should be g_var; the g_var values
# from metafor do not match those from compute.es or from Martin's data file

# trying a second set of values from the data file:

# values from Martin's data file:
# ID 20, Furnham & Bradley 1997
mean_E = 7.7
mean_C = 9.3
sd1i =  1.29 #intervention
sd2i =  1.835 # control
N_Intervention = 10
N_Control = 10
Total.N = 20
d = -1.00877
d_var = .225441
g = -0.96615   # value in the data file for comparison to the output below
gvar = .206792  # value in the data file for comparison to the output below
dftotal <- N_Intervention + N_Control - 2
J <- 1 - (3 / (4 * dftotal -1))
gbyj <- J * d
g_varbyj <- J^2 * d_var


gcalc <- metafor::escalc (measure = "SMD", m1i = mean_E, m2i = mean_C, 
                 sd1i = sd1i, sd2i = sd2i, n1i = N_Intervention, n2i = N_Control, 
                 vtype = "LS2")
g <- gcalc$yi[[1]]
g_var <- gcalc$vi

metafor::escalc (measure = "SMD", di = d,  n1i = N_Intervention, n2i = N_Control, 
                 vtype = "LS2")

gstats <- compute.es::des(d= d, n.1 = N_Intervention, n.2 = N_Control, dig = 6)
g <- gstats$g
g_var <- gstats$var.g

gbyj
g
g_varbyj
g_var




#else { # for within designs (anything other than "between")
#  if (is.null(var_E)) { # using control SD only; SMCR for within; SDM1 for between
    metafor::escalc (measure = "SMCR", vtype = "LS2",
                     m1i = mean_E, m2i = mean_C,
                     sd1i = sd2i, ni = N_Control, ri = r)   # using SD of the control condition sd2i
#  } else { # using both control SD and intervention SD; SMCC for within; SMD for between
    metafor::escalc (measure = "SMCC", vtype = "LS2",
                     m1i = mean_E, m2i = mean_C, 
                     sd1i = sd1i, sd2i = sd2i, ni = N_Control, ri = r) 
    
    metafor::escalc(measure = "SMCC", vtype = "LS2", di = d,
                    m1i = mean_E, m2i = mean_C, 
                     ni = N_Control, ri = r)
    
    
    
    
    Hedges_g<- function(d, N_C=NULL, N_E=NULL, N=NULL, design= "between")
      
      
      
      
   tempg <-      getEffectSize(g=1.2, g_var=.88)
 tempg   
 length(tempg$yi)
    
    tempg <-      getEffectSize(d=1.1, d_var=.55, Total.N=NULL, N_Intervention=11, N_Control=12,
                                Design="between"
                              )
    tempg  
    
    tempg <-      getEffectSize(mean_E=mean_E, mean_C=mean_C,
                                var_E=var_E, var_C=var_C,  Total.N=Total.N, N_Intervention=11, N_Control=12,  
                                Design="within", r = r_estimate, var_type="Standard deviation",
                                reverseCode="No")
    tempg
    
    
    
    
    
    tempg <-      getEffectSize(mean_E=mean_E, mean_C=mean_C,
                                var_E=var_E, var_C=var_C,  Total.N=Total.N,  
                                N_Control=N_Control, Design="between", r = r_estimate, var_type="Standard deviation",
                                reverseCode="No")
    tempg
    
    
    tempg <-      getEffectSize(mean_E=mean_E, mean_C=mean_C,
                                var_E=var_E, var_C=var_C,  Total.N=Total.N, N_Intervention=N_Intervention, 
                                N_Control=N_Control, Design="between", r = r_estimate, var_type="Standard deviation",
                                reverseCode="No")
    tempg
    
    
    tempg <-      getEffectSize(mean_E=mean_E, mean_C=mean_C,
                                var_E=var_E, var_C=var_C,  Total.N=Total.N, N_Intervention=N_Intervention, 
                                N_Control=N_Control, Design="between", r = r_estimate, var_type="Standard deviation",
                                reverseCode="No")
    tempg
    
    
    
    
    tempg <-      getEffectSize(g=NULL, g_var=NULL, d=NULL, d_var=NULL, mean_E=NULL, mean_C=NULL,
                                var_E=NULL, var_C=NULL, var_type=NULL, Total.N=NULL, N_Intervention=NULL, 
                                N_Control=NULL, Design="between", r = r_estimate,
                                reverseCode="No")
    tempg
    
    
    tempg <-      getEffectSize(g=NULL, g_var=NULL, d=NULL, d_var=NULL, mean_E=NULL, mean_C=NULL,
                                var_E=NULL, var_C=NULL, var_type=NULL, Total.N=NULL, N_Intervention=NULL, 
                                N_Control=NULL, Design="between", r = r_estimate,
                                reverseCode="No")
    tempg
    
    tempg <-      getEffectSize(g=NULL, g_var=NULL, d=NULL, d_var=NULL, mean_E=NULL, mean_C=NULL,
                                var_E=NULL, var_C=NULL, var_type=NULL, Total.N=NULL, N_Intervention=NULL, 
                                N_Control=NULL, Design="between", r = r_estimate,
                                reverseCode="No")
    tempg
    
    tempg <-      getEffectSize(g=NULL, g_var=NULL, d=NULL, d_var=NULL, mean_E=NULL, mean_C=NULL,
                                var_E=NULL, var_C=NULL, var_type=NULL, Total.N=NULL, N_Intervention=NULL, 
                                N_Control=NULL, Design="between", r = r_estimate,
                                reverseCode="No")
    tempg
    
    tempg <-      getEffectSize(g=NULL, g_var=NULL, d=NULL, d_var=NULL, mean_E=NULL, mean_C=NULL,
                                var_E=NULL, var_C=NULL, var_type=NULL, Total.N=NULL, N_Intervention=NULL, 
                                N_Control=NULL, Design="between", r = r_estimate,
                                reverseCode="No")
    tempg
    