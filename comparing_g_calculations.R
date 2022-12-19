# comparing calculations of g from d to the values of g in the original data file
library(metafor)
library(compute.es)
library(esc)
library(dplyr)

source("effect_sizes.R")
source("HelperFunctions.R")

load(file = "df_Vasilev_et_al.Rda")             # loads a dataframe called "df"
load(file = "r.Rda")                            # loads the estimate of r for within subjects designs

# just trying different values of r to see if that explains the discrepancies in within-subjects g values...
###  the default value of 0.743 results in calculated values of g that are much higher than those in the data file
###  r=0   ## close, but the calculated values are still slightly higher than those in the data file
###  r= -r  ## calculated values are less than data file values
###  r = r - 1  ## slightly less than data file values


for (i in 1:nrow(df)) {
  d <- df[i,"d"]
  d_var <- df[i,"d_var"]
  N_C <- df[i,"N_Control"]
  N_E <- df[i,"N_Intervention"]
  N <- df[i,"N_Total"]
  design <- df[i,"Design"]
  m1i = df[i,"mean_E"]
  m2i = df[i,"mean_C"]
  sd1i = df[i,"var_C"]
  sd2i = df[i,"var_E"]
  n1i = df[i,"N_Intervention"]
  n2i = df[i,"N_Control"]
  var_type = df[i,"var_type"]
  
  g2 <- Hedges_g(d=d, N = N, N_C=N_C, N_E=N_E, design=design) #from Martin's scripts
  df[i,"g2"] <- g2
  g3 <- (metafor::escalc (measure = "SMD", di = d,  n1i = N_E, n2i = N_C, vtype = "LS2"))$yi
  if (df$Design[i] == "within") g3 <- ""
  df[i,"g3"] <- g3
  g4 <- (metafor::escalc (measure = "SMD", m1i = df[i,"mean_E"], m2i = df[i,"mean_C"], 
                         sd1i = df[i,"var_C"], sd2i = df[i,"var_E"], n1i = df[i,"N_Intervention"],
                         n2i = df[i,"N_Control"], vtype = "LS2"))$yi
  
  if (df$Design[i] == "within") {
    g4 <- (metafor::escalc (measure = "SMCC", vtype = "LS2",
                            m1i = df[i,"mean_E"], m2i = df[i,"mean_C"], 
                            sd1i = df[i,"var_C"], sd2i = df[i,"var_E"],
                            ni = N,  ri = r))$yi 
  }
  if (is.na(df[i,"var_type"]) | df[i,"var_type"] != "SD") g4 <- ""
  if (df$Design[i] == "within") g4 <- ""
  df[i,"g4"] <- g4
  
  nratio <- N_E / N_C
  df[i,"nratio"] <- nratio
  g5 <- (compute.es::des(d= d, n.1 = N_E, n.2 = N_C, dig = 6))[["g"]]
  if (df$Design[i] == "within") g5 <- ""
  df[i,"g5"] <- g5

  g6 <- if(df$Design[i] == "between") {
    esc::esc_mean_sd(
      grp1m = m1i,
      grp2m = m2i,
      grp1n = n1i,
      grp2n = n2i,
      grp1sd = sd1i,
      grp2sd = sd2i,
      es.type = "g"
    )
  } else {
    esc::esc_mean_sd(
      grp1m = m1i,
      grp2m = m2i,
      grp1n = n1i,
      grp2n =  n2i,
      grp1sd = sd1i,
      grp2sd = sd2i,
      es.type = "g",
      r = r
    )
  }
  if (is.na(df[i,"var_type"]) | df[i,"var_type"] != "SD") g6 <- ""
  df[i,"g6"] <- g6
  g7 <- esc::hedges_g(d = d, totaln = N)
  df[i,"g7"] <- g7
  
  
  g9 <- getEffectSize (  mean_E=m1i, mean_C=m2i,
                       var_E=sd1i, var_C=sd2i, var_type=var_type, N_Total=N, N_Intervention=N_E, 
                       N_Control=N_C, Design= design, r = r,
                       reverseCode="No")
  
  
  g8 <- getEffectSize (d=d, d_var=d_var, mean_E=m1i, mean_C=m2i,
                       var_E=sd1i, var_C=sd2i, var_type=var_type, N_Total=N, N_Intervention=N_E, 
                       N_Control=N_C, Design= design, r = r,
                       reverseCode="No")
}


df$ratio <- df$g / df$g2

df2 <- (select(df, ID, Design, d, g, g2, g3, g5, g7,  g4, g6, N_Total, nratio, ratio) %>% 
       arrange(., Design, N_Total))

df2 <- dplyr::rename(df2, metafor_d_to_g = g3)
df2 <- dplyr::rename(df2, metafor_means_to_g = g4)
df2 <- dplyr::rename(df2, compute.es_des = g5)
df2 <- dplyr::rename(df2, esc_mean_sd = g6)
df2 <- dplyr::rename(df2, esc_d_to_g = g7)

View(df2)
save(df2, file = "df2.Rda")
# g = g in the data file for the paper
# g2 = g recalculated from the d's in the data file, using Martin's functions in effect_sizes.R
# g3 = g calculated from d, using metafor, for "between" designs
# g4 = g calculated from means & sds, using metafor for "between" designs
# g5 = g calculated from d using compute.es::des  (can not find anything in compute.es for within designs)
# g6 = g calculated from means and sds using esc::esc_mean_sd
# g7 = g calculated from d using esc::hedges_g





