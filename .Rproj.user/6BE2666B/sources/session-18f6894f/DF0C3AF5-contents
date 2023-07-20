#Code to calculate SMDH and LRR from raw data and coefficients

##load packages
library(tidyverse); library(esc); library(metafor); library(effectsize)

##read in dataset
fulldat2 <- read.csv("./Data/FinalDataWRound2.csv")

##Note: effect size calculations are not applicable to those observations
##within BC; as these observations come directly from the Halliday et al. 2020
##database

#subset data to only those with raw data (not coefficients)
noncoef_dat <- fulldat2 %>%
  filter(Coef_Type == "") %>%
  filter(Error_Type != "")

#Calculate Log Response Ratio (LRR)
d2 <- escalc(measure = "ROM",
             m1i = MeanTreatment, sd1i = SDTreatment, n1i = SampleSizeTreatment,
             m2i = MeanControl, sd2i = SDControl, n2i = SampleSizeControl,
             data = noncoef_dat)



noncoef_dat$LRRyi = d2$yi
noncoef_dat$LRRvi = d2$vi

#Calculate Hedge's g (SMDH)
d3 <- escalc(measure = "SMDH",
             m1i = MeanTreatment, sd1i = SDTreatment, n1i = SampleSizeTreatment,
             m2i = MeanControl, sd2i = SDControl, n2i = SampleSizeControl,
             data = noncoef_dat)

noncoef_dat$SMDHyi = d3$yi
noncoef_dat$SMDHvi = d3$vi


##Coefficient conversion for SMDH only:
## OR, z_stat, f_stat, t_stat, Regression, Correlation, chisq, 
###
##Odds ratio
OR <- fulldat2 %>%
  filter(Coef_Type == "OR")

dat <- convert_or2d(or = as.numeric(OR$Coefficient),
                    se = OR$CoefficientVariance,
                    totaln = OR$Sample_size,
                    es.type = "g")
OR$SMDHyi = dat$es
OR$SMDHvi = dat$var


###
##Z statistic
zs <- fulldat2 %>%
  filter(Coef_Type == "z_stat")

##convert Z statistic to Cohen's D
zd <- z_to_d(z = zs$Coefficient,
             n = zs$Sample_size)

##Calculate variance for Cohen's D
zd$var <- (((zd$CI_high - zd$CI_low) / 3.92) * sqrt(zs$Sample_size))^2

##Convert Cohen's D to Hedge's G
zd$J = 1 - (3 / ((4 * zs$Sample_size + 2)- 1))
zd$g = zd$J * zd$d
zd$var_g = (zd$J^2) * zd$var

##Z statistics are non-directional; update signs to reflect direction of
##effect found in studies
zs$SMDHyi = zd$g * c(rep(1, times = 8), rep(-1, times = 5), 
                 rep(1, times = 5), -1, -1, 1, 1
                 )
zs$SMDHvi = zd$var_g


###
##F statistic
fs <- fulldat2 %>%
  filter(Coef_Type == "f_stat")

dat_f <- esc_f(f = as.numeric(fs$Coefficient),
               totaln = fs$Sample_size,
               es.type = "g")

##F statistics are non-directional; update signs to reflect direction of
##effect found in studies
fs$SMDHyi = dat_f$es * c(rep(1,times = 14), -1, -1, -1,
                     rep(1, times = 21), -1, -1, 1)
fs$SMDHvi = dat_f$var


###
##T statistic
ts <- fulldat2 %>%
  filter(Coef_Type == "t_stat")

dat_t <- esc_t(t = as.numeric(ts$Coefficient),
               totaln = ts$Sample_size,
               es.type = "g")


##Some T statistics have incorrect signs; update signs to reflect direction of
##effect found in studies
ts$SMDHyi = dat_t$es * c(rep(1, times = 23),-1,-1)
ts$SMDHvi = dat_t$var



###
##Regression
regs <- fulldat2 %>%
  filter(Coef_Type == "Regression")

regress <- esc_B(b = regs$Coefficient,       # unstandardized regression coefficient
              sdy = regs$SD_Y,       # standard deviation of predicted variable y
              grp1n = regs$Sample_size,   # sample size of the first group
              grp2n = regs$Sample_size,   # sample size of the second group
              es.type = "g") # convert to SMD; use "g" for Hedges' g

regs$SMDHyi <- regress$es
regs$SMDHvi = regress$var


###
##Correlation coefficients
corrs <- fulldat2 %>%
  filter(Coef_Type == "Correlation")

corrs = corrs %>%
  mutate(SMDHyi = ((2 * Coefficient) / sqrt(1 - Coefficient^2)) * (1 - (3 / ((4 * SampleSizeTreatment + 2)- 1))),
         SMDHvi = ((4 * CoefficientVariance) / ((1 - Coefficient^2)^3)) * ((1 - (3 / ((4 * SampleSizeTreatment + 2)- 1)))^2))


###
##Chi-square statistic
chis <- fulldat2 %>%
  filter(Coef_Type == "chisq")

dat_chisq <- (esc_chisq(chisq = chis$Coefficient,
                        totaln = chis$Sample_size,
                        es.type = "g"))

##Chisq are always positive; update signs to reflect direction of
##effect found in studies
chis$SMDHyi = dat_chisq$es * c(rep(1, times = 5), -1, 1, -1,
                           rep(1, times = 4), -1, 1, 1, -1)
chis$SMDHvi = dat_chisq$var

##combine and match to original dataset
newES <- bind_rows(noncoef_dat, chis, fs, ts, zs, corrs, regs, OR)

fulldat2$LRR_yich = newES$LRRyi[match(fulldat2$ind_id, newES$ind_id)]
fulldat2$LRR_vich = newES$LRRvi[match(fulldat2$ind_id, newES$ind_id)]

fulldat2$SMDH_yich = newES$SMDHyi[match(fulldat2$ind_id, newES$ind_id)]
fulldat2$SMDH_vich = newES$SMDHvi[match(fulldat2$ind_id, newES$ind_id)]

##Invert Enemy Release, if different from the previously calculated SMDH 
fulldat2$LRR_yich = ifelse(fulldat2$Effect == "Enemy Release" & (fulldat2$SMDH_yich * fulldat2$SMDH_yi) < 0,
                           -fulldat2$LRR_yich,
                           fulldat2$LRR_yich)

fulldat2$SMDH_yich = ifelse(fulldat2$Effect == "Enemy Release" & (fulldat2$SMDH_yich * fulldat2$SMDH_yi) < 0,
                            -fulldat2$SMDH_yich,
                            fulldat2$SMDH_yich)

##Need to change variances: many magnitudes of difference between high and low 
##LRR and SMDH variance values; Wolfgang Viechtbauer (author of `metafor` package)
##recommends changing those very large and very small values to less extreme values 

decr <- sort(fulldat2$LRR_vich, decreasing = T)
incr <- sort(fulldat2$LRR_vich)

decr2 <- sort(fulldat2$SMDH_vich, decreasing = T)
incr2 <- sort(fulldat2$SMDH_vich)


##convert all variances that are above 10 to be the greatest variance value in
##the dataset that is below 10 (for both LRR and SMDH)
##simultaneously convert all variances below 0.00001 (1E-5) to be the smallest
##variance value that is above 0.00001; this results in 7 magnitudes of difference
##between high and low variance values; while converting 21 LRR and 45 SMDH variances
##this is ~0.7% and ~1.5% of the data, respectively

##This approach allows for variance data to be very close to that of the original
##data, while allowing the rma.mv function to actually run; error will happen if 
##magnitude of difference between high and low variance values is < 8

fulldat2 = fulldat2 %>%
  mutate(LRR_vichC = ifelse(LRR_vich < incr[min(which(incr > 0.00001))],
                          incr[min(which(incr > 0.00001))],
                          ifelse(LRR_vich > decr[min(which(decr < 10))],
                                 decr[min(which(decr < 10))],
                                 LRR_vich)),
         SMDH_vichC = ifelse(SMDH_vich < incr2[min(which(incr2 > 0.00001))],
                          incr2[min(which(incr2 > 0.00001))],
                          ifelse(SMDH_vich > decr2[min(which(decr2 < 10))],
                                 decr2[min(which(decr2 < 10))],
                                 SMDH_vich)))

