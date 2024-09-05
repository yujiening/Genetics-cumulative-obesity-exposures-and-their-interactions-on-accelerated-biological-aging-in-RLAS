## Calculation of the cumulative obesity exposures
# Cumulative exposures of BMI
add_eBMI_to_df <- function(df) {
  calculate_ovw_gap <- function(BMI1, BMI2) {
    ovw_gap <- (BMI1 + BMI2) / 2 - 24
    ovw_gap[ovw_gap < 0] <- 0
    return(ovw_gap)
  }
  
  ovw_gap1 <- calculate_ovw_gap(df$BMI_14_V1, df$BMI_17_V1)
  ovw_gap2 <- calculate_ovw_gap(df$BMI_17_V1, df$BMI_19_V1)
  
  df$eBMI_V1 <- ovw_gap1 * 3 + ovw_gap2 * 2
  
  return(df)
}
cfat0322 <- add_eBMI_to_df(cfat0322)

# Cumulative exposures of WC
add_eWC_to_df <- function(df) {
  calculate_wc_gap <- function(wc1, wc2, sex) {
    wc_gap <- (wc1 + wc2) / 2 
    wc_gap <- ifelse(sex == 1, wc_gap - 90, wc_gap - 80)
    wc_gap <- pmax(wc_gap, 0)
    return(wc_gap)
  }
  
  wc_gap1 <- calculate_wc_gap(df$waist_14, df$waist_17, df$sex)
  wc_gap2 <- calculate_wc_gap(df$waist_17, df$waist_19, df$sex)

  df$eWC <- wc_gap1 * 3 + wc_gap2 * 2
  
  return(df)
}
cfat0322 <- add_eWC_to_df(cfat0322)


## Association of cumulative obesity exposures and aging metrics
# e.g. cBMI & KDM-BA
model1 <- lm(BA.21~eBMI_cat_V1+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=cfat_BAPD)
regress.display(model1) #categorical variable
model1 <- lm(BA.21~eBMI_num_V1+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=cfat_BAPD)
regress.display(model1) #P for trend
model1 <- lm(BA.21~eBMI_V1+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=cfat_BAPD)
summary(model1)  #continuous variable
confint(model1)


## Interactions of cumulative obesity exposures and longevity_PRS on aging metrics
# e.g. cBMI & KDM-BA
cfat_BAPD_PRS$PRS_scale <- scale(cfat_BAPD_PRS$Longevity_PRS_YJN)
Model1 <- lm(PD.21~eBMI_cat_V1+PRS_scale+eBMI_cat_V1*PRS_scale+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=cfat_BAPD_PRS)
regress.display(Model1)

## Stratified analyses of cumulative obesity exposures and aging metrics by longevity_PRS
# e.g. cBMI & KDM-BA
m <- median(cfat_BAPD_PRS$PRS_scale)
cfat_BAPD_PRS$Longevity_cat[cfat_BAPD_PRS$PRS_scale < m] <- 1
cfat_BAPD_PRS$Longevity_cat[cfat_BAPD_PRS$PRS_scale >= m] <- 2
table(cfat_BAPD_PRS$Longevity_cat)
Longevity_low <- cfat_BAPD_PRS[cfat_BAPD_PRS$Longevity_cat==1,]
Longevity_high <- cfat_BAPD_PRS[cfat_BAPD_PRS$Longevity_cat==2,]

#low PRS
Model1 <- lm(BA.21~eBMI_cat_V1+PRS_scale+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=Longevity_low)
regress.display(Model1)

#high PRS
Model1 <- lm(BA.21~eBMI_cat_V1+PRS_scale+age.19+sex.x+marital+education+occupation+smoke1_19+alcohol1_19+activity4_19, data=Longevity_high)
regress.display(Model1)


