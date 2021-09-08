#---------------------------------------------------------------------------------------------------------------------------
#Primary analysis multiple imputation
#---------------------------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#---------------------------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("4_analysis_dataset_with_ipw.rds")
#---------------------------------------------------------------------------------------------------------------------------
#Reinclude patients who died within 3 days who were eligible for both interventions based on the propensity score 
#---------------------------------------------------------------------------------------------------------------------------

analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==1) %>% 
  count()

#62 died within 3 days


analysis_dataset %>% 
  filter(propsgroup2<2 | propsgroup2>7) %>% 
  count()

#370 had extreme propensity score


analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==1 & (propsgroup2<2 | propsgroup2>7)) %>% 
  count()

#6 died within 3 days AND had extreme propensity


analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==1 & (propsgroup2>1 & propsgroup2<8)) %>% 
  count()

#56 died within 3 days and had valid propensity score to include for multiple imputation

#---------------------------------------------------------------------------------------------------------------------------
#Check
#analysis_dataset %>% 
#  filter(propsgroup2>1 & propsgroup2<8) %>% 
#  count()

#2986
#---------------------------------------------------------------------------------------------------------------------------
#Remove extreme propensity score but keep those who have died so should be 56 additional people compared to the dataset which removes
#those who died within 3 days and extreme propensity score

analysis_dataset %>% 
  filter(propsgroup2>1 & propsgroup2<8)-> analysis_restrict_3
#---------------------------------------------------------------------------------------------------------------------------
#Check
#analysis_dataset %>% 
#  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2>1 & propsgroup2<8)) %>% count()

#---------------------------------------------------------------------------------------------------------------------------






#---------------------------------------------------------------------------------------------------------------------------
#Multiple Imputation
#---------------------------------------------------------------------------------------------------------------------------
#Setting a seed so every time the MI values created are 
#reproducible as there is a random component

set.seed(23423432)

#Generating 20 uniformly distributed values between 0 and 1
for (i in 1:20) {
  
  analysis_restrict_3[, paste0("mi_", i)] = runif(nrow(analysis_restrict_3), min= 0, max = 1)
}
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  select(starts_with("mi_")) %>% View()
#---------------------------------------------------------------------------------------------------------------------------
#This code generates 20 imputed invasive/not invasive variables based on the uniform generated variables above and the propensity scores
#If the propensity score is smaller than the uniform generated value then they are placed in the non-invasive group and vice versa
#For patients who didnt die before 3 days, replace the invasive variables values with those in the original variable.

for (i in 1:20) {
  
  analysis_restrict_3[ , paste0("invasive_mi_", i)] <- ifelse(analysis_restrict_3$DIEDwithin3daysFROMpeakTROP==1 &
                                                                analysis_restrict_3$propscore2<analysis_restrict_3[ , paste0("mi_", i)], 0,
                                                           ifelse(analysis_restrict_3$DIEDwithin3daysFROMpeakTROP==1 &
                                                                    analysis_restrict_3$propscore2>=analysis_restrict_3[ , paste0("mi_", i)], 1,
                                                                  ifelse(analysis_restrict_3$DIEDwithin3daysFROMpeakTROP==0, analysis_restrict_3$Invasive_at_3_daysFROMpeakTROP, NA))
  )
  
  
}

#---------------------------------------------------------------------------------------------------------------------------
#For those who died within 3 days and had invasive management, these are also assigned as their original invasive variable
#Can assign this in the above loop but separated for clarity

for (i in 1:20) {
   
  analysis_restrict_3[ , paste0("invasive_mi_", i)] <- ifelse(analysis_restrict_3$DIEDwithin3daysFROMpeakTROP==1 & analysis_restrict_3$Invasive_at_3_daysFROMpeakTROP==1,
                                                                analysis_restrict_3$Invasive_at_3_daysFROMpeakTROP,  analysis_restrict_3[ , paste0("invasive_mi_", i)])
  
  
}


#---------------------------------------------------------------------------------------------------------------------------

#Check of code
#could use starts_with...

vars<- c("propscore2", "mi_1", "mi_2", "mi_3", "mi_4", "mi_5", "mi_6", "mi_7", "mi_8", 
         "mi_9", "mi_10", "mi_11", "mi_12", "mi_13", "mi_14", "mi_15", 
         "mi_16", "mi_17", "mi_18", "mi_19", "mi_20", "invasive_mi_1", 
         "invasive_mi_2", "invasive_mi_3", "invasive_mi_4", 
         "invasive_mi_5", "invasive_mi_6", "invasive_mi_7", "invasive_mi_8", 
         "invasive_mi_9", "invasive_mi_10", "invasive_mi_11", "invasive_mi_12", 
         "invasive_mi_13", "invasive_mi_14", "invasive_mi_15", "invasive_mi_16", 
         "invasive_mi_17", "invasive_mi_18", "invasive_mi_19", "invasive_mi_20", 
         "DIEDwithin3daysFROMpeakTROP", "Invasive_at_3_daysFROMpeakTROP", "propsgroup2")

analysis_restrict_3[, vars] %>%View()

#---------------------------------------------------------------------------------------------------------------------------
#Code to check how imputed variables compare with the propensity scores
#---------------------------------------------------------------------------------------------------------------------------
analysis_restrict_3$mi_score = 0


for (i in 1:20) {
  analysis_restrict_3$mi_score<- analysis_restrict_3$mi_score + analysis_restrict_3[ , paste0("invasive_mi_", i)]
}

analysis_restrict_3$mi_score<- analysis_restrict_3$mi_score/20

#Creates scatterplot
#is propscore2 predicted mean??? just uses predict function after logreg so same as this
#Checks how imputed variables compare with propensity scores

analysis_restrict_3 %>% 
  filter(DIEDwithin3daysFROMpeakTROP==1 & Invasive_at_3_daysFROMpeakTROP==0) %>% 
  ggplot(aes(x=mi_score, y = propscore2)) +
  geom_point()+
  geom_smooth(method="glm", formula = y~x) + #adds regression line +
  xlab("Imputed scores") +
  ylab("Propensity Score")

#---------------------------------------------------------------------------------------------------------------------------













#---------------------------------------------------------------------------------------------------------------------------
#Select variables of interest for MICE modelling
#---------------------------------------------------------------------------------------------------------------------------

analysis_restrict_3 %>% 
  select(Invasive_at_3_daysFROMpeakTROP, ageyears, creatinine_result, haemoglobin_result, 
         plateletcount_result, sodium_result, log_spt, whitecellcount_result, diabetes, FhxCHD, 
           Smoking, Aortic_aneurysm, aortic_stenosis, atrial_fibrillation, Cardiac_arrest, HF, PVD,
           Previous_MI, VT, AKI, Urinary_incontinence, Interstitial_lung_disease, Obstructive_Lung_Disease, 
           Asthma, Pulmonary_embolism, Respiratory_Failure, Ischaemic_stroke, Constipation,Liver_disease, 
           Fracture, Malignant_neoplasm, Benign_neoplasm, Lymphoma, Sepsis, Fall, Hearing, Visual, 
           Assistance, starts_with("invasive_mi_"), propscore2, propscore2sp1, propscore2sp2, propsgroup2,
         PEAKTntoCENSOR, lifeststusall ,brcname, exemplarid,
         agesp1, agesp2, creatininesp1, creatininesp2, haemoglobinsp1, haemoglobinsp2,
         plateletcountsp1, plateletcountsp2, sodiumsp1, sodiumsp2, lsptsp1, lsptsp2, whitecellsp1, whitecellsp2, DIEDwithin3daysFROMpeakTROP) ->analysis_dataset_mice


#---------------------------------------------------------------------------------------------------------------------------
#Rename or probably better to create new column the invasive 3 day variable as you need the original data which will be
#invasive_mi_0

analysis_dataset_mice$invasive_mi_0 <- analysis_dataset_mice$Invasive_at_3_daysFROMpeakTROP

# I imputed whether invasive or not earlier, need to make dataset 0 or the original dataset missing

analysis_dataset_mice %>% 
  mutate(invasive_mi_0 = replace(invasive_mi_0, DIEDwithin3daysFROMpeakTROP==1 & invasive_mi_0==0, NA))-> analysis_dataset_mice
#---------------------------------------------------------------------------------------------------------------------------
#Now manipulate the data into long format using gather from tidyr, .imp is the name of the variable needed for MICE

vlong <- analysis_dataset_mice %>% tidyr::gather(".imp", "invasive_mi", c("invasive_mi_0","invasive_mi_1", "invasive_mi_2", 
                                                                          "invasive_mi_3", "invasive_mi_4", "invasive_mi_5", "invasive_mi_6", 
                                                                          "invasive_mi_7", "invasive_mi_8", "invasive_mi_9", "invasive_mi_10", 
                                                                          "invasive_mi_11", "invasive_mi_12", "invasive_mi_13", "invasive_mi_14", 
                                                                          "invasive_mi_15", "invasive_mi_16", "invasive_mi_17", "invasive_mi_18", 
                                                                          "invasive_mi_19", "invasive_mi_20"))


#vlong %>% View
#---------------------------------------------------------------------------------------------------------------------------
#This code then extracts the format of the data needed for .imp and creates .id variable also needed for MICE
#---------------------------------------------------------------------------------------------------------------------------
management_vlong<- vlong %>% 
  tidyr::extract(".imp", c("colname", ".imp"),
                 regex = "([a-z_a-z_]+)(\\d+)") %>%
  rename(".id" = "exemplarid")   

management_vlong %>% View

management_vlong$.imp<-as.numeric(management_vlong$.imp)


management_vlong %>% 
  select(.id, .imp, DIEDwithin3daysFROMpeakTROP, Invasive_at_3_daysFROMpeakTROP, invasive_mi) %>% 
  filter(DIEDwithin3daysFROMpeakTROP==1) %>% View()

#only NA for .imp==0

management_vlong %>% 
  filter(.imp==0) %>% 
  count()
#Gives 2,986

table(management_vlong$.imp, useNA = 'always')
#---------------------------------------------------------------------------------------------------------------------------
#mids is the format of data needed for MICE so it converts the long format we have created to mids format which essentially stores
#a load of imputed datasets

long_mids<-as.mids(management_vlong)

#To get it back anesimp_long <- mice::complete(imp2, action="long", include = TRUE)

#---------------------------------------------------------------------------------------------------------------------------
#loggedEvents can help identify problems with the data
#only include variables you need to avoid these constants etc 
#long_mids$loggedEvents

#---------------------------------------------------------------------------------------------------------------------------
#Finally run the cox regression on each of the 20 datasets and pool the estimates together 
#R will estimate our regression model separately for each imputed dataset
#This can be achieved using the 'with' function in the mice package.
#Which summarizes or pools those estimates to get one overall set of parameter estimates.
#Make sure this is invasive_mi NOT Invasive_at_3_daysFROMpeakTROP

options(scipen=999)


primarymi_model <- with(long_mids,
                            coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ invasive_mi + agesp1 + agesp2 + 
                                    creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                                    sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                                    Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                                    Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                                    Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
                                    Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
                                    Assistance + propscore2sp1 + propscore2sp2 + strata(brcname)))
                            





pooled_model_results <-summary(pool(primarymi_model))

pooled_model_results_CI <- summary(pool(primarymi_model), conf.int = TRUE,conf.level = 0.95)


pooled_model_results_CI$exp_2.5 <- exp(pooled_model_results_CI$`2.5 %`)
pooled_model_results_CI$exp_97.5 <- exp(pooled_model_results_CI$`97.5 %`)
pooled_model_results_CI$exp_estimate<- exp(pooled_model_results_CI$estimate)


#---------------------------------------------------------------------------------------------------------------------------
write.csv(pooled_model_results_CI, "primarymi_model.csv")

#---------------------------------------------------------------------------------------------------------------------------
#I think the warning messages are ignorable - https://stefvanbuuren.name/fimd/sec-sensitivity.html
#Get the same answer as with Stata method.
#The function pool in the mice package pools and analyzes by Rubin's rules
#---------------------------------------------------------------------------------------------------------------------------
#Pool the proportion of invasive
#https://stackoverflow.com/questions/42438965/pool-scalar-function-of-mice-package-returning-nas
#Proportions are means of indicator variables (same method as Stata)
#---------------------------------------------------------------------------------------------------------------------------

m<- long_mids$m

Q <- rep(NA, m)

U <- rep(NA, m)

for (i in 1:m) {
  Q[i] <- mean(complete(long_mids, i)$invasive_mi==1)
  U[i] <- var(complete(long_mids, i)$invasive_mi==1) / nrow(analysis_dataset_mice) # (standard error of estimate)^2
}

pool.scalar(Q, U) -> estimated_prop

nrow(subset(management_vlong, .imp==0))*estimated_prop[["qbar"]]-> num_invas
nrow(subset(management_vlong, .imp==0))*(1-estimated_prop[["qbar"]]) -> num_noninvas

num_invas
#1648.85
num_noninvas
#1337.15

#---------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------


