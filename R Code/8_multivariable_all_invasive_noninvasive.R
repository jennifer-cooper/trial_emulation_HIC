#---------------------------------------------------------------------------------------------------------------------------
#Multivariable deaths within 3 days assigned to all invasive or all non invasive
#---------------------------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#---------------------------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("4_analysis_dataset_with_ipw.rds")
options(scipen=999)
#---------------------------------------------------------------------------------------------------------------------------
#Death in 3 days all assigned as NON-INVASIVE
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(propsgroup2>1 & propsgroup2<8) %>% 
  mutate(deaths3days_noninvasive=if_else(DIEDwithin3daysFROMpeakTROP==1,0,Invasive_at_3_daysFROMpeakTROP))->analysis_dataset_d_noninvasive
#---------------------------------------------------------------------------------------------------------------------------
multi_d_noninvasive <- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ deaths3days_noninvasive + agesp1 + agesp2 + 
        creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
        sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
        Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
        Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
        Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
        Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
        Assistance + propscore2sp1 + propscore2sp2 + strata(brcname),
      data = analysis_dataset_d_noninvasive)


summary(multi_d_noninvasive)  #output provides HR CIs
confint(multi_d_noninvasive)  #coefficient CIs
exp(confint(multi_d_noninvasive))
#---------------------------------------------------------------------------------------------------------------------------
multi_d_noninvasive %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> multi_d_noninvasive

round(multi_d_noninvasive$p.value, 3) -> multi_d_noninvasive$p.value

multi_d_noninvasive$model_name <- c("multivariable with deaths3days non-invasive")

#---------------------------------------------------------------------------------------------------------------------------







#---------------------------------------------------------------------------------------------------------------------------
#Death in 3 days all assigned as INVASIVE
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(propsgroup2>1 & propsgroup2<8) %>% 
  mutate(deaths3days_invasive=if_else(DIEDwithin3daysFROMpeakTROP==1,1,Invasive_at_3_daysFROMpeakTROP))->analysis_dataset_d_invasive
#---------------------------------------------------------------------------------------------------------------------------
multi_d_invasive <- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ deaths3days_invasive + agesp1 + agesp2 + 
                               creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                               sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                               Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                               Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                               Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
                               Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
                               Assistance + propscore2sp1 + propscore2sp2 + strata(brcname),
                             data = analysis_dataset_d_invasive)


summary(multi_d_invasive)  #output provides HR CIs
confint(multi_d_invasive)  #coefficient CIs
exp(confint(multi_d_invasive))

#---------------------------------------------------------------------------------------------------------------------------
multi_d_invasive %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> multi_d_invasive

round(multi_d_invasive$p.value, 3) -> multi_d_invasive$p.value

multi_d_invasive$model_name <- c("multivariable with deaths3days invasive")

#---------------------------------------------------------------------------------------------------------------------------
#Notes:
#In Stata if follow-up ends at 0 then patients aren't included in the analysis
#You can alter this by adding 0.001 to PEAKTntoCENSOR but this isnt required in R
#analysis_set$PEAKTntoCENSOR[analysis_set$PEAKTntoCENSOR==0] <- 0.001
#---------------------------------------------------------------------------------------------------------------------------






#---------------------------------------------------------------------------------------------------------------------------
#Multivariable model Including patients whose propensity score were between the 25th and 75th percentiles
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2==5 | propsgroup2==6))-> analysis_dataset_25_75

#1647
#---------------------------------------------------------------------------------------------------------------------------
multi_25_75 <- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
                            creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                            sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                            Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                            Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                            Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
                            Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
                            Assistance + propscore2sp1 + propscore2sp2 + strata(brcname),
                          data = analysis_dataset_25_75, control = coxph.control(iter.max = 50))


summary(multi_25_75)  #output provides HR CIs
confint(multi_25_75)  #coefficient CIs
exp(confint(multi_25_75))


#---------------------------------------------------------------------------------------------------------------------------

multi_25_75 %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> multi_25_75

round(multi_25_75$p.value, 3) -> multi_25_75$p.value

multi_25_75$model_name <- c("multivariable propen between 25 and 75")

#---------------------------------------------------------------------------------------------------------------------------
#Combine all 3 model results together for export
rbind(multi_d_noninvasive, multi_d_invasive, multi_25_75) -> multi_invas_noninvas_25_75
#---------------------------------------------------------------------------------------------------------------------------

write.csv(multi_invas_noninvas_25_75, "table3_multiinvas_noninvas_25_75.csv")

#---------------------------------------------------------------------------------------------------------------------------










#---------------------------------------------------------------------------------------------------------------------------
#Model converging investigations if needed
#---------------------------------------------------------------------------------------------------------------------------

as.data.frame(exp(multi_25_75[["coefficients"]])) -> multi_tab
round(multi_tab, 3)-> multi_tab
multi_tab$observation <- 1:nrow(multi_tab) 

#Convert row names into first column
multi_tab<- cbind(rownames(multi_tab), data.frame(multi_tab, row.names=NULL))

multi_tab %>% 
  filter(observation==28| observation==35 | observation==44) %>% View()

#Urinary_incontinence
#Constipation
#Visual


#---------------------------------------------------------------------------------------------------------------------------
contVars <- c("ageyears","crp_result", "creatinine_result", 
              "haemoglobin_result","plateletcount_result", "potassium_result", "sodium_result", "whitecellcount_result", "Standardised_peak_troponin",
              "fraility_score")


catVars <- c("Male_sex",  "hosp_gstt",  "hosp_ichnt","hosp_oxfor", "hosp_uclh",  "diabetes",  "FhxCHD",  "Hypercholesterolaemia",  "Hypertension",  "Smoking",
             "Aortic_aneurysm",  "Angina",  "aortic_stenosis",  "atrial_fibrillation",  "Cardiac_arrest",  "Low_grade_AV_block",  "Complete_AV_block",
             "HF",  "PVD",  "Previous_MI",  "SVT",  "VFib",  "VT",  "AKI",  "CKD",  "UTI",  "Urinary_catheterisation",  "Urinary_incontinence",  "Interstitial_lung_disease",
             "Obstructive_Lung_Disease",  "Asthma",  "Pneumonia",  "Pulmonary_embolism",  "Respiratory_Failure",  "Ischaemic_stroke",  "TIA",  "Haemorrhagic_stroke",
             "SAH",  "SDH",  "Parkinsons",  "Alcohol_excess",  "Anxiety",  "Bipolar",  "Delirium",  "Depression",  "Dementia",  "Gastric_ulcer",
             "GI_haemorrhage",  "Constipation",  "Faecal_incontinence",  "Liver_disease",  "Arthritis",  "Fracture",  "Osteoporosis",  "Malignant_neoplasm",
             "Benign_neoplasm",  "Leukaemia",  "Lymphoma",  "Sepsis",  "Inflammatory_disorder",  "fraility_score",  "Fall",  "Hearing",  "Visual",
             "Assistance",  "Weight_loss")


for (i in catVars){
  print(i)
  print(table(analysis_dataset_25_75[, i], useNA='always'))
  
}


#For this population these vars have 0 occurrences

#Weight_loss
#Leukaemia
#Haemorrhagic_stroke
#---------------------------------------------------------------------------------------------------------------------------
#Removing problematic vars

multi_25_75_2 <- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
                       creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                       sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                       Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                       Previous_MI + VT + AKI + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                       Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Liver_disease + 
                       Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing +  
                       Assistance + propscore2sp1 + propscore2sp2 + strata(brcname),
                     data = analysis_dataset_25_75, control = coxph.control(iter.max = 50))


summary(multi_25_75_2)  #output provides HR CIs
confint(multi_25_75_2)  #coefficient CIs
exp(confint(multi_25_75_2))


#---------------------------------------------------------------------------------------------------------------------------

multi_25_75_2 %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> multi_25_75_2

round(multi_25_75_2$p.value, 3) -> multi_25_75_2$p.value

multi_25_75_2$model_name <- c("multivariable propen between 25 and 75 removal vars")

#---------------------------------------------------------------------------------------------------------------------------
write.csv(multi_25_75_2, "table3_multi_25_75_removal_vars.csv")
#---------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------


