#---------------------------------------------------------------------------------------------------------------------------
#Multivariable model plus propensity score adjustment
#---------------------------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#---------------------------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("4_analysis_dataset_with_ipw.rds")
#---------------------------------------------------------------------------------------------------------------------------
#Restricting the population to those who have not died in 3 days and those without extreme propensity score
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2>1 & propsgroup2<8)) -> analysis_restrict_2
#---------------------------------------------------------------------------------------------------------------------------
#Determining variables which will be included in the multivariable model
#---------------------------------------------------------------------------------------------------------------------------
#Remove variables with an occurrence of 1 or 0

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
  print(table(analysis_restrict_2[, i], useNA='always'))
  
}

#SAH, SDH


for (i in contVars){
  print(i)
  print(summary(analysis_restrict_2[, i]))
  
}

#---------------------------------------------------------------------------------------------------------------------------




#Considering all variables as in logistic regression

fm<- "Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ 
    Invasive_at_3_daysFROMpeakTROP + rcs(ageyears, 3) + Male_sex + rcs(creatinine_result, 3) + 
    rcs(haemoglobin_result, 3) + rcs(plateletcount_result, 3) +
    rcs(potassium_result, 3) + rcs(sodium_result, 3) + rcs(log_spt, 3) +
    rcs(whitecellcount_result, 3) + diabetes + FhxCHD + Hypercholesterolaemia +
    Hypertension + Smoking + Aortic_aneurysm + Angina + aortic_stenosis +
    atrial_fibrillation + Cardiac_arrest + Low_grade_AV_block + Complete_AV_block +
    HF + PVD + Previous_MI + SVT + VFib + VT + AKI + CKD + UTI + Urinary_catheterisation +
    Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease +
    Asthma + Pneumonia + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke +
    TIA + Haemorrhagic_stroke + SAH + SDH + Parkinsons + Alcohol_excess + Anxiety +
    Bipolar + Delirium + Depression + Dementia + Gastric_ulcer +
    GI_haemorrhage+ Constipation+ Faecal_incontinence + Liver_disease + Arthritis +
    Fracture + Osteoporosis + Malignant_neoplasm + Benign_neoplasm + Leukaemia+Lymphoma +
    Sepsis + Inflammatory_disorder + fraility_score + Fall + Hearing + Visual + Assistance + 
    Weight_loss + rms::strat(brcname)"

#with SAH and SDH removed

fm<- "Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ 
    Invasive_at_3_daysFROMpeakTROP + rcs(ageyears, 3) + Male_sex + rcs(creatinine_result, 3) + 
    rcs(haemoglobin_result, 3) + rcs(plateletcount_result, 3) +
    rcs(potassium_result, 3) + rcs(sodium_result, 3) + rcs(log_spt, 3) +
    rcs(whitecellcount_result, 3) + diabetes + FhxCHD + Hypercholesterolaemia +
    Hypertension + Smoking + Aortic_aneurysm + Angina + aortic_stenosis +
    atrial_fibrillation + Cardiac_arrest + Low_grade_AV_block + Complete_AV_block +
    HF + PVD + Previous_MI + SVT + VFib + VT + AKI + CKD + UTI + Urinary_catheterisation +
    Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease +
    Asthma + Pneumonia + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke +
    TIA + Haemorrhagic_stroke + Parkinsons + Alcohol_excess + Anxiety +
    Bipolar + Delirium + Depression + Dementia + Gastric_ulcer +
    GI_haemorrhage+ Constipation+ Faecal_incontinence + Liver_disease + Arthritis +
    Fracture + Osteoporosis + Malignant_neoplasm + Benign_neoplasm + Leukaemia+Lymphoma +
    Sepsis + Inflammatory_disorder + fraility_score + Fall + Hearing + Visual + Assistance + 
    Weight_loss + rms::strat(brcname)"

#---------------------------------------------------------------------------------------------------------------------------
fit_full <- rms::cph(formula = as.formula(fm),## Run full model with all covariates
                     data= analysis_restrict_2, 
                     x = TRUE,
                     y = TRUE,
                     maxit=100000)

#Ran out of iterations did not converge, you can fit a coxph model which tells you the problematic variables
#---------------------------------------------------------------------------------------------------------------------------
cox_step<- coxph(Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ 
        Invasive_at_3_daysFROMpeakTROP + rcs(ageyears, 3) + Male_sex + rcs(creatinine_result, 3) + 
        rcs(haemoglobin_result, 3) + rcs(plateletcount_result, 3) +
        rcs(potassium_result, 3) + rcs(sodium_result, 3) + rcs(log_spt, 3) +
        rcs(whitecellcount_result, 3) + diabetes + FhxCHD + Hypercholesterolaemia +
        Hypertension + Smoking + Aortic_aneurysm + Angina + aortic_stenosis +
        atrial_fibrillation + Cardiac_arrest + Low_grade_AV_block + Complete_AV_block +
        HF + PVD + Previous_MI + SVT + VFib + VT + AKI + CKD + UTI + Urinary_catheterisation +
        Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease +
        Asthma + Pneumonia + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke +
        TIA + Haemorrhagic_stroke + SAH + SDH + Parkinsons + Alcohol_excess + Anxiety +
        Bipolar + Delirium + Depression + Dementia + Gastric_ulcer +
        GI_haemorrhage+ Constipation+ Faecal_incontinence + Liver_disease + Arthritis +
        Fracture + Osteoporosis + Malignant_neoplasm + Benign_neoplasm + Leukaemia+Lymphoma +
        Sepsis + Inflammatory_disorder + fraility_score + Fall + Hearing + Visual + Assistance + 
        Weight_loss + strata(brcname), data = analysis_restrict_2)


#Warning message:
#  In fitter(X, Y, istrat, offset, init, control, weights = weights,  :
#              Loglik converged before variable  40,49,56,70 ; coefficient may be infinite.

#---------------------------------------------------------------------------------------------------------------------------
#Identify vars 40, 49, 56 and 70

as.data.frame(exp(cox_step[["coefficients"]])) -> cox_step_tab
round(cox_step_tab, 3)-> cox_step_tab
cox_step_tab$observation <- 1:nrow(cox_step_tab) 

#Convert row names into first column
cox_step_tab<- cbind(rownames(cox_step_tab), data.frame(cox_step_tab, row.names=NULL))

cox_step_tab %>% 
  filter(observation==40| observation==49 | observation==56 | observation==70) %>% View()

#40 Urinary_catheterisation
#49 TIA
#56 Bipolar
#70 Leukaemia

#Remove these for the modelling
#---------------------------------------------------------------------------------------------------------------------------
fm_corrected <- "Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ 
    Invasive_at_3_daysFROMpeakTROP + rcs(ageyears, 3) + Male_sex + rcs(creatinine_result, 3) + 
    rcs(haemoglobin_result, 3) + rcs(plateletcount_result, 3) +
    rcs(potassium_result, 3) + rcs(sodium_result, 3) + rcs(log_spt, 3) +
    rcs(whitecellcount_result, 3) + diabetes + FhxCHD + Hypercholesterolaemia +
    Hypertension + Smoking + Aortic_aneurysm + Angina + aortic_stenosis +
    atrial_fibrillation + Cardiac_arrest + Low_grade_AV_block + Complete_AV_block +
    HF + PVD + Previous_MI + SVT + VFib + VT + AKI + CKD + UTI + 
    Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease +
    Asthma + Pneumonia + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke +
    Haemorrhagic_stroke + SAH + SDH + Parkinsons + Alcohol_excess + Anxiety +
    Delirium + Depression + Dementia + Gastric_ulcer +
    GI_haemorrhage+ Constipation+ Faecal_incontinence + Liver_disease + Arthritis +
    Fracture + Osteoporosis + Malignant_neoplasm + Benign_neoplasm + Lymphoma +
    Sepsis + Inflammatory_disorder + fraility_score + Fall + Hearing + Visual + Assistance + 
    Weight_loss + rms::strat(brcname)"

#---------------------------------------------------------------------------------------------------------------------------
fit_full <- rms::cph(formula = as.formula(fm_corrected),## Run full model with all covariates
                     data= analysis_restrict_2, 
                     x = TRUE,
                     y = TRUE,
                     maxit=100000)


#---------------------------------------------------------------------------------------------------------------------------
#Backwards model, p value 0.2 forcing invasive to remain in the model 
#---------------------------------------------------------------------------------------------------------------------------

backwards <- rms::fastbw(fit_full, rule = "p", sls = 0.2, type = "individual", force =analysis_restrict_2$Invasive_at_3_daysFROMpeakTROP)

#---------------------------------------------------------------------------------------------------------------------------
formula <- ""

for (i in backwards$names.kept) {
  formula<- paste0(formula, " + ",i)
}

formula <- paste0("Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ ", 
                  substr(formula,4,nchar(formula)))
#---------------------------------------------------------------------------------------------------------------------------
#Selected model 
#---------------------------------------------------------------------------------------------------------------------------
"Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + ageyears + creatinine_result + haemoglobin_result + plateletcount_result + sodium_result + log_spt + whitecellcount_result + diabetes + FhxCHD + Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + Previous_MI + VFib + VT + AKI + UTI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + SAH + SDH + GI_haemorrhage + Liver_disease + Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + Assistance"

#Adding the splines (can add them within model or I have also created them outside the model):

#Adding propensity score splines and strata brcname  
selected_formula <- "Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + Previous_MI + VFib + VT + AKI + UTI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + SAH + SDH + GI_haemorrhage + Liver_disease + Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + Assistance + propscore2sp1 + propscore2sp2 + strata(brcname)"

  
#After removing SAH and SDH

"Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + Assistance + propscore2sp1 + propscore2sp2 + strata(brcname)"

#---------------------------------------------------------------------------------------------------------------------------

multivariable_ps_adjust<- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + Previous_MI + VFib + VT + AKI + UTI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + SAH + SDH + GI_haemorrhage + Liver_disease + Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data = analysis_restrict_2)


#After removing SAH and SDH
multivariable_ps_adjust<- coxph(formula = Surv(PEAKTntoCENSOR, lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
                                  creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                                  sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                                  Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                                  Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                                  Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
                                  Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
                                  Assistance + propscore2sp1 + propscore2sp2 + strata(brcname),
                                data = analysis_restrict_2)

#---------------------------------------------------------------------------------------------------------------------------
multivariable_ps_adjust %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> multivariable_ps_adjust

round(multivariable_ps_adjust$p.value, 3) -> multivariable_ps_adjust$p.value

multivariable_ps_adjust$model_name <- c("multivariable with PS adjustment")
#---------------------------------------------------------------------------------------------------------------------------
cox_mods<- read.csv("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/outputs/Tables/cox_mods.csv")

cox_mods %>% select(-X) ->cox_mods

rbind(cox_mods, multivariable_ps_adjust) -> cox_mods_2

#---------------------------------------------------------------------------------------------------------------------------
cox_mods_2<- write.csv(cox_mods_2, "S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/outputs/Tables/cox_mods_2.csv")
#---------------------------------------------------------------------------------------------------------------------------

















