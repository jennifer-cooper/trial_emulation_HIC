#------------------------------------------------------------------------------------------------------
#Propensity Scoring
#------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
#------------------------------------------------------------------------------------------------------
#Sort the package installation
#------------------------------------------------------------------------------------------------------
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/e1071_1.7-4.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/VIM_6.1.0.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/survMisc_0.5.5.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/laeken_0.5.1.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/KMsurv_0.1-5.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/ranger_0.12.1.zip", repos = NULL, type = "win.binary")

install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/survminer_0.4.8.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/km.ci_0.5-2.zip", repos = NULL, type = "win.binary")

install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/simputation_0.2.6.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/vcd_1.4-8.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/xtable_1.8-4.zip", repos = NULL, type = "win.binary")

install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/survey_4.0.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/tableone_0.12.0.zip", repos = NULL, type = "win.binary")
install.packages("S:/Business Intelligence - Covid Analytics Project/R Packages/labelled_2.7.0.zip", repos = NULL, type = "win.binary")

#------------------------------------------------------------------------------------------------------
#Ignore warning of 'Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding'

library(mice)
library(survival)
library(survminer)
library(VIM)
library(tidyverse)
library(simputation)

library(dplyr)
library(broom)
library(tableone)
library(Hmisc)
library(rms)
library(pROC)
library(ggplot2)
library(purrr)
#------------------------------------------------------------------------------------------------------
#Read in analysis dataset dervied from the initial script
#------------------------------------------------------------------------------------------------------
analysis_dataset <- readRDS("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/1_analysis_dataset.rds")
#------------------------------------------------------------------------------------------------------
#Propensity score analysis - GLM: binomial with logit link and Splines
#------------------------------------------------------------------------------------------------------
#filter analysis dataset to not died

analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) %>%count() 

#new 3294
#------------------------------------------------------------------------------------------------------
#Generating splines for continuous variables
#------------------------------------------------------------------------------------------------------
#library(Hmisc)
#library(rms)
#library(dplyr)

#check<-rcs(analysis_dataset$haemoglobin_result, 3)


agesp<- as.data.frame(rcspline.eval(analysis_dataset$ageyears, nk=3, inclx=TRUE))
analysis_dataset<- agesp %>% 
  rename(agesp1 = x,
         agesp2= V2) %>% 
  cbind(analysis_dataset)


creatininesp<- as.data.frame(rcspline.eval(analysis_dataset$creatinine_result, nk=3, inclx=TRUE))
analysis_dataset<- creatininesp %>% 
  rename(creatininesp1 = x,
         creatininesp2= V2) %>% 
  cbind(analysis_dataset)

haemoglobinsp<- as.data.frame(rcspline.eval(analysis_dataset$haemoglobin_result, nk=3, inclx=TRUE))
analysis_dataset<- haemoglobinsp %>% 
  rename(haemoglobinsp1 = x,
         haemoglobinsp2= V2) %>% 
  cbind(analysis_dataset)

plateletcountsp<- as.data.frame(rcspline.eval(analysis_dataset$plateletcount_result, nk=3, inclx=TRUE))
analysis_dataset<- plateletcountsp %>% 
  rename(plateletcountsp1 = x,
         plateletcountsp2= V2) %>% 
  cbind(analysis_dataset)

potassiumsp<- as.data.frame(rcspline.eval(analysis_dataset$potassium_result, nk=3, inclx=TRUE))
analysis_dataset<- potassiumsp %>% 
  rename(potassiumsp1 = x,
         potassiumsp2= V2) %>% 
  cbind(analysis_dataset)


sodiumsp<- as.data.frame(rcspline.eval(analysis_dataset$sodium_result, nk=3, inclx=TRUE))
analysis_dataset<- sodiumsp %>% 
  rename(sodiumsp1 = x,
         sodiumsp2= V2) %>% 
  cbind(analysis_dataset)


whitecellsp<- as.data.frame(rcspline.eval(analysis_dataset$whitecellcount_result, nk=3, inclx=TRUE))
analysis_dataset<- whitecellsp %>% 
  rename(whitecellsp1 = x,
         whitecellsp2= V2) %>% 
  cbind(analysis_dataset)


analysis_dataset$log_spt<-log(analysis_dataset$Standardised_peak_troponin)

log_spt_sp<- as.data.frame(rcspline.eval(analysis_dataset$log_spt, nk=3, inclx=TRUE))
analysis_dataset<- log_spt_sp %>% 
  rename(lsptsp1 = x,
         lsptsp2= V2) %>% 
  cbind(analysis_dataset)

#------------------------------------------------------------------------------------------------------
#Create individual hospital variables
#------------------------------------------------------------------------------------------------------

table(analysis_dataset$brcname)


analysis_dataset %>% 
  mutate(hosp_gstt = if_else(brcname=="GSTT", 1,0),
         hosp_ichnt = if_else(brcname=="ICHNT", 1,0),
         hosp_oxfor = if_else(brcname=="OXFOR", 1,0),
         hosp_uclh = if_else(brcname=="UCLH", 1,0)) -> analysis_dataset


xtabs(~hosp_uclh + Invasive_at_3_daysFROMpeakTROP, alive_analysis_dataset)

table(analysis_dataset$hosp_gstt)
table(analysis_dataset$hosp_ichnt)
table(analysis_dataset$hosp_oxfor)
table(analysis_dataset$hosp_uclh)

#------------------------------------------------------------------------------------------------------
#Check any NAs as would could screw up modelling for specific variables
#Need to look at missingness patterns before modelling as can bias
#We may also have correlated terms not needed in the model/colinearity
#------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t(.) %>% View()

#None

#------------------------------------------------------------------------------------------------------
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


analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) -> alive_analysis_dataset

for (i in catVars){
  print(i)
  print(table(alive_analysis_dataset[, i], useNA='always'))

}

#For some variables there are very few events may want to consider not including
#table(analysis_dataset[, "GI_haemorrhage"], useNA='always')

for (i in contVars){
  print(i)
  print(summary(alive_analysis_dataset[, i]))
  
}


#------------------------------------------------------------------------------------------------------
#Logistic regression model
#------------------------------------------------------------------------------------------------------
analysis_dataset$brcname<- as.factor(analysis_dataset$brcname)#there were modelling errors when including separate as binary

analysis_dataset %>% filter(DIEDwithin3daysFROMpeakTROP==0) -> analysis_dataset_alive


#hosp_gstt+ hosp_ichnt+hosp_oxfor+hosp_uclh

model_formula_rcs <- "Invasive_at_3_daysFROMpeakTROP~ rcs(ageyears, 3) + Male_sex + brcname + rcs(creatinine_result, 3) +
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
  Sepsis + Inflammatory_disorder + fraility_score + Fall + Hearing + Visual + Assistance + Weight_loss"



#------------------------------------------------------------------------------------------------------
prop_model <- rms::lrm(formula = as.formula(model_formula_rcs),
                        data = analysis_dataset_alive, #filter data to those who have not died
                        x = T,
                        y = T,
                        maxit=10000,
                        tol=1e-10) #need maxit=1000 as the MLE may not converge in a number of steps

#------------------------------------------------------------------------------------------------------
#Backwards elimination using p value of 0.2
backwards<- rms::fastbw(prop_model, rule = "p", sls=0.2, type='individual')
#use type = individual to get similar behaviour to step
#https://stat.ethz.ch/pipermail/r-help/2010-February/228073.html


#Returns the variables which have been retained in the model
keptvars<- backwards$names.kept
#cat(noquote(keptvars))

#Can use the following to make it less manual for the final model but will still need to put the RCS in

formula <- ""

for (i in backwards$names.kept) {
  formula<- paste0(formula, " + ",i)
}

formula <- paste0("Invasive_at_3_daysFROMpeakTROP ~ ", substr(formula,4,nchar(formula)))

#------------------------------------------------------------------------------------------------------
#Final model formula for the propensity score
#------------------------------------------------------------------------------------------------------
propen_formula <- "Invasive_at_3_daysFROMpeakTROP ~ Male_sex + brcname + rcs(haemoglobin_result, 3) + rcs(plateletcount_result,3) + rcs(log_spt, 3) + FhxCHD + Hypercholesterolaemia + Hypertension + Smoking + Angina + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + Previous_MI + VT + AKI + Interstitial_lung_disease + Obstructive_Lung_Disease + Pneumonia + Ischaemic_stroke + Anxiety + Bipolar + Dementia + GI_haemorrhage + Arthritis + Osteoporosis + Malignant_neoplasm + Sepsis + Inflammatory_disorder + Fall + Visual"

propen_model<- rms::lrm(formula = as.formula(propen_formula),
                        data = analysis_dataset_alive, #filter data to those who have not died
                        x = T,
                        y = T,
                        maxit=1000) 
#------------------------------------------------------------------------------------------------------
#Predicted probabilities for the whole dataset
#turn off scientific notation or it will print 10 to the minus numbers which isnt helpful!
options(scipen=999)

analysis_dataset$propscore2<- predict(propen_model, newdata=analysis_dataset, type="fitted.ind")

#------------------------------------------------------------------------------------------------------
#car::vif(propen_model)
#Test of multicollinearity
#Rule of thumb vif score over 5 potential prob, anything over 10 needs to be remedied
#drop problematic varaible or create index of closely related variables
#------------------------------------------------------------------------------------------------------
#Keep check of prognostic model AUC to check it not doing something crazy
#------------------------------------------------------------------------------------------------------
#subset to those who did not die within 3 days to check the prop score model
analysis_dataset_nodeath <- subset(analysis_dataset, DIEDwithin3daysFROMpeakTROP == 0)

#library(pROC)

ROC <- roc(analysis_dataset_nodeath$Invasive_at_3_daysFROMpeakTROP, analysis_dataset_nodeath$propscore2)

plot(ROC, col = "red")

auc(ROC)
#0.8301
#This AUC seems reasonable
#------------------------------------------------------------------------------------------------------

saveRDS(analysis_dataset, "S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/2_analysis_dataset_with_propen.rds")

#------------------------------------------------------------------------------------------------------
















  
 