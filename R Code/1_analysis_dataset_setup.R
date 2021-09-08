#------------------------------------------------------------------------------------------------------
#Data Setup
#------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper & Amit Kaura
#------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")

#Install required packages
install.packages("tidyverse")
#------------------------------------------------------------------------------------------------------
library("tidyverse")
#------------------------------------------------------------------------------------------------------
#Make so can view up to 1000 columns
#Package not installed....
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
View(emulation_data[1:255])
#------------------------------------------------------------------------------------------------------
##import database

emulation_data<- read.csv("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/master_ACS_29_01_2021.csv", na.strings="")

mean(emulation_data$peaktropadmissionresult_a)
table(emulation_data$peaktropadmissionhighernormalvalue)
table(emulation_data$diagnosis1)
#------------------------------------------------------------------------------------------------------
#Define new variables:

emulation_data$Standardised_peak_troponin <- emulation_data$peaktropadmissionresult_a / emulation_data$peaktropadmissionhighernormalvalue
emulation_data$NoOfDaysBetweenPeakTropAndCABG <- emulation_data$noofdaysbetweenfirsttropandcabg - emulation_data$noofdaysbetweenfirsttropandpeaktropadmissiondt
emulation_data$NoOfDaysBetweenPeakTropAndPCI <- emulation_data$noofdaysbetweenfirsttropandpci - emulation_data$noofdaysbetweenfirsttropandpeaktropadmissiondt
emulation_data$NoOfDaysBetweenPeakTropAndAngiography <- emulation_data$noofdaysbetweenfirsttropandangiography - emulation_data$noofdaysbetweenfirsttropandpeaktropadmissiondt
emulation_data$PEAKTntoCENSOR <- emulation_data$firsttntocensor - emulation_data$noofdaysbetweenfirsttropandpeaktropadmissiondt


#------------------------------------------------------------------------------------------------------
#Add other additional variables
#Add age-bracket, troponin status, Ethnicity category, Sex, Assay type collumns and remove all other unnecessary columns

emulation_data_summary_data <- emulation_data %>%
  mutate(AgeBracket = case_when(
    ageyears < 55 ~ "<55",    #Amit I have updated this it was c <55 before
    ageyears >= 55 & ageyears <= 75 ~ "55-75",
    ageyears >75 ~ ">75"),
    AgeBracket = factor(AgeBracket, levels=c("<55","55-75",">75"))) %>%
  mutate(troponin_status = case_when(Standardised_peak_troponin <= 1 ~ "Negative", Standardised_peak_troponin > 1 ~ "Positive"), troponin_status = factor(troponin_status, c("Negative", "Positive"))) %>%
  mutate(Ethnicity = case_when(
    etnicity == "British" | etnicity == "Irish" | etnicity == "Any other White background" ~ "White",
    etnicity == "Indian" | etnicity == "Pakistani" | etnicity == "Bangladeshi" | etnicity == "Chinese" | etnicity == "Any other Asian background" ~ "Asian or Asian British",
    etnicity == "Caribbean" | etnicity == "African" | etnicity == "Any other Black background" ~ "Black or Black British",
    etnicity == "White and Black Caribbean" | etnicity == "White and Black African" | etnicity == "White and Asian" | etnicity == "Any other mixed background" ~ "Mixed",
    etnicity == "Not stated" | etnicity == "Any other ethnic group" | etnicity == "Not known" ~ "Other"), Ethnicity = factor(Ethnicity, levels = c("White", "Black or Black British", "Asian or Asian British", "Mixed", "Other")))%>%
  mutate(Male_sex = case_when(gender == "1" ~ 1, gender == "2" ~ 0))%>%
  mutate(Anaemia = case_when(
    haemoglobin_result < 12 & gender == "2" ~ 1,
    haemoglobin_result >= 12 & gender == "2" ~ 0,
    haemoglobin_result < 13 & gender == "1" ~ 1,
    haemoglobin_result >= 13 & gender == "1" ~ 0)) %>%
  mutate(CABGinLAST6monthsFROMfirstTROP = case_when (
    cabg =="1" & noofdaysbetweenfirsttropandcabg >= "-180" & noofdaysbetweenfirsttropandcabg < "-1" ~ 1,
    cabg =="1" & (noofdaysbetweenfirsttropandcabg < "-180" | noofdaysbetweenfirsttropandcabg >= "-1") ~ 0,  
    cabg =="0" ~ 0)) %>%
  mutate(PCIinLAST6monthsFROMfirstTROP = case_when (
    pci =="1" & noofdaysbetweenfirsttropandpci >= "-180" & noofdaysbetweenfirsttropandpci < "-1" ~ 1,
    pci =="1" & (noofdaysbetweenfirsttropandpci < "-180" | noofdaysbetweenfirsttropandpci >= "-1") ~ 0,  
    pci =="0" ~ 0)) %>%
  mutate(Invasive_at_3_daysFROMfirstTROP = case_when (
    noofdaysbetweenfirsttropandangiography >= "-1" & noofdaysbetweenfirsttropandangiography <= 3 ~ 1,
    noofdaysbetweenfirsttropandangiography >3 | noofdaysbetweenfirsttropandangiography < "-1"  ~ 0,
    angiogram == "0" ~ 0)) %>%
  mutate(DIEDwithin3daysFROMfirstTROP = case_when (
    firsttntocensor >= 0 & firsttntocensor <4 & lifeststusall =="1"  ~ 1,
    firsttntocensor >=4 ~ 0)) %>%
  
  mutate(CABGinLAST6monthsFROMpeakTROP = case_when (
    cabg =="1" & NoOfDaysBetweenPeakTropAndCABG >= "-180" & NoOfDaysBetweenPeakTropAndCABG < "-1" ~ 1,
    cabg =="1" & (NoOfDaysBetweenPeakTropAndCABG < "-180" | NoOfDaysBetweenPeakTropAndCABG >= "-1") ~ 0,  
    cabg =="0" ~ 0)) %>%
  mutate(PCIinLAST6monthsFROMpeakTROP = case_when (
    pci =="1" & NoOfDaysBetweenPeakTropAndPCI >= "-180" & NoOfDaysBetweenPeakTropAndPCI < "-1" ~ 1,
    pci =="1" & (NoOfDaysBetweenPeakTropAndPCI < "-180" | NoOfDaysBetweenPeakTropAndPCI >= "-1") ~ 0,  
    pci =="0" ~ 0)) %>%
  mutate(Invasive_at_3_daysFROMpeakTROP = case_when (
    NoOfDaysBetweenPeakTropAndAngiography >= "-1" & NoOfDaysBetweenPeakTropAndAngiography <= 3 ~ 1,
    NoOfDaysBetweenPeakTropAndAngiography >3 | NoOfDaysBetweenPeakTropAndAngiography < "-1"  ~ 0,
    angiogram == "0" ~ 0)) %>%
  mutate(DIEDwithin3daysFROMpeakTROP = case_when (
    PEAKTntoCENSOR >= 0 & PEAKTntoCENSOR <4 & lifeststusall =="1"  ~ 1,
    PEAKTntoCENSOR >=4 ~ 0)) %>%
  
  
  mutate(creatinineGR220  = case_when(creatinine_result >220 ~ 1, creatinine_result <= 220 ~ 0))%>%
  mutate(positive_trop = case_when(Standardised_peak_troponin > 1 ~ 1,Standardised_peak_troponin <= 1 ~ 0))%>%
  mutate(thrombocytopaenia  = case_when(plateletcount_result <150 ~ 1, plateletcount_result >= 150 ~ 0))%>%
  mutate(age_GR75 = case_when(ageyears > 75 ~ 1,ageyears <= 75 ~ 0))%>%
  mutate(age_GR80 = case_when(ageyears > 80 ~ 1,ageyears <= 80 ~ 0))%>%
  mutate(Sex = case_when(gender == "1" ~ "Male", gender == "2" ~ "Female"),  Sex = factor(Sex, c("Male", "Female")))%>%
  mutate(Assay = case_when(peaktropadmissiontypeofassay == "Highly Sensitive" ~ 1, peaktropadmissiontypeofassay == "Standard" ~ 0))%>%
  #as.data.frame() %>% #removed Amit I don't think this is needed
  select(exemplarid, brcname, firsttntocensor, PEAKTntoCENSOR, lifeststusall, DIEDwithin3daysFROMfirstTROP, DIEDwithin3daysFROMpeakTROP, 
         angiogram, pci, cabg, Invasive_at_3_daysFROMfirstTROP, CABGinLAST6monthsFROMfirstTROP, PCIinLAST6monthsFROMfirstTROP, 
         noofdaysbetweenfirsttropandangiography, noofdaysbetweenfirsttropandcabg, noofdaysbetweenfirsttropandpci, 
         noofdaysbetweenfirsttropandpeaktropadmissiondt,Invasive_at_3_daysFROMpeakTROP, CABGinLAST6monthsFROMpeakTROP, 
         PCIinLAST6monthsFROMpeakTROP, NoOfDaysBetweenPeakTropAndAngiography, NoOfDaysBetweenPeakTropAndCABG, NoOfDaysBetweenPeakTropAndPCI,
         ageyears, age_GR75, age_GR80, AgeBracket,Ethnicity,etnicity, Sex, Male_sex, Standardised_peak_troponin, positive_trop, 
         troponin_status, peaktropadmissiontypeofassay,Assay, thrombocytopaenia, haemoglobin_result, Anaemia, whitecellcount_result, 
         plateletcount_result, creatinine_result,creatinineGR220, sodium_result, potassium_result, urea_result, crp_result, glucose_result, 
         hba1c_result, cholesteroltotal_result, hdl_cholesterol_result, triglycerides_result, smokingstatus, priorhistoryofhypertension, 
         diabetes, familyhistoryofcoronaryheartdisease, previousmi, heartfailure, diagnosis1, diagnosis2, diagnosis3, diagnosis4, 
         diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, 
         diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, 
         diagnosis24, diagnosis25) #Added Ethnicity which was created above but only the etnicity column was included, so now 83 variables in total
#There was also diagnosis1 twice so I have removed one of these

#------------------------------------------------------------------------------------------------------

#Detect comorbidities from Diagnosis Columns
#Cardiovascular risk factors
#Drawing out a list of patients who have one of these comorbidities
#------------------------------------------------------------------------------------------------------

#NSTEMI - PRIMARY
NSTEMI_PRIMARY <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Acute subendocardial myocardial infarction", "Acute myocardial infarction "),collapse = '|'))))
NSTEMI_PRIMARY_ID <- NSTEMI_PRIMARY$exemplarid

#Unstable Angina - PRIMARY
Unstable_ACS_PRIMARY <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Unstable angina"),collapse = '|'))))
Unstable_ACS_PRIMARY_ID <- Unstable_ACS_PRIMARY$exemplarid


#Unstable Angina - SECONDARY
Unstable_ACS_SECONDARY <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Unstable angina"),collapse = '|'))))
Unstable_ACS_SECONDARY_ID <- Unstable_ACS_SECONDARY$exemplarid


#NSTEMI - SECONDARY
NSTEMI_SECONDARY <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Acute subendocardial myocardial infarction", "Acute myocardial infarction "),collapse = '|'))))

#Amit you can specify columns with starts_with or column range or diagnosis2:diagnosis25
#NSTEMI_SECONDARY <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis2:diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Acute subendocardial myocardial infarction", "Acute myocardial infarction "),collapse = '|'))))


NSTEMI_SECONDARY_ID <- NSTEMI_SECONDARY$exemplarid
#------------------------------------------------------------------------------------------------------
#Further Conditions:

#diabetes
diabetes <- emulation_data_summary_data %>% filter_at(.vars = vars(diabetes, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Yes", "diabetes mellitus", "Diabetes mellitus", "Diabetic", "diabetic"),collapse = '|'))))
diabetes_ID <- diabetes$exemplarid

#Family History of Ischaemic Heart Disease and other diseases of the circulatory system
FhxCHD <- emulation_data_summary_data %>% filter_at(.vars = vars(familyhistoryofcoronaryheartdisease, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Yes", "Family history of ischaemic heart disease and other diseases of the circulatory system"),collapse = '|'))))
FhxCHD_ID <- FhxCHD$exemplarid

#Hypercholesterolaemia
Hypercholesterolaemia <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("hypercholesterolaemia", "Hypercholesterolaemia"),collapse = '|'))))
Hypercholesterolaemia_ID <- Hypercholesterolaemia$exemplarid

#Hypertension
Hypertension <- emulation_data_summary_data %>% filter_at(.vars = vars(priorhistoryofhypertension, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Yes", "Essential \\(primary\\) hypertension", "essential \\(primary\\) hypertension", "hypertensive", "Hypertensive"),collapse = '|'))))
Hypertension_ID <- Hypertension$exemplarid

#Smoking History
Smoking <- emulation_data_summary_data %>% filter_at(.vars = vars(smokingstatus, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("smoker", "Tobacco use", "Mental and behavioural disorders due to use of tobacco"),collapse = '|'))))
Smoking_ID <- Smoking$exemplarid

#Cardiovascular Disease
#Aortic aneurysm
Aortic_aneurysm <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Aortic aneurysm", "aortic aneurysm"),collapse = '|'))))
Aortic_aneurysm_ID <- Aortic_aneurysm$exemplarid

#Angina Pectoris
Angina <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Angina pectoris", "angina pectoris"),collapse = '|'))))
Angina_ID <- Angina$exemplarid

#atrial fibrillation and flutter
atrial_fibrillation <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Atrial fibrillation", "atrial fibrillation" ),collapse = '|'))))
atrial_fibrillation_ID <- atrial_fibrillation$exemplarid

#aortic stenosis
aortic_stenosis <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Aortic stenosis", "aortic stenosis", "Aortic \\(valve\\) stenosis", "aortic \\(valve\\) stenosis"),collapse = '|'))))
aortic_stenosis_ID <- aortic_stenosis$exemplarid

#Cardiac Arrest
Cardiac_arrest <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Sudden cardiac death so described", "Cardiac arrest"),collapse = '|'))))
Cardiac_arrest_ID <- Cardiac_arrest$exemplarid

#Cardiogenic Shock
Cardiogenic_shock <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Cardiogenic shock", "cardiogenic shock"),collapse = '|'))))
Cardiogenic_shock_ID <- Cardiogenic_shock$exemplarid

#Atrioventricular block - 1st and 2nd degree - no complete heart blocks
Low_grade_AV_block <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Atrioventricular block first degree", "Atrioventricular block second degree"),collapse = '|'))))
Low_grade_AV_block_ID <- Low_grade_AV_block$exemplarid

#Atrioventricular block -  complete heart blocks
Complete_AV_block <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Atrioventricular block complete", "Atrioventricular block complete"),collapse = '|'))))
Complete_AV_block_ID <- Complete_AV_block$exemplarid

#Heart Failure
HF <- emulation_data_summary_data %>% filter_at(.vars = vars(heartfailure, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Ye", "Congestive heart failure", "Left ventricular failure", "Heart failure unspecified"),collapse = '|'))))
HF_ID <- HF$exemplarid

#Peripheral vascular disease 
PVD <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Peripheral vascular disease", "peripheral vascular disease"),collapse = '|'))))
PVD_ID <- PVD$exemplarid

#Previous MI
Previous_MI <- emulation_data_summary_data %>% filter_at(.vars = vars(previousmi, diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("1", "Old myocardial infarction"),collapse = '|'))))
Previous_MI_ID <- Previous_MI$exemplarid

#Supraventricular Tachycardia
SVT <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Supraventricular tachycardia"),collapse = '|'))))
SVT_ID <- SVT$exemplarid

#Ventricular Fibrillation 
VFib <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Ventricular fibrillation and flutter"),collapse = '|'))))
VFib_ID <- VFib$exemplarid

#VT
VT <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Ventricular tachycardia"),collapse = '|'))))
VT_ID <- VT$exemplarid

#Unstable Angina - Acute Coronary Syndrome
Unstable_ACS <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Unstable angina"),collapse = '|'))))
Unstable_ACS_ID <- Unstable_ACS$exemplarid

##NSTEMI - Acute Coronary Syndrome
NSTEMI <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Acute subendocardial myocardial infarction", "Acute myocardial infarction "),collapse = '|'))))
NSTEMI_ID <- NSTEMI$exemplarid

#STEMI - Acute Coronary Syndrome
STEMI <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Acute transmural myocardial infarction"),collapse = '|'))))
STEMI_ID <- STEMI$exemplarid

#Subsequent MI - Acute Coronary Syndrome
Subsequent_MI <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c( "Subsequent myocardial infarction"),collapse = '|'))))
Subsequent_MI_ID <- Subsequent_MI$exemplarid

#------------------------------------------------------------------------------------------------------
#Renal Diseases
##AKI
AKI <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Acute renal failure", "acute renal failure"),collapse = '|'))))
AKI_ID <- AKI$exemplarid


#CKD - 18 >Stage 2
CKD <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Chronic kidney disease", "chroniic kidney disease"),collapse = '|'))))
CKD_ID <- CKD$exemplarid

#UTI
UTI <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Acute pyelonephritis", "Pyelonephritis", "pyelonephritis", "pyonephrosis", "Pyelonephrosis", "Renal abscess", "renal abscess", "Kidney infection", "kidney infection", "Acute cystitis", "Cystitis", "cystitis", "Urethritis", "urethritis", "urethral abscess", "Urinary tract infections", "Prostatitis", "prostatitis", "abscess of prostate", "Orchitis", "orchitis", "epididymitis", "Epididymitis", "Catheter associated urinary tract infections", "Urinary tract infection", "urinary tract infection"),collapse = '|'))))
UTI_ID <- UTI$exemplarid

#------------------------------------------------------------------------------------------------------
#Respiratory Diseases

#Chronic Obstructive lung disease - Emphysema, COPD
Obstructive_Lung_Disease <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Bronchitis", "bronchitis", "Emphysema", "emphysema", "Chronic obstructive pulmonary disease", "chronic obstructive pulmonary disease","asthma", "Asthma unspecified", "Predominantly allergic asthma"),collapse = '|'))))
Obstructive_Lung_Disease_ID <- Obstructive_Lung_Disease$exemplarid

#Asthma 
Asthma <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("asthma", "Asthma", "Predominantly allergic asthma"),collapse = '|'))))
Asthma_ID <- Asthma$exemplarid

#Interstitial lung disease 
Interstitial_lung_disease <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("interstitial lung", "Interstitial lung", "interstitial pulmonary", "Interstitial pulmonary", "Langerhan’s cell histiocytosis", "Hypersensitivity pneumonitis", "Sarcoidosis of lung", "Rheumatoid lung disease", "Respiratory disorders in other diffuse connective tissue disorders"  ),collapse = '|'))))
Interstitial_lung_disease_ID <- Interstitial_lung_disease$exemplarid

#Pneumonia
Pneumonia <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Pneumonia", "pneumonia", "Bronchopneumonia"),collapse = '|'))))
Pneumonia_ID <- Pneumonia$exemplarid

#Respiratory Failure
Respiratory_Failure <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Respiratory failure", "respiratory failure"),collapse = '|'))))
Respiratory_Failure_ID <- Respiratory_Failure$exemplarid

#Pulmonary embolism
Pulmonary_embolism <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Pulmonary embolism", "pulmonary embolsim"),collapse = '|'))))
Pulmonary_embolism_ID <- Pulmonary_embolism$exemplarid

#GI haemorrhage
GI_haemorrhage <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Gastrointestinal haemorrhage unspecified", "Acute haemorrhagic gastritis", "Haemorrhage of anus and rectum" ),collapse = '|'))))
GI_haemorrhage_ID <- GI_haemorrhage$exemplarid

#Ischaemic stroke
Ischaemic_stroke <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Cerebral infarction due to thrombosis of precerebral arteries", "Cerebral infarction due to embolism of precerebral arteries", "Cerebral infarction due to unspecified occlusion or stenosis of precerebral arteries", "Cerebral infarction due to thrombosis of cerebral arteries", "Cerebral infarction due to embolism of cerebral arteries", "Cerebral infarction due to unspecified occlusion or stenosis of cerebral arteries", "Cerebral infarction due to cerebral venous thrombosis nonpyogenic", "Other cerebral infarction", "Cerebral infarction unspecified"),collapse = '|'))))
Ischaemic_stroke_ID <- Ischaemic_stroke$exemplarid

#Haemorrhagic stroke
Haemorrhagic_stroke <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Intracerebral haemorrhage in hemisphere subcortical", "Intracerebral haemorrhage in hemisphere cortical", "Intracerebral haemorrhage in hemisphere unspecified", "Intracerebral haemorrhage in brain stem", "Intracerebral haemorrhage in cerebellum", "Intracerebral haemorrhage intraventricular", "Intracerebral haemorrhage multiple localized", "Other intracerebral haemorrhage", "Intracerebral haemorrhage unspecified"),collapse = '|'))))
Haemorrhagic_stroke_ID <- Haemorrhagic_stroke$exemplarid

#SAH
SAH <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Subarachnoid haemorrhage from carotid siphon and bifurcation", "Subarachnoid haemorrhage from middle cerebral artery", "Subarachnoid haemorrhage from anterior communicating artery", "Subarachnoid haemorrhage from posterior communicating artery", "Subarachnoid haemorrhage from basilar artery", "Subarachnoid haemorrhage from vertebral artery", "Subarachnoid haemorrhage from other intracranial arteries", "Subarachnoid haemorrhage from intracranial artery unspecified", "Other subarachnoid haemorrhage", "Subarachnoid haemorrhage unspecified"),collapse = '|'))))
SAH_ID <- SAH$exemplarid

#SDH
SDH <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Subdural haemorrhage", "subdural haemorrhage"),collapse = '|'))))
SDH_ID <- SDH$exemplarid

##TIA
TIA <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Other transient cerebral ischaemic attacks and related syndromes", "Transient cerebral ischaemic attack unspecified", "Amaurosis fugax"),collapse = '|'))))
TIA_ID <- TIA$exemplarid

##Parkinsons Disease
Parkinsons<- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Parkinson's"),collapse = '|'))))
Parkinsons_ID <- Parkinsons$exemplarid

#Osteoporosis
Osteoporosis<- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Osteoporosis", "osteoporosis", "osteoporotic", "Osteoporotic"),collapse = '|'))))
Osteoporosis_ID <- Osteoporosis$exemplarid

#Liver disease
Liver_disease<- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Alcoholic liver disease", "Alcoholic fatty liver", "Alcoholic hepatitis", "Alcoholic fibrosis and sclerosis of liver", "Alcoholic cirrhosis of liver", "Alcoholic hepatic failure", "Alcoholic liver disease unspecified", "Toxic liver disease with other disorders of liver", "Hepatic failure, not elsewhere classified","Acute and subacute hepatic failure", "Chronic hepatic failure", "Hepatic failure, unspecified", "Other and unspecified cirrhosis of liver", "Other specified inflammatory liver diseases", "Fatty \\(change of\\) liver, not elsewhere classified", "Hepatic veno-occlusive disease", "Portal hypertension", "Hepato-renal syndrome", "Liver disease unspecified", "Malignant neoplasm: Liver cell carcinoma", "Oesophageal varices", "Oesophageal varices with bleeding", "Oesophageal varices without bleeding", "Gastric varices"),collapse = '|'))))
Liver_disease_ID <- Liver_disease$exemplarid
#------------------------------------------------------------------------------------------------------

#"Alcoholic liver disease", "Alcoholic fatty liver", "Alcoholic hepatitis", "Alcoholic fibrosis and sclerosis of liver", "Alcoholic cirrhosis of liver", "Alcoholic hepatic failure", "Alcoholic liver disease unspecified", "Toxic liver disease with other disorders of liver", "Hepatic failure, not elsewhere classified","Acute and subacute hepatic failure", "Chronic hepatic failure", "Hepatic failure, unspecified", "Other and unspecified cirrhosis of liver", "Other specified inflammatory liver diseases", "Fatty \\(change of\\) liver, not elsewhere classified", "Hepatic veno-occlusive disease", "Portal hypertension", "Hepato-renal syndrome", "Liver disease unspecified", "Malignant neoplasm: Liver cell carcinoma", "Oesophageal varices", "Oesophageal varices with bleeding", "Oesophageal varices without bleeding", "Gastric varices"

##Neurological Disease

#------------------------------------------------------------------------------------------------------
##Alcohol 
Alcohol_excess <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Alcohol-induced", "alcoholic", "Alcoholic"),collapse = '|'))))
Alcohol_excess_ID <- Alcohol_excess$exemplarid

#Anxiety 
Anxiety <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Anxiety", "anxiety", "anxious", "Anxious"),collapse = '|'))))
Anxiety_ID <- Anxiety$exemplarid

#Bipolar 
Bipolar <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("bipolar", "Bipolar"),collapse = '|'))))
Bipolar_ID <- Bipolar$exemplarid

#Delirium
Delirium <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Delirium", "delirium"),collapse = '|'))))
Delirium_ID <- Delirium$exemplarid

#Depression
Depression <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Depression", "depression", "Depressive", "depressive"),collapse = '|'))))
Depression_ID <- Depression$exemplarid

#Fracture
Fracture <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Fracture", "fracture"),collapse = '|'))))
Fracture_ID <- Fracture$exemplarid

##Constipation
Constipation <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Constipation", "constipation"),collapse = '|'))))
Constipation_ID <- Constipation$exemplarid

##Arthritis 
Arthritis <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Arthritis", "arthritis"),collapse = '|'))))
Arthritis_ID <- Arthritis$exemplarid

#Gastric ulcer 
Gastric_ulcer <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Gastric ulcer", "gastric ulcer"),collapse = '|'))))
Gastric_ulcer_ID <- Gastric_ulcer$exemplarid

#Inflammatory disorder 
Inflammatory_disorder <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("itis"),collapse = '|'))))
Inflammatory_disorder_ID <- Inflammatory_disorder$exemplarid

#Sepsis
Sepsis <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Sepsis", "sepsis"),collapse = '|'))))
Sepsis_ID <- Sepsis$exemplarid

#Malignant neoplasm
Malignant_neoplasm <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Malignant neoplasm", "malignant neoplasm"),collapse = '|'))))
Malignant_neoplasm_ID <- Malignant_neoplasm$exemplarid

#Benign neoplasm
Benign_neoplasm <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Benign neoplasm", "benign neoplasm"),collapse = '|'))))
Benign_neoplasm_ID <- Benign_neoplasm$exemplarid

#Leukaemia
Leukaemia <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("leukaemia", "Lukaemia"),collapse = '|'))))
Leukaemia_ID <- Leukaemia$exemplarid

#Lymphoma
Lymphoma <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Lymphoma", "lymphoma"),collapse = '|'))))
Lymphoma_ID <- Lymphoma$exemplarid
#------------------------------------------------------------------------------------------------------
##FRAILITY

#Faecal incontinence 
Faecal_incontinence <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Faecal incontinence", "faecal incontinence", "Incontinence of faeces", "incontinence of faeces"),collapse = '|'))))
Faecal_incontinence_ID <- Faecal_incontinence$exemplarid

#Dementia
Dementia <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("dementia", "Dementia"),collapse = '|'))))
Dementia_ID <- Dementia$exemplarid

#Fall
Fall <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Fall", "fall", "Falls", "falls"),collapse = '|'))))
Fall_ID <- Fall$exemplarid

#Hearing Impairment
Hearing <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Hearing", "hearing", "impairment of hearing", "Impairment of hearing"),collapse = '|'))))
Hearing_ID <- Hearing$exemplarid

#Visual Impairment
Visual <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Visual", "visual", "Vision", "visual", "diplopia", "Diplopia", "Blindness" , "blindness", "Blind", "blind"),collapse = '|'))))
Visual_ID <- Visual$exemplarid

#Assistance at home or personla care oor due too reduced moobility 
Assistance <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Assistance", "assistance"),collapse = '|'))))
Assistance_ID <- Assistance$exemplarid

#Urinary incontinence 
Urinary_incontinence <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Urinary incontinence", "urinary incontinence", "Stress incontinence", "stress incontinence"),collapse = '|'))))
Urinary_incontinence_ID <- Urinary_incontinence$exemplarid

#Urinary catheterisation 
Urinary_catheterisation <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Urinary catheterisation", "Urinary catheterization", "urinary catheterisation", "urinary catheterization"),collapse = '|'))))
Urinary_catheterisation_ID <- Urinary_catheterisation$exemplarid

#Weight loss
Weight_loss <- emulation_data_summary_data %>% filter_at(.vars = vars(diagnosis1, diagnosis2, diagnosis3, diagnosis4, diagnosis5, diagnosis6, diagnosis7, diagnosis8, diagnosis9, diagnosis10, diagnosis11, diagnosis12, diagnosis13, diagnosis14, diagnosis15, diagnosis16, diagnosis17, diagnosis18, diagnosis19, diagnosis20, diagnosis21, diagnosis22, diagnosis23, diagnosis24, diagnosis25), .vars_predicate = any_vars(str_detect(.,pattern = paste(c("Weight loss", "weight loss"),collapse = '|'))))
Weight_loss_ID <- Weight_loss$exemplarid

#------------------------------------------------------------------------------------------------------
#Adding the additional variables from the above onto the emulation_data_summary_data dataframe

emulation_data_comorbidity <- emulation_data_summary_data %>% mutate(Unstable_ACS = case_when(exemplarid %in% Unstable_ACS_ID ~ 1, !exemplarid %in% Unstable_ACS_ID ~ 0))%>%
  mutate(NSTEMI_PRIMARY = case_when(exemplarid %in% NSTEMI_PRIMARY_ID ~ 1, !exemplarid %in% NSTEMI_PRIMARY_ID ~ 0))%>%
  mutate(Unstable_ACS_PRIMARY = case_when(exemplarid %in% Unstable_ACS_PRIMARY_ID ~ 1, !exemplarid %in% Unstable_ACS_PRIMARY_ID ~ 0))%>%
  
  mutate(NSTEMI_SECONDARY = case_when(exemplarid %in% NSTEMI_SECONDARY_ID ~ 1, !exemplarid %in% NSTEMI_SECONDARY_ID ~ 0))%>%
  mutate(Unstable_ACS_SECONDARY = case_when(exemplarid %in% Unstable_ACS_SECONDARY_ID ~ 1, !exemplarid %in% Unstable_ACS_SECONDARY_ID ~ 0))%>%
  
  mutate(NSTEMI = case_when(exemplarid %in% NSTEMI_ID ~ 1, !exemplarid %in% NSTEMI_ID ~ 0))%>%
  mutate(STEMI = case_when(exemplarid %in% STEMI_ID ~ 1, !exemplarid %in% STEMI_ID ~ 0))%>%
  mutate(Subsequent_MI = case_when(exemplarid %in% Subsequent_MI_ID ~ 1, !exemplarid %in% Subsequent_MI_ID ~ 0))%>%
  
  mutate(diabetes = case_when(exemplarid %in% diabetes_ID ~ 1, !exemplarid %in% diabetes_ID ~ 0))%>%
  mutate(FhxCHD = case_when(exemplarid %in% FhxCHD_ID ~ 1, !exemplarid %in% FhxCHD_ID ~ 0))%>%
  mutate(Hypercholesterolaemia = case_when(exemplarid %in% Hypercholesterolaemia_ID ~ 1, !exemplarid %in% Hypercholesterolaemia_ID ~ 0))%>%
  mutate(Hypertension = case_when(exemplarid %in% Hypertension_ID ~ 1, !exemplarid %in% Hypertension_ID ~ 0))%>%
  mutate(Aortic_aneurysm = case_when(exemplarid %in% Aortic_aneurysm_ID ~ 1, !exemplarid %in% Aortic_aneurysm_ID ~ 0))%>%
  mutate(atrial_fibrillation = case_when(exemplarid %in% atrial_fibrillation_ID ~ 1, !exemplarid %in% atrial_fibrillation_ID ~ 0))%>%
  mutate(aortic_stenosis = case_when(exemplarid %in% aortic_stenosis_ID ~ 1, !exemplarid %in% aortic_stenosis_ID ~ 0))%>%
  
  mutate(Cardiac_arrest = case_when(exemplarid %in% Cardiac_arrest_ID ~ 1, !exemplarid %in% Cardiac_arrest_ID ~ 0))%>%
  mutate(Cardiogenic_shock = case_when(exemplarid %in% Cardiogenic_shock_ID ~ 1, !exemplarid %in% Cardiogenic_shock_ID ~ 0))%>%
  mutate(Low_grade_AV_block = case_when(exemplarid %in% Low_grade_AV_block_ID ~ 1, !exemplarid %in% Low_grade_AV_block_ID ~ 0))%>%
  mutate(Complete_AV_block = case_when(exemplarid %in% Complete_AV_block_ID ~ 1, !exemplarid %in% Complete_AV_block_ID ~ 0))%>%
  mutate(HF = case_when(exemplarid %in% HF_ID ~ 1, !exemplarid %in% HF_ID ~ 0))%>%
  mutate(PVD = case_when(exemplarid %in% PVD_ID ~ 1, !exemplarid %in% PVD_ID ~ 0))%>%
  mutate(Previous_MI = case_when(exemplarid %in% Previous_MI_ID ~ 1, !exemplarid %in% Previous_MI_ID ~ 0))%>%
  mutate(SVT = case_when(exemplarid %in% SVT_ID ~ 1, !exemplarid %in% SVT_ID ~ 0))%>%
  mutate(VFib = case_when(exemplarid %in% VFib_ID ~ 1, !exemplarid %in% VFib_ID ~ 0))%>%
  mutate(VT = case_when(exemplarid %in% VT_ID ~ 1, !exemplarid %in% VT_ID ~ 0))%>%
  
  mutate(AKI = case_when(exemplarid %in% AKI_ID ~ 1, !exemplarid %in% AKI_ID ~ 0))%>%
  mutate(CKD = case_when(exemplarid %in% CKD_ID ~ 1, !exemplarid %in% CKD_ID ~ 0))%>%
  mutate(UTI = case_when(exemplarid %in% UTI_ID ~ 1, !exemplarid %in% UTI_ID ~ 0))%>%
  mutate(Obstructive_Lung_Disease = case_when(exemplarid %in% Obstructive_Lung_Disease_ID ~ 1, !exemplarid %in% Obstructive_Lung_Disease_ID ~ 0))%>%
  mutate(Asthma = case_when(exemplarid %in% Asthma_ID ~ 1, !exemplarid %in% Asthma_ID ~ 0))%>%
  mutate(Interstitial_lung_disease = case_when(exemplarid %in% Interstitial_lung_disease_ID ~ 1, !exemplarid %in% Interstitial_lung_disease_ID ~ 0))%>%
  mutate(Pneumonia = case_when(exemplarid %in% Pneumonia_ID ~ 1, !exemplarid %in% Pneumonia_ID ~ 0))%>%
  mutate(Respiratory_Failure = case_when(exemplarid %in% Respiratory_Failure_ID ~ 1, !exemplarid %in% Respiratory_Failure_ID ~ 0))%>%
  mutate(Pulmonary_embolism = case_when(exemplarid %in% Pulmonary_embolism_ID ~ 1, !exemplarid %in% Pulmonary_embolism_ID ~ 0))%>%
  mutate(GI_haemorrhage = case_when(exemplarid %in% GI_haemorrhage_ID ~ 1, !exemplarid %in% GI_haemorrhage_ID ~ 0))%>%
  mutate(Ischaemic_stroke = case_when(exemplarid %in% Ischaemic_stroke_ID ~ 1, !exemplarid %in% Ischaemic_stroke_ID ~ 0))%>%
  mutate(Haemorrhagic_stroke = case_when(exemplarid %in% Haemorrhagic_stroke_ID ~ 1, !exemplarid %in% Haemorrhagic_stroke_ID ~ 0))%>%
  mutate(SAH = case_when(exemplarid %in% SAH_ID ~ 1, !exemplarid %in% SAH_ID ~ 0))%>%
  mutate(SDH = case_when(exemplarid %in% SDH_ID ~ 1, !exemplarid %in% SDH_ID ~ 0))%>%
  mutate(TIA = case_when(exemplarid %in% TIA_ID ~ 1, !exemplarid %in% TIA_ID ~ 0))%>%
  mutate(Parkinsons = case_when(exemplarid %in% Parkinsons_ID ~ 1, !exemplarid %in% Parkinsons_ID ~ 0))%>%
  mutate(Osteoporosis = case_when(exemplarid %in% Osteoporosis_ID ~ 1, !exemplarid %in% Osteoporosis_ID ~ 0))%>%
  mutate(Liver_disease = case_when(exemplarid %in% Liver_disease_ID ~ 1, !exemplarid %in% Liver_disease_ID ~ 0))%>%
  mutate(Alcohol_excess = case_when(exemplarid %in% Alcohol_excess_ID ~ 1, !exemplarid %in% Alcohol_excess_ID ~ 0))%>%
  mutate(Anxiety = case_when(exemplarid %in% Anxiety_ID ~ 1, !exemplarid %in% Anxiety_ID ~ 0))%>%
  mutate(Angina = case_when(exemplarid %in% Angina_ID ~ 1, !exemplarid %in% Angina_ID ~ 0))%>% #added Angina as was not included previously JC
  mutate(Bipolar = case_when(exemplarid %in% Bipolar_ID ~ 1, !exemplarid %in% Bipolar_ID ~ 0))%>%
  mutate(Delirium = case_when(exemplarid %in% Delirium_ID ~ 1, !exemplarid %in% Delirium_ID ~ 0))%>%
  mutate(Depression = case_when(exemplarid %in% Depression_ID ~ 1, !exemplarid %in% Depression_ID ~ 0))%>%
  mutate(Fracture = case_when(exemplarid %in% Fracture_ID ~ 1, !exemplarid %in% Fracture_ID ~ 0))%>%
  mutate(Constipation = case_when(exemplarid %in% Constipation_ID ~ 1, !exemplarid %in% Constipation_ID ~ 0))%>%
  mutate(Arthritis = case_when(exemplarid %in% Arthritis_ID ~ 1, !exemplarid %in% Arthritis_ID ~ 0))%>%
  mutate(Gastric_ulcer = case_when(exemplarid %in% Gastric_ulcer_ID ~ 1, !exemplarid %in% Gastric_ulcer_ID ~ 0))%>%
  mutate(Inflammatory_disorder = case_when(exemplarid %in% Inflammatory_disorder_ID ~ 1, !exemplarid %in% Inflammatory_disorder_ID ~ 0))%>%
  mutate(Sepsis = case_when(exemplarid %in% Sepsis_ID ~ 1, !exemplarid %in% Sepsis_ID ~ 0))%>%
  mutate(Malignant_neoplasm = case_when(exemplarid %in% Malignant_neoplasm_ID ~ 1, !exemplarid %in% Malignant_neoplasm_ID ~ 0))%>%
  mutate(Benign_neoplasm = case_when(exemplarid %in% Benign_neoplasm_ID ~ 1, !exemplarid %in% Benign_neoplasm_ID ~ 0))%>%
  mutate(Leukaemia = case_when(exemplarid %in% Leukaemia_ID ~ 1, !exemplarid %in% Leukaemia_ID ~ 0))%>%
  mutate(Lymphoma = case_when(exemplarid %in% Lymphoma_ID ~ 1, !exemplarid %in% Lymphoma_ID ~ 0))%>%
  mutate(Faecal_incontinence = case_when(exemplarid %in% Faecal_incontinence_ID ~ 1, !exemplarid %in% Faecal_incontinence_ID ~ 0))%>%
  mutate(Dementia = case_when(exemplarid %in% Dementia_ID ~ 1, !exemplarid %in% Dementia_ID ~ 0))%>%
  mutate(Fall = case_when(exemplarid %in% Fall_ID ~ 1, !exemplarid %in% Fall_ID ~ 0))%>%
  mutate(Hearing = case_when(exemplarid %in% Hearing_ID ~ 1, !exemplarid %in% Hearing_ID ~ 0))%>%
  mutate(Visual = case_when(exemplarid %in% Visual_ID ~ 1, !exemplarid %in% Visual_ID ~ 0))%>% #Added visualID as not included previously
  mutate(Assistance = case_when(exemplarid %in% Assistance_ID ~ 1, !exemplarid %in% Assistance_ID ~ 0))%>%
  mutate(Urinary_incontinence = case_when(exemplarid %in% Urinary_incontinence_ID ~ 1, !exemplarid %in% Urinary_incontinence_ID ~ 0))%>%
  mutate(Urinary_catheterisation = case_when(exemplarid %in% Urinary_catheterisation_ID ~ 1, !exemplarid %in% Urinary_catheterisation_ID ~ 0))%>%
  mutate(Weight_loss = case_when(exemplarid %in% Weight_loss_ID ~ 1, !exemplarid %in% Weight_loss_ID ~ 0)) %>% 
  mutate(Smoking = case_when(exemplarid %in% Smoking_ID ~ 1, !exemplarid %in% Smoking_ID ~ 0)) #Added SmokingID as not included previously

#------------------------------------------------------------------------------------------------------
#Adding a fraility score
emulation_data_comorbidity$fraility_score <- (emulation_data_comorbidity$Unstable_ACS | emulation_data_comorbidity$NSTEMI | emulation_data_comorbidity$STEMI  | emulation_data_comorbidity$Subsequent_MI  | emulation_data_comorbidity$Previous_MI) + (emulation_data_comorbidity$HF) + (emulation_data_comorbidity$PVD) +
  (emulation_data_comorbidity$Ischaemic_stroke | emulation_data_comorbidity$Haemorrhagic_stroke | emulation_data_comorbidity$SAH | emulation_data_comorbidity$TIA) + (emulation_data_comorbidity$Malignant_neoplasm | emulation_data_comorbidity$Benign_neoplasm  | emulation_data_comorbidity$Leukaemia | emulation_data_comorbidity$Lymphoma ) +
  (emulation_data_comorbidity$diabetes) + (emulation_data_comorbidity$Arthritis) + (emulation_data_comorbidity$Constipation) +  (emulation_data_comorbidity$Alcohol_excess) +  (emulation_data_comorbidity$Anxiety) +
  (emulation_data_comorbidity$Obstructive_Lung_Disease | emulation_data_comorbidity$Asthma | emulation_data_comorbidity$Interstitial_lung_disease) +  (emulation_data_comorbidity$CKD) +
  (emulation_data_comorbidity$Bipolar | emulation_data_comorbidity$Depression) +
  (emulation_data_comorbidity$Hypercholesterolaemia | emulation_data_comorbidity$Hypertension | emulation_data_comorbidity$atrial_fibrillation | emulation_data_comorbidity$aortic_stenosis | 
     emulation_data_comorbidity$SVT  | emulation_data_comorbidity$Pulmonary_embolism | emulation_data_comorbidity$GI_haemorrhage | emulation_data_comorbidity$Parkinsons | 
     emulation_data_comorbidity$Osteoporosis | emulation_data_comorbidity$Liver_disease | emulation_data_comorbidity$Gastric_ulcer | emulation_data_comorbidity$Inflammatory_disorder | 
     emulation_data_comorbidity$Sepsis)


summary(emulation_data_comorbidity$fraility_score)
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#This ends with the following dataset:
emulation_data_comorbidity
#------------------------------------------------------------------------------------------------------
#Combined outcomes

emulation_data_comorbidity$PRIMARY_DIAGNOSIS_NSTEMI_OR_UNSTABLE_ANGINA <- emulation_data_comorbidity$Unstable_ACS_PRIMARY |  emulation_data_comorbidity$NSTEMI_PRIMARY
emulation_data_comorbidity$ALL_DIAGNOSIS_NSTEMI_OR_UNSTABLE_ANGINA <- emulation_data_comorbidity$Unstable_ACS |  emulation_data_comorbidity$NSTEMI

#------------------------------------------------------------------------------------------------------
#Filtered the data to only include those with ALL_DIAGNOSIS_NSTEMI_OR_UNSTABLE_ANGINA

INCLUSION <- emulation_data_comorbidity %>% filter(ALL_DIAGNOSIS_NSTEMI_OR_UNSTABLE_ANGINA == TRUE)


#------------------------------------------------------------------------------------------------------
#Numbers for flowchart

nrow(INCLUSION)
#8304

INCLUSION %>% 
  count(Unstable_ACS)
#2744

INCLUSION %>% 
  count(NSTEMI)
#5864

table(INCLUSION$Unstable_ACS, INCLUSION$NSTEMI)

#------------------------------------------------------------------------------------------------------
#Amit original exclusion criteria - adds variable
INCLUSION$Exclusion_points_SELECT <- (INCLUSION$Cardiogenic_shock) +(INCLUSION$CABGinLAST6monthsFROMpeakTROP | INCLUSION$PCIinLAST6monthsFROMpeakTROP ) +
  (INCLUSION$NSTEMI_SECONDARY | INCLUSION$Unstable_ACS_SECONDARY) + INCLUSION$age_GR80


#------------------------------------------------------------------------------------------------------
#Other exclusions - When number of days between peak troponin and angiography is <2
INCLUSION$NoOfDaysBetweenPeakTropAndAngiography
addmargins(table(INCLUSION$NoOfDaysBetweenPeakTropAndAngiography, useNA = "always"))
#There are NAs if someone hasnt had angio

INCLUSION %>% 
  filter(INCLUSION$NoOfDaysBetweenPeakTropAndAngiography< -2) %>% count() 
#191

INCLUSION %>% 
  mutate(lessthanminustwoinvas = if_else(INCLUSION$NoOfDaysBetweenPeakTropAndAngiography< -2, 1, 0, 0)) -> INCLUSION
#If NA or over -2 then given a 0

table(INCLUSION$lessthanminustwoinvas, useNA="always")

#------------------------------------------------------------------------------------------------------
#Other exclusions - when missing data

#Add new variable for 1 = missing data to include in the removal for the following variables
#Counting the number missing any of the following:

INCLUSION %>% 
  filter(is.na(whitecellcount_result) | 
           is.na(plateletcount_result)|
           is.na(haemoglobin_result )|
           is.na(potassium_result)|
           is.na(sodium_result)|
           is.na(creatinine_result)) %>% 
  count()

#104

INCLUSION %>% 
  mutate(missing_data = if_else((is.na(whitecellcount_result) | 
                                   is.na(plateletcount_result)|
                                   is.na(haemoglobin_result )|
                                   is.na(potassium_result)|
                                   is.na(sodium_result)|
                                   is.na(creatinine_result)), 1,0)) -> INCLUSION

table(INCLUSION$missing_data, useNA="always")


#------------------------------------------------------------------------------------------------------
#Numbers for flow chart:

INCLUSION %>% 
  filter(Cardiogenic_shock==1) %>% 
  count()
#100


INCLUSION %>% 
  filter(CABGinLAST6monthsFROMpeakTROP==1) %>% 
  count()
#0

INCLUSION %>% 
  filter(PCIinLAST6monthsFROMpeakTROP==1) %>% 
  count()
#0

INCLUSION %>% 
  filter(NSTEMI_SECONDARY==1) %>% 
  count()
#2064


INCLUSION %>% 
  filter(Unstable_ACS_SECONDARY==1) %>% 
  count()
#1445

1445+2064
#3509


INCLUSION %>% 
  filter(Unstable_ACS_SECONDARY==1|NSTEMI_SECONDARY==1) %>% 
  count()
#3394


INCLUSION %>% 
  filter(age_GR80==1) %>% 
  count()
#2368

#------------------------------------------------------------------------------------------------------

INCLUSION %>% 
  filter(Cardiogenic_shock==1 | 
           CABGinLAST6monthsFROMpeakTROP==1|
           PCIinLAST6monthsFROMpeakTROP==1|
           NSTEMI_SECONDARY==1|
           Unstable_ACS_SECONDARY==1|
           age_GR80==1|
           missing_data==1|
           lessthanminustwoinvas==1) %>% 
  count()

#4948
#------------------------------------------------------------------------------------------------------
#SELECTING ONLY PATIENTS WHO FIT THE INCLUSION AND EXCLUSION CRITERIA
#------------------------------------------------------------------------------------------------------

ANALYSIS_DATASET <- INCLUSION %>% filter(Exclusion_points_SELECT == 0 & missing_data == 0 & lessthanminustwoinvas==0)

nrow(ANALYSIS_DATASET)
#3356
          
#------------------------------------------------------------------------------------------------------
#Recoding NoOfDaysBetweenPeakTropAndAngiography if -2, -1,  to 0
#------------------------------------------------------------------------------------------------------

#Remember there are NAs for this variable which we want to keep as NAs for those not invasively managed.

table(ANALYSIS_DATASET$NoOfDaysBetweenPeakTropAndAngiography, useNA="always")

ANALYSIS_DATASET %>% 
  mutate(NoOfDaysBetweenPeakTropAndAngiography = replace(NoOfDaysBetweenPeakTropAndAngiography, NoOfDaysBetweenPeakTropAndAngiography < 0, 0)) ->ANALYSIS_DATASET

table(ANALYSIS_DATASET$NoOfDaysBetweenPeakTropAndAngiography, useNA="always")


#For everyone invasively managed, AFTER exclusion criteria and recoding of -2,-1 to 0
plot<-ANALYSIS_DATASET %>% filter(NoOfDaysBetweenPeakTropAndAngiography<=93) %>% #93 chosen as value available in the Table command above
  ggplot(aes(x=NoOfDaysBetweenPeakTropAndAngiography)) + 
  geom_histogram(color="black", fill="red", bins = 50) +
  scale_x_continuous(breaks=c(0,3,7,14,30,60,90))+
  #scale_x_continuous(limits = c(-2,14))+
  scale_y_continuous(breaks=c(0,500,1000,1500,2000), limits = c(0,2000)) +
  xlab("Time from peak troponin to invasive procedure (days)") +
  ylab("Count") +
  theme_classic()


#Bold the axis labels

plot + theme(
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold")
)


#Alternate colours :D

plot + theme(
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)


plot<-ANALYSIS_DATASET %>% filter(NoOfDaysBetweenPeakTropAndAngiography<=14) %>% #93 chosen as value available in the Table command above
  ggplot(aes(x=NoOfDaysBetweenPeakTropAndAngiography)) + 
  geom_histogram(color="black", fill="red", bins = 10) +
  scale_x_continuous(breaks=c(0,3,7,14))+
  #scale_x_continuous(limits = c(-2,14))+
  scale_y_continuous(breaks=c(0,500,1000,1500,2000), limits = c(0,2000)) +
  xlab("Time from peak troponin to invasive procedure (days)") +
  ylab("Count") +
  theme_classic()

plot + theme(
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold")
)


#------------------------------------------------------------------------------------------------------
saveRDS(ANALYSIS_DATASET, "S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/1_analysis_dataset.rds")
#------------------------------------------------------------------------------------------------------





