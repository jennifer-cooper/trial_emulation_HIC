#------------------------------------------------------------------------------------------------------
#Table 2 and flowchart: characteristics of participants invasive versus non invasive strategy
#------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("3_analysis_dataset_with_propen_splines.rds")

#------------------------------------------------------------------------------------------------------
#For populating the numbers in the flowchart
#extreme_propensity, Invasive_at_3_daysFROMpeakTROP, DIEDwithin3daysFROMpeakTROP
#------------------------------------------------------------------------------------------------------
analysis_dataset %>% #the dataset including everyone = analysis_dataset (i.e. includes those who died within 3 days AND extreme propensity scores)
  mutate(extreme_propensity = 
           if_else((propsgroup2<2 | propsgroup2>7), 1,0))-> analysis_dataset  
#------------------------------------------------------------------------------------------------------
#(a)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==1) %>% 
  count()

#(b)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==0) %>% 
  count()

#(c)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==1 & DIEDwithin3daysFROMpeakTROP==0) %>% 
  count()

#(d)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==0 & DIEDwithin3daysFROMpeakTROP==0) %>% 
  count()

#(e)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==1 & DIEDwithin3daysFROMpeakTROP==0 & extreme_propensity==0) %>% 
  count()

#(f)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==0 & DIEDwithin3daysFROMpeakTROP==0 & extreme_propensity==0) %>% 
  count()

#(i)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==1 & DIEDwithin3daysFROMpeakTROP==1) %>% 
  count()

#(j)
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==0 & DIEDwithin3daysFROMpeakTROP==1) %>% 
  count()


#------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==1 & DIEDwithin3daysFROMpeakTROP==1 & extreme_propensity==1) %>% 
  count()

analysis_dataset %>% 
  filter(Invasive_at_3_daysFROMpeakTROP==0 & DIEDwithin3daysFROMpeakTROP==1 & extreme_propensity==1) %>% 
  count()
#------------------------------------------------------------------------------------------------------





















#------------------------------------------------------------------------------------------------------
#Logistic Regression for Table 2
#------------------------------------------------------------------------------------------------------
#Population: those with eligible propensity scores who did not die within 3 days

analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2>1 & propsgroup2<8)) -> analysis_restrict_2

options(scipen=999) #turns off scientific notation if its come back on

#------------------------------------------------------------------------------------------------------
#e.g. 

mod <- glm(formula = Invasive_at_3_daysFROMpeakTROP ~Weight_loss,
           data = analysis_restrict_2)

summary(mod)
exp(mod$coefficients)
exp(confint(mod, level = 0.95))

table(analysis_restrict_2$Weight_loss, useNA='always')


#------------------------------------------------------------------------------------------------------
#Check missingness of variables or whether zero occurrences for this restricted population before putting in LR model
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


for (i in catVars){
  print(i)
  print(table(analysis_restrict_2[, i], useNA='always'))
  
}


#They all have at least one occurrence


for (i in contVars){
  print(i)
  print(summary(analysis_restrict_2[, i]))
  
}

# 279 NAs crp_result

#-----------------------------------------------------------------------------------------------------------------
#Categorical variables ORs: Lets automate for all logistic models, uses base R only
#-----------------------------------------------------------------------------------------------------------------
#List of binary variables - saving as a vector so we can run a loop type formula to automatically generate 
#all the models we need.

varlist<-c(
  "Male_sex",
  "hosp_gstt",
  "hosp_ichnt",
  "hosp_oxfor",
  "hosp_uclh",
  "diabetes",
  "FhxCHD",
  "Hypercholesterolaemia",
  "Hypertension",
  "Smoking",
  "Aortic_aneurysm",
  "Angina",
  "aortic_stenosis",
  "atrial_fibrillation",
  "Cardiac_arrest",
  #"Cardiogenic_shock" Zero occurrences
  "Low_grade_AV_block",
  "Complete_AV_block",
  "HF",
  "PVD",
  "Previous_MI",
  "SVT",
  "VFib",
  "VT",
  "AKI",
  "CKD",
  "UTI",
  "Urinary_catheterisation",
  "Urinary_incontinence",
  "Interstitial_lung_disease",
  "Obstructive_Lung_Disease",
  "Asthma",
  "Pneumonia",
  "Pulmonary_embolism",
  "Respiratory_Failure",
  "Ischaemic_stroke",
  "TIA",
  "Haemorrhagic_stroke",
  "SAH",
  "SDH",
  "Parkinsons",
  "Alcohol_excess",
  "Anxiety",
  "Bipolar",
  "Delirium",
  "Depression",
  "Dementia",
  "Gastric_ulcer",
  "GI_haemorrhage",
  "Constipation",
  "Faecal_incontinence",
  "Liver_disease",
  "Arthritis",
  "Fracture",
  "Osteoporosis",
  "Malignant_neoplasm",
  "Benign_neoplasm",
  "Leukaemia",
  "Lymphoma",
  "Sepsis",
  "Inflammatory_disorder",
  "fraility_score",
  "Fall",
  "Hearing",
  "Visual",
  "Assistance",
  "Weight_loss")
#-----------------------------------------------------------------------------------------------------------------
#Function to automate into one table (run from here to line 835)
#-----------------------------------------------------------------------------------------------------------------

#options("scipen")
#options( scipen = 0 )


models <- lapply(paste("Invasive_at_3_daysFROMpeakTROP", varlist, sep = "~"), formula)
res.models <- lapply(models, FUN = function(x) {summary(glm(formula = x, data = analysis_restrict_2))})
names(res.models) <- varlist

res <- lapply(seq_along(res.models), function(i) {
  
  data.frame(variable = names(res.models)[i],
             intercept = res.models[[i]]$coefficients[1],
             coef = res.models[[i]]$coefficients[2],
             odds_ratio = exp(res.models[[i]]$coefficients[2]),
             p_value = res.models[[i]]$coefficients[2,4],
             std_error = res.models[[i]]$coefficients[2,2],
             #conf_lower = exp(confint(mod, level = 0.95))[2,1],
             #conf_upper = exp(confint(mod, level = 0.95))[2,2],
             
             stringsAsFactors = FALSE)
  
})

do.call(rbind, res) -> output_table

#-----------------------------------------------------------------------------------------------
#This second part is to add on the confidence intervals:

models <- lapply(paste("Invasive_at_3_daysFROMpeakTROP", varlist, sep = "~"), formula)
res.models <- lapply(models, FUN = function(x) {glm(formula = x, data = analysis_restrict_2)})
names(res.models) <- varlist

res <- lapply(seq_along(res.models), function(i) {
  
  data.frame(variable = names(res.models)[i],
             conf_lower = round(exp(confint(res.models[[i]], level = 0.95))[2,1], digits = 3),
             conf_upper = round(exp(confint(res.models[[i]], level = 0.95))[2,2], digits = 3),
             
             stringsAsFactors = FALSE)
  
})

do.call(rbind, res) -> output_table2

#-----------------------------------------------------------------------------------------------
#join these tables together by model name variable

output_table %>% left_join(output_table2, by=c("variable"="variable")) ->combined_glms

combined_glms %>% 
  mutate(intercept = round(intercept, 3),
         coef = round(coef, 3),
         odds_ratio = round(odds_ratio, 3),
         p_value = round(p_value, 3),
         std_error = round(std_error, 3))->combined_glms

View(combined_glms)

#------------------------------------------------------------------------------------------------------
#Format the table so I can just copy and paste results into the table!

combined_glms$or_ci<- paste0(combined_glms$odds_ratio, " (", combined_glms$conf_lower, ", ", combined_glms$conf_upper, ")")

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#Save for export so I can copy and paste
#------------------------------------------------------------------------------------------------------
write.csv(combined_glms, "table_2_ORs_categorical_01_08_2021.csv")
#------------------------------------------------------------------------------------------------------











#------------------------------------------------------------------------------------------------------
#Continuous variable means and mean difference manual version for each variable
#------------------------------------------------------------------------------------------------------
#Continuous with missing
analysis_restrict_2 %>%
  filter(!is.na(crp_result)) %>% 
  group_by(Invasive_at_3_daysFROMpeakTROP) %>% 
  summarise(avg=mean(crp_result), n=n(), sd=sd(crp_result))

#------------------------------------------------------------------------------------------------------
#Continuous
analysis_restrict_2 %>%
  filter(DIEDwithin3daysFROMpeakTROP == 0) %>% 
  group_by(Invasive_at_3_daysFROMpeakTROP) %>% 
  summarise(avg=mean(whitecellcount_result), n=n(), sd=sd(whitecellcount_result)) %>% 
  as.data.frame()

#------------------------------------------------------------------------------------------------------
#mean difference
#------------------------------------------------------------------------------------------------------
#Two sample t test with equal variances
#The unpaired two-samples t-test is used to compare the mean of two independent groups.
#------------------------------------------------------------------------------------------------------
#Manual
#------------------------------------------------------------------------------------------------------
tmod<-t.test(ageyears ~ Invasive_at_3_daysFROMpeakTROP, data=analysis_restrict_2, var.equal=TRUE)
tmod2<- as.data.frame(tidy(tmod))

tmod2
#crp_pop is missing so have to minus the NA results before running

crp_pop<-analysis_restrict_2 %>% 
  filter(!is.na(crp_result))

t.test(crp_pop$crp_result ~ crp_pop$Invasive_at_3_daysFROMpeakTROP, var.equal=TRUE)

#------------------------------------------------------------------------------------------------------
#Binary variable proportions manual
#------------------------------------------------------------------------------------------------------

#Binary
analysis_restrict_2 %>%
  filter(DIEDwithin3daysFROMpeakTROP == 0) %>% 
  group_by(Invasive_at_3_daysFROMpeakTROP, hosp_uclh) %>% #insert variable here
  summarise(n=n()) %>% 
  mutate(pct = (n / sum(n))*100)

#------------------------------------------------------------------------------------------------------
#Automated Mean Difference for all continuous variables
#------------------------------------------------------------------------------------------------------

continuous_vars<-c("ageyears", "creatinine_result", "haemoglobin_result","plateletcount_result", "potassium_result", 
                   "sodium_result", "whitecellcount_result", "Standardised_peak_troponin", "crp_result", "fraility_score")

ttest_data=NULL

for (i in continuous_vars) {
  
  tmod<-t.test(get(i) ~ Invasive_at_3_daysFROMpeakTROP, data =analysis_restrict_2 %>% filter(!is.na(get(i))),  var.equal=TRUE)
  tmod2<- as.data.frame(tidy(tmod))
  tmod2$variable <- c(i)
  
  ttest_data =rbind(ttest_data, tmod2)
}

#Some cleanup
ttest_data %>% 
  mutate(mean_difference = round((estimate1 - estimate2), digits=2),
         estimate1 = round(estimate1, digits = 2),
         estimate2 = round(estimate2, digits = 2),
         statistic = round(statistic, digits = 2),
         p.value = round(p.value, digits = 2),
         conf.low = round(conf.low, digits = 2),
         conf.high = round(conf.high, digits = 2),
         combined = paste0(mean_difference, " (", conf.low, ", ", conf.high, ")")) -> ttest_data

#------------------------------------------------------------------------------------------------------
write.csv(ttest_data, "table2_combinedttest_01_08_2021.csv")
#------------------------------------------------------------------------------------------------------






#------------------------------------------------------------------------------------------------------
#TableOne method
#This gets all the proportions, means and SDs by invasive versus non invasive in an automatic way
#Mean difference isnt included with CI and so we need to run that separately above
#------------------------------------------------------------------------------------------------------
#To get this code working in iCARE (it would be fine if Haven was just up to date)
#Have no packages installed
#remove haven
#log out and in again - reinstall haven (correct imported version)
#install tableone, survey and labelled
#library dplyr
#then it should work
#------------------------------------------------------------------------------------------------------

#dput(names(analysis_restrict_2))

#vector of variable names to summarise

allvars <- c("Male_sex",  "hosp_gstt",  "hosp_ichnt","hosp_oxfor", "hosp_uclh",  "diabetes",  "FhxCHD",  "Hypercholesterolaemia",  "Hypertension",  "Smoking",
              "Aortic_aneurysm",  "Angina",  "aortic_stenosis",  "atrial_fibrillation",  "Cardiac_arrest",  "Low_grade_AV_block",  "Complete_AV_block",
              "HF",  "PVD",  "Previous_MI",  "SVT",  "VFib",  "VT",  "AKI",  "CKD",  "UTI",  "Urinary_catheterisation",  "Urinary_incontinence",  "Interstitial_lung_disease",
              "Obstructive_Lung_Disease",  "Asthma",  "Pneumonia",  "Pulmonary_embolism",  "Respiratory_Failure",  "Ischaemic_stroke",  "TIA",  "Haemorrhagic_stroke",
              "SAH",  "SDH",  "Parkinsons",  "Alcohol_excess",  "Anxiety",  "Bipolar",  "Delirium",  "Depression",  "Dementia",  "Gastric_ulcer",
              "GI_haemorrhage",  "Constipation",  "Faecal_incontinence",  "Liver_disease",  "Arthritis",  "Fracture",  "Osteoporosis",  "Malignant_neoplasm",
              "Benign_neoplasm",  "Leukaemia",  "Lymphoma",  "Sepsis",  "Inflammatory_disorder",  "fraility_score",  "Fall",  "Hearing",  "Visual",
              "Assistance",  "Weight_loss", "ageyears","crp_result", "creatinine_result", "haemoglobin_result","plateletcount_result", "potassium_result", "sodium_result", 
              "whitecellcount_result", "Standardised_peak_troponin", "fraility_score")


catVars <- c("Male_sex",  "hosp_gstt",  "hosp_ichnt","hosp_oxfor", "hosp_uclh",  "diabetes",  "FhxCHD",  "Hypercholesterolaemia",  "Hypertension",  "Smoking",
             "Aortic_aneurysm",  "Angina",  "aortic_stenosis",  "atrial_fibrillation",  "Cardiac_arrest",  "Low_grade_AV_block",  "Complete_AV_block",
             "HF",  "PVD",  "Previous_MI",  "SVT",  "VFib",  "VT",  "AKI",  "CKD",  "UTI",  "Urinary_catheterisation",  "Urinary_incontinence",  "Interstitial_lung_disease",
             "Obstructive_Lung_Disease",  "Asthma",  "Pneumonia",  "Pulmonary_embolism",  "Respiratory_Failure",  "Ischaemic_stroke",  "TIA",  "Haemorrhagic_stroke",
             "SAH",  "SDH",  "Parkinsons",  "Alcohol_excess",  "Anxiety",  "Bipolar",  "Delirium",  "Depression",  "Dementia",  "Gastric_ulcer",
             "GI_haemorrhage",  "Constipation",  "Faecal_incontinence",  "Liver_disease",  "Arthritis",  "Fracture",  "Osteoporosis",  "Malignant_neoplasm",
             "Benign_neoplasm",  "Leukaemia",  "Lymphoma",  "Sepsis",  "Inflammatory_disorder", "Fall",  "Hearing",  "Visual",
             "Assistance",  "Weight_loss")

#------------------------------------------------------------------------------------------------------
# Create a TableOne object
table_1 <- CreateTableOne(vars = allvars, data = analysis_restrict_2, factorVars = catVars, strata = "Invasive_at_3_daysFROMpeakTROP")
print(table_1, formatOptions = list(big.mark = ","), smd = TRUE)

#To export

table_export <- print(table_1, formatOptions = list(big.mark = ","), smd = TRUE, quote=FALSE, noSpaces=TRUE, printToggle = FALSE)
#------------------------------------------------------------------------------------------------------
write.csv(table_export, file = "table2_automation_01_08_2021.csv")
#------------------------------------------------------------------------------------------------------


