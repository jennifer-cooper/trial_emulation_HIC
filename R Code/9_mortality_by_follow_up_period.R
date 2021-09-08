#---------------------------------------------------------------------------------------------------------------------------
#Mortality hazard ratios by follow-up period
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
#Splitting follow up time for survival analysis use survSplit
#This splits the survival time data at the specified points these are calculated in days 
#3 subsitituted from each to account for the first three days
#A year is 365.25 days to account for the leap year
#---------------------------------------------------------------------------------------------------------------------------
#Need this due to the error - Error in survSplit(Surv(PEAKTntoCENSOR, lifeststusall) ~ ., analysis_dataset_mice,  : 
#'zero' parameter must be less than any observed times

analysis_restrict_2 %>% 
  mutate(PEAKTntoCENSOR = replace(PEAKTntoCENSOR, PEAKTntoCENSOR==0, 0.001)) ->analysis_restrict_2
#Check this is for all or invasive only
#---------------------------------------------------------------------------------------------------------------------------
#3 months, 1 year, 3 years
#---------------------------------------------------------------------------------------------------------------------------
#Check the split is behaving as expected for one ID

survSplit(Surv(PEAKTntoCENSOR, lifeststusall) ~., analysis_restrict_2,
          cut=c(87.75, 362.25, 1092.75), episode = "timegroup") %>%
  select(timegroup, exemplarid, tstart, PEAKTntoCENSOR, lifeststusall) %>% 
  rename(tstop = PEAKTntoCENSOR) %>% 
  filter(exemplarid=="[removed]") %>% 
  View()

#---------------------------------------------------------------------------------------------------------------------------
survSplit(Surv(PEAKTntoCENSOR, lifeststusall) ~., analysis_restrict_2,
          cut=c(87.75, 362.25, 1092.75), episode = "timegroup") %>%
  rename(tstop = PEAKTntoCENSOR) -> analysis_set_split
#---------------------------------------------------------------------------------------------------------------------------
analysis_set_split %>% 
  filter(timegroup==1) %>% 
  coxph(Surv(tstart, tstop, event = lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
          creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
          sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
          Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
          Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
          Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
          Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
          Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data=., method = "breslow") -> mod1

mod1 %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> mod1_3months

mod1_3months$model_name <- c("up to 3 months")


#---------------------------------------------------------------------------------------------------------------------------

analysis_set_split %>% 
  filter(timegroup<=2) %>%
  coxph(Surv(tstart, tstop, event = lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
          creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
          sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
          Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
          Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
          Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
          Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
          Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data=., method = "breslow") ->mod2

mod2 %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> mod2_upto1year


mod2_upto1year$model_name <- c("up to 1 year")

#---------------------------------------------------------------------------------------------------------------------------

analysis_set_split %>% 
  filter(timegroup<=3) %>%
  coxph(Surv(tstart, tstop, event = lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
          creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
          sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
          Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
          Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
          Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
          Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
          Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data=., method = "breslow") ->mod3

mod3 %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> mod3_upto3years


mod3_upto3years$model_name <- c("up to 3 years")

#---------------------------------------------------------------------------------------------------------------------------
#Crude for all follow-up (should produce the same results as the very first survival model in this code)
#If the data has been setup in the right way - a good check to make.
#---------------------------------------------------------------------------------------------------------------------------

analysis_set_split %>% 
  coxph(Surv(tstart, tstop, event = lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + agesp1 + agesp2 + 
          creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
          sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
          Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
          Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
          Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
          Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
          Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data=., method = "breslow") -> overall_mod

overall_mod %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> mod4_all_followup


mod4_all_followup$model_name <- c("all follow up")


#---------------------------------------------------------------------------------------------------------------------------
#combine and save these separate models
#---------------------------------------------------------------------------------------------------------------------------
rbind(mod1_3months, mod2_upto1year, mod3_upto3years, mod4_all_followup) -> mortality_by_follow_up_mods
#---------------------------------------------------------------------------------------------------------------------------

write.csv(mortality_by_follow_up_mods, "supptable_S5_mods.csv")

#---------------------------------------------------------------------------------------------------------------------------



























#---------------------------------------------------------------------------------------------------------------------------
#Combined model
#---------------------------------------------------------------------------------------------------------------------------

summary(analysis_restrict_2$PEAKTntoCENSOR)
#Median follow up time is 1263 days = 3.46 years

survSplit(Surv(PEAKTntoCENSOR, lifeststusall) ~., analysis_restrict_2,
          cut=c(30.25, 87.75, 175.5, 362.25, 1092.75), episode = "timegroup") %>%
  rename(tstop = PEAKTntoCENSOR) -> analysis_set_split2


table(analysis_set_split2$timegroup)


analysis_set_split2 %>% 
  mutate(time1 = if_else(timegroup==1 & Invasive_at_3_daysFROMpeakTROP==1, 1,0),
         time2 = if_else(timegroup==2 & Invasive_at_3_daysFROMpeakTROP==1, 1,0), 
         time3 = if_else(timegroup==3 & Invasive_at_3_daysFROMpeakTROP==1, 1,0),
         time4 = if_else(timegroup==4 & Invasive_at_3_daysFROMpeakTROP==1, 1,0),
         time5 = if_else(timegroup==5 & Invasive_at_3_daysFROMpeakTROP==1, 1,0),
         time6 = if_else(timegroup==6 & Invasive_at_3_daysFROMpeakTROP==1, 1,0)) -> analysis_set_split2
#---------------------------------------------------------------------------------------------------------------------------
#Unadjusted model

analysis_set_split2 %>% coxph(Surv(tstart, tstop, event = lifeststusall) ~ time1 + time2 + time3 + time4 + time5 + time6 +
                                 + strata(brcname), data=., method = "breslow") -> combinedmod_unadjusted

#---------------------------------------------------------------------------------------------------------------------------

combinedmod_unadjusted %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> combinedtimegroup_unadjusted

combinedtimegroup_unadjusted$model_name <- c("Combined time group unadjusted")

#---------------------------------------------------------------------------------------------------------------------------

analysis_set_split2 %>% coxph(Surv(tstart, tstop, event = lifeststusall) ~ time1 + time2 + time3 + time4 + time5 + time6 +
                          + agesp1 + agesp2 + 
                          creatininesp1 + creatininesp2 + haemoglobinsp1 + haemoglobinsp2 + plateletcountsp1 + plateletcountsp2 + 
                          sodiumsp1 + sodiumsp2 + lsptsp1 + lsptsp2 + whitecellsp1 + whitecellsp2 + diabetes + FhxCHD + 
                          Smoking + Aortic_aneurysm + aortic_stenosis + atrial_fibrillation + Cardiac_arrest + HF + PVD + 
                          Previous_MI + VT + AKI + Urinary_incontinence + Interstitial_lung_disease + Obstructive_Lung_Disease + 
                          Asthma + Pulmonary_embolism + Respiratory_Failure + Ischaemic_stroke + Constipation + Liver_disease + 
                          Fracture + Malignant_neoplasm + Benign_neoplasm + Lymphoma + Sepsis + Fall + Hearing + Visual + 
                          Assistance + propscore2sp1 + propscore2sp2 + strata(brcname), data=., method = "breslow") -> combinedmod_adjusted

#---------------------------------------------------------------------------------------------------------------------------
combinedmod_adjusted %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) -> combinedtimegroupmod

combinedtimegroupmod$model_name <- c("Combined time group model adjusted")
#---------------------------------------------------------------------------------------------------------------------------
rbind(combinedtimegroup_unadjusted, combinedtimegroupmod) -> combinedtimegroupmodels
#---------------------------------------------------------------------------------------------------------------------------
write.csv(combinedtimegroupmodels, "combinedtimegroupmodels.csv")
#---------------------------------------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------------------------------------
#Tabulate number of invasive versus non invasive for each time interval
#---------------------------------------------------------------------------------------------------------------------------
#analysis_set_split2 %>% 
#  group_by(timegroup, Invasive_at_3_daysFROMpeakTROP) %>% 
#  count(lifeststusall)


analysis_set_split2 %>%
  filter(Invasive_at_3_daysFROMpeakTROP==1) %>% 
  group_by(timegroup) %>% 
  summarise(invasive_deaths = sum(lifeststusall)) -> invas_deaths


analysis_set_split2 %>%
  filter(Invasive_at_3_daysFROMpeakTROP==0) %>% 
  group_by(timegroup) %>% 
  summarise(noninvasive_deaths = sum(lifeststusall)) %>% 
  select(-timegroup) -> noninvas_deaths


cbind(invas_deaths, noninvas_deaths) -> tab_deaths_timegroup
#---------------------------------------------------------------------------------------------------------------------------
write.csv(tab_deaths_timegroup, "supptable_S5_tab_deaths.csv")
#---------------------------------------------------------------------------------------------------------------------------



