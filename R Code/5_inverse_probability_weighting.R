#---------------------------------------------------------------------------------------------------------------------------
##Inverse Probability Weighting
#---------------------------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#---------------------------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("3_analysis_dataset_with_propen_splines.rds")
#---------------------------------------------------------------------------------------------------------------------------

#Generating the inverse probability weighting variable
#I think this is generated for the entire analysis dataset

#*1/propscore if invasive but 1/(1-propscore) for non-invasive

analysis_dataset %>% 
  mutate(ipw= if_else(Invasive_at_3_daysFROMpeakTROP==1, (1/propscore2), (1/(1-propscore2)))) -> analysis_dataset

#Note: No NAs for whether someone was invasively managed or not so no need to specify NAs
table(analysis_dataset$Invasive_at_3_daysFROMpeakTROP, useNA = 'always')
#---------------------------------------------------------------------------------------------------------------------------
saveRDS(analysis_dataset, "4_analysis_dataset_with_ipw.rds")
#---------------------------------------------------------------------------------------------------------------------------











#---------------------------------------------------------------------------------------------------------------------------
#Restricting the population to those who have not died in 3 days and those without extreme propensity score
#---------------------------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2>1 & propsgroup2<8)) -> analysis_restrict_2

#---------------------------------------------------------------------------------------------------------------------------
#Crude cox with IPW weighting
#---------------------------------------------------------------------------------------------------------------------------

#Good explanation here for the method if needed - https://stackoverflow.com/questions/50590909/cox-regression-with-inverse-propensity-treatment-weighting
Surv(time = analysis_restrict_2$PEAKTntoCENSOR, event = analysis_restrict_2$lifeststusall) -> surv_o

#Crude cox with IPW weighting
ipw_cox<- coxph(surv_o~as.factor(Invasive_at_3_daysFROMpeakTROP) + strata(brcname), weights = ipw, data = analysis_restrict_2) 


#filter(propsgroup2>2 & propsgroup2<8) -> analysis_restrict_2

summary(ipw_cox)  #output provides HR CIs
confint(ipw_cox)  #coefficient CIs
exp(confint(ipw_cox))  #Also HR CIs
#---------------------------------------------------------------------------------------------------------------------------
#Save output for export

ipw_cox %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> crude_ipw

round(crude_ipw$p.value, 3) -> crude_ipw$p.value

crude_ipw$model_name <- c("crude with ipw")

#---------------------------------------------------------------------------------------------------------------------------
# Completely crude model
crude_cox<- coxph(surv_o~as.factor(Invasive_at_3_daysFROMpeakTROP) + strata(brcname), data = analysis_restrict_2) 

crude_cox %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf"), p.value) -> crude_cox

round(crude_cox$p.value, 3) -> crude_cox$p.value

crude_cox$model_name <- c("completely crude")
#---------------------------------------------------------------------------------------------------------------------------
#Bind the two models together in a dataframe
#---------------------------------------------------------------------------------------------------------------------------
options(scipen=999)

rbind(crude_ipw, crude_cox) -> cox_mods

write.csv(cox_mods, "S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/cox_mods.csv")
#---------------------------------------------------------------------------------------------------------------------------















#---------------------------------------------------------------------------------------------------------------------------
#KM Curve using ipw weights
#---------------------------------------------------------------------------------------------------------------------------
#PEAKTntoCENSOR is time from peak troponin to death/censored
print(survfit(surv_o ~ Invasive_at_3_daysFROMpeakTROP, weights = ipw, data = analysis_restrict_2))
#These are just checks and visualisations:
kmcurve <- survfit(surv_o ~ Invasive_at_3_daysFROMpeakTROP, data = analysis_restrict_2)
summary(kmcurve)
plot(kmcurve)


ggsurvplot(kmcurve, 
           conf.int = FALSE,
           palette = c("cyan3", "red3"),
           risk.table = TRUE, risk.table.col = "strata",
           fun = "event",
           legend.labs = c("Non-Invasive", "Invasive"),
           xlab = "Time in Years",
           ylab = "Cumulative Mortality",
           xscale = "d_y",
           xlim = c(0,2191.5), #Equivalent to 6 years if you wanted to make it 6 instead of 7 just 365.2*6
           break.time.by=365.25*1,
           ylim = c(0, 1),
           legend.title="")
#xlim = c(0,2191.5)) #cumhaz
#survival curve for the different groups


#ggsurvplot(kmcurve, data = (analysis_dataset %>% filter(DIEDwithin3daysFROMpeakTROP==0)), pval = TRUE)

#---------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------





