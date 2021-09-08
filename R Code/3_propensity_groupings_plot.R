#------------------------------------------------------------------------------------------------------
#Propensity groupings and plot
#------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
#------------------------------------------------------------------------------------------------------
setwd("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code")
analysis_dataset <- readRDS("S:/Business Intelligence - Covid Analytics Project/NIHR HIC - ACS Theme Work/Bristol Work/JC/NSTEMI Project/final_code/2_analysis_dataset_with_propen.rds")
#------------------------------------------------------------------------------------------------------
#Create a vector of percentiles you want, these correspond to table 1
p<- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

#Use quantile function to apply these percentiles to propscore2
percentiles<- as.data.frame(quantile(analysis_dataset$propscore2, p))

#------------------------------------------------------------------------------------------------------
#Create propensity score groups based on the above output
#------------------------------------------------------------------------------------------------------

analysis_dataset %>%
  mutate(propsgroup2 = case_when(
    propscore2 < percentiles[1,] ~ 1,
    propscore2>=percentiles[1,] & propscore2<percentiles[2,] ~ 2,
    propscore2>=percentiles[2,] & propscore2<percentiles[3,] ~ 3,
    propscore2>=percentiles[3,] & propscore2<percentiles[4,] ~ 4,
    propscore2>=percentiles[4,] & propscore2<percentiles[5,] ~ 5,
    propscore2>=percentiles[5,] & propscore2<percentiles[6,] ~ 6,
    propscore2>=percentiles[6,] & propscore2<percentiles[7,] ~ 7,
    propscore2>=percentiles[7,] & propscore2<percentiles[8,] ~ 8,
    propscore2>=percentiles[8,] & propscore2<percentiles[9,] ~ 9,
    propscore2>=percentiles[9,] ~10))-> analysis_dataset

#prop score is calculated using the whole dataset including those who died within 3 days of peak troponin
#looking at Adams code and discussion with Amit and Jonathan.
#------------------------------------------------------------------------------------------------------
#summarise proportions
#------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) %>% 
  group_by(propsgroup2) %>% 
  summarise(n=n()) %>% 
  mutate(percent = (n / sum(n))*100) %>% 
  as.data.frame() -> propensity_proportions

propensity_proportions
#------------------------------------------------------------------------------------------------------
#Summarise propensity groups for those who did not die in 3 days of peak trop
#0 is non-invasive, 1 is invasive
#------------------------------------------------------------------------------------------------------

analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) %>% 
  group_by(propsgroup2, Invasive_at_3_daysFROMpeakTROP) %>% 
  summarise(num_patients=n(), num_deaths = sum(lifeststusall)) %>% 
  pivot_wider(names_from = Invasive_at_3_daysFROMpeakTROP, values_from = c(num_patients, num_deaths)) %>% 
  #mutate(total = `0` + `1`) %>% 
  rename(num_patients_noninvasive=num_patients_0,
         num_patients_invasive=num_patients_1,
         num_deaths_noninvasive= num_deaths_0,
         num_deaths_invasive= num_deaths_1) %>% 
  replace(is.na(.), 0) %>% 
  mutate(num_deaths_percent_noninvas = round(num_deaths_noninvasive*100/num_patients_noninvasive,1),
         num_deaths_percent_invas = round(num_deaths_invasive*100/num_patients_invasive, 1)) %>% 
  as.data.frame() %>% 
  replace(is.na(.), 0) -> table_1_output

table_1_output$death_noninvas_percent<- paste0(table_1_output$num_deaths_noninvasive, " (", table_1_output$num_deaths_percent_noninvas, ")")
table_1_output$death_invas_percent<- paste0(table_1_output$num_deaths_invasive, " (", table_1_output$num_deaths_percent_invas, ")")
#------------------------------------------------------------------------------------------------------
percentiles %>% 
  rename(prop_upper_limit = `quantile(analysis_dataset$propscore2, p)`) %>% 
  add_row( prop_upper_limit=1.00000000) %>% 
  arrange(desc(prop_upper_limit)) %>% 
  mutate(prop_upper_limit = round(prop_upper_limit, 3))-> tmp_percentile

#Change order so matches Amit's word document
#------------------------------------------------------------------------------------------------------
table_1_output %>% 
  select(propsgroup2, 
         num_patients_invasive, 
         death_invas_percent, 
         num_patients_noninvasive, 
         death_noninvas_percent) %>% 
  arrange(desc(propsgroup2))-> table_1_output_slim

#------------------------------------------------------------------------------------------------------
table_1_output_slim_comp<- cbind(tmp_percentile, table_1_output_slim)
table_1_output_slim_comp %>% View()
#------------------------------------------------------------------------------------------------------
write.csv(table_1_output_slim_comp, "table_1_output_01_08_2021.csv")
#------------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------------
#Hazard Ratios for Table 1
#------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0)-> analysis_restrict_1
#------------------------------------------------------------------------------------------------------
# HR for each propensity group for invasive versus non invasive (minus those who died)
#------------------------------------------------------------------------------------------------------
#So this is slightly different data each time, subsets of propensity score group
#Can automate this using purrr

#library(purrr)

analysis_restrict_1 %>%
  select(propsgroup2, lifeststusall, Invasive_at_3_daysFROMpeakTROP, PEAKTntoCENSOR, brcname) %>% 
  split(.$propsgroup2) %>% # from base R
  map(~ coxph(Surv(PEAKTntoCENSOR,lifeststusall) ~ Invasive_at_3_daysFROMpeakTROP + strata(brcname), data = .)  ) %>%
  map(summary) -> univariate_cox

#------------------------------------------------------------------------------------------------------
options("scipen")

original1 = NULL
for (i in 1:10) {
  new <- as.data.frame(round(univariate_cox[[i]][["conf.int"]], 2))
  new$prop_group<- i
  new$hr_ci<- paste0(new$`exp(coef)`, " (", new$`lower .95`, "-", new$`upper .95`, ")")
  original1 <- rbind(original1, new)
}

#------------------------------------------------------------------------------------------------------
original1 %>% 
  select(prop_group, hr_ci) -> univariate_results

rownames(univariate_results) <- c()
#------------------------------------------------------------------------------------------------------
saveRDS(univariate_results, "table_1_HR_01_08_2021.rds")
#------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------
#Cox Regression overall for table 1
#------------------------------------------------------------------------------------------------------
analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0 & (propsgroup2>1 & propsgroup2<8))-> analysis_overall_hr
#------------------------------------------------------------------------------------------------------
Surv(analysis_overall_hr$PEAKTntoCENSOR, analysis_overall_hr$lifeststusall) -> surv_o

cox_overall <- coxph(surv_o ~ Invasive_at_3_daysFROMpeakTROP + strata(brcname), 
                     data = analysis_overall_hr)

summary(cox_overall)  #output provides HR CIs
confint(cox_overall)  #coefficient CIs
exp(confint(cox_overall))  #Also HR CIs
#------------------------------------------------------------------------------------------------------
#You can output as a table for further processing/automation using Broom
#------------------------------------------------------------------------------------------------------
#library(broom)

cox_overall %>%
  tidy %>%
  mutate(
    estimate=exp(estimate),
    conf.low=exp(conf.low),
    conf.high=exp(conf.high)
  ) %>%
  select(term, estimate, starts_with("conf")) %>% 
  as.data.frame() -> overallhazard

#------------------------------------------------------------------------------------------------------
saveRDS(overallhazard, "table_1_overallhazard_01_08_2021.rds")
#------------------------------------------------------------------------------------------------------










#------------------------------------------------------------------------------------------------------
#Figure 2
#Combined histogram and probability density function of the propensity score for 
#those who had both non invasive and invasive management who survived after 3 days of peak troponin concentration
#------------------------------------------------------------------------------------------------------
#setup:
#Make management variable as a factor for ggplot

analysis_dataset<-analysis_dataset %>% 
  mutate(management = case_when(Invasive_at_3_daysFROMpeakTROP ==1 ~ "Invasive",Invasive_at_3_daysFROMpeakTROP == 0 ~ "Non-invasive"),
         management=factor(management, c("Invasive", "Non-invasive")))

#------------------------------------------------------------------------------------------------------
#line for mean propensity score
#Dont necessarily need to add this but I include for completion
means <- analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) %>% 
  group_by(management) %>% 
  summarise(grp.mean = mean(propscore2))

#------------------------------------------------------------------------------------------------------
#For the plot
#windowsFonts(Arial=windowsFont("TT Arial"))
#dev.off()
#library(ggplot2)

analysis_dataset %>% 
  filter(DIEDwithin3daysFROMpeakTROP==0) %>% 
  ggplot(aes(x=propscore2)) -> p

p + geom_rect(aes(xmin=-0.05, xmax=0.036, ymin=0, ymax=Inf), fill="#D3D3D3", alpha=0.2)+ #lower limit for the propensity score group 5 to <10
  geom_rect(aes(xmin=0.931, xmax=1.02, ymin=0, ymax=Inf), fill="#D3D3D3", alpha=0.2)+
  geom_histogram(aes(y = ..density.., fill=management), alpha=0.4, color="black", position = "identity", bins = 30)+
  geom_density(aes(color = management), size=0.5, show.legend = FALSE) + 
  geom_vline(xintercept =c(0.036, 0.931), linetype = "dotted", size=1) +
  #geom_vline(data = means, aes(xintercept = grp.mean, color = management), linetype = "dashed", show.legend = FALSE)+ #this is if you want to add the means
  xlab("Propensity Score") + ylab("Probability Density Function")+
  #geom_text(aes(x=0.02, y=-0.05, label="0.02"), size=3, family="Arial")+
  #geom_text(aes(x=0.08, y=3, label="Cutoff point\n for inclusion"), size=3, family="Arial", fontface="plain")+
  #geom_text(aes(x=0.82, y=3, label="Cutoff point\n for inclusion"), size=3, family="Arial")+
  #annotate("text", x=0.1165, y=-0.05, label="0.12", size=3)+
  #annotate("text", x=0.9346, y=-0.05, label="0.93", size=3)+
  annotate("text", x=0.13, y=2.7, label="Cutoff point\n for inclusion", size=3)+
  annotate("text", x=0.82, y=2.7, label="Cutoff point\n for inclusion", size=3)+
  annotate("text", x=-0.02, y=2.7, label="<1st\n percentile\nexcluded", size=3)+
  annotate("text", x=0.98, y=2.7, label=">90th\n percentile\nexcluded", size=3)+
  #geom_label(x = 0.036, y = -0.05, label = "0.036", label.padding =unit(0.1, "lines"))+
  #scale_fill_manual(values = c("#00AFBB", "#E7B800")) + scale_color_manual(values = c("#00AFBB", "#E7B800")) 
  theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title = element_blank())+
  scale_fill_discrete(labels=c("Invasive (1934)", "Non-invasive (1360)"))

#------------------------------------------------------------------------------------------------------
#Take splines of propensity score variable (before restricting the data)
#The order of when the splines are developed will matter
#------------------------------------------------------------------------------------------------------

propscore2sp<- as.data.frame(rcspline.eval(analysis_dataset$propscore2, nk=3, inclx=TRUE))

analysis_dataset<- propscore2sp %>% 
  rename(propscore2sp1 =x,
         propscore2sp2= V2) %>% 
  cbind(analysis_dataset)

#------------------------------------------------------------------------------------------------------
saveRDS(analysis_dataset, "3_analysis_dataset_with_propen_splines.rds")
#------------------------------------------------------------------------------------------------------




