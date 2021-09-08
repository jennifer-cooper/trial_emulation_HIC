### R Code

This folder contains the R code for the trial emulation using multiple imputation.
A brief description of each script is given in the table below.

|     **Name**                                    |     **Brief description**                                                                                                                                                                                               |
|-------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|     0_project_package_installation              |     Run this to install all the packages needed in the iCARE environment.     Tableone is slightly tricky to install in the HIC environment you can log out and in again in a particular order detailed in the code.    |
|     1_analysis_dataset_setup                    |     Takes the raw data and creates the variables needed, applies the inclusion and exclusion criteria.  Creates time from peak troponin plot                                                                            |
|     2_propensity_scoring                        |     Runs stepwise logistic regression to create propensity score model and adds variable to the dataset for probabilities     ROC curve plot                                                                            |
|     3_propensity_groupings_plot                 |     Creates propensity score groups for table 1, HRs, probability density function plot (figure 2)                                                                                                                      |
|     4_characteristics_of_participants           |     Automatically creates table 2 (OR for invasive management) mean difference for continuous variables, proportions etc and populates some of the flowchart                                                            |
|     5_inverse_probability_weighting             |     IPTW Cox model      KM curve plot                                                                                                                                                                                   |
|     6_multivariable_model_propen_adjustment     |     Selects stepwise multivariable cox model Multivariable model plus propensity score and BRC as strata model output.                                                                                                  |
|     7_primary_analysis_multiple_imputation      |     Multiple imputation and pooled model results using Rubin’s rule. The primary analysis model – multivariable plus propensity score adjustment and estimation of proportions in invasive/non invasive                 |
|     8_multivariable_all_invasive_noninvasive    |     This assigns all deaths in 3 days to invasive then non invasive and gets the model outputs.  Also performs the model for the 25th and 75th percentiles.                                                             |
|     9_mortality_by_follow_up_period             |     Splits time of follow up 3 separate models so up to 3 months, up to a year and up to 3 years and then a combined model with up to 1 month, 3 months, 6 months, 12 months, 3 years and over 3 years.                 |
