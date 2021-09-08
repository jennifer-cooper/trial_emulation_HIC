#------------------------------------------------------------------------------------------------------
#Package installation for this project in iCARE environment
#Note that for tableone you have to install new haven, log on and off again?! Not sure why but it works after this
#------------------------------------------------------------------------------------------------------
#Author: Jennifer Cooper
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