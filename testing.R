# testing

library(haven)

rm(list=ls())

dir <- 'C:/Users/kgr2vr/The Lab Dropbox/Kyle Addison/Sea_Level_Rise/'

proj <- read_dta(paste0(dir, 'data_proj/slr_1deg_proj.dta'))
