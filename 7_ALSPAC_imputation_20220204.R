## ================== Load packages ====================================
library(foreign)
library(haven)
library(psych)
library(Amelia)
library(Zelig)
library(semTools)
library(lavaan)
library(Hmisc)
set.seed(123) #set seed for reproducibility of multiple imputation

## ================== Run scripts to derive DAWBA vars====================================
setwd("/Users/jessie/Dropbox/Wellcome fellowship/Registered Report/R Scripts/Analysis_finished_Feb2022")
source("ALSPAC_DeriveMHmeasures_20220204.R") # Internalising & externalising at age 7, 10, and 13

## ================== Select DAWBA vars and drop others ====================================
dawba_data <- subset(data, select=c("cidB3219", "qlet", "internalising7", "internalising10", "internalising13", 
                                    "externalising7", "externalising10", "externalising13"))
names(dawba_data) <- tolower(names(dawba_data))

# Remove unnecessary objects in environment
rm(data, vars7, vars10, vars13, cols, dawba_vars, 
   dawba_anx, dawba_ext_subscales, dawba_int_subscales,
   dawba_externalising, dawba_internalising)

## ================== Load dataset with ACEs and auxiliary vars====================================
load(file = "/Users/jessie/Desktop/alspacKids_ACE_0_18.RData")
class(alspacKids_ACE_data_2018)
colnames(alspacKids_ACE_data_2018)
names(alspacKids_ACE_data_2018) <- tolower(names(alspacKids_ACE_data_2018)) # convert to lower case

# Code ID variable to numeric
class(alspacKids_ACE_data_2018$cidb3219)
alspacKids_ACE_data_2018$cidb3219 <- as.numeric(as.character(alspacKids_ACE_data_2018$cidb3219))

## ================== Rename auxiliary variables ====================================
# Here I rename the auxiliary variables in the alspacKids_ACE_data_2018 file
# This file includes auxiliary variables constituting "early sociodemographic indicators",
# "adversity exposure before birth", "adversity exposure in adolescence", and 
# "other forms of adversity (0-18 years)"
# The other auxiliary variables (Adversity exposure in young adulthood and DAWBA)
# can be found in the other data file

alspacKids_ACE_data_2018 <- alspacKids_ACE_data_2018 %>% 
  rename(
    # Early sociodemographic indicators
    sex = kz021,
    soc_class_preg = sc_household_18wgest_org,
    ethnicity = c804,
    mat_age_delivery = mz028b,
    home_owner_preg = a006,
    parity = b032,
    marital_status_preg = a525,
    edu_mother_selfrep = c645a,
    edu_ptnr_motherrep = c666a_org,
    edu_ptnr_selfrep = pb325a_org,
    edu_mother_ptnrrep = pb342a_org, 
    birthweight = kz030,
    gest_age = kz029_org,
    mat_prepreg_weight = dw002_org,
    mat_prepreg_bmi = dw042_org,
    matsmok_tri1 = matsmok_tri1_org,
    matsmok_tri2 = matsmok_tri2_org,
    matsmok_tri3_c = matsmok_tri3_c_org,
    matsmok_tri3_e = matsmok_tri3_e_org, 
    # Adversity exposure before birth
    mat_homeless_preg = b593,
    mat_homeless_preg2 = c472,
    mat_epds_18wk = b370_org,
    ptrn_epds_18wk = pb260_org, 
    mat_epds_32wk = c600_org,
    mat_antidep_preg = b122,
    diff_afford_food_preg = c520_org,
    diff_afford_heat_preg = c522_org,
    mat_neighb_opinion = a600_org,
    ptnr_convict_preg = pb188a,
    ptnr_sep_sincepreg = pb177,
    mat_divorce_sincepreg = b587,
    ptnr_drug_preg = pb098
  )

colnames(alspacKids_ACE_data_2018)

## ================== Make reduced dataset with ACEs and auxiliary vars needed ====================================
aces_aux <- subset(alspacKids_ACE_data_2018, 
                   select = c("cidb3219", "qlet", 
                              # ACEs between birth - 9 years
                              "physical_abuse_0_9.5yrs",                    
                              "sexual_abuse_0_9.5yrs", "emotional_abuse_0_9.5yrs",                  
                              "emotional_neglect_0_9.5yrs",                      
                              "violence_between_parents_0_9.5yrs", 
                              "substance_household_0_9.5yrs",               
                              "mental_health_problems_or_suicide_0_9.5yrs", 
                              "parent_convicted_offence_0_9.5yrs",          
                              "parental_separation_0_9.5yrs",   
                              # ACEs between 10 - 18 years
                              "physical_abuse_10_18yrs", "sexual_abuse_10_18yrs",
                              "emotional_abuse_10_18yrs", "emotional_neglect_10_18yrs",
                              "violence_between_parents_10_18yrs", "substance_household_10_18yrs",               
                              "mental_health_problems_or_suicide_10_18yrs", "parent_convicted_offence_10_18yrs",          
                              "parental_separation_10_18yrs",
                              # Other forms of adversity (0-18 years)
                              "bullying_0_18yrs",
                              "parent_child_bond_0_18yrs",
                              "financial_difficulties_0_18yrs", 
                              "social_class_0_18yrs",
                              # Early sociodemographic indicators
                              "sex",
                              "soc_class_preg",
                              "ethnicity",
                              "mat_age_delivery",
                              "home_owner_preg",
                              "parity",
                              "marital_status_preg",
                              "edu_mother_selfrep",
                              "edu_ptnr_motherrep",
                              "edu_ptnr_selfrep",
                              "edu_mother_ptnrrep", 
                              "birthweight",
                              "gest_age",
                              "mat_prepreg_weight",
                              "mat_prepreg_bmi",
                              "matsmok_tri1",
                              "matsmok_tri2",
                              "matsmok_tri3_c",
                              "matsmok_tri3_e", 
                              # Adversity exposure before birth
                              "mat_homeless_preg",
                              "mat_homeless_preg2",
                              "mat_epds_18wk",
                              "ptrn_epds_18wk", 
                              "mat_epds_32wk",
                              "mat_antidep_preg",
                              "diff_afford_food_preg",
                              "diff_afford_heat_preg",
                              "mat_neighb_opinion",
                              "ptnr_convict_preg",
                              "ptnr_sep_sincepreg",
                              "mat_divorce_sincepreg",
                              "ptnr_drug_preg"))
# Remove alspacKids_ACE file
rm(alspacKids_ACE_data_2018)

## ================== Convert variables to correct format ====================================
str(aces_aux)

# All ACEs are logi, should be factor
# mat_age_delivery is integer, should be continuous
# parity is integer, should be numeric
# birthweight is integer, should be numeric
# gest_age is integer, should be numeric
# matsmok_tri1 is numeric, should be factor
# matsmok_tri2 is numeric, should be factor                              
# matsmok_tri3_c is numeric, should be factor                        
# matsmok_tri3_e is numeric, should be factor
# mat_homeless_preg is logi, should be factor                      
# mat_homeless_preg2 is logi, should be factor  
# mat_epds_18wk is logi, should be numeric
# ptrn_epds_18wk is logi, should be numeric
# mat_epds_32wk is logi, should be numeric
# mat_antidep_preg is logi, should be factor
# diff_afford_food_preg is logi, should be ordered factor      
# diff_afford_heat_preg is logi, should be ordered factor                         
# mat_neighb_opinion is logi, should be ordered factor  
# ptnr_convict_preg is logi, should be factor
# ptnr_sep_sincepreg is logi, should be factor
# mat_divorce_sincepreg is logi, should be factor
# ptnr_drug_preg is logi, should be factor  

# Convert variables to numeric
numeric_vars <- c("mat_age_delivery", "parity", 
                  "birthweight", "gest_age",
                  "mat_epds_18wk", "ptrn_epds_18wk", "mat_epds_32wk")
aces_aux[numeric_vars] <- lapply(aces_aux[numeric_vars], as.numeric)

# Convert relevant logic/numeric variables to factor
factor_vars <- c("matsmok_tri1", "matsmok_tri2", "matsmok_tri3_c", "matsmok_tri3_e",
                 "mat_homeless_preg", "mat_homeless_preg2", "mat_antidep_preg",
                 "ptnr_convict_preg", "ptnr_sep_sincepreg", "mat_divorce_sincepreg",
                 "ptnr_drug_preg", 
                 #ACEs vars
                 "physical_abuse_0_9.5yrs",   
                 "sexual_abuse_0_9.5yrs", "emotional_abuse_0_9.5yrs",                  
                 "emotional_neglect_0_9.5yrs",                      
                 "violence_between_parents_0_9.5yrs", 
                 "substance_household_0_9.5yrs",               
                 "mental_health_problems_or_suicide_0_9.5yrs", 
                 "parent_convicted_offence_0_9.5yrs",          
                 "parental_separation_0_9.5yrs",   
                 # ACEs between 10 - 18 years
                 "physical_abuse_10_18yrs", "sexual_abuse_10_18yrs",
                 "emotional_abuse_10_18yrs", "emotional_neglect_10_18yrs",
                 "violence_between_parents_10_18yrs", "substance_household_10_18yrs",               
                 "mental_health_problems_or_suicide_10_18yrs", "parent_convicted_offence_10_18yrs",          
                 "parental_separation_10_18yrs",
                 # Other forms of adversity (0-18 years)
                 "bullying_0_18yrs",
                 "parent_child_bond_0_18yrs",
                 "financial_difficulties_0_18yrs", 
                 "social_class_0_18yrs")

aces_aux[factor_vars] <- lapply(aces_aux[factor_vars], factor)
str(aces_aux)

## ================== Load big ALSPAC dataset to derive ACE adulthood exposure ==========================
data <- read.spss("/Users/jessie/Desktop/Baldwin_08Jan21.sav", to.data.frame=TRUE, max.value.labels = 10)
names(data) <- tolower(names(data)) # convert to lower case

## ================== Derive Adversity exposure in young adulthood (18-21 years) vars ====================================

# Mother’s partner was emotionally cruel to child (recode to dichotomous)
table(data$t3336)
data$ptnr_cruel_child18 <- NA
data$ptnr_cruel_child18[data$t3336=="Yes & affected respondent a lot" | 
                          data$t3336=="Yes, respondent moderately affected" |
                          data$t3336=="Yes, respondent mildly affected" |
                          data$t3336=="Yes, but did not affect respondent at all"] <- "Yes"
data$ptnr_cruel_child18[data$t3336=="No, did not happen"] <- "No"
table(data$ptnr_cruel_child18, useNA="always")
table(data$t3336, useNA="always")

# Antidepressant use by mother when child was 18yrs -Dichotomous	Noms (recode to dichotomous)
table(data$t5404)
data$antidep_mother18 <- NA
data$antidep_mother18[data$t5404=="Every day" | 
                        data$t5404=="Often" |
                        data$t5404=="Sometimes" ] <- "Yes"
data$antidep_mother18[data$t5404=="Not at all"] <- "No"
table(data$antidep_mother18, useNA="always")
table(data$t5404, useNA="always")

# Maternal EPDS score when child was 18yrs	- Continuous	Continuous
describe(data$t3255)
data$mat_epds18y <- data$t3255

# Mother separated from partner when child was 18yrs	- Dichotomous	Noms (recode to dichotomous)
table(data$t3316)
data$sep_mother_ptnr18 <- NA
data$sep_mother_ptnr18[data$t3316=="Yes & affected respondent a lot" | 
                         data$t3316=="Yes, respondent moderately affected" |
                         data$t3316=="Yes, respondent mildly affected" |
                         data$t3316=="Yes, but did not affect respondent at all" ] <- "Yes"
data$sep_mother_ptnr18[data$t3316=="No, did not happen"] <- "No"
table(data$sep_mother_ptnr18, useNA="always")
table(data$t3316, useNA="always")

# Maternal AUDIT score when child was 18yrs -	Continuous	Continuous
describe(data$t5510)
data$mat_audit_18y <- data$t5510

# Paternal AUDIT score when child was 18yrs -	Continuous	Continuous
describe(data$fa5510)
data$pat_audit_18y <- data$fa5510

# Participant’s partner used physical force -	Dichotomous	Noms
data$child_ptnr_violence21 <- NA
data$child_ptnr_violence21[data$ypa5004=="Often" | 
                             data$ypa5004=="A few times" |
                             data$ypa5004=="Once" ] <- "Yes"
data$child_ptnr_violence21[data$ypa5004=="Never"] <- "No"
table(data$ypa5004, useNA="always")
table(data$child_ptnr_violence21, useNA="always")

# Participant’s partner used more severe physical force - Dichotomous	Noms
data$child_ptnr_sev_violence21 <- NA
data$child_ptnr_sev_violence21[data$ypa5006=="Often" | 
                                 data$ypa5006=="A few times" |
                                 data$ypa5006=="Once"] <- "Yes"
data$child_ptnr_sev_violence21[data$ypa5006=="Never"] <- "No"
table(data$child_ptnr_sev_violence21, useNA="always")
table(data$ypa5006, useNA="always")

# Participant’s partner pressured them into kissing/touching - Dichotomous	Noms
table(data$ypa5008)
data$child_ptnr_press_kiss21 <- NA
data$child_ptnr_press_kiss21[data$ypa5008=="Often" | 
                               data$ypa5008=="A few times" |
                               data$ypa5008=="Once"] <- "Yes"
data$child_ptnr_press_kiss21[data$ypa5008=="Never"] <- "No"
table(data$child_ptnr_press_kiss21, useNA="always")
table(data$ypa5008, useNA="always")

#Participant’s partner physically forced them into kissing/touching	Dichotomous	Noms
table(data$ypa5010)
data$child_ptnr_force_kiss21 <- NA
data$child_ptnr_force_kiss21[data$ypa5010=="Often" | 
                               data$ypa5010=="A few times" |
                               data$ypa5010=="Once"] <- "Yes"
data$child_ptnr_force_kiss21[data$ypa5010=="Never"] <- "No"
table(data$child_ptnr_force_kiss21, useNA="always")
table(data$ypa5010, useNA="always")

# Participant’s partner pressured them into sexual intercourse -	Dichotomous	Noms
table(data$ypa5012)
data$child_ptnr_press_sex21 <- NA
data$child_ptnr_press_sex21[data$ypa5012=="Often" | 
                              data$ypa5012=="A few times" |
                              data$ypa5012=="Once"] <- "Yes"
data$child_ptnr_press_sex21[data$ypa5012=="Never"] <- "No"
table(data$child_ptnr_press_sex21, useNA="always")
table(data$ypa5012, useNA="always")

#P articipant’s partner physically forced them into sexual intercourse -	Dichotomous	Noms
table(data$ypa5014)
data$child_ptnr_force_sex21 <- NA
data$child_ptnr_force_sex21[data$ypa5014=="Often" | 
                              data$ypa5014=="A few times" |
                              data$ypa5014=="Once"] <- "Yes"
data$child_ptnr_force_sex21[data$ypa5014=="Never"] <- "No"
table(data$child_ptnr_force_sex21, useNA="always")
table(data$ypa5014, useNA="always")

# Participant’s partner made them feel scared of frightened - Dichotomous	Noms
table(data$ypa5016)
data$child_ptnr_scared21 <- NA
data$child_ptnr_scared21[data$ypa5016=="Often" | 
                           data$ypa5016=="A few times" |
                           data$ypa5016=="Once"] <- "Yes"
data$child_ptnr_scared21[data$ypa5016=="Never"] <- "No"
table(data$child_ptnr_scared21, useNA="always")
table(data$ypa5016, useNA="always")

## ================== Make small dataset with adult ACE vars needed ====================================
adv_adulthood <- subset(data, select = c("cidb3219", "qlet", "ptnr_cruel_child18", "antidep_mother18", 
                                         "mat_epds18y", "sep_mother_ptnr18", "mat_audit_18y",
                                         "pat_audit_18y","child_ptnr_violence21", "child_ptnr_sev_violence21",
                                         "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
                                         "child_ptnr_press_sex21", "child_ptnr_force_sex21",
                                         "child_ptnr_scared21"))
str(adv_adulthood)
# Convert all character variables to factor
adv_adulthood[sapply(adv_adulthood, is.character)] <- lapply(adv_adulthood[sapply(adv_adulthood, is.character)], 
                                       as.factor)
str(adv_adulthood)

# Remove big dataset
rm(data)

## ================== Load dataset with polygenic scores and principal components ====================================
# This file contains 10 polygenic scores and 10 principal components 
pgs_pc_data <- read.csv("/Users/jessie/Desktop/all_PGS_PCs_20210607.csv")
head(pgs_pc_data)

# The ID variables "FID and IID" are identical so one can be used for merging and the other can be dropped
identical(pgs_pc_data[['FID']], pgs_pc_data[['IID']])
pgs_pc_data <- subset(pgs_pc_data, select=-c(FID))
head(pgs_pc_data)

## ================== Merge datasets ====
# Note: alspacKids_ACE contained 11,916 obs (because participants were removed if: 
# - they were not part of alspac children dataset mentioned in Boyd et al, n=14701 
# - 1 of every twin was removed
# - participants answered less than 10% of ACE questions

# First make unique ID for each datafile by combining cidb3219 (family ID) and qlet (sibling ID)
aces_aux$id <- paste0(aces_aux$cidb3219, aces_aux$qlet)
adv_adulthood$id <- paste0(adv_adulthood$cidb3219, adv_adulthood$qlet)
dawba_data$id <- paste0(dawba_data$cidb3219, dawba_data$qlet)
names(pgs_pc_data)[names(pgs_pc_data)=="IID"] <- "id"

# Merge datafiles
list_data <- list(aces_aux, adv_adulthood, dawba_data, pgs_pc_data)
alspac_merged <- Reduce(function(...) merge(..., by="id", all=T), list_data)
dim(alspac_merged) 
colnames(alspac_merged)

# Remove duplicate ID variables
alspac_merged <- subset(alspac_merged, select=-c(qlet.x, qlet.y, cidb3219.x, cidb3219.y))

## ================== Check variables are correct format ====================================

str(alspac_merged, list.len=ncol(alspac_merged))

## Regress PCs and sex out of PGSs
# ADHD
alspac_merged$ADHD_PGS_r <- residuals(lm(ADHD_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                         alspac_merged, na.action=na.exclude))
# Alcohol
alspac_merged$alcohol_PGS_r <- residuals(lm(alcohol_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                            alspac_merged, na.action=na.exclude))
# Antisocial
alspac_merged$antisocial_PGS_r <- residuals(lm(antisocial_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                               alspac_merged, na.action=na.exclude))
# Anxiety
alspac_merged$anxiety_PGS_r <- residuals(lm(anxiety_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                            alspac_merged, na.action=na.exclude))
# Autism
alspac_merged$autism_PGS_r <- residuals(lm(autism_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                           alspac_merged, na.action=na.exclude))
# Bipolar
alspac_merged$bipolar_PGS_r <- residuals(lm(bipolar_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                            alspac_merged, na.action=na.exclude))
# Bipolar 2019
alspac_merged$bipolar2019_PGS_r <- residuals(lm(bipolar_2019_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                            alspac_merged, na.action=na.exclude))
# Depression
alspac_merged$depression_PGS_r <- residuals(lm(depression_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                               alspac_merged, na.action=na.exclude))
# Schizoprehnia
alspac_merged$schizophrenia_PGS_r <- residuals(lm(schizophrenia_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                                  alspac_merged, na.action=na.exclude))
# Cataracts
alspac_merged$cataracts_PGS_r <- residuals(lm(cataracts_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                                  alspac_merged, na.action=na.exclude))
# Schizoprehnia
alspac_merged$handedness_PGS_r <- residuals(lm(handedness_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex, 
                                                  alspac_merged, na.action=na.exclude))

## ================== Examine levels of missingness ====================================
# Subset to those with 10% ACEs data (included in aces_aux dataframe)
alspac_imput <- alspac_merged[which(alspac_merged$id %in% aces_aux$id),]

# Subset to sample with DAWBA data at 7, 10 or 13 
alspac_imput <- subset(alspac_imput, !is.na(internalising7) & !is.na(externalising7) |
                         !is.na(ADHD_PGS) & !is.na(internalising10) & !is.na(externalising10) |
                         !is.na(ADHD_PGS) & !is.na(internalising13) & !is.na(externalising13) )

# Subset to sample with genetic data
alspac_imput <- alspac_imput[which(alspac_imput$id %in% pgs_pc_data$id),]

dim(alspac_imput) #6411, this includes people with genetic data, >10% of ACE data from 0-9 and DAWBA measures at >=1 time point

# Save complete case sample
setwd("/Users/jessie/Desktop")
saveRDS(alspac_imput, file = "ALSPACcompleteCases_20220204.rds")

# Examine missingness in sample
p_missing <- unlist(lapply(alspac_imput, function(x) sum(is.na(x))))/nrow(alspac_imput)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sort(p_missing[p_missing > 0.25], decreasing = TRUE)

## ================== Specify categories for transformation in imputation model ====================================
colnames(alspac_imput)
str(alspac_imput)

## Noms variables
noms = c( # ACEs between birth and 9.5 
  "physical_abuse_0_9.5yrs", "sexual_abuse_0_9.5yrs",                      
  "emotional_abuse_0_9.5yrs", "emotional_neglect_0_9.5yrs",
  "violence_between_parents_0_9.5yrs", "substance_household_0_9.5yrs",
  "mental_health_problems_or_suicide_0_9.5yrs", "parent_convicted_offence_0_9.5yrs",
  "parental_separation_0_9.5yrs", 
  # Early sociodemographic indicators
  "sex", 
  "ethnicity", 
  "home_owner_preg", "marital_status_preg", 
  "matsmok_tri1", "matsmok_tri2",                    
  "matsmok_tri3_c",  "matsmok_tri3_e",
  "mat_homeless_preg", "mat_homeless_preg2", 
  "mat_antidep_preg",
  "ptnr_convict_preg", "ptnr_sep_sincepreg",
  "mat_divorce_sincepreg", "ptnr_drug_preg",
  # ACEs in adolescence 
  "physical_abuse_10_18yrs", "sexual_abuse_10_18yrs", 
  "emotional_abuse_10_18yrs", "emotional_neglect_10_18yrs", 
  "violence_between_parents_10_18yrs","substance_household_10_18yrs", 
  "mental_health_problems_or_suicide_10_18yrs",
  "parent_convicted_offence_10_18yrs", "parental_separation_10_18yrs",
  # Other ACEs
  "bullying_0_18yrs", "parent_child_bond_0_18yrs", 
  "financial_difficulties_0_18yrs", "social_class_0_18yrs",
  # Adversity exposure in young adulthood
  "ptnr_cruel_child18", "antidep_mother18",
  "sep_mother_ptnr18", "child_ptnr_violence21",                    
  "child_ptnr_sev_violence21",  "child_ptnr_press_kiss21",                   
  "child_ptnr_force_kiss21", "child_ptnr_press_sex21",                   
  "child_ptnr_force_sex21", "child_ptnr_scared21")

## Ords variables
ords = c("soc_class_preg", #Parental social class during pregnancy
         "edu_mother_selfrep", #Mother’s highest educational level (mother-reported)
         "edu_ptnr_motherrep", #Partner’s highest educational level (mother-reported)
         "edu_ptnr_selfrep", #Mother’s highest educational level (partner-reported)
         "edu_mother_ptnrrep", #Partner’s highest educational level (partner-reported)
         "diff_afford_food_preg", #Difficulty affording food during pregnancy
         "diff_afford_heat_preg", #Difficulty affording heating during pregnancy          
         "mat_neighb_opinion") #Mother’s opinion of neighbourhood during pregnancy

## ID variables (those that will not be imputed but will remain in imputed datasets)
idvars = c("id", "cidb3219", "qlet",
           "ADHD_PGS", "alcohol_PGS", 
           "antisocial_PGS", "anxiety_PGS", "autism_PGS",
           "bipolar_PGS", "depression_PGS", "schizophrenia_PGS", 
           "handedness_PGS", "cataracts_PGS", 
           "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
           "antisocial_PGS_r", "anxiety_PGS_r", "autism_PGS_r",
           "bipolar_PGS_r", "bipolar2019_PGS_r", "depression_PGS_r", "schizophrenia_PGS_r", 
           "handedness_PGS_r", "cataracts_PGS_r")

# Impute 50 datasets (takes about 30 minutes)
a.out_alspac = amelia(alspac_imput, noms = noms, ords=ords, idvars = idvars, 
               incheck = FALSE, emburn=c(5,30),
               m=50)

class(a.out_alspac)
summary(a.out_alspac)

# Save workspace including Amelia datasets
save(a.out_alspac, file = "alspac_imputations_20220204.RData")
rm(aces_aux, adv_adulthood, complete_cases, dawba_data, list_data, vars10, vars13, vars7)

