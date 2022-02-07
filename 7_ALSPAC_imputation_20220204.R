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


## Correlation between the ACE and the mental health outcome 	
# - 1. Run a linear regression model predicting the mental health outcome from the ACE 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the ACE
## Correlation between observed polygenic scores for mental health problems and the ACE (a path)	
# - 1. Run a probit regression model predicting the ACE from the polygenic scores 
# - 2. Take the square root of the R2 value reflecting the variance in the (latent-response) ACE variable explained by the polygenic scores
## Correlation between observed polygenic scores and the mental health outcome (b path)	
# - 1. Run a linear regression model predicting the mental health outcome from the polygenic scores for mental health problems 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the polygenic scores
# Note. All models will be estimated using the lavaan package27, accounting for sex and principal components. 


##### Step 1: correlation between ACE and MH outcome
# Specify lavaan model
reg_mh_ace <- "
# Regression  
MH ~ ACE
# Variance
MH ~~ MH
# Intercept
MH ~ 1
"
# Trial
a.out <- transform(a.out, MH = scale(residuals(lm(externalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))), 
                    ACE = maltreatment_0_9.5yrs) 
ext_mal <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan")
summary(ext_mal, rsquare=TRUE)[6,5]

#### Function
r2_ace <- function(data) {
  
  # Maltreatment analyses
  a.out <- transform(a.out, ACE = maltreatment_0_9.5yrs) #transform so MH = externalising & ACE = maltreatment
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_mal <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Domestic violence analyses
  a.out <- transform(a.out, ACE = violence_between_parents_0_9.5yrs) #transform so ACE = domestic violence
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_domvi <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Parental substance abuse analyses
  a.out <- transform(a.out, ACE = substance_household_0_9.5yrs) #transform so ACE = parental substance abuse
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_parsub <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Parental psychopathology analyses
  a.out <- transform(a.out, ACE = mental_health_problems_or_suicide_0_9.5yrs) #transform so ACE = parental psychopathology
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_parpsych <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Parental criminality analyses
  a.out <- transform(a.out, ACE = parent_convicted_offence_0_9.5yrs) #transform so ACE = parental criminality
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_parcrim <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Parental separation analyses
  a.out <- transform(a.out, ACE = parental_separation_0_9.5yrs) #transform so ACE = parental substance abuse
  fit <- runMI(reg_mh_ace, data=a.out$imputations, fun="lavaan") # fit lavaan model
  r2_parsep <- summary(fit, rsquare=TRUE)[6,5] # extract results
  
  # Return results
  return(c(r2_mal, r2_domvi, r2_parsub, r2_parpsych, r2_parcrim, r2_parsep))
}

ACE <- c("maltreatment", "domestic_violence", "par_substance", "par_psych", "par_criminal", "par_sep")

# Run function for internalising problems
a.out <- transform(a.out, MH = scale(residuals(lm(internalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude)))
r2 <- r2_ace(a.out) 
r2_int.df <- data.frame(ACE, r2)

# Run function for externalising problems
a.out <- transform(a.out, MH = scale(residuals(lm(externalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))
r2 <- r2_ace(a.out) 
r2_ext.df <- data.frame(ACE, r2)

# Combine DF for internalising and externalising and calculate r
r2_int_ext <- rbind(r2_int.df, r2_ext.df)
r2_int_ext$outcome <- c(rep("int", 6), rep("ext", 6))
r2_int_ext$r <- sqrt(r2_int_ext$r2)

#### Step 2. Correlation between observed polygenic scores for mental health problems and the ACE (a path)	

# Specify lavaan model
reg_ace_pgs <- "
    # Regression  
    ACE ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar_PGS_r + depression_PGS_r + schizophrenia_PGS_r
"
# Trial
a.out <- transform(a.out, ACE = maltreatment_0_9.5yrs, ADHD_PGS_r = scale(ADHD_PGS_r), 
                   alcohol_PGS_r = scale(alcohol_PGS_r),
                   antisocial_PGS_r = scale(antisocial_PGS_r),
                   anxiety_PGS_r = scale(anxiety_PGS_r),
                   autism_PGS_r = scale(autism_PGS_r),
                   bipolar_PGS_r = scale(bipolar_PGS_r),
                   depression_PGS_r = scale(depression_PGS_r),
                   schizophrenia_PGS_r = scale(schizophrenia_PGS_r)) 
ext_mal <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV")
summary(ext_mal, rsquare=TRUE)[57,5]

#### Run across ACE types

# Maltreatment analyses
a.out <- transform(a.out, ACE = maltreatment_0_9.5yrs) #transform so ACE = maltreatment
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Domestic violence analyses
a.out <- transform(a.out, ACE = violence_between_parents_0_9.5yrs) #transform so ACE = domestic violence
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental substance abuse analyses
a.out <- transform(a.out, ACE = substance_household_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental psychopathology analyses
a.out <- transform(a.out, ACE = mental_health_problems_or_suicide_0_9.5yrs) #transform so ACE = parental psychopathology
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental criminality analyses
a.out <- transform(a.out, ACE = parent_convicted_offence_0_9.5yrs) #transform so ACE = parental criminality
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental separation analyses
a.out <- transform(a.out, ACE = parental_separation_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[57,5] # extract results
  
# Return results
pgs_r2 <- c(r2_mal, r2_domvi, r2_parsub, r2_parpsych, r2_parcrim, r2_parsep)
pgs_ace <- data.frame(ACE, pgs_r2)
pgs_ace$r <- sqrt(pgs_ace$pgs_r2)

##### Step 3. Correlation between observed polygenic scores and the mental health outcome (b path)	
# - 1. Run a linear regression model predicting the mental health outcome from the polygenic scores for mental health problems 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the polygenic scores

# Specify lavaan model
reg_mh_pgs <- "
# Regression  
MH ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar_PGS_r + depression_PGS_r + schizophrenia_PGS_r
# Variance
MH ~~ MH
# Intercept
MH ~ 1
"

# Internalising analyses
a.out <- transform(a.out, MH = scale(residuals(lm(internalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                         PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))
fit <- runMI(reg_mh_pgs, data=a.out$imputations, fun="lavaan") # fit lavaan model
r2_int <- summary(fit, rsquare=TRUE)[55,5] # extract results

# Externalising analyses
a.out <- transform(a.out, MH = scale(residuals(lm(externalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                   PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))
fit <- runMI(reg_mh_pgs, data=a.out$imputations, fun="lavaan") # fit lavaan model
r2_ext <- summary(fit, rsquare=TRUE)[55,5] # extract results

mh_pgs_r2 <- c(r2_int, r2_ext)
outcome <- c("int", "ext")
pgs_mh <- data.frame(outcome, mh_pgs_r2)
pgs_mh$r <- sqrt(pgs_mh$mh_pgs_r2)

##### Run GSens
library(devtools)
#install_github("JBPG/Gsens")
library(Gsens)

#### Edit Gsens function to provide the proportion #####
gsensY_prop <- function (rxy, rgx, rgy, n, h2, constrain = NULL, print = FALSE) 
{
  mat = matrix(c(1, rgx, rgy, rgx, 1, rxy, rgy, rxy, 1), ncol = 3, 
               nrow = 3)
  colnames(mat) = c("G", "X", "Y")
  rownames(mat) = c("G", "X", "Y")
  model1 <- paste("\n                  Y ~ bxy*X+bgy*GG # Y depends on X and true polygenic score\n                  X ~ bgx*GG # X depends on true polygenic score\n                  GG =~ l*G # true polygenic score is estimated by G\n                  GG ~~ 1*GG # true polygenic score will be standardised\n                  Y ~~ Y # residual error of Y\n                  X ~~ X # residual error of X\n                  G ~~ vg*G # measurement error in G due to SNP selection & sampling error\n                  bgy+bgx*bxy == sqrt(", 
                  h2, ") # heritability constraint\n                  conf:=bgx*bgy                  #Confounding effect in standardized model\n                  total:=conf+bxy #total effect\n   prop:=conf/total\n", 
                  constrain, " #optional constraints\n                  ")
  fit1 = lavaan(model1, sample.cov = mat, sample.nobs = n, 
                estimator = "GLS")
  if (print) {
    summary(fit1)
  }
  pe = parameterestimates(fit1)
  pe$pvalue = formatC(2 * pnorm(-abs(pe$z)), digits = 5)
  results = data.frame(rbind(pe[pe$label == "bxy", ], 
                             pe[pe$label ==  "conf", ], 
                             pe[pe$label == "total", ],
                             pe[pe$label == "prop", ]))[, 5:10]
  results = results %>% mutate_if(is.numeric, round, 3)
  rownames(results) = c("Adjusted Bxy", "Genetic confounding", 
                        "Total effect", "Prop")
  results
}

## Results I need for Gsens
# rxy: Correlation between the ACE and the mental health outcome 
r2_int_ext
# rgx: Correlation between observed polygenic scores for mental health problems and the ACE 
pgs_ace
# rgy: Correlation between observed polygenic scores and the mental health outcome (b path)
pgs_mh

#### Function to Run Gsens across all ACEs 
gsens_func <- function(ace_var, rxy_data, outcome_var, rgx_data, rgy_data, heritability_outcome){
gsens <- gsensY_prop(
         rxy = rxy_data$r[rxy_data$ACE==ace_var &  rxy_data$outcome==outcome_var],
         rgx = rgx_data$r[rgx_data$ACE == ace_var],
         rgy = rgy_data$r[rgy_data$outcome == outcome_var],
         n = 6411,
         h2 = heritability_outcome, print=TRUE, 
         constrain = 'bxy > 0') 
return(c(gsens$est, gsens$se, gsens$pvalue, gsens$ci.lower, gsens$ci.upper))
}

#### Function to format results 
format_gsens_res <- function(data, N) {
  # Convert to a dataframe with 6 rows and 20 columns
  results <- as.data.frame(matrix(data, nrow = 6, ncol = 20, byrow=TRUE,
                                  # Specify rownames to correspond to ACEs
                                  dimnames = list(c("maltreatment", "domestic_violence", "par_psych", "par_substance","par_criminal","par_sep"),
                                                  # Specify column names to correspond to type of effect 
                                                  c(paste0(c("Adjusted Bxy", "Genetic confounding","Total effect", "Prop"), "_est"), 
                                                    paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_se"),
                                                    paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_pvalue"),
                                                    paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_lowCI"),
                                                    paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_upCI")))))
  # Convert columns for the proportion of the association explained by genetics to numeric
  results$Prop_est <- as.numeric(levels(results$Prop_est))[results$Prop_est]
  results$Prop_se <- as.numeric(levels(results$Prop_se))[results$Prop_se]
  # Derive variance of the proportion of the association explained by genetics
  results$Prop_var <- results$Prop_se^2
  # Derive variable indexing N of sample
  results$ni <- N
  # Derive ID variable (for later aggregation)
  results$id <- 1
  
  return(as.data.frame(results))
}

#### Function to aggregate proportions across ACEs
agg_res <- function(data, corr_aces, N) {
  # Apply Freeman-Tukey double arcsine transformation to proportions
  data$ft_prop <- transf.pft(xi = data$Prop_est, ni = data$ni) 
  # Apply Freeman-Tukey double arcsine transformation to variance of proportions
  data$ft_var <- transf.pft(xi = data$Prop_var, ni = data$ni)  
  
  # Aggregate effect sizes 
  agg_2b <- agg(id = id, es = ft_prop, var = ft_var, method = "BHHR", cor = corr_aces, mod = NULL, data = data) 
  
  # Extract transformed estimates
  est_model <- agg_2b$es
  var_model <- agg_2b$var
  se_model <- sqrt(agg_2b$var)
  lowci_model <- est_model - 1.96*se_model
  upci_model <- est_model + 1.96*se_model
  
  # Back-transform proportions 
  # Pooled effect
  proportion <- transf.ipft(xi=est_model, ni=N)
  # Low CI
  lowci <- transf.ipft(xi=lowci_model, ni=N)
  # Upper CI
  upci <- transf.ipft(xi=upci_model, ni=N)
  return(c(proportion, lowci, upci))
}

# Specify ACEs
aces <- list("maltreatment", "domestic_violence", "par_psych",
               "par_substance", "par_criminal", "par_sep")

#### Internalising problems 
# Run Gsens 
results_int <- sapply(aces, 
       gsens_func,
       rxy_data = r2_int_ext,
       outcome_var = "int",
       rgx_data = pgs_ace,
       rgy_data = pgs_mh,
       heritability_outcome = 0.06) # apply function

# Format results
results_int <- format_gsens_res(results_int, 6411)

# Aggregate proportions
agg_results_int <- agg_res(results_int, mean_ace_cor, 6411)

#### Externalising problems 
# Run Gsens 
results_ext <- sapply(aces, 
                      gsens_func,
                      rxy_data = r2_int_ext,
                      outcome_var = "ext",
                      rgx_data = pgs_ace,
                      rgy_data = pgs_mh,
                      heritability_outcome = 0.09) # apply function

# Format results
results_ext <- format_gsens_res(results_ext, 6411)

# Aggregate proportions
agg_results_ext <- agg_res(results_ext, mean_ace_cor, 6411)


######################################## END #############################################









#Freeman-Tukey double arcsine transformation

### Aggregate effect sizes
results$ni <- 6411
results$Prop_var <- results$Prop_se^2
# Transform with double arscine method
results$ft_prop <- transf.pft(xi=results$Prop_est, ni=results$ni) # transform estimate
# check if backtransforming gets same values as original proportions (yes)
transf.ipft(xi=results$ft_prop, ni=6411)
results$ft_var <- transf.pft(xi=results$Prop_var, ni=results$ni) #transform variance

#### Aggregate effect sizes (specify cor=0.3 due to correlations between ACEs)
results$id <- 1
agg_2b <- agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = 0.25, mod=NULL, data=results) 
## Get p-value and other estimates
est_model <- agg_2b$es
var_model <- agg_2b$var
se_model <- sqrt(agg_2b$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
z_model <- est_model/se_model 
p_model <- exp(-0.717*z_model - 0.416*(z_model^2))

## Back-transform proportions for checking
# pooled effect
av_prop <- transf.ipft(xi=est_model, ni=6411)
# low CI
transf.ipft(xi=lowci_model, ni=6411)
# upper CI
transf.ipft(xi=upci_model, ni=6411)

# Run Gsens for externeralising problems
results <- sapply(aces, 
                  gsens_func,
                  rxy_data = r2_int_ext,
                  outcome_var = "ext",
                  rgx_data = pgs_ace,
                  rgy_data = pgs_mh,
                  heritability_outcome = 0.09) # apply function

# Format results 
results <- as.data.frame(matrix(results, nrow = 6, ncol = 20, byrow=TRUE,
                                dimnames = list(c("maltreatment", "domestic_violence",
                                                  "par_psych", "par_substance",
                                                  "par_criminal","par_sep"),
                                                c(paste0(c("Adjusted Bxy", "Genetic confounding", 
                                                           "Total effect", "Prop"), "_est"), 
                                                  paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_se"),
                                                  paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_pvalue"),
                                                  paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_lowCI"),
                                                  paste0(c("Adjusted Bxy", "Genetic confounding", "Total effect", "Prop"), "_upCI")))))

class(results$Prop_est)  
results$Prop_est <- as.numeric(levels(results$Prop_est))[results$Prop_est]
results$Prop_se <- as.numeric(levels(results$Prop_se))[results$Prop_se]
mean(results$Prop_est)

### Aggregate effect sizes
results$ni <- 6411
results$Prop_var <- results$Prop_se^2
# Transform with double arscine method
results$ft_prop <- transf.pft(xi=results$Prop_est, ni=results$ni) # transform estimate
# check if backtransforming gets same values as original proportions (yes)
transf.ipft(xi=results$ft_prop, ni=6411)
results$ft_var <- transf.pft(xi=results$Prop_var, ni=results$ni) #transform variance

#### Aggregate effect sizes (specify cor=0.3 due to correlations between ACEs)
results$id <- 1
agg_2b <- agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = 0.25, mod=NULL, data=results) 
## Get p-value and other estimates
est_model <- agg_2b$es
var_model <- agg_2b$var
se_model <- sqrt(agg_2b$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
z_model <- est_model/se_model 
p_model <- exp(-0.717*z_model - 0.416*(z_model^2))

## Back-transform proportions for checking
# pooled effect
av_prop <- transf.ipft(xi=est_model, ni=6411)
# low CI
transf.ipft(xi=lowci_model, ni=6411)
# upper CI
transf.ipft(xi=upci_model, ni=6411)

                                
## Try for each ACE to check results
# Maltreatment
gsensY_prop(
  rxy = r2_int_ext$r[r2_int_ext$ACE=="maltreatment" &  r2_int_ext$outcome=="int"],
  rgx = pgs_ace$r[pgs_ace$ACE == "maltreatment"],
  rgy = pgs_mh$r[pgs_mh$outcome == "int"],
  n = 6411,
  h2 = 0.06) 

gsensY(
  rxy = r2_int_ext$r[r2_int_ext$ACE=="maltreatment" &  r2_int_ext$outcome=="int"],
  rgx = pgs_ace$r[pgs_ace$ACE == "maltreatment"],
  rgy = pgs_mh$r[pgs_mh$outcome == "int"],
  n = 6411,
  h2 = 0.06) 

# domestic_violence
gsensY_prop(
  rxy = r2_int_ext$r[r2_int_ext$ACE=="domestic_violence" &  r2_int_ext$outcome=="int"],
  rgx = pgs_ace$r[pgs_ace$ACE == "domestic_violence"],
  rgy = pgs_mh$r[pgs_mh$outcome == "int"],
  n = 6411,
  h2 = 0.06) 

# Parental psychopathology
gsensY_prop(
  rxy = r2_int_ext$r[r2_int_ext$ACE=="par_psych" &  r2_int_ext$outcome=="int"],
  rgx = pgs_ace$r[pgs_ace$ACE == "par_psych"],
  rgy = pgs_mh$r[pgs_mh$outcome == "int"],
  n = 6411,
  h2 = 0.06) 

# Parental substance
gsensY_prop(
  rxy = r2_int_ext$r[r2_int_ext$ACE=="par_substance" &  r2_int_ext$outcome=="int"],
  rgx = pgs_ace$r[pgs_ace$ACE == "par_substance"],
  rgy = pgs_mh$r[pgs_mh$outcome == "int"],
  n = 6411,
  h2 = 0.06) 

mat = matrix(c(1, 0.2, 0.3, 0.2, 1, 0.5, 0.3, 0.5, 1), ncol = 3, 
             nrow = 3)
colnames(mat) = c("G", "X", "Y")
rownames(mat) = c("G", "X", "Y")
model1 <- paste("Y ~ bxy*X+bgy*GG # Y depends on X and true polygenic score
                X ~ bgx*GG # X depends on true polygenic score
                GG =~ l*G # true polygenic score is estimated by G\
                GG ~~ 1*GG # true polygenic score will be standardised
                Y ~~ Y # residual error of Y\n                  
                X ~~ X # residual error of X\n                  
                G ~~ vg*G # measurement error in G due to SNP selection & sampling error\n                  
                bgy+bgx*bxy == sqrt(", 0.4, ") # heritability constraint\n                  
                conf:=bgx*bgy                  #Confounding effect in standardized model\n                  
                total:=conf+bxy #total effect\n   
                prop:=conf/total\n"
                )
fit1 = lavaan(model1, sample.cov = mat, sample.nobs = 100, 
              estimator = "GLS")

# Maltreatment and internalising problems
gsens <- gsensY_edit(rxy = r2_int_ext$r[r2_int_ext$ACE=="maltreatment" &  r2_int_ext$outcome=="int"],
       rgx = pgs_ace$r[pgs_ace$ACE == "maltreatment"],
       rgy = pgs_mh$r[pgs_mh$outcome == "int"],
       n = 6411,
       h2 = 0.06) # heritability estimate from Rosa's paper

# Maltreatment and externalising problems
gsensY(rxy = r2_int_ext$r[r2_int_ext$ACE=="maltreatment" &  r2_int_ext$outcome=="ext"],
       rgx = pgs_ace$r[pgs_ace$ACE == "maltreatment"],
       rgy = pgs_mh$r[pgs_mh$outcome == "ext"],
       n = 6411,
       h2 = 0.09) # heritability estimate from Rosa's paper

# Might need the following?
# Variance
MH ~~ MH
# Intercept
MH ~ 1
"

# Co-variance
ADHD_PGS_r ~~ alcohol_PGS_r
ADHD_PGS_r ~~ antisocial_PGS_r
ADHD_PGS_r ~~ anxiety_PGS_r
ADHD_PGS_r ~~ autism_PGS_r
ADHD_PGS_r ~~ bipolar_PGS_r
ADHD_PGS_r ~~ depression_PGS_r
ADHD_PGS_r ~~ schizophrenia_PGS_r
alcohol_PGS_r ~~ antisocial_PGS_r
alcohol_PGS_r ~~ anxiety_PGS_r
alcohol_PGS_r ~~ autism_PGS_r
alcohol_PGS_r ~~ bipolar_PGS_r
alcohol_PGS_r ~~ depression_PGS_r
alcohol_PGS_r ~~ schizophrenia_PGS_r
antisocial_PGS_r ~~ anxiety_PGS_r
antisocial_PGS_r ~~ autism_PGS_r
antisocial_PGS_r ~~ bipolar_PGS_r
antisocial_PGS_r ~~ depression_PGS_r
antisocial_PGS_r ~~ schizophrenia_PGS_r
anxiety_PGS_r ~~ autism_PGS_r
anxiety_PGS_r ~~ bipolar_PGS_r
anxiety_PGS_r ~~ depression_PGS_r
anxiety_PGS_r ~~ schizophrenia_PGS_r
autism_PGS_r ~~ bipolar_PGS_r
autism_PGS_r ~~ depression_PGS_r
autism_PGS_r ~~ schizophrenia_PGS_r
bipolar_PGS_r ~~ depression_PGS_r
bipolar_PGS_r ~~ schizophrenia_PGS_r
depression_PGS_r ~~ schizophrenia_PGS_r"
##### Pool results for negative controls 

est <- c(summary(nc_int_maltreatment)[12, 5], 
         summary(nc_int_dom_violence)[12, 5], 
         summary(nc_int_par_substance)[12, 5],
         summary(nc_int_par_psych)[12, 5],
         summary(nc_int_par_crim)[12, 5],
         summary(nc_int_par_sep)[12, 5],
         summary(nc_ext_maltreatment)[12, 5], 
         summary(nc_ext_dom_violence)[12, 5], 
         summary(nc_ext_par_substance)[12, 5],
         summary(nc_ext_par_psych)[12, 5],
         summary(nc_ext_par_crim)[12, 5],
         summary(nc_ext_par_sep)[12, 5])
se <- c(summary(nc_int_maltreatment)[12, 6], 
        summary(nc_int_dom_violence)[12, 6], 
        summary(nc_int_par_substance)[12, 6],
        summary(nc_int_par_psych)[12, 6],
        summary(nc_int_par_crim)[12, 6],
        summary(nc_int_par_sep)[12, 6],
        summary(nc_ext_maltreatment)[12, 6], 
        summary(nc_ext_dom_violence)[12, 6], 
        summary(nc_ext_par_substance)[12, 6],
        summary(nc_ext_par_psych)[12, 6],
        summary(nc_ext_par_crim)[12, 6],
        summary(nc_ext_par_sep)[12, 6])
ACE <- rep(c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"), 2)
nc <- data.frame(ACE, est, se)
nc$var <- nc$se^2 # get variance
nc$ni <- 6411 # sample size

# Transform with double arscine method
int_ext_comb$ft_prop <- transf.pft(xi=int_ext_comb$est, ni=int_ext_comb$ni) # transform estimate
# check if backtransforming gets same values as original proportions (yes)
transf.ipft(xi=int_ext_comb$ft_prop, ni=6411)
int_ext_comb$ft_var <- transf.pft(xi=int_ext_comb$var, ni=int_ext_comb$ni) #transform variance

#### Aggregate effect sizes (specify cor=0.3 due to correlations between ACEs)
int_ext_comb$id <- 1
agg_2a <- agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = mean_ace_cor, mod=NULL, data=int_ext_comb) 
## Get p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
z_model <- est_model/se_model 
p_model <- exp(-0.717*z_model - 0.416*(z_model^2))

## Back-transform proportions for checking
av_prop <- transf.ipft(xi=est_model, ni=6411)
av_var <- transf.ipft(xi=var_model, ni=6411)

#### Reproducible example
raw_prop <- c(0.03903113599,  0.06421417304,  0.08055710621,  0.02801081943, -0.01343618067,  0.05958131290)
se <- c(0.011821054216, 0.019884958389, 0.030320175637, 0.008348501546, 0.030915263561, 0.022649971701)
var <- se^2
id <- 1:6
df <- data.frame(id, raw_prop, se, var)

# Transform raw proportions using the Freeman-Tukey double arcsine transformation
df$ft_prop <- transf.pft(xi=df$raw_prop, ni=6411) # Note: 6411 is the N for each effect size
#Warning messages:
#1: In sqrt(xi/(ni + 1)) : NaNs produced
#2: In sqrt((xi + 1)/(ni + 1)) : NaNs produced

# Convert negative proportion to 0 
df$raw_prop[df$raw_prop == -0.01343618067] <- 0 

# Transform raw proportions (with no negative proportion) using the Freeman-Tukey double arcsine transformation
df$ft_prop <- transf.pft(xi=df$raw_prop, ni=6411) # transform proportions
df$ft_var <- transf.pft(xi=df$var, ni=6411) # transform variance

# Run meta-analysis on transformed proportions
prop <- rma(yi=ft_prop, vi=ft_var, data=df)

Model Results:
  
  estimate      se    zval    pval   ci.lb   ci.ub 
0.1951  0.0573  3.4054  0.0007  0.0828  0.3074  *** 
  
# Back transform proportions
transf.ipft(xi=prop$beta, ni=6411)  # raw prop = 0.03751326977
transf.ipft(xi=prop$se^2, ni=6411) # raw var = 0.003282383855

# Run meta-analysis on raw proportions to compare 
rma(yi=raw_prop, vi=var, data=df)

df$id <- 1
agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = mean_ace_cor, data=df) 
transf.ipft(xi=0.1941079392, ni=6411) 

# Function to calculate log odds difference and 90% CIs for equivalence test
diff_logOdds <- function (log_odds1, log_odds2, se1, se2) {
  log_odds_diff <- log_odds1 - log_odds2 #change numbers
  se_diff <- sqrt( (se1^2) + (se2^2) ) # check for calculating SE
  low_ci <- log_odds_diff - (1.645*se_diff) # 90% low CI
  high_ci <- log_odds_diff + (1.645*se_diff) # 90% upper CI
  return(c(log_odds_diff, low_ci, high_ci))
}

# ADHD versus alcohol
adhd_alcohol <- diff_logOdds(pgs_data$log_odds[PGS=="Av_ADHD"], pgs_data$log_odds[PGS=="Av_alcohol"], 
                             pgs_data$log_se[PGS=="Av_ADHD"], pgs_data$log_se[PGS=="Av_alcohol"])
# ADHD vs autism
adhd_autism <- diff_logOdds(pgs_data$log_odds[PGS=="Av_ADHD"], pgs_data$log_odds[PGS=="Av_autism"], 
                             pgs_data$log_se[PGS=="Av_ADHD"], pgs_data$log_se[PGS=="Av_autism"])
# Alcohol vs depression
alc_dep <- diff_logOdds(pgs_data$log_odds[PGS=="Av_alcohol"], pgs_data$log_odds[PGS=="Av_depression"], 
                            pgs_data$log_se[PGS=="Av_alcohol"], pgs_data$log_se[PGS=="Av_depression"])
# Alcohol vs schizophrenia
alc_schiz <- diff_logOdds(pgs_data$log_odds[PGS=="Av_alcohol"], pgs_data$log_odds[PGS=="Av_schizophrenia"], 
                            pgs_data$log_se[PGS=="Av_alcohol"], pgs_data$log_se[PGS=="Av_schizophrenia"])
# Antisocial vs depression
antisoc_dep <- diff_logOdds(pgs_data$log_odds[PGS=="Av_antisocial"], pgs_data$log_odds[PGS=="Av_depression"], 
                            pgs_data$log_se[PGS=="Av_antisocial"], pgs_data$log_se[PGS=="Av_depression"])
# Autism vs depression
autism_dep <- diff_logOdds(pgs_data$log_odds[PGS=="Av_autism"], pgs_data$log_odds[PGS=="Av_depression"], 
                           pgs_data$log_se[PGS=="Av_autism"], pgs_data$log_se[PGS=="Av_depression"])
# Autism vs schizophrenia
autism_schiz <- diff_logOdds(pgs_data$log_odds[PGS=="Av_autism"], pgs_data$log_odds[PGS=="Av_schizophrenia"], 
                            pgs_data$log_se[PGS=="Av_autism"], pgs_data$log_se[PGS=="Av_schizophrenia"])
# Bipolar vs depression
bipolar_dep <- diff_logOdds(pgs_data$log_odds[PGS=="Av_bipolar"], pgs_data$log_odds[PGS=="Av_depression"], 
                            pgs_data$log_se[PGS=="Av_bipolar"], pgs_data$log_se[PGS=="Av_depression"])

# Combine into dataframe and plot
pgs_diff <- as.data.frame(rbind(adhd_alcohol, adhd_autism, alc_dep, alc_schiz, 
                                antisoc_dep, autism_dep, autism_schiz, bipolar_dep))
colnames(pgs_diff) <- c("log_odds_diff","low_ci", "up_ci")
library(data.table)
setDT(pgs_diff, keep.rownames = "comparison")[]
library(ggplot2)

# Get each combination of polygenic score effects to compare in pairwise comparisons
combs <- combn(c("Av_ADHD", "Av_alcohol", "Av_antisocial","Av_anxiety", "Av_autism", "Av_bipolar", "Av_depression", "Av_schizophrenia"), 2)
pair_combs <- as.vector(mapply(paste, sep = " == ", combs[c(T,F),], combs[c(F,T),])) #https://stackoverflow.com/questions/37901142/paste-multiple-rows-together-in-r
# Function to pairwise compare
pairwise_Wald <- function(model, pair) {
  wald <- lavTestWald.mi(model, constraints = pair)
  return(c(pair, as.numeric(wald["pvalue"])))
}


diff_pgs <- function(Estimate, Std.Err, )

# Calculate difference in OR and SE between 6v8
or_diff_5v7 <- (-0.009*1.8) - (0.049*1.8)
se_diff_5v7 <- sqrt( ((0.012*1.8)^2) + ((0.012*1.8)^2) )
# Calculate the lower and upper 90% CI for 6v8 from the SE
lowci_5v7 <- or_diff_5v7 - 1.645*se_diff_5v7
upci_5v7 <- or_diff_5v7 + 1.645*se_diff_5v7

summary(Fit_test)
parameterTable(Fit_test)


exp(0.031*1.8)
exp(0.049*1.8)
exp(0.027*1.8)
exp(-0.006*1.8)


# Calculate difference in OR and SE between 6v8
or_diff_5v7 <- (0.023*1.8) - (0.003*1.8)
se_diff_5v7 <- sqrt( ((0.006*1.8)^2) + ((0.008*1.8)^2) )
# Calculate the lower and upper 90% CI for 6v8 from the SE
lowci_5v7 <- or_diff_5v7 - 1.645*se_diff_5v7
upci_5v7 <- or_diff_5v7 + 1.645*se_diff_5v7

p_any["pvalue"]

Fit_test <- lavaan(lavaan_model, data=study_data, ordered=c("ACE_1","ACE_2", "ACE_3", "ACE_4", "ACE_5", "ACE_6"), estimator = "WLSMV")



### Write function for Zelig 

# Define function
aces_pgs <- function(ace, data) {
  
  ace <- a.out$imputations$
  
  # Run logistic regressions 
  adhd <- zelig(ace ~ scale(ADHD_PGS) + sex, model="logit", data=data, cite=FALSE)
  alcohol <- zelig(ace ~ scale(alcohol_PGS) + sex, model="logit", data=data, cite=FALSE)
  antisocial <- zelig(ace ~ scale(antisocial_PGS) + sex, model="logit", data=data, cite=FALSE)
  anxiety <- zelig(ace ~ scale(anxiety_PGS) + sex, model="logit", data=data, cite=FALSE)
  autism <- zelig(ace ~ scale(autism_PGS) + sex, model="logit", data=data, cite=FALSE)
  bipolar <- zelig(ace ~ scale(bipolar_PGS) + sex, model="logit", data=data, cite=FALSE)
  depression <- zelig(ace ~ scale(depression_PGS) + sex, model="logit", data=data, cite=FALSE)
  schizophrenia <- zelig(ace ~ scale(schizophrenia_PGS) + sex, model="logit", data=data, cite=FALSE)
  
  # Put odds ratios and CIs into vector
  results <- ( c(exp(combine_coef_se(adhd)[2,1]), combine_coef_se(adhd)[2,2], # ADHD OR and log SE
                 exp(combine_coef_se(alcohol)[2,1]), combine_coef_se(alcohol)[2,2], # Alcohol OR and log SE
                 exp(combine_coef_se(antisocial)[2,1]), combine_coef_se(antisocial)[2,2], # Antisocial OR and log SE
                 exp(combine_coef_se(anxiety)[2,1]), combine_coef_se(anxiety)[2,2], # Anxiety OR and log SE
                 exp(combine_coef_se(autism)[2,1]), combine_coef_se(autism)[2,2],  # Autism OR and log SE
                 exp(combine_coef_se(bipolar)[2,1]), combine_coef_se(bipolar)[2,2], # Bipolar OR and log SE
                 exp(combine_coef_se(depression)[2,1]), combine_coef_se(depression)[2,2], # Depression OR and log SE
                 exp(combine_coef_se(schizophrenia)[2,1]), combine_coef_se(schizophrenia)[2,2])) # Scizophrenia OR and log SE
  
  # Return results in a matrix
  return(matrix(results, nrow=1, ncol=16, dimnames=list(c(""), 
                                                        c("OR_adhd", "logSE_adhd", 
                                                          "OR_alcohol", "logSE_alcohol", 
                                                          "OR_antisocial", "logSE_antisocial", 
                                                          "OR_anxiety", "logSE_anxiety", 
                                                          "OR_autism", "logSE_autism", 
                                                          "OR_bipolar", "logSE_bipolar", 
                                                          "OR_depression", "logSE_depression", 
                                                          "OR_schizophrenia", "logSE_schizophrenia"))))
  
}
       
## Apply function
# Maltreatment
maltreatment <- aces_pgs(maltreatment_0_9.5yrs, a.out)
maltreatment <- aces_pgs(a.out$imputations$i$maltreatment_0_9.5yrs, a.out)
# Domestic violence
domestic_violence <- aces_pgs("violence_between_parents_0_9.5yrs", a.out)
# Parental mental illness
par_psych <- aces_pgs(complete_cases$mental_health_problems_or_suicide_0_9.5yrs, a.out)
# Parental substance abuse
par_substance <- aces_pgs(complete_cases$substance_household_0_9.5yrs, a.out)
# Parental criminality
par_criminal <- aces_pgs(complete_cases$parent_convicted_offence_0_9.5yrs, a.out)
# Parental separation
par_sep <- aces_pgs(complete_cases$parental_separation_0_9.5yrs, a.out)

               
               
    nobs(adhd), exp(coef(adhd)[2]), exp(confint(adhd))[2,1:2],
    nobs(antisocial), exp(coef(antisocial)[2]), exp(confint(antisocial))[2,1:2],
    nobs(alcohol), exp(coef(alcohol)[2]), exp(confint(alcohol))[2,1:2],
    nobs(anxiety), exp(coef(anxiety)[2]), exp(confint(anxiety))[2,1:2],
    nobs(autism), exp(coef(autism)[2]), exp(confint(autism))[2,1:2],
    nobs(bipolar), exp(coef(bipolar)[2]), exp(confint(bipolar))[2,1:2],
    nobs(depression), exp(coef(depression)[2]), exp(confint(depression))[2,1:2],
    nobs(schizophrenia), exp(coef(schizophrenia)[2]), exp(confint(schizophrenia))[2,1:2]))
  
  

## Regress PCs out of PGSs
# ADHD
alspac_merged$ADHD_PGS_r <- residuals(lm(ADHD_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                        alspac_merged, na.action=na.exclude))
# Alcohol
alspac_merged$alcohol_PGS_r <- residuals(lm(alcohol_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                            alspac_merged, na.action=na.exclude))
# Antisocial
alspac_merged$antisocial_PGS_r <- residuals(lm(antisocial_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                               alspac_merged, na.action=na.exclude))
# Anxiety
alspac_merged$anxiety_PGS_r <- residuals(lm(anxiety_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                            alspac_merged, na.action=na.exclude))
# Autism
alspac_merged$autism_PGS_r <- residuals(lm(autism_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                          alspac_merged, na.action=na.exclude))
# Bipolar
alspac_merged$bipolar_PGS_r <- residuals(lm(bipolar_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                              alspac_merged, na.action=na.exclude))
# Depression
alspac_merged$depression_PGS_r <- residuals(lm(depression_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                               alspac_merged, na.action=na.exclude))
# Schizoprehnia
alspac_merged$schizophrenia_PGS_r <- residuals(lm(schizophrenia_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                                  alspac_merged, na.action=na.exclude))

## Check whether ACEs are associated with polygenic scores 
# Define function
aces_pgs <- function(ace, data) {
  
  # Run logistic regressions 
  adhd <- glm(ace ~ scale(ADHD_PGS_r) + sex, family=binomial(link="logit"), data)
  alcohol <- glm(ace ~ scale(alcohol_PGS_r) + sex, family=binomial(link="logit"), data)
  antisocial <- glm(ace ~ scale(antisocial_PGS_r) + sex, family=binomial(link="logit"), data)
  anxiety <- glm(ace ~ scale(anxiety_PGS_r) + sex, family=binomial(link="logit"), data)
  autism <- glm(ace ~ scale(autism_PGS_r) + sex, family=binomial(link="logit"), data)
  bipolar <- glm(ace ~ scale(bipolar_PGS_r) + sex, family=binomial(link="logit"), data)
  depression <- glm(ace ~ scale(depression_PGS_r) + sex, family=binomial(link="logit"), data)
  schizophrenia <- glm(ace ~ scale(schizophrenia_PGS_r) + sex, family=binomial(link="logit"), data)
  
  # Put odds ratios and CIs into vector
  results <- ( c(
    nobs(adhd), exp(coef(adhd)[2]), exp(confint(adhd))[2,1:2],
    nobs(antisocial), exp(coef(antisocial)[2]), exp(confint(antisocial))[2,1:2],
    nobs(alcohol), exp(coef(alcohol)[2]), exp(confint(alcohol))[2,1:2],
    nobs(anxiety), exp(coef(anxiety)[2]), exp(confint(anxiety))[2,1:2],
    nobs(autism), exp(coef(autism)[2]), exp(confint(autism))[2,1:2],
    nobs(bipolar), exp(coef(bipolar)[2]), exp(confint(bipolar))[2,1:2],
    nobs(depression), exp(coef(depression)[2]), exp(confint(depression))[2,1:2],
    nobs(schizophrenia), exp(coef(schizophrenia)[2]), exp(confint(schizophrenia))[2,1:2]))
  
  # Return results in a matrix
  return(matrix(results, nrow=1, ncol=32, dimnames=list(c(""), 
                                                        c("n_adhd", "OR_adhd", "lowCI_adhd", "upCI_adhd",
                                                          "n_alcohol", "OR_alcohol", "lowCI_alcohol", "upCI_alcohol",
                                                          "n_antisocial", "OR_antisocial", "lowCI_antisocial", "upCI_antisocial",
                                                          "n_anxiety", "OR_anxiety", "lowCI_anxiety", "upCI_anxiety",
                                                          "n_autism", "OR_autism", "lowCI_autism", "upCI_autism", 
                                                          "n_bipolar", "OR_bipolar", "lowCI_bipolar", "upCI_bipolar",
                                                          "n_depression", "OR_depression", "lowCI_depression", "upCI_depression",
                                                          "n_schizophrenia", "OR_schizophrenia", "lowCI_schizophrenia", "upCI_schizophrenia"))))
  
}

# Maltreatment
maltreatment <- aces_pgs(alspac_merged$maltreatment_0_9.5yrs, alspac_merged)
# Domestic violence
domestic_violence <- aces_pgs(alspac_merged$violence_between_parents_0_9.5yrs, alspac_merged)
# Parental mental illness
par_psych <- aces_pgs(alspac_merged$mental_health_problems_or_suicide_0_9.5yrs, alspac_merged)
# Parental substance abuse
par_substance <- aces_pgs(alspac_merged$substance_household_0_9.5yrs, alspac_merged)
# Parental criminality
par_criminal <- aces_pgs(alspac_merged$parent_convicted_offence_0_9.5yrs, alspac_merged)
# Parental separation
par_sep <- aces_pgs(alspac_merged$parental_separation_0_9.5yrs, alspac_merged)

## Complete cases
# Maltreatment
maltreatment <- aces_pgs(complete_cases$maltreatment_0_9.5yrs, complete_cases)
# Domestic violence
domestic_violence <- aces_pgs(complete_cases$violence_between_parents_0_9.5yrs, complete_cases)
# Parental mental illness
par_psych <- aces_pgs(complete_cases$mental_health_problems_or_suicide_0_9.5yrs, complete_cases)
# Parental substance abuse
par_substance <- aces_pgs(complete_cases$substance_household_0_9.5yrs, complete_cases)
# Parental criminality
par_criminal <- aces_pgs(complete_cases$parent_convicted_offence_0_9.5yrs, complete_cases)
# Parental separation
par_sep <- aces_pgs(complete_cases$parental_separation_0_9.5yrs, complete_cases)

# Bind all results into one dataframe
results <- as.data.frame(do.call("rbind", list(maltreatment, domestic_violence, par_psych, par_substance, par_criminal, par_sep)))
# Add info on predictor
results$ace <- c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep")
# Make dataframe long
long_results <- reshape(results, varying=names(results)[1:32],
                        direction="long", timevar = "PGS",
                        times=c("adhd", 
                                "alcohol", 
                                "antisocial", "anxiety", "autism",
                                "bipolar", "depression", "schizophrenia"),
                        sep="_")
mean(long_results$n)
library(MAd)
long_results$se <- (log(long_results$upCI)-log(long_results$lowCI))/3.92
long_results$var <- long_results$se^2
long_results$id <- 1
aggregate <- agg(id, OR, var, method = "BHHR", mod=NULL, data=long_results)
se <- sqrt(aggregate$var)
aggregate$es - 1.96*se
aggregate$es + 1.96*se
# p value
z <- log(aggregate$es) / se
P = exp(-0.717*z - 0.416*z^2)



pnorm(z, lower.tail=FALSE)

mean(long_results$`_lowCI`)

library(data.table)
long <- melt(setDT(results), id.vars = c("Code","Country"), variable.name = "year")

physical abuse <- aces_pgs(alspac_merged$physical_abuse_0_9.5yrs, alspac_merged)
physical abuse <- aces_pgs(alspac_merged$physical_abuse_0_9.5yrs, alspac_merged)
physical abuse <- aces_pgs(alspac_merged$physical_abuse_0_9.5yrs, alspac_merged)


alcohol, antisocial, anxiety,
autism, bipolar, cataracts, 
depression, handedness, schizophrenia

adhd <- glm(physical_abuse_0_9.5yrs ~ scale(ADHD_PGS), family=binomial(link="logit"), data=alspac_merged)
exp(coef(adhd)[2]), exp(confint(adhd))[2,1:2],

#### Check whether ACEs are associated with DAWBA internalising outcomes at age 10
z.out <- zelig(scale(internalising10) ~ maltreatment_0_9.5yrs, model = "normal", 
               data = a.out)
summary(z.out)

summary(lm(scale(internalising10) ~ physical_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ sexual_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ emotional_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ emotional_neglect_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ violence_between_parents_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ substance_household_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ parent_convicted_offence_0_9.5yrs, data=alspac_merged))
summary(lm(scale(internalising10) ~ parental_separation_0_9.5yrs, data=alspac_merged))

#### Check whether ACEs are associated with DAWBA externalising outcomes at age 10
summary(lm(scale(externalising10) ~ physical_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ sexual_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ emotional_abuse_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ emotional_neglect_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ violence_between_parents_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ substance_household_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ parent_convicted_offence_0_9.5yrs, data=alspac_merged))
summary(lm(scale(externalising10) ~ parental_separation_0_9.5yrs, data=alspac_merged))


# Note there are issues:

sapply(alspac_imput, class)
sapply(alspac_imput, levels)
lapply(alspac_imput[sapply(alspac_imput, is.factor)], levels)
# CHeck prevalence of factor levels
sapply(apply(alspac_imput[sapply(alspac_imput, is.factor)], 2, table), function(x) x/sum(x))

aces_pgs <- function(ace, data) {
  
  # Run logistic regressions 
  adhd <- zelig(ace ~ scale(ADHD_PGS) + sex, model = "logit", data = data)
  alcohol <- zelig(ace ~ scale(alcohol_PGS) + sex,model = "logit", data)
  antisocial <- zelig(ace ~ scale(antisocial_PGS) + sex,  model = "logit", data)
  anxiety <- zelig(ace ~ scale(anxiety_PGS) + sex,  model = "logit", data)
  autism <- zelig(ace ~ scale(autism_PGS) + sex,  model = "logit", data)
  bipolar <- zelig(ace ~ scale(bipolar_PGS) + sex,  model = "logit", data)
  depression <- zelig(ace ~ scale(depression_PGS) + sex,  model = "logit", data)
  schizophrenia <- zelig(ace ~ scale(schizophrenia_PGS) + sex,  model = "logit", data)
  
  # Put odds ratios and CIs into vector
  # FOllow: https://rstudio-pubs-static.s3.amazonaws.com/445649_5f323f9cc6aa4333b404882e67e9c344.html to get summary
  results <- ( c(
    exp(coef(adhd)[2]), exp(confint(adhd))[2,1:2],
    exp(coef(antisocial)[2]), exp(confint(antisocial))[2,1:2],
    exp(coef(alcohol)[2]), exp(confint(alcohol))[2,1:2],
    exp(coef(anxiety)[2]), exp(confint(anxiety))[2,1:2],
    exp(coef(autism)[2]), exp(confint(autism))[2,1:2],
    exp(coef(bipolar)[2]), exp(confint(bipolar))[2,1:2],
    exp(coef(depression)[2]), exp(confint(depression))[2,1:2],
    exp(coef(schizophrenia)[2]), exp(confint(schizophrenia))[2,1:2]))
  
  # Return results in a matrix
  return(matrix(results, nrow=1, ncol=24, dimnames=list(c(""), 
                                                        c("OR_adhd", "lowCI_adhd", "upCI_adhd",
                                                          "OR_alcohol", "lowCI_alcohol", "upCI_alcohol",
                                                          "OR_antisocial", "lowCI_antisocial", "upCI_antisocial",
                                                          "OR_anxiety", "lowCI_anxiety", "upCI_anxiety",
                                                          "OR_autism", "lowCI_autism", "upCI_autism", 
                                                          "OR_bipolar", "lowCI_bipolar", "upCI_bipolar",
                                                          "OR_depression", "lowCI_depression", "upCI_depression",
                                                          "OR_schizophrenia", "lowCI_schizophrenia", "upCI_schizophrenia"))))
  
}


#50 warnings:
50: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.08646527606346,  ... :
                                                  invalid factor level, NA generated

Warning messages:
  1: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.00921936853666,  ... :
                                                   invalid factor level, NA generated
                                                 2: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.963517048217356,  ... :
                                                                                                  invalid factor level, NA generated
                                                                                                3: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.13752908042871,  ... :
                                                                                                                                                 invalid factor level, NA generated
                                                                                                                                               4: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.957861036781815,  ... :
                                                                                                                                                                                                invalid factor level, NA generated
                                                                                                                                                                                              5: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.983499254273976,  ... :
                                                                                                                                                                                                                                              invalid factor level, NA generated
                                                                                                                                                                                                                                             6: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.920441143254891,  ... :
                                                                                                                                                                                                                                                                                              invalid factor level, NA generated
                                                                                                                                                                                                                                                                                            7: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.08827196860626,  ... :
                                                                                                                                                                                                                                                                                                                                             invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                           8: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.06164737171356,  ... :
                                                                                                                                                                                                                                                                                                                                                                                            invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                          9: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.958578743761465,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                         10: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.802890695912681,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         11: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.21859796142111,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         12: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.932164340664886,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         13: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.14111404761637,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         14: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.05393004322583,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         15: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.753987676502334,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         16: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.999841309681855,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         17: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.859955533975242,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         18: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.875100793041764,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         19: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.02875665892672,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         20: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.18026527242398,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         21: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.864186926414693,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         22: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.876429152407959,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         23: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.964048440338502,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         24: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.958214716815174,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         25: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.817444503364215,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         26: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.19169197773707,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         27: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.86885876260096,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         28: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.17188502247036,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         29: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.05426102818913,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         30: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.02697011940466,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         31: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.02887263998076,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         32: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.9721042959878,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         33: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.05893845542307,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         34: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.854043806598356,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         35: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.964913042891478,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         36: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.10274409932485,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         37: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.00698050531799,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         38: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.918179763584072,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         39: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.07725647296045,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         40: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.888911735484058,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         41: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.945485088131493,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         42: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.778792469978199,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.0305052732211,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         44: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.968812547904615,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         45: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.16438426578923,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         46: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.05937671590812,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         47: In `[<-.factor`(`*tmp*`, thisvar, value = c(0.990629370386851,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         48: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.16992254314573,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         49: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.0062530276494,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         50: In `[<-.factor`(`*tmp*`, thisvar, value = c(1.03113219617634,  ... :
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           invalid factor level, NA generated


# 
  "sc_household_18wgest", "ethnicity", "home_own_preg", "mother_edu",
         "ptnr_edu", "ptnr_edu_p",
         "mother_homeless_preg", "mother_antidep_preg", 
         "diff_afford_food", "diff_afford_heat",
         "mat_opin_neighb", "ptnr_convict_preg", 
         "ptnr_sep_preg",  "mother_div_preg", "ptnr_drug_preg",
         "ptnr_cruel_child18", "antidep_mother18", 
         "sep_mother_ptnr18", "child_ptnr_violence21", "child_ptnr_sev_violence21",
         "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
         "child_ptnr_press_sex21", "child_ptnr_force_sex21",
         "child_ptnr_scared21")



## Early sociodemographic indicators

## Maternal smoking variables (following Lotte's script)
# Maternal smoking during the 1st trimester 
aux$matsmok_tri1 <- ifelse(data$b665=='N',0,1)
# Maternal smoking during the 2nd trimester 
aux$matsmok_tri2 <- ifelse(data$b667=='N',0,1)
# Maternal smoking during the 3rd trimester (prospectively reported)
aux$matsmok_tri3 <- ifelse(data$c482==0,0,1)

# Ensure there is only one variable per construct
#Sex	Dichotomous	Noms 
table(aux$kz021)
aux$sex <- aux$kz021

#Parental social class during pregnancy	Categorical (6)	Ords - average across parents
# follow: https://stackoverflow.com/questions/39120226/r-obtain-the-connection-between-the-numeric-values-and-the-level-labels-in-a-f 
# to check levels of a factor
factor_reference(aux$pb_sc_p)[[1]]
factor_reference(aux$b_sc_m)[[1]]
# recode to numeric, mother
aux$sc_household_18wgest <- NA
aux$sc_household_18wgest[aux$b_sc_m=="V - Unskileld" | aux$pb_sc_p=="V - Unskileld"] <- "V - Unskilled"
aux$sc_household_18wgest[aux$b_sc_m=="IV - Partly skilled" | aux$pb_sc_p=="IV - Partly skilled"] <- "IV - Partly skilled"
aux$sc_household_18wgest[aux$b_sc_m=="IIINM - Skilled non-manual" | aux$pb_sc_p=="IIINM - Skilled non-manual"] <- "IIINM - Skilled non-manual"
aux$sc_household_18wgest[aux$b_sc_m=="IIIM - Skilled manual" | aux$pb_sc_p=="IIIM - Skilled manual"] <- "IIIM - Skilled manual"
aux$sc_household_18wgest[aux$b_sc_m=="II - Mangerial and technical" | aux$pb_sc_p=="II - Mangerial and technical"] <- "II - Mangerial and technical"
aux$sc_household_18wgest[aux$b_sc_m=="I - Professional" | aux$pb_sc_p=="I - Professional"] <- "I - Professional"
table(aux$sc_household_18wgest, useNA="always")
table(aux$b_sc_m, aux$pb_sc_p, useNA="always")

#Child ethnicity	Dichotomous	Noms
table(aux$c804)
aux$ethnicity <- aux$c804

#Maternal age at birth	Continuous	Continuous
table(aux$mz028b)
class(aux$mz028b)
aux$mat_age_birth <- aux$mz028b

#Mother’s home ownership during pregnancy	Categorical (7)	Noms
table(aux$a006)
aux$home_own_preg <-aux$a006

#Parity	Continuous	Continuous
table(aux$b032)
class(aux$b032)
aux$parity <- aux$b032 

#Maternal marital status at birth	Categorical (6)	Noms - MISSING

#Mother’s highest educational level (mother-reported)	Categorical (5)	Ords
table(aux$c645)
aux$mother_edu <- aux$c645

#Partner’s highest educational level (mother-reported)	Categorical (5)	Ords
table(aux$c666)
aux$ptnr_edu <- aux$c666

# Mother’s highest educational level (partner-reported) -MISSING Categorical (5)	Ords
# Partner’s highest educational level (partner-reported)	Categorical (5)	Ords
table(aux$pb325)
aux$ptnr_edu_p <- aux$pb325

# Birthweight	Continuous	Continuous
describe(aux$kz030)
aux$birthweight <- aux$kz030

#Gestational age	Continuous	Continuous
describe(aux$bestgest)

#Maternal pre-pregnancy weight (kg)	Continuous	Continuous
describe(aux$dw002)
aux$mat_prepreg_weight <- aux$dw002

#Maternal pre-pregnancy BMI	Continuous	Continuous
describe(aux$dw042)
aux$mat_bmi <- aux$dw042

#Maternal smoking during the 1st trimester 	Dichotomous	Noms
table(aux$matsmok_tri1)
#Maternal smoking during the 2nd trimester 	Dichotomous	Noms
table(aux$matsmok_tri2)
#Maternal smoking during the 3rd trimester (prospectively reported)	Dichotomous	Noms
table(aux$matsmok_tri3_c)

#early_socdem <- c("kz021", "sc_household_18wgest", "c804", "mz028b", "b032", "a006", "c645", "c666", "pb325", 
                  "kz030", "bestgest", "dw002", 
                  "dw042", "matsmok_tri1", "matsmok_tri2", "matsmok_tri3_c")

early_socdem <- c("sex", "sc_household_18wgest", "ethnicity", "mat_age_birth", "parity", "home_own_preg",
                  "mother_edu", "ptnr_edu", "ptnr_edu_p", 
                  "birthweight", "bestgest", "mat_prepreg_weight", 
                  "mat_bmi", "matsmok_tri1", "matsmok_tri2", "matsmok_tri3_c")


length(early_socdem)

### Adversity exposure before birth
#Mother became homeless	Categorical (2)	Noms
aux$mother_homeless_preg <- NA
aux$mother_homeless_preg[aux$c472=="N" | aux$b593=="didnt happen"] <- "No"
aux$mother_homeless_preg[aux$c472=="Y" | aux$b593=="affected a lot" |
                           aux$b593=="fairly affected" | aux$b593=="mildly affected" |
                           aux$b593=="N effect at all"] <- "Yes"
table(aux$mother_homeless_preg, useNA="always")
table(aux$c472, useNA="always")
table(aux$b593, useNA="always")

#Maternal depression score (EPDS) at 18 wks gestation	Continuous	Continuous
describe(aux$b370)
aux$mat_epds18w <- aux$b370

#Partner EPDS score at 18 wks gestation	Continuous	Continuous
describe(aux$pb260)
aux$ptnr_epds18w <- aux$pb260

#Maternal EPDS score at 32 wks gestation	Continuous	Continuous
describe(aux$c600)
aux$mat_epds32w <- aux$c600

#Antidepressant use by mother	Dichotomous	Noms
aux$mother_antidep_preg <- NA
aux$mother_antidep_preg[aux$f063=="Not at all"] <- "No"
aux$mother_antidep_preg[aux$f063=="Daily" | aux$f063=="Often" |
                           aux$f063=="SMTS"] <- "Yes"
table(aux$mother_antidep_preg, useNA="always")
table(aux$f063)

#Difficulty affording food during pregnancy	Categorical (4)	Ords
table(aux$c520)
aux$diff_afford_food <- aux$c520

#Difficulty affording heating during pregnancy	Categorical (4)	Ords
table(aux$c522)
aux$diff_afford_heat <- aux$c522

#Mother’s opinion of neighbourhood during pregnancy	Categorical (4)	Ords
table(aux$a600)
aux$mat_opin_neighb <- aux$a600

#Partner convicted of an offence during pregnancy	Categorical (2)	Noms
table(aux$e428)
aux$ptnr_convict_preg <- NA
aux$ptnr_convict_preg[aux$e428=="Did not happen"] <- "No"
aux$ptnr_convict_preg[aux$e428=="Affected a lot" | aux$e428=="MOD affected" |
                          aux$e428=="Mildly affected" | aux$e428=="No effect"] <- "Yes"
table(aux$ptnr_convict_preg, useNA="always") 
table(aux$e428, useNA="always")

#Partner separated since pregnancy	Dichotomous	Noms
table(aux$pb177)
aux$ptnr_sep_preg <- NA
aux$ptnr_sep_preg[aux$pb177=="didnt happen"] <- "No"
aux$ptnr_sep_preg[aux$pb177=="affected a lot" | aux$pb177=="fairly affected" |
                        aux$pb177=="mildly affected" | aux$pb177=="N affect at all"] <- "Yes"
table(aux$ptnr_sep_preg, useNA="always") 
table(aux$pb177, useNA="always")

#Mother divorced since pregnancy	Dichotomous	Noms
table(aux$e408)
aux$mother_div_preg <- NA
aux$mother_div_preg[aux$e408=="Did not happen"] <- "No"
aux$mother_div_preg[aux$e408=="Affected a lot" | aux$e408=="MOD affected" |
                      aux$e408=="Mildly affected" | aux$e408=="No effect"] <- "Yes"
table(aux$mother_div_preg, useNA="always") 
table(aux$e408, useNA="always")

#Partner hard drug use during pregnancy	Categorical (2)	Noms
table(aux$pb098)
aux$ptnr_drug_preg <- aux$pb098

#adv_before_birth <- c("mother_homeless_preg", "b370", "pb260", "c600", "mother_antidep_preg",
                      #"c520", "c522", "a600", "ptnr_convict_preg", "ptnr_sep_preg", "mother_div_preg", "pb098")
adv_before_birth <- c("mother_homeless_preg", "mat_epds18w", "ptnr_epds18w", "mat_epds32w", "mother_antidep_preg",
                      "diff_afford_food", "diff_afford_heat", "mat_opin_neighb", "ptnr_convict_preg", "ptnr_sep_preg", 
                      "mother_div_preg", "ptnr_drug_preg")

length(adv_before_birth)

### Adversity exposure in young adulthood (18-21 years)	
#Mother’s partner was emotionally cruel to child	Dichotomous	Noms (recode to dichotomous)
table(aux$t3336)
aux$ptnr_cruel_child18 <- NA
aux$ptnr_cruel_child18[aux$t3336=="Yes & affected respondent a lot" | 
                        aux$t3336=="Yes, respondent moderately affected" |
                         aux$t3336=="Yes, respondent mildly affected" |
                         aux$t3336=="Yes, but did not affect respondent at all"] <- "Yes"
aux$ptnr_cruel_child18[aux$t3336=="No, did not happen"] <- "No"
table(aux$ptnr_cruel_child18, useNA="always")
table(aux$t3336, useNA="always")

#Antidepressant use by mother when child was 18yrs	Dichotomous	Noms (recode to dichotomous)
table(aux$t5404)
aux$antidep_mother18 <- NA
aux$antidep_mother18[aux$t5404=="Every day" | 
                         aux$t5404=="Often" |
                         aux$t5404=="Sometimes" ] <- "Yes"
aux$antidep_mother18[aux$t5404=="Not at all"] <- "No"
table(aux$antidep_mother18, useNA="always")
table(aux$t5404, useNA="always")

#Maternal EPDS score when child was 18yrs	Continuous	Continuous
describe(aux$t3255)
aux$mat_epds18y <- aux$t3255

#Mother separated from partner when child was 18yrs	Dichotomous	Noms (recode to dichotomous)
table(aux$t3316)
aux$sep_mother_ptnr18 <- NA
aux$sep_mother_ptnr18[aux$t3316=="Yes & affected respondent a lot" | 
                       aux$t3316=="Yes, respondent moderately affected" |
                       aux$t3316=="Yes, respondent mildly affected" |
                       aux$t3316=="Yes, but did not affect respondent at all" ] <- "Yes"
aux$sep_mother_ptnr18[aux$t3316=="No, did not happen"] <- "No"
table(aux$sep_mother_ptnr18, useNA="always")
table(aux$t3316, useNA="always")

#Maternal AUDIT score when child was 18yrs	Continuous	Continuous
describe(aux$t5510)
aux$mat_audit_18y <- aux$t5510

#Paternal AUDIT score when child was 18yrs	Continuous	Continuous
describe(aux$fa5510)
aux$pat_audit_18y <- aux$fa5510

#Participant’s partner used physical force 	Dichotomous	Noms
aux$child_ptnr_violence21 <- NA
aux$child_ptnr_violence21[aux$ypa5004=="Often" | 
                            aux$ypa5004=="A few times" |
                            aux$ypa5004=="Once" ] <- "Yes"
aux$child_ptnr_violence21[aux$ypa5004=="Never"] <- "No"
table(aux$child_ptnr_violence21, useNA="always")

#Participant’s partner used more severe physical force 	Dichotomous	Noms
aux$child_ptnr_sev_violence21 <- NA
aux$child_ptnr_sev_violence21[aux$ypa5006=="Often" | 
                            aux$ypa5006=="A few times" |
                            aux$ypa5006=="Once"] <- "Yes"
aux$child_ptnr_sev_violence21[aux$ypa5006=="Never"] <- "No"
table(aux$child_ptnr_sev_violence21, useNA="always")
table(aux$ypa5006, useNA="always")

#Participant’s partner pressured them into kissing/touching 	Dichotomous	Noms
table(aux$ypa5008)
aux$child_ptnr_press_kiss21 <- NA
aux$child_ptnr_press_kiss21[aux$ypa5008=="Often" | 
                                aux$ypa5008=="A few times" |
                                aux$ypa5008=="Once"] <- "Yes"
aux$child_ptnr_press_kiss21[aux$ypa5008=="Never"] <- "No"
table(aux$child_ptnr_press_kiss21, useNA="always")
table(aux$ypa5008, useNA="always")

#Participant’s partner physically forced them into kissing/touching	Dichotomous	Noms
table(aux$ypa5010)
aux$child_ptnr_force_kiss21 <- NA
aux$child_ptnr_force_kiss21[aux$ypa5010=="Often" | 
                              aux$ypa5010=="A few times" |
                              aux$ypa5010=="Once"] <- "Yes"
aux$child_ptnr_force_kiss21[aux$ypa5010=="Never"] <- "No"
table(aux$child_ptnr_force_kiss21, useNA="always")
table(aux$ypa5010, useNA="always")

#Participant’s partner pressured them into sexual intercourse 	Dichotomous	Noms
table(aux$ypa5012)
aux$child_ptnr_press_sex21 <- NA
aux$child_ptnr_press_sex21[aux$ypa5012=="Often" | 
                              aux$ypa5012=="A few times" |
                              aux$ypa5012=="Once"] <- "Yes"
aux$child_ptnr_press_sex21[aux$ypa5012=="Never"] <- "No"
table(aux$child_ptnr_press_sex21, useNA="always")
table(aux$ypa5012, useNA="always")

#Participant’s partner physically forced them into sexual intercourse	Dichotomous	Noms
table(aux$ypa5014)
aux$child_ptnr_force_sex21 <- NA
aux$child_ptnr_force_sex21[aux$ypa5014=="Often" | 
                           aux$ypa5014=="A few times" |
                           aux$ypa5014=="Once"] <- "Yes"
aux$child_ptnr_force_sex21[aux$ypa5014=="Never"] <- "No"
table(aux$child_ptnr_force_sex21, useNA="always")
table(aux$ypa5014, useNA="always")

#Participant’s partner made them feel scared of frightened 	Dichotomous	Noms
table(aux$ypa5016)
aux$child_ptnr_scared21 <- NA
aux$child_ptnr_scared21[aux$ypa5016=="Often" | 
                           aux$ypa5016=="A few times" |
                           aux$ypa5016=="Once"] <- "Yes"
aux$child_ptnr_scared21[aux$ypa5016=="Never"] <- "No"
table(aux$child_ptnr_scared21, useNA="always")
table(aux$ypa5016, useNA="always")

#adv_adulthood <- c("ptnr_cruel_child18", "antidep_mother18", "t3255", "sep_mother_ptnr18", "t5510",
                   "fa5510","child_ptnr_violence21", "child_ptnr_sev_violence21",
                   "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
                   "child_ptnr_press_sex21", "child_ptnr_force_sex21",
                   "child_ptnr_scared21")

adv_adulthood <- c("ptnr_cruel_child18", "antidep_mother18", "mat_epds18y", "sep_mother_ptnr18", "mat_audit_18y",
                   "pat_audit_18y","child_ptnr_violence21", "child_ptnr_sev_violence21",
                   "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
                   "child_ptnr_press_sex21", "child_ptnr_force_sex21",
                   "child_ptnr_scared21")



length(adv_adulthood)

### Add DAWBA variables to auxiliary dataset
aux$internalising7 <- data$internalising7
aux$internalising10 <- data$internalising10
aux$internalising13 <- data$internalising13
aux$externalising7 <- data$externalising7
aux$externalising10 <- data$externalising10
aux$externalising13 <- data$externalising13

### Select variables needed for multiple imputation
imputation_subset <- subset(aux, select=c("cidb3219", "qlet", "sex", "sc_household_18wgest", "ethnicity", 
                                          "mat_age_birth", "parity", "home_own_preg",
                                          "mother_edu", "ptnr_edu", "ptnr_edu_p", 
                                          "birthweight", "bestgest", "mat_prepreg_weight",
                                          "mat_bmi", "matsmok_tri1", "matsmok_tri2", "matsmok_tri3",
                                          "mother_homeless_preg", "mat_epds18w", "ptnr_epds18w", 
                                          "mat_epds32w", "mother_antidep_preg",
                                          "diff_afford_food", "diff_afford_heat", "mat_opin_neighb", 
                                          "ptnr_convict_preg", "ptnr_sep_preg", 
                                          "mother_div_preg", "ptnr_drug_preg",
                                          "ptnr_cruel_child18", "antidep_mother18", "mat_epds18y", 
                                          "sep_mother_ptnr18", "mat_audit_18y",
                                          "pat_audit_18y","child_ptnr_violence21", "child_ptnr_sev_violence21",
                                          "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
                                          "child_ptnr_press_sex21", "child_ptnr_force_sex21",
                                          "child_ptnr_scared21", 
                                          "internalising7", "internalising10", "internalising13",
                                          "externalising7", "externalising10", "externalising13"))




#imputation_subset <- subset(aux, select=c("kz021", "sc_household_18wgest", "c804", "mz028b",
                                           "b032", "a006", "c645", "c666", "pb325", 
                                           "kz030", "bestgest", "dw002", 
                                           "dw042", "matsmok_tri1", "matsmok_tri2", "matsmok_tri3_c",
                                           "mother_homeless_preg", "b370", "pb260", "c600", 
                                           "mother_antidep_preg", "c520", "c522", "a600", 
                                           "ptnr_convict_preg", "ptnr_sep_preg", "mother_div_preg", "pb098",
                                           "ptnr_cruel_child18", "antidep_mother18", "t3255", 
                                           "sep_mother_ptnr18", "t5510",
                                           "fa5510","child_ptnr_violence21", "child_ptnr_sev_violence21",
                                           "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
                                           "child_ptnr_press_sex21", "child_ptnr_force_sex21",
                                           "child_ptnr_scared21",
                                           "internalising7", "internalising10", "internalising13",
                                           "externalising7", "externalising10", "externalising13"))

## Examine missingness in variables (for the sake of imputation, impute for DAWBA at age 10 outcomes)
library(mice)
md.pattern(imputation_subset, plot = FALSE)
p_missing <- unlist(lapply(imputation_subset, function(x) sum(is.na(x))))/nrow(imputation_subset)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sort(p_missing[p_missing > 0.25], decreasing = TRUE)

## Should you include predictors of missingness that have a lot of missing data themselves? Yes it's ok

# DAWBA internalising at age 10 and externalising at age 10 are missing 50% of data
# Note: in reality, will only impute for those with genetic data and >10% of ACEs
# Will also impute ACEs (between 0-9 years) and not only the DAWBA, as here

noms = c("sex", "sc_household_18wgest", "ethnicity", "home_own_preg", "mother_edu",
         "ptnr_edu", "ptnr_edu_p",
         "mother_homeless_preg", "mother_antidep_preg", 
         "diff_afford_food", "diff_afford_heat",
         "mat_opin_neighb", "ptnr_convict_preg", 
         "ptnr_sep_preg",  "mother_div_preg", "ptnr_drug_preg",
         "ptnr_cruel_child18", "antidep_mother18", 
         "sep_mother_ptnr18", "child_ptnr_violence21", "child_ptnr_sev_violence21",
         "child_ptnr_press_kiss21", "child_ptnr_force_kiss21",
         "child_ptnr_press_sex21", "child_ptnr_force_sex21",
         "child_ptnr_scared21")
idvars = c('qlet', 'cidb3219', "matsmok_tri2", "matsmok_tri3", "matsmok_tri1")
# Trial to impute 50 datasets (started at 13:52, finished at 14:11, including 49 variables)
a.out = amelia(imputation_subset, noms = noms, idvars = idvars, incheck = FALSE, emburn=c(5,30),
               m=50)
class(a.out)

# Access imputed outputs
dim(a.out$imputations[[1]]) # the no. of each imputed dataset goes in the square brackets
# View missing data in imputed dataset (0%)
p_missing <- unlist(lapply(a.out$imputations[[1]], function(x) sum(is.na(x))))/nrow(a.out$imputations[[1]])
sort(p_missing[p_missing > 0], decreasing = TRUE)
# Run regression on imputed datasets
z.out1 <- zelig(internalising10 ~ sex, model = "normal", data = a.out,  cite = FALSE)
summary(z.out1)
# Compare to regression on complete dataset (results are broadly consistent)
complete <- lm(internalising10 ~ sex, data = imputation_subset)
summary(complete)


# Work out sample of complete cases (4753, that's with all maltreatment vars; would need 50%)
# Make maltreatment var
alspac_merged$maltreatment_0_9.5yrs <- "FALSE"
alspac_merged$maltreatment_0_9.5yrs[alspac_merged$physical_abuse_0_9.5yrs == "TRUE" | 
                                      alspac_merged$sexual_abuse_0_9.5yrs == "TRUE" |
                                      alspac_merged$emotional_abuse_0_9.5yrs == "TRUE" |
                                      alspac_merged$emotional_neglect_0_9.5yrs == "TRUE"] <- "TRUE"
alspac_merged$maltreatment_0_9.5yrs[is.na(alspac_merged$physical_abuse_0_9.5yrs) &
                                      is.na(alspac_merged$sexual_abuse_0_9.5yrs) &
                                      is.na(alspac_merged$emotional_abuse_0_9.5yrs) &
                                      is.na(alspac_merged$emotional_neglect_0_9.5yrs)] <- NA
table(alspac_merged$maltreatment_0_9.5yrs)
alspac_merged$maltreatment_0_9.5yrs <- as.factor(alspac_merged$maltreatment_0_9.5yrs)
prop.table(table(alspac_merged$maltreatment_0_9.5yrs))

complete_cases <- subset(alspac_merged, !is.na(maltreatment_0_9.5yrs) & !is.na(violence_between_parents_0_9.5yrs) &
                           !is.na(substance_household_0_9.5yrs) & !is.na(mental_health_problems_or_suicide_0_9.5yrs) &
                           !is.na(parent_convicted_offence_0_9.5yrs) & !is.na(parental_separation_0_9.5yrs) &
                           !is.na(internalising10) & !is.na(externalising10) & !is.na(ADHD_PGS))
dim(complete_cases)
# 4106


############## CORRELATION MATRIX FOR AGGREGATE MODLE
# Derive correlation matrix
cor_matrix <- matrix(NA, nrow = 48, ncol = 48)
rownames(cor_matrix) <- paste0(long_results$PGS, "_", long_results$ace)
colnames(cor_matrix) <- paste0(long_results$PGS, "_", long_results$ace)
options(max.print=999999)
cor_matrix

# FIll the correlation matrix
# Same PGS, different ACEs
cor_matrix[1:6, 1:6] <- ace_cor # ADHD PGS, different ACEs
cor_matrix[7:12, 7:12] <- ace_cor # Alcohol PGS, different ACEs
cor_matrix[13:18, 13:18] <- ace_cor # Antisocial PGS, different ACEs
cor_matrix[19:24, 19:24] <- ace_cor # Anxiety PGS, different ACEs
cor_matrix[25:30, 25:30] <- ace_cor # Autism PGS, different ACEs
cor_matrix[31:36, 31:36] <- ace_cor # Bipolar PGS, different ACEs
cor_matrix[37:42, 37:42] <- ace_cor # Depression PGS, different ACEs
cor_matrix[43:48, 43:48] <- ace_cor # Schizophrenia PGS, different ACEs
# Same ACE, different PGS
## Start at ADHD PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 1] <- pgs_cor[,1] # Maltreatment, correlation between ADHD PGS with different PGSs 
cor_matrix[1, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,1] # Maltreatment, transpose ADHD PGS
# could you do a loop so it repeats every 6 items?
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 2] <- pgs_cor[,1] # Domestic violence, correlation between ADHD PGS with different PGSs 
cor_matrix[2, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,1] # Domestic violence, transpose ADHD PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 3] <- pgs_cor[,1] # Parental psychopathology, correlation between ADHD PGS with different PGSs 
cor_matrix[3, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,1] # Parental psychopathology, transpose ADHD PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 4] <- pgs_cor[,1] # Parental substance abuse, correlation between ADHD PGS with different PGSs 
cor_matrix[4, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,1] # Parental substance abuse, transpose ADHD PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 5] <- pgs_cor[,1] # Parental criminality, correlation between ADHD PGS with different PGSs 
cor_matrix[5, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,1] # Parental criminality, transpose ADHD PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 6] <- pgs_cor[,1] # Parental separation, correlation between ADHD PGS with different PGSs 
cor_matrix[6, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,1] # Parental separation, transpose ADHD PGS
## Start at Alcohol PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 7] <- pgs_cor[,2] # Maltreatment, correlation between alcohol PGS with different PGSs 
cor_matrix[7, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,2] # Maltreatment, transpose alcohol PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 8] <- pgs_cor[,2] # Domestic violence, correlation between alcohol PGS with different PGSs 
cor_matrix[8, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,2] # Domestic violence, transpose alcohol PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 9] <- pgs_cor[,2] # Parental psychopathology, correlation between alcohol PGS with different PGSs 
cor_matrix[9, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,2] # Parental psychopathology, transpose alcohol PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 10] <- pgs_cor[,2] # Parental substance abuse, correlation between alcohol PGS with different PGSs 
cor_matrix[10, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,2] # Parental substance abuse, transpose alcohol PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 11] <- pgs_cor[,2] # Parental criminality, correlation between alcohol PGS with different PGSs 
cor_matrix[11, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,2] # Parental criminality, transpose alcohol PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 12] <- pgs_cor[,2] # Parental separation, correlation between alcohol PGS with different PGSs 
cor_matrix[12, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,2] # Parental separation, transpose alcohol PGS
## Start at Antisocial PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 13] <- pgs_cor[,3] # Maltreatment, correlation between antisocial PGS with different PGSs 
cor_matrix[7, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,3] # Maltreatment, transpose antisocial PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 14] <- pgs_cor[,3] # Domestic violence, correlation between antisocial PGS with different PGSs 
cor_matrix[14, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,3] # Domestic violence, transpose antisocial PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 15] <- pgs_cor[,3] # Parental psychopathology, correlation between antisocial PGS with different PGSs 
cor_matrix[15, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,3] # Parental psychopathology, transpose antisocial PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 16] <- pgs_cor[,3] # Parental substance abuse, correlation between antisocial PGS with different PGSs 
cor_matrix[16, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,3] # Parental substance abuse, transpose antisocial PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 17] <- pgs_cor[,3] # Parental criminality, correlation between antisocial PGS with different PGSs 
cor_matrix[17, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,3] # Parental criminality, transpose antisocial PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 18] <- pgs_cor[,3] # Parental separation, correlation between antisocial PGS with different PGSs 
cor_matrix[18, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,3] # Parental separation, transpose antisocial PGS
## Start at Anxiety PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 19] <- pgs_cor[,4] # Maltreatment, correlation between anxiety PGS with different PGSs 
cor_matrix[19, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,4] # Maltreatment, transpose anxiety PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 20] <- pgs_cor[,4] # Domestic violence, correlation between anxiety PGS with different PGSs 
cor_matrix[20, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,4] # Domestic violence, transpose anxiety PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 21] <- pgs_cor[,4] # Parental psychopathology, correlation between anxiety PGS with different PGSs 
cor_matrix[21, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,4] # Parental psychopathology, transpose anxiety PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 22] <- pgs_cor[,4] # Parental substance abuse, correlation between anxiety PGS with different PGSs 
cor_matrix[22, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,4] # Parental substance abuse, transpose anxiety PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 23] <- pgs_cor[,4] # Parental criminality, correlation between anxiety PGS with different PGSs 
cor_matrix[23, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,4] # Parental criminality, transpose anxiety PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 24] <- pgs_cor[,4] # Parental separation, correlation between anxiety PGS with different PGSs 
cor_matrix[24, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,4] # Parental separation, transpose anxiety PGS
## Start at Autism PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 25] <- pgs_cor[,5] # Maltreatment, correlation between autism PGS with different PGSs 
cor_matrix[25, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,5] # Maltreatment, transpose autism PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 26] <- pgs_cor[,5] # Domestic violence, correlation between autism PGS with different PGSs 
cor_matrix[26, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,5] # Domestic violence, transpose autism PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 27] <- pgs_cor[,5] # Parental psychopathology, correlation between autism PGS with different PGSs 
cor_matrix[27, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,5] # Parental psychopathology, transpose autism PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 28] <- pgs_cor[,5] # Parental substance abuse, correlation between autism PGS with different PGSs 
cor_matrix[28, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,5] # Parental substance abuse, transpose autism PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 29] <- pgs_cor[,5] # Parental criminality, correlation between autism PGS with different PGSs 
cor_matrix[29, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,5] # Parental criminality, transpose autism PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 30] <- pgs_cor[,5] # Parental separation, correlation between autism PGS with different PGSs 
cor_matrix[30, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,5] # Parental separation, transpose autism PGS
## Start at bipolar PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 31] <- pgs_cor[,6] # Maltreatment, correlation between bipolar PGS with different PGSs 
cor_matrix[31, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,6] # Maltreatment, transpose bipolar PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 32] <- pgs_cor[,6] # Domestic violence, correlation between bipolar PGS with different PGSs 
cor_matrix[32, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,6] # Domestic violence, transpose bipolar PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 33] <- pgs_cor[,6] # Parental psychopathology, correlation between bipolar PGS with different PGSs 
cor_matrix[33, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,6] # Parental psychopathology, transpose bipolar PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 34] <- pgs_cor[,6] # Parental substance abuse, correlation between bipolar PGS with different PGSs 
cor_matrix[34, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,6] # Parental substance abuse, transpose bipolar PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 35] <- pgs_cor[,6] # Parental criminality, correlation between bipolar PGS with different PGSs 
cor_matrix[35, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,6] # Parental criminality, transpose bipolar PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 36] <- pgs_cor[,6] # Parental separation, correlation between bipolar PGS with different PGSs 
cor_matrix[36, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,6] # Parental separation, transpose bipolar PGS
## Start at depression PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 37] <- pgs_cor[,7] # Maltreatment, correlation between depression PGS with different PGSs 
cor_matrix[37, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,7] # Maltreatment, transpose depression PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 38] <- pgs_cor[,7] # Domestic violence, correlation between depression PGS with different PGSs 
cor_matrix[38, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,7] # Domestic violence, transpose depression PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 39] <- pgs_cor[,7] # Parental psychopathology, correlation between depression PGS with different PGSs 
cor_matrix[39, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,7] # Parental psychopathology, transpose depression PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 40] <- pgs_cor[,7] # Parental substance abuse, correlation between depression PGS with different PGSs 
cor_matrix[40, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,7] # Parental substance abuse, transpose depression PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 41] <- pgs_cor[,7] # Parental criminality, correlation between depression PGS with different PGSs 
cor_matrix[41, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,7] # Parental criminality, transpose depression PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 42] <- pgs_cor[,7] # Parental separation, correlation between depression PGS with different PGSs 
cor_matrix[42, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,7] # Parental separation, transpose depression PGS
## Start at schizophrenia PGS
cor_matrix[c(1, 7, 13, 19, 25, 31, 37, 43), 43] <- pgs_cor[,8] # Maltreatment, correlation between schizophrenia PGS with different PGSs 
cor_matrix[43, c(1, 7, 13, 19, 25, 31, 37, 43)] <- pgs_cor[,8] # Maltreatment, transpose schizophrenia PGS
cor_matrix[c(2, 8, 14, 20, 26, 32, 38, 44), 44] <- pgs_cor[,8] # Domestic violence, correlation between schizophrenia PGS with different PGSs 
cor_matrix[44, c(2, 8, 14, 20, 26, 32, 38, 44)] <- pgs_cor[,8] # Domestic violence, transpose schizophrenia PGS
cor_matrix[c(3, 9, 15, 21, 27, 33, 39, 45), 45] <- pgs_cor[,8] # Parental psychopathology, correlation between schizophrenia PGS with different PGSs 
cor_matrix[45, c(3, 9, 15, 21, 27, 33, 39, 45)] <- pgs_cor[,8] # Parental psychopathology, transpose schizophrenia PGS
cor_matrix[c(4, 10, 16, 22, 28, 34, 40, 46), 46] <- pgs_cor[,8] # Parental substance abuse, correlation between schizophrenia PGS with different PGSs 
cor_matrix[46, c(4, 10, 16, 22, 28, 34, 40, 46)] <- pgs_cor[,8] # Parental substance abuse, transpose schizophrenia PGS
cor_matrix[c(5, 11, 17, 23, 29, 35, 41, 47), 47] <- pgs_cor[,8] # Parental criminality, correlation between schizophrenia PGS with different PGSs 
cor_matrix[47, c(5, 11, 17, 23, 29, 35, 41, 47)] <- pgs_cor[,8] # Parental criminality, transpose schizophrenia PGS
cor_matrix[c(6, 12, 18, 24, 30, 36, 42, 48), 48] <- pgs_cor[,8] # Parental separation, correlation between schizophrenia PGS with different PGSs 
cor_matrix[48, c(6, 12, 18, 24, 30, 36, 42, 48)] <- pgs_cor[,8] # Parental separation, transpose schizophrenia PGS

# Different ACE, different PGS (here replace with mean ACE correlation)
cor_matrix[which(is.na(cor_matrix))] <- mean_ace_cor

# Get the mean ACE correlation excluding the diagonal
cor_nodiag <- cor_matrix
diag(cor_nodiag) <- NA
mean_cor <- mean(cor_nodiag, na.rm=TRUE)

mean(cor_matrix)
utils::View(cor_matrix)

## ================== Examine correlations for aggregate model ====================================
cor()
cor_matrix <- alspac_imput[ ,c(#"maltreatment_0_9.5yrs", 
  "violence_between_parents_0_9.5yrs", 
  "mental_health_problems_or_suicide_0_9.5yrs",
  "substance_household_0_9.5yrs",
  "parent_convicted_offence_0_9.5yrs", 
  "parental_separation_0_9.5yrs")]
class(cor_matrix)
tetrachoric(cor_matrix, na.rm=TRUE)
library(polycor)
res_cor <- hetcor(cor_matrix)$correlations
diag(res_cor) <- NA
mean(res_cor, na.rm=TRUE)

## Note: back transformation is accurate if no proportions at zero or close to zero are included (e.g., 0.05 was fine)
## Note: one proportion for parental criminality is negative. Report 2 results: one when excluded and one when set to 0?
dat <- escalc(measure="PFT", yi=est, sei=se, data=int)
agg_2a <- agg(id=id, es=yi, var=vi, method = "BHHR", cor = mean_ace_cor, mod=NULL, data=dat) 

### Transform using Freeman-Tukey
#Prior to meta-analysis, proportions will be transformed using the Freeman-Tukey double arcsine transformation79 to normalise and 
# stabilise the variance of the sampling distribution. 
# After meta-analysis, estimates will be back transformed using the inverse of the Freeman-Tukey double arcsine transformation80.
## transf.pft Freeman-Tukey (double arcsine) transformation for proportions. See Freeman & Tukey (1950). The xi argument is used to specify the proportions and the ni argument the corresponding sample sizes.
library(metafor)
int$ft_prop <- transf.pft(xi=int$est, ni=int$ni) # transform estimate
mean(int$ft_prop)
# check if backtransforming gets same values as original proportions (yes)
transf.ipft(xi=int$ft_prop, ni=6411)
int$est

# Warning messages FOR NEGATIVE PROPORTION (PAR CRIM)
#1: In sqrt(xi/(ni + 1)) : NaNs produced
#2: In sqrt((xi + 1)/(ni + 1)) : NaNs produced
int$ft_var <- transf.pft(xi=int$var, ni=int$ni) #transform variance
#transf.ipft(xi=0.2256608, ni=7000)
#transf.ipft(xi=0.02164982, ni=7000)
#mean(df$var)
library(MAd)
int$id <- 1
#### Aggregate effect sizes (specify cor=0.3 due to correlations between ACEs)
agg_2a <- agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = mean_ace_cor, mod=NULL, data=int) 
# Using meta-analysis results in the same as aggregate (problem is not aggregate model and back transforming)
# Another study has said that when values are 0%, the SE is zero and CIs can't be computed. these studies are sometimes excluded
# https://archpublichealth.biomedcentral.com/articles/10.1186/2049-3258-72-39
# it says: a continuity correction is applied if any studies has a zero cell count. By default, 0.5 is added to all cell frequencies of studies with a zero cell count (argument incr).
prop <- rma(yi=ft_prop, vi=ft_var, data=int)
pred <- predict(prop, transf=transf.ipft.hm, targs=list(ni=int$ni))

agg(id=id, es=int$est, var=int$var, method = "BHHR", cor = mean_ace_cor, mod=NULL, data=int) 
mean(c(0.03903113599,  0.06421417304,  0.08055710621,  0.02801081943, 0,  0.05958131290))
## Get p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
z_model <- est_model/se_model 
p_model <- exp(-0.717*z_model - 0.416*(z_model^2))

## Back-transform proportions for checking
av_prop <- transf.ipft(xi=est_model, ni=6411)
av_var <- transf.ipft(xi=var_model, ni=6411)

pred <- predict(agg2a, transf=transf.ipft.hm, targs=list(ni=int$ni))


## Join ext and int dataframes
int_ext_comb <- rbind(int, ext)

# Transform with double arscine method
int_ext_comb$ft_prop <- transf.pft(xi=int_ext_comb$est, ni=int_ext_comb$ni) # transform estimate
# check if backtransforming gets same values as original proportions (yes)
transf.ipft(xi=int_ext_comb$ft_prop, ni=6411)
int_ext_comb$ft_var <- transf.pft(xi=int_ext_comb$var, ni=int_ext_comb$ni) #transform variance

#### Aggregate effect sizes (specify cor=0.3 due to correlations between ACEs)
int_ext_comb$id <- 1
agg_2a <- agg(id=id, es=ft_prop, var=ft_var, method = "BHHR", cor = mean_ace_cor, mod=NULL, data=int_ext_comb) 
## Get p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
z_model <- est_model/se_model 
p_model <- exp(-0.717*z_model - 0.416*(z_model^2))

## Back-transform proportions for checking
av_prop <- transf.ipft(xi=est_model, ni=6411)
av_var <- transf.ipft(xi=var_model, ni=6411)



## ================== Define function to run regressions ====================================
regressions <- function(data, ace) {
  
  adhd <- zelig(ace ~ scale(ADHD_PGS_r), model="logit", data=a.out, cite=FALSE)
  alcohol <- zelig(ace ~ scale(alcohol_PGS_r), model="logit", data=a.out, cite=FALSE)
  antisocial <- zelig(ace ~ scale(antisocial_PGS_r) , model="logit", data=a.out, cite=FALSE)
  anxiety <- zelig(ace ~ scale(anxiety_PGS_r) , model="logit", data=a.out, cite=FALSE)
  autism <- zelig(ace ~ scale(autism_PGS_r) , model="logit", data=a.out, cite=FALSE)
  bipolar <- zelig(ace ~ scale(bipolar_PGS_r) , model="logit", data=a.out, cite=FALSE)
  depression <- zelig(ace ~ scale(depression_PGS_r) , model="logit", data=a.out, cite=FALSE)
  schizophrenia <- zelig(ace ~ scale(schizophrenia_PGS_r) , model="logit", data=a.out, cite=FALSE)
  
  results <- ( c(exp(combine_coef_se(adhd)[2,1]), combine_coef_se(adhd)[2,2], # ADHD OR and log SE
                 exp(combine_coef_se(alcohol)[2,1]), combine_coef_se(alcohol)[2,2], # Alcohol OR and log SE
                 exp(combine_coef_se(antisocial)[2,1]), combine_coef_se(antisocial)[2,2], # Antisocial OR and log SE
                 exp(combine_coef_se(anxiety)[2,1]), combine_coef_se(anxiety)[2,2], # Anxiety OR and log SE
                 exp(combine_coef_se(autism)[2,1]), combine_coef_se(autism)[2,2],  # Autism OR and log SE
                 exp(combine_coef_se(bipolar)[2,1]), combine_coef_se(bipolar)[2,2], # Bipolar OR and log SE
                 exp(combine_coef_se(depression)[2,1]), combine_coef_se(depression)[2,2], # Depression OR and log SE
                 exp(combine_coef_se(schizophrenia)[2,1]), combine_coef_se(schizophrenia)[2,2])) # Scizophrenia OR and log SE
  return(matrix(results, nrow=1, ncol=16, dimnames=list(c(""), 
                                                        c("OR_adhd", "logSE_adhd", 
                                                          "OR_alcohol", "logSE_alcohol", 
                                                          "OR_antisocial", "logSE_antisocial", 
                                                          "OR_anxiety", "logSE_anxiety", 
                                                          "OR_autism", "logSE_autism", 
                                                          "OR_bipolar", "logSE_bipolar", 
                                                          "OR_depression", "logSE_depression", 
                                                          "OR_schizophrenia", "logSE_schizophrenia"))))
  
}

check <- regressions(a.out,  maltreatment_0_9.5yrs)
check <- regressions(a.out,  a.out$imputations$.id$maltreatment_0_9.5yrs)
check <- regressions(a.out,  ace=a.out$imputations$imp1$maltreatment_0_9.5yrs)
check <- regressions(a.out,  "maltreatment_0_9.5yrs")
check <- regressions(a.out,  a.out["maltreatment_0_9.5yrs"])

results_int <- sapply(aces, 
                      gsens_func,
                      rxy_data = r2_int_ext,
                      outcome_var = "int",
                      rgx_data = pgs_ace,
                      rgy_data = pgs_mh,
                      heritability_outcome = 0.06) # apply function

