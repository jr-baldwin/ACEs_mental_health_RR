################################################################################
#################### ABCD Study - Impute missing data ##########################
################################################################################

## Import phenotypic data
datABCD_all <- readRDS("~/Desktop/ABCD_preImput_20211108.rds")
dim(datABCD_all)
head(datABCD_all)

## Import polygenic score and principal components dataset
pgs_pc_data <- read.csv("~/Desktop/all_PGS_PCs_batch_20220121.csv")
head(pgs_pc_data)

# Name the ID variable consistently for each datafile 
pgs_pc_data$src_subject_id <- pgs_pc_data$IID
head(datABCD_all$src_subject_id)
head(pgs_pc_data$src_subject_id)

# Merge datafiles
list_data <- list(datABCD_all, pgs_pc_data)
abcd_merged <- Reduce(function(...) merge(..., by="src_subject_id", all=T), list_data)
dim(abcd_merged) 
head(abcd_merged)

## Save merged datafile including phenotypic and genetic measures
saveRDS(abcd_merged, file = "ABCD_PGS_preImput_20220121.rds")

## ================== Check variables are correct format ====================================
str(abcd_merged, list.len=ncol(abcd_merged))

## ================== Regress PCs and sex and batch out of PGSs ====================================
# note PGSs will not be imputed, so fine to do this step now
# ADHD
abcd_merged$ADHD_PGS_r <- residuals(lm(ADHD_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                       abcd_merged, na.action=na.exclude))
# Alcohol
abcd_merged$alcohol_PGS_r <- residuals(lm(alcohol_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                          abcd_merged, na.action=na.exclude))
# Antisocial
abcd_merged$antisocial_PGS_r <- residuals(lm(antisocial_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                             abcd_merged, na.action=na.exclude))
# Anxiety
abcd_merged$anxiety_PGS_r <- residuals(lm(anxiety_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                          abcd_merged, na.action=na.exclude))
# Autism
abcd_merged$autism_PGS_r <- residuals(lm(autism_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                         abcd_merged, na.action=na.exclude))
# Bipolar
abcd_merged$bipolar_PGS_r <- residuals(lm(bipolar_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                          abcd_merged, na.action=na.exclude))
# Bipolar 2019
abcd_merged$bipolar_2019_PGS_r <- residuals(lm(bipolar_2019_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                               abcd_merged, na.action=na.exclude))
# Depression
abcd_merged$depression_PGS_r <- residuals(lm(depression_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                             abcd_merged, na.action=na.exclude))
# Schizoprehnia
abcd_merged$schizophrenia_PGS_r <- residuals(lm(schizophrenia_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                                abcd_merged, na.action=na.exclude))
# Cataracts
abcd_merged$cataracts_PGS_r <- residuals(lm(cataracts_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                            abcd_merged, na.action=na.exclude))
# Schizoprehnia
abcd_merged$handedness_PGS_r <- residuals(lm(handedness_PGS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + BATCH, 
                                             abcd_merged, na.action=na.exclude))

## ======================= Subset to complete case sample ====================================
# Subset to sample with genetic data, data on all ACEs and data on internalising and externalising
abcd_completeCases <- subset(abcd_merged, !is.na(ADHD_PGS_r) & !is.na(alcohol_PGS_r) & !is.na(antisocial_PGS_r) &
                               !is.na(anxiety_PGS_r) & !is.na(autism_PGS_r) & !is.na(bipolar_PGS_r)  &
                               !is.na(bipolar_2019_PGS_r)  &
                               !is.na(depression_PGS_r) & !is.na(schizophrenia_PGS_r) & 
                               !is.na(cataracts_PGS_r) &  !is.na(handedness_PGS_r) &
                               !is.na(maltreatment) & !is.na(domestic_violence) & 
                               !is.na(parental_psychopathology) & !is.na(parental_substance) &
                               !is.na(parental_criminality) & !is.na(parental_separation) &
                               !is.na(cbcl_internalising) & !is.na(cbcl_externalising))
dim(abcd_completeCases) # 4662 complete cases
# Save complete case sample
setwd("/Users/jessie/Desktop")
saveRDS(abcd_completeCases, file = "abcd_completeCases_20220121.rds")

## ================== Subset to sample to be imputed ====================================
# Note: sample to be imputed contains genetic data and >- 10% of questionnaire responses used to define exposure to ACEs 
# between birth and age 9/10. As the whole sample has at least 10% of ACE questionnaire responses, subset by the polygenic score
abcd_imput <- subset(abcd_merged, !is.na(ADHD_PGS_r) & !is.na(alcohol_PGS_r) & !is.na(antisocial_PGS_r) &
                       !is.na(anxiety_PGS_r) & !is.na(autism_PGS_r) & !is.na(bipolar_2019_PGS_r)  &
                       !is.na(bipolar_PGS_r) & !is.na(depression_PGS_r) & !is.na(schizophrenia_PGS_r) & 
                       !is.na(cataracts_PGS_r) &  !is.na(handedness_PGS_r))
dim(abcd_imput) # 4996, includes people with genetic data & >10% of ACE data from 0-9/10

# Examine missingness in sample to be imputed
p_missing <- unlist(lapply(abcd_imput, function(x) sum(is.na(x))))/nrow(abcd_imput)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sort(p_missing[p_missing > 0.25], decreasing = TRUE)

# Remove unnecessary variables
str(abcd_imput)
abcd_imput <- subset(abcd_imput, select=-c V2, V3, IID))

## ================== Specify categories for transformation in imputation model ====================================
## Save dataset to be imputed
setwd("/Users/jessie/Desktop")
saveRDS(abcd_imput, file = "abcd_toImpute20220121.rds")
abcd_imput <- readRDS("abcd_toImpute20220121.rds")

# Recode parental employement
abcd_imput$employment_r <- as.factor(ifelse(abcd_imput$employment=="Working now",0,1))
table(abcd_imput$employment_r, useNA="always")

colnames(abcd_imput)
str(abcd_imput)

## Specify Noms variables
noms = c( # ACEs between birth and 9.5 
  "maltreatment",
  "physical_abuse", 
  "sexual_abuse",                      
  "emotional_abuse", 
  "emotional_neglect",
  "domestic_violence", 
  "parental_psychopathology",
  "parental_substance", 
  "parental_criminality",
  "parental_separation", 
  # Early sociodemographic indicators
  "sex", 
  "employment_r", 
  "premature_birth",                    
  "maternal_alcohol_bef_preg",  
  "maternal_alcohol_aft_preg",
  "preg_complications", 
  "diff_afford_food", 
  "diff_afford_gas",
  "evicted_home")

## Specify Ords variables
ords = c("high.educ", # Parental education
         "household.income") # Household income

## Specify ID variables (those that will not be imputed but will remain in imputed datasets)
idvars = c("src_subject_id", "FID", "IID", 
           "race", # do not impute as only European ancestry participants in dataset with genetic data
           "ADHD_PGS", "alcohol_PGS", 
           "antisocial_PGS", "anxiety_PGS", "autism_PGS",
           "bipolar_PGS", "bipolar_2019_PGS", "depression_PGS", "schizophrenia_PGS", 
           "handedness_PGS", "cataracts_PGS", 
           "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10",
           "Axiom_Plate", "BATCH",
           "antisocial_PGS_r", "anxiety_PGS_r", "autism_PGS_r",
           "bipolar_PGS_r","bipolar_2019_PGS_r", "depression_PGS_r", "schizophrenia_PGS_r", 
           "handedness_PGS_r", "cataracts_PGS_r", "employment")
dim(abcd_imput)
str(abcd_imput)

## ================== Impute 50 datasets ====================================
# Load Amelia package
library(Amelia)
# Set seed for reproducibility of multiple imputation
set.seed(123)

# Impute datasets
a.out_abcd = amelia(abcd_imput, noms = noms, ords=ords, idvars = idvars, 
                    incheck = FALSE, emburn=c(5,30),
                    m=50)

# Save imputed dataset
save(a.out_abcd, file = "imputated_ABCD_20220121.RData")
rm(list=ls())
