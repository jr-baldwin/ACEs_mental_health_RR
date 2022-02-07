Robustness analysis using bipolar PGS derived from Stahl et al. (2019)
================
Jessie Baldwin

-   [Load packages](#load-packages)
-   [ALSPAC - Load imputed data and create
    variables](#alspac---load-imputed-data-and-create-variables)
-   [ABCD - Load imputed data and create
    variables](#abcd---load-imputed-data-and-create-variables)
-   [Hypothesis 1A](#hypothesis-1a)
    -   [Association between PGSs for psychopathology and
        ACEs](#association-between-pgss-for-psychopathology-and-aces)
        -   [Define function to format results for the association
            between PGSs for psychopathology and
            ACEs](#define-function-to-format-results-for-the-association-between-pgss-for-psychopathology-and-aces)
        -   [ALSPAC - Run regressions for associations between PGSs for
            psychopathology and
            ACEs](#alspac---run-regressions-for-associations-between-pgss-for-psychopathology-and-aces)
        -   [ALSPAC - Get correlation between
            ACEs](#alspac---get-correlation-between-aces)
        -   [ALSPAC - Prepare to run aggregate
            model](#alspac---prepare-to-run-aggregate-model)
        -   [ALSPAC - Run aggregate meta-analysis to pool results across
            individual regressions for psychopathology PGSs and
            ACEs](#alspac---run-aggregate-meta-analysis-to-pool-results-across-individual-regressions-for-psychopathology-pgss-and-aces)
        -   [ABCD - Run regressions for associations between PGSs for
            psychopathology and
            ACEs](#abcd---run-regressions-for-associations-between-pgss-for-psychopathology-and-aces)
        -   [ABCD - Get correlation between ACEs for aggregate
            model](#abcd---get-correlation-between-aces-for-aggregate-model)
        -   [ABCD - Prepare to run aggregate
            model](#abcd---prepare-to-run-aggregate-model)
        -   [ABCD - Run aggregate meta-analysis to pool results across
            individual regressions for psychopathology PGSs and
            ACEs](#abcd---run-aggregate-meta-analysis-to-pool-results-across-individual-regressions-for-psychopathology-pgss-and-aces)
-   [Hypothesis 1B](#hypothesis-1b)
    -   [ALSPAC - Specify SEM](#alspac---specify-sem)
    -   [Fit model and run Wald test to assess differences between
        polygenic
        scores](#fit-model-and-run-wald-test-to-assess-differences-between-polygenic-scores)
    -   [Run pairwise comparisons to test which polygenic scores
        differ](#run-pairwise-comparisons-to-test-which-polygenic-scores-differ)
    -   [ABCD - Specify SEM](#abcd---specify-sem)
    -   [Fit model and run Wald test to assess differences between
        polygenic
        scores](#fit-model-and-run-wald-test-to-assess-differences-between-polygenic-scores-1)
    -   [Run pairwise comparisons to test which polygenic scores differ
        in
        ABCD](#run-pairwise-comparisons-to-test-which-polygenic-scores-differ-in-abcd)
    -   [Combine ALSPAC and ABCD
        figures](#combine-alspac-and-abcd-figures)
-   [Hypothesis 1C - ALSPAC](#hypothesis-1c---alspac)
    -   [Specify SEM](#specify-sem)
    -   [Fit model and run Wald test](#fit-model-and-run-wald-test)
-   [Hypothesis 1C - ABCD](#hypothesis-1c---abcd)
    -   [Specify SEM](#specify-sem-1)
    -   [Fit model and run Wald test](#fit-model-and-run-wald-test-1)
-   [Hypothesis 2A](#hypothesis-2a)
-   [Specify SEM](#specify-sem-2)
    -   [Define function to extract
        results](#define-function-to-extract-results)
    -   [ALSPAC - Run models for internalising
        problems](#alspac---run-models-for-internalising-problems)
    -   [ALSPAC - Extract results for internalising problems into a
        dataframe](#alspac---extract-results-for-internalising-problems-into-a-dataframe)
        -   [ALSPAC - Get correlation between
            ACEs](#alspac---get-correlation-between-aces-1)
    -   [Aggregate results for proportions explained by polygenic
        scores](#aggregate-results-for-proportions-explained-by-polygenic-scores)
    -   [Run models for externalising
        problems](#run-models-for-externalising-problems)
    -   [ALSPAC - Extract results for externalising problems into a
        dataframe](#alspac---extract-results-for-externalising-problems-into-a-dataframe)
    -   [ALSPAC - Aggregate results for proportions explained by
        polygenic
        scores](#alspac---aggregate-results-for-proportions-explained-by-polygenic-scores)
    -   [Remove models saved for ALSPAC
        data](#remove-models-saved-for-alspac-data)
-   [ABCD - Specify SEM](#abcd---specify-sem-1)
    -   [ABCD - Run models for internalising
        problems](#abcd---run-models-for-internalising-problems)
    -   [Define function to extract results (necessary to re-define
        function as maltreatment and parental criminality used
        ordered=ACE)](#define-function-to-extract-results-necessary-to-re-define-function-as-maltreatment-and-parental-criminality-used-orderedace)
    -   [ABCD - Extract results for internalising problems into a
        dataframe](#abcd---extract-results-for-internalising-problems-into-a-dataframe)
        -   [ABCD - Get correlation between ACEs for aggregate
            model](#abcd---get-correlation-between-aces-for-aggregate-model-1)
    -   [ABCD - Aggregate results for proportions explained by polygenic
        scores](#abcd---aggregate-results-for-proportions-explained-by-polygenic-scores)
    -   [ABCD - Run models for externalising
        problems](#abcd---run-models-for-externalising-problems)
    -   [ABCD - Extract results for externalising problems into a
        dataframe](#abcd---extract-results-for-externalising-problems-into-a-dataframe)
    -   [ABCD -Aggregate results for proportions explained by polygenic
        scores](#abcd--aggregate-results-for-proportions-explained-by-polygenic-scores)
-   [Hypothesis 2B](#hypothesis-2b)
    -   [Define Gsens function for
        proportions](#define-gsens-function-for-proportions)
    -   [Define functions for formatting Gsens
        results](#define-functions-for-formatting-gsens-results)
    -   [ALSPAC - Step 1. Obtain correlation between the ACE and the
        mental health outcome (c
        path)](#alspac---step-1-obtain-correlation-between-the-ace-and-the-mental-health-outcome-c-path)
    -   [ALSPAC - Step 2. Obtain correlation between observed polygenic
        scores for mental health problems and the ACE (a
        path)](#alspac---step-2-obtain-correlation-between-observed-polygenic-scores-for-mental-health-problems-and-the-ace-a-path)
    -   [ALSPAC - Step 3. Obtain correlation between observed polygenic
        scores and the mental health outcome (b
        path)](#alspac---step-3-obtain-correlation-between-observed-polygenic-scores-and-the-mental-health-outcome-b-path)
    -   [ALSPAC - Run Gsens for internalising
        problems](#alspac---run-gsens-for-internalising-problems)
    -   [ALSPAC - Run Gsens for externalising
        problems](#alspac---run-gsens-for-externalising-problems)
    -   [ALSPAC - Pool gsens results using all PGSs and format for
        table/plot](#alspac---pool-gsens-results-using-all-pgss-and-format-for-tableplot)
    -   [ABCD - Repeat Gsens analyses in ABCD using all
        PGSs](#abcd---repeat-gsens-analyses-in-abcd-using-all-pgss)
        -   [ABCD - Get correlation between ACEs for aggregate
            model](#abcd---get-correlation-between-aces-for-aggregate-model-2)
    -   [ABCD - Step 1. Obtain correlation between the ACE and the
        mental health
        outcome](#abcd---step-1-obtain-correlation-between-the-ace-and-the-mental-health-outcome)
    -   [ABCD - Step 2. Obtain correlation between observed polygenic
        scores for mental health problems and the ACE (a
        path)](#abcd---step-2-obtain-correlation-between-observed-polygenic-scores-for-mental-health-problems-and-the-ace-a-path)
    -   [ABCD - Step 3. Obtain correlation between observed polygenic
        scores and the mental health outcome (b
        path)](#abcd---step-3-obtain-correlation-between-observed-polygenic-scores-and-the-mental-health-outcome-b-path)
    -   [ABCD - Run Gsens for internalising
        problems](#abcd---run-gsens-for-internalising-problems)
    -   [ABCD - Run Gsens for externalising
        problems](#abcd---run-gsens-for-externalising-problems)
    -   [ABCD - Pool gsens results](#abcd---pool-gsens-results)

# Load packages

``` r
library(Amelia)
library(Zelig)
library(metafor)
library(MAd)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(broom)
library(polycor)
library(MAd)
```

# ALSPAC - Load imputed data and create variables

``` r
setwd(Data)
load("imputations_20210607.RData")
a.out_alspac <- a.out # rename to specify imputed dataset is ALSPAC
rm(a.out)

## ================== Derive maltreatment variable ====================================
# Code exposed if any exposure to physical abuse, sexual abuse, emotional abuse, or emotional neglect
a.out_alspac <- transform(a.out_alspac, maltreatment_0_9.5yrs = ifelse(physical_abuse_0_9.5yrs == "TRUE" | 
                                                           sexual_abuse_0_9.5yrs == "TRUE" |
                                                           emotional_abuse_0_9.5yrs == "TRUE" |
                                                           emotional_neglect_0_9.5yrs == "TRUE", "TRUE", "FALSE"))

# Check proportion exposed to maltreatment and individual subtypes as a sense check
table(a.out_alspac$imputations$imp1$maltreatment_0_9.5yrs)
prop.table(table(a.out_alspac$imputations$imp1$maltreatment_0_9.5yrs))
prop.table(table(a.out_alspac$imputations$imp1$physical_abuse_0_9.5yrs))
prop.table(table(a.out_alspac$imputations$imp1$sexual_abuse_0_9.5yrs))
prop.table(table(a.out_alspac$imputations$imp1$emotional_abuse_0_9.5yrs))
prop.table(table(a.out_alspac$imputations$imp1$emotional_neglect_0_9.5yrs))

## ================== Convert ACE (character) variables to factor ====================================
a.out_alspac <- transform(a.out_alspac, 
                   maltreatment_0_9.5yrs = as.factor(maltreatment_0_9.5yrs),
                   violence_between_parents_0_9.5yrs = as.factor(violence_between_parents_0_9.5yrs),
                   mental_health_problems_or_suicide_0_9.5yrs=as.factor(mental_health_problems_or_suicide_0_9.5yrs),
                   substance_household_0_9.5yrs = as.factor(substance_household_0_9.5yrs),
                   parent_convicted_offence_0_9.5yrs = as.factor(parent_convicted_offence_0_9.5yrs),
                   parental_separation_0_9.5yrs = as.factor(parental_separation_0_9.5yrs))

## ================== Standardise polygenic scores ====================================
a.out_alspac <- transform(a.out_alspac, ADHD_PGS_r = scale(ADHD_PGS_r), 
                   alcohol_PGS_r = scale(alcohol_PGS_r),
                   antisocial_PGS_r = scale(antisocial_PGS_r),
                   anxiety_PGS_r = scale(anxiety_PGS_r),
                   autism_PGS_r = scale(autism_PGS_r),
                   bipolar2019_PGS_r = scale(bipolar2019_PGS_r),
                   depression_PGS_r = scale(depression_PGS_r),
                   schizophrenia_PGS_r = scale(schizophrenia_PGS_r))
```

# ABCD - Load imputed data and create variables

``` r
setwd(Data)
load("imputated_ABCD_20220121.RData")
summary(a.out_abcd)
dim(a.out_abcd$imputations$imp1)

## ================== Derive maltreatment variable ====================================
# Code exposed if any exposure to physical abuse, sexual abuse, emotional abuse, or emotional neglect
a.out_abcd <- transform(a.out_abcd, maltreatment_r = ifelse(physical_abuse == "1" | 
                                                    sexual_abuse == "1" |
                                                    emotional_abuse == "1" |
                                                    emotional_neglect == "1", "1", "0"))
table(a.out_abcd$imputations$imp1$maltreatment_r)

## ================== Convert maltreatment (character) variables to factor ====================================
a.out_abcd <- transform(a.out_abcd, 
                   maltreatment_r = as.factor(maltreatment_r))

## ================== Standardise residualised polygenic scores ====================================
a.out_abcd <- transform(a.out_abcd, ADHD_PGS_r = scale(ADHD_PGS_r), 
                   alcohol_PGS_r = scale(alcohol_PGS_r),
                   antisocial_PGS_r = scale(antisocial_PGS_r),
                   anxiety_PGS_r = scale(anxiety_PGS_r),
                   autism_PGS_r = scale(autism_PGS_r),
                   bipolar2019_PGS_r = scale(bipolar_2019_PGS_r),
                   depression_PGS_r = scale(depression_PGS_r),
                   schizophrenia_PGS_r = scale(schizophrenia_PGS_r))
```

# Hypothesis 1A

## Association between PGSs for psychopathology and ACEs

### Define function to format results for the association between PGSs for psychopathology and ACEs

``` r
format_res <- function(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia) {
  # for each regression by polygenic score, extract the odds ratio, standard error, and p-value
  results <- ( c(exp(combine_coef_se(adhd)[2,1]), combine_coef_se(adhd)[2,2], combine_coef_se(adhd)[2,4], # ADHD 
                 exp(combine_coef_se(alcohol)[2,1]), combine_coef_se(alcohol)[2,2], combine_coef_se(alcohol)[2,4], # Alcohol 
                 exp(combine_coef_se(antisocial)[2,1]), combine_coef_se(antisocial)[2,2], combine_coef_se(antisocial)[2,4], #Antisocial
                 exp(combine_coef_se(anxiety)[2,1]), combine_coef_se(anxiety)[2,2], combine_coef_se(anxiety)[2,4], # Anxiety 
                 exp(combine_coef_se(autism)[2,1]), combine_coef_se(autism)[2,2], combine_coef_se(autism)[2,4],  # Autism 
                 exp(combine_coef_se(bipolar)[2,1]), combine_coef_se(bipolar)[2,2], combine_coef_se(bipolar)[2,4], # Bipolar 
                 exp(combine_coef_se(depression)[2,1]), combine_coef_se(depression)[2,2], combine_coef_se(depression)[2,4], #Depression
                 exp(combine_coef_se(schizophrenia)[2,1]), combine_coef_se(schizophrenia)[2,2], combine_coef_se(schizophrenia)[2,4])) #Schiz
  return(matrix(results, nrow=1, ncol=24, dimnames=list(c(""), # Name results in matrix
                                                        c("OR_adhd", "logSE_adhd", "p_adhd", 
                                                          "OR_alcohol", "logSE_alcohol", "p_alcohol", 
                                                          "OR_antisocial", "logSE_antisocial", "p_antisocial", 
                                                          "OR_anxiety", "logSE_anxiety", "p_anxiety", 
                                                          "OR_autism", "logSE_autism", "p_autism", 
                                                          "OR_bipolar", "logSE_bipolar", "p_bipolar", 
                                                          "OR_depression", "logSE_depression", "p_depression", 
                                                          "OR_schizophrenia", "logSE_schizophrenia", "p_schizophrenia"))))
}
```

### ALSPAC - Run regressions for associations between PGSs for psychopathology and ACEs

``` r
# Maltreatment
adhd <- zelig(maltreatment_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(maltreatment_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(maltreatment_0_9.5yrs ~ antisocial_PGS_r + sex , model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(maltreatment_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(maltreatment_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(maltreatment_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE) #bipolar 2019
depression <- zelig(maltreatment_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(maltreatment_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

maltreatment <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Domestic violence
adhd <- zelig(violence_between_parents_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(violence_between_parents_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(violence_between_parents_0_9.5yrs ~ antisocial_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(violence_between_parents_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(violence_between_parents_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(violence_between_parents_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)#bipolar 2019
depression <- zelig(violence_between_parents_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(violence_between_parents_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

domestic_violence <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental mental illness violence
adhd <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ antisocial_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)#bipolar 2019
depression <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(mental_health_problems_or_suicide_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

parent_psychopathology <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental subtance abuse 
adhd <- zelig(substance_household_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(substance_household_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(substance_household_0_9.5yrs ~ antisocial_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(substance_household_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(substance_household_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(substance_household_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)#bipolar 2019
depression <- zelig(substance_household_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(substance_household_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

par_substance <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental criminality 
adhd <- zelig(parent_convicted_offence_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(parent_convicted_offence_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(parent_convicted_offence_0_9.5yrs ~ antisocial_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(parent_convicted_offence_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(parent_convicted_offence_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(parent_convicted_offence_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)#bipolar 2019
depression <- zelig(parent_convicted_offence_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(parent_convicted_offence_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

par_criminal <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental separation 
adhd <- zelig(parental_separation_0_9.5yrs ~ ADHD_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
alcohol <- zelig(parental_separation_0_9.5yrs ~ alcohol_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
antisocial <- zelig(parental_separation_0_9.5yrs ~ antisocial_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
anxiety <- zelig(parental_separation_0_9.5yrs ~ anxiety_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
autism <- zelig(parental_separation_0_9.5yrs ~ autism_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
bipolar <- zelig(parental_separation_0_9.5yrs ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)#bipolar 2019
depression <- zelig(parental_separation_0_9.5yrs ~ depression_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)
schizophrenia <- zelig(parental_separation_0_9.5yrs ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_alspac, cite=FALSE)

par_sep <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

### ALSPAC - Get correlation between ACEs

``` r
# The correlation between ACEs is necessary to specify in the aggregate model (to run later) as the level of dependency between effect sizes 

# Combine imputed datasets into one big data frame with bind_rows(), following guidance: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
all_imputations_alspac <- bind_rows(unclass(a.out_alspac$imputations), .id = "m") %>%
  group_by(m) 

# Get correlation between ACE variables
ace_cor_alspac <- hetcor(all_imputations_alspac[,c("maltreatment_0_9.5yrs", 
                                     "violence_between_parents_0_9.5yrs", 
                                     "mental_health_problems_or_suicide_0_9.5yrs", 
                                     "substance_household_0_9.5yrs", 
                                     "parent_convicted_offence_0_9.5yrs", 
                                     "parental_separation_0_9.5yrs")])$correlations
# Get the mean ACE correlation excluding the diagonal
ace_cor_alspac_nodiag <- ace_cor_alspac
diag(ace_cor_alspac_nodiag) <- NA
mean_ace_cor_alspac <- mean(ace_cor_alspac_nodiag, na.rm=TRUE)
mean_ace_cor_alspac
```

### ALSPAC - Prepare to run aggregate model

``` r
# Bind all results from regression models into one dataframe
results <- as.data.frame(do.call("rbind", list(maltreatment, domestic_violence, 
                                               parent_psychopathology, par_substance, par_criminal, par_sep)))

# Add ACE variable specifying which ACE is the outcome
results$ace <- c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep")

# Make dataframe long [remove ID column] 
long_results <- reshape(results, varying=names(results)[1:24],
                        direction="long", timevar = "PGS", idvar="ace",
                        times=c("adhd", 
                                "alcohol", 
                                "antisocial", "anxiety", "autism",
                                "bipolar", "depression", "schizophrenia"),
                        sep="_")

# Prepare dataframe for aggregate meta-analysis
long_results$var <- long_results$logSE^2
long_results$log_OR <- log(long_results$OR)
long_results$id <- 1
```

### ALSPAC - Run aggregate meta-analysis to pool results across individual regressions for psychopathology PGSs and ACEs

``` r
aggregate <- agg(id, log_OR, var, cor=mean_ace_cor_alspac, method = "BHHR", mod=NULL, data=long_results)
se <- sqrt(aggregate$var)

# Odds ratio
round(exp(aggregate$es),2)
# Low 95% CI
round(exp(aggregate$es - 1.96*se),2)
# Upper 95% CI
round(exp(aggregate$es + 1.96*se),2)
# Low 90% CI
round(exp(aggregate$es - 1.645*se),2)
# Upper 90% CI
round(exp(aggregate$es + 1.645*se),2)
# p value for a 2-sided test (see: https://www.bmj.com/content/343/bmj.d2304)
z <- aggregate$es / se
p <- exp(-0.717*z - 0.416*z^2)
p 
```

### ABCD - Run regressions for associations between PGSs for psychopathology and ACEs

``` r
# Maltreatment
adhd <- zelig(maltreatment_r ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(maltreatment_r ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(maltreatment_r ~ antisocial_PGS_r + sex , model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(maltreatment_r ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(maltreatment_r ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(maltreatment_r ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(maltreatment_r ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(maltreatment_r ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

maltreatment <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Domestic violence
adhd <- zelig(domestic_violence ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(domestic_violence ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(domestic_violence ~ antisocial_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(domestic_violence ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(domestic_violence ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(domestic_violence ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(domestic_violence ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(domestic_violence ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

domestic_violence <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental mental illness
adhd <- zelig(parental_psychopathology ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(parental_psychopathology ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(parental_psychopathology ~ antisocial_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(parental_psychopathology ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(parental_psychopathology ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(parental_psychopathology ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(parental_psychopathology ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(parental_psychopathology ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

parent_psychopathology <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental subtance abuse
adhd <- zelig(parental_substance ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(parental_substance ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(parental_substance ~ antisocial_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(parental_substance ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(parental_substance ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(parental_substance ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(parental_substance ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(parental_substance ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

par_substance <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental criminality 
adhd <- zelig(parental_criminality ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(parental_criminality ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(parental_criminality ~ antisocial_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(parental_criminality ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(parental_criminality ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(parental_criminality ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(parental_criminality ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(parental_criminality ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

par_criminal <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

``` r
# Parental separation 
adhd <- zelig(parental_separation ~ ADHD_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
alcohol <- zelig(parental_separation ~ alcohol_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
antisocial <- zelig(parental_separation ~ antisocial_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
anxiety <- zelig(parental_separation ~ anxiety_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
autism <- zelig(parental_separation ~ autism_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
bipolar <- zelig(parental_separation ~ bipolar2019_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)#bipolar 2019
depression <- zelig(parental_separation ~ depression_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)
schizophrenia <- zelig(parental_separation ~ schizophrenia_PGS_r + sex, model="logit", data=a.out_abcd, cite=FALSE)

par_sep <- format_res(adhd, alcohol, antisocial, anxiety, autism, bipolar, depression, schizophrenia)
```

### ABCD - Get correlation between ACEs for aggregate model

``` r
# The correlation between ACEs is necessary to specify in the aggregate model (to run later) as the level of dependency between effect sizes 

# Combine imputed datasets into one big data frame with bind_rows(), following guidance: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
all_imputations_abcd <- bind_rows(unclass(a.out_abcd$imputations), .id = "m") %>%
  group_by(m) 

# Get correlation between ACE variables
ace_cor_abcd <- hetcor(all_imputations_abcd[,c("maltreatment_r", 
                                     "domestic_violence", 
                                     "parental_psychopathology", 
                                     "parental_substance", 
                                     "parental_criminality", 
                                     "parental_separation")])$correlations

# Get the mean ACE correlation excluding the diagonal
ace_cor_abcd_nodiag <- ace_cor_abcd
diag(ace_cor_abcd_nodiag) <- NA
mean_ace_cor_abcd <- mean(ace_cor_abcd_nodiag, na.rm=TRUE) # r=0.31
mean_ace_cor_abcd
```

### ABCD - Prepare to run aggregate model

``` r
# Bind all results into one dataframe
results <- as.data.frame(do.call("rbind", list(maltreatment, domestic_violence, parent_psychopathology, 
                                               par_substance, par_criminal, par_sep)))
# Add info on ACE outcome
results$ace <- c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep")

# Make dataframe long [remove ID column]
long_results <- reshape(results, varying=names(results)[1:24],
                        direction="long", timevar = "PGS", idvar="ace",
                        times=c("adhd", 
                                "alcohol", 
                                "antisocial", "anxiety", "autism",
                                "bipolar", "depression", "schizophrenia"),
                        sep="_")

# Prepare dataframe for aggregate meta-analysis
long_results$var <- long_results$logSE^2
long_results$log_OR <- log(long_results$OR)
long_results$id <- 1
```

### ABCD - Run aggregate meta-analysis to pool results across individual regressions for psychopathology PGSs and ACEs

``` r
aggregate <- agg(id, log_OR, var, cor=mean_ace_cor_abcd, method = "BHHR", mod=NULL, data=long_results)
se <- sqrt(aggregate$var)

# Odds ratio
round(exp(aggregate$es),2)
# Low 95% CI
round(exp(aggregate$es - 1.96*se),2)
# Upper 95% CI
round(exp(aggregate$es + 1.96*se),2)
# Low 90% CI
round(exp(aggregate$es - 1.645*se),2)
# Upper 90% CI
round(exp(aggregate$es + 1.645*se),2)
# p value for a 2-sided test (see: https://www.bmj.com/content/343/bmj.d2304)
z <- aggregate$es / se
p <- exp(-0.717*z - 0.416*z^2)
p 
```

# Hypothesis 1B

## ALSPAC - Specify SEM

``` r
### NOTE: the semTools package will not load with the currently loaded packages, returns an error:
# Error: package or namespace load failed for ‘semTools’: 
# Function found when exporting methods from the namespace ‘semTools’ which is not S4 generic: ‘anova’
# It is therefore necessary to restart the R session, load only the Amelia package, open the data and run the above model 

pgs_ace_model <- '
## Regressions
# maltreatment
maltreatment_0_9.5yrs ~ a1*ADHD_PGS_r + b1*alcohol_PGS_r + c1*antisocial_PGS_r + d1*anxiety_PGS_r + e1*autism_PGS_r + f1*bipolar2019_PGS_r + g1*depression_PGS_r + h1*schizophrenia_PGS_r + sex
# domestic violence
violence_between_parents_0_9.5yrs ~ a2*ADHD_PGS_r + b2*alcohol_PGS_r + c2*antisocial_PGS_r + d2*anxiety_PGS_r + e2*autism_PGS_r + f2*bipolar2019_PGS_r + g2*depression_PGS_r + h2*schizophrenia_PGS_r + sex
# parental substance abuse
substance_household_0_9.5yrs ~ a3*ADHD_PGS_r + b3*alcohol_PGS_r + c3*antisocial_PGS_r + d3*anxiety_PGS_r + e3*autism_PGS_r + f3*bipolar2019_PGS_r + g3*depression_PGS_r + h3*schizophrenia_PGS_r + sex
# parental psychopathology
mental_health_problems_or_suicide_0_9.5yrs ~ a4*ADHD_PGS_r + b4*alcohol_PGS_r + c4*antisocial_PGS_r + d4*anxiety_PGS_r + e4*autism_PGS_r + f4*bipolar2019_PGS_r + g4*depression_PGS_r + h4*schizophrenia_PGS_r + sex
# parental criminality
parent_convicted_offence_0_9.5yrs ~ a5*ADHD_PGS_r + b5*alcohol_PGS_r + c5*antisocial_PGS_r + d5*anxiety_PGS_r + e5*autism_PGS_r + f5*bipolar2019_PGS_r + g5*depression_PGS_r + h5*schizophrenia_PGS_r + sex
# parental separation
parental_separation_0_9.5yrs ~ a6*ADHD_PGS_r + b6*alcohol_PGS_r + c6*antisocial_PGS_r + d6*anxiety_PGS_r + e6*autism_PGS_r + f6*bipolar2019_PGS_r + g6*depression_PGS_r + h6*schizophrenia_PGS_r + sex

# Variances and covariances
maltreatment_0_9.5yrs ~~ cvy12* violence_between_parents_0_9.5yrs #covariance between ACE_1 and ACE_2
maltreatment_0_9.5yrs ~~ cvy13* substance_household_0_9.5yrs #covariance between ACE_1 and ACE_3
maltreatment_0_9.5yrs ~~ cvy14*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_1 and ACE_4
maltreatment_0_9.5yrs ~~ cvy15*parent_convicted_offence_0_9.5yrs #covariance between ACE_1 and ACE_5
maltreatment_0_9.5yrs ~~ cvy16*parental_separation_0_9.5yrs #covariance between ACE_1 and ACE_6

violence_between_parents_0_9.5yrs ~~ cvy23*substance_household_0_9.5yrs #covariance between ACE_2 and ACE_3
violence_between_parents_0_9.5yrs ~~ cvy24*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_2 and ACE_4
violence_between_parents_0_9.5yrs ~~ cv25*parent_convicted_offence_0_9.5yrs #covariance between ACE_2 and ACE_5
violence_between_parents_0_9.5yrs ~~ cvy26*parental_separation_0_9.5yrs #covariance between ACE_2 and ACE_6

substance_household_0_9.5yrs ~~ cvy34*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_3 and ACE_4
substance_household_0_9.5yrs ~~ cvy35*parent_convicted_offence_0_9.5yrs #covariance between ACE_3 and ACE_5
substance_household_0_9.5yrs ~~ cvy36*parental_separation_0_9.5yrs #covariance between ACE_3 and ACE_6

mental_health_problems_or_suicide_0_9.5yrs ~~ cvy45*parent_convicted_offence_0_9.5yrs #covariance between ACE_4 and ACE_5
mental_health_problems_or_suicide_0_9.5yrs ~~ cvy46*parental_separation_0_9.5yrs #covariance between ACE_4 and ACE_6

parent_convicted_offence_0_9.5yrs ~~ cvy56*parental_separation_0_9.5yrs #covariance between ACE_5 and ACE_6

# Average effects per PGS
Av_ADHD:= (a1 + a2 + a3 + a4 + a5 + a6)/6
Av_alcohol:= (b1 + b2 + b3 + b4 + b5 + b6)/6
Av_antisocial:= (c1 + c2 + c3 + c4 + c5 + c6)/6
Av_anxiety:= (d1 + d2 + d3 + d4 + d5 + d6)/6
Av_autism:= (e1 + e2 + e3 + e4 + e5 + e6)/6
Av_bipolar:= (f1 + f2 + f3 + f4 + f5 + f6)/6
Av_depression:= (g1 + g2 + g3 + g4 + g5 + g6)/6
Av_schizophrenia:= (h1 + h2 + h3 + h4 + h5 + h6)/6
'
```

## Fit model and run Wald test to assess differences between polygenic scores

``` r
library(semTools)
pgs_diff_alspac <- semTools::runMI(pgs_ace_model, data=a.out_alspac$imputations, fun="lavaan", estimator = "WLSMV",
                ordered=c("maltreatment_0_9.5yrs", "violence_between_parents_0_9.5yrs",
                          "substance_household_0_9.5yrs", "mental_health_problems_or_suicide_0_9.5yrs", 
                          "parent_convicted_offence_0_9.5yrs", "parental_separation_0_9.5yrs"))
summary(pgs_diff_alspac)

## Test whether there are differences across PGSs using Wald test
p_any <- lavTestWald.mi(pgs_diff_alspac, constraints =  c("Av_ADHD == Av_alcohol", "Av_alcohol == Av_antisocial",
                                                 "Av_antisocial == Av_anxiety", "Av_anxiety == Av_autism",
                                                 "Av_autism == Av_bipolar", "Av_bipolar == Av_depression",
                                                 "Av_depression == Av_schizophrenia"))
p_any["pvalue"]
```

## Run pairwise comparisons to test which polygenic scores differ

``` r
# Get each combination of polygenic score effects to compare in pairwise comparisons
combs <- combn(c("Av_ADHD", "Av_alcohol", "Av_antisocial","Av_anxiety", "Av_autism", "Av_bipolar", "Av_depression", "Av_schizophrenia"), 2)
pair_combs <- as.vector(mapply(paste, sep = " == ", combs[c(T,F),], combs[c(F,T),])) #https://stackoverflow.com/questions/37901142/paste-multiple-rows-together-in-r

# Generate function to conduct pairwise comparisons between polygenic scores
pairwise_Wald <- function(model, pair) {
  wald <- lavTestWald.mi(model, constraints = pair)
  return(c(pair, as.numeric(wald["pvalue"])))
}

# Apply function to run pairwise Wald tests examining which PGSs differ 
pgs_comparison_alspac <- lapply(pair_combs, pairwise_Wald, model=pgs_diff_alspac)
# Make dataframe with pairwise comparison p-values
alspac_wald_pairwise_comp <- data.frame(matrix(unlist(pgs_comparison_alspac), nrow=length(pgs_comparison_alspac), 
                                               byrow=TRUE, dimnames=list(NULL, c("PGS_comparison", "pvalue"))))
alspac_wald_pairwise_comp$pvalue <- as.numeric(as.character(alspac_wald_pairwise_comp$pvalue)) # convert p-value to numeric

# Show significant differences
library(dplyr)
sig_alspac <- alspac_wald_pairwise_comp %>% filter(pvalue<=0.05)
sig_alspac


# Test whether pairwise differences fall within the equivalence bounds
# Extract results
results_pgs_alspac <- summary(pgs_diff_alspac)
#Make dataframe including PGS, probit estimate, and probit SE, and log odds estimate and log odds SE
pgs_data_alspac <- data.frame(PGS = results_pgs_alspac[148:155,1],
                              est = results_pgs_alspac[148:155,5], 
                              se = results_pgs_alspac[148:155,6])
pgs_data_alspac$log_odds = pgs_data_alspac$est*1.8
pgs_data_alspac$log_se = pgs_data_alspac$se*1.8

# Get list of PGSs to compare in log odds difference
combs_PGS <- combn(c("ADHD", "alcohol dependence", "antisocial behaviour","anxiety", "autism", "bipolar disorder", "depression", "schizophrenia"), 2)

# Edit labels for pairwise comparisons
library(Hmisc)
pair_combs <- capitalize(as.vector(mapply(paste, sep = " vs. ", combs_PGS[c(T,F),], combs_PGS[c(F,T),])))

## Calculate pairwise log odds differences
# Define function to get difference in log odds
diffs_odds <- function(log_odds1, log_odds2) {
  log_odds_diff <- log_odds1 - log_odds2 # check for calculating SE
  return(log_odds_diff)
}

log_odds_combs_alspac <- combn(pgs_data_alspac$log_odds, 2) # get combinations of log odds ratios between PGSs

# Calculate differences between PGS 1 - PGS2
log_odds_diff_p12 <- mapply(diffs_odds, log_odds_combs_alspac[1,], log_odds_combs_alspac[2,])# get differences between
log_odds_diff_p12

# Calculate differences between PGS 2 - PGS1 (i.e., other way around)
log_odds_diff_p21 <- mapply(diffs_odds, log_odds_combs_alspac[2,], log_odds_combs_alspac[1,])# get differences between
log_odds_diff_p21

# Pairwise SE
# Define function to get SE of difference in log odds
diffs_se <- function(se1, se2) {
  se_diff <- sqrt( (se1^2) + (se2^2) ) # check for calculating SE
  return(se_diff)
}

se_combs_alspac <- combn(pgs_data_alspac$log_se, 2)
# Calculate SE for differences between PGS 1 - PGS2
se_diff_p12  <- mapply(diffs_se, se_combs_alspac[1,], se_combs_alspac[2,])
# Calculate SE for differences between PGS 2 - PGS1 (i.e., other way around)
se_diff_p21 <- mapply(diffs_se, se_combs_alspac[2,], se_combs_alspac[1,])

# Make dataframe
pgs_diff_alspac <- data.frame(pair_combs, log_odds_diff_p12, log_odds_diff_p21, se_diff_p12, se_diff_p21) # log odds diffs and SEs are same

# Add 90% CIs
pgs_diff_alspac$low_ci_p12 <- pgs_diff_alspac$log_odds_diff_p12 - (1.645*se_diff_p12) # 90% low CI
pgs_diff_alspac$high_ci_p12 <- pgs_diff_alspac$log_odds_diff_p12 + (1.645*se_diff_p12) # 90% upper CI
# Add 90% CIs for differences between PGS 2 - PGS1
pgs_diff_alspac$low_ci_p21 <- pgs_diff_alspac$log_odds_diff_p21 - (1.645*se_diff_p21) # 90% low CI
pgs_diff_alspac$high_ci_p21 <- pgs_diff_alspac$log_odds_diff_p21 + (1.645*se_diff_p21) # 90% upper CI
pgs_diff_alspac

# Add pair combs column for log_odds_diff_p21 results
library(stringr)
pgs_diff_alspac$pair_combs_p21 <- paste(capitalize(trimws(sapply(strsplit(pgs_diff_alspac$pair_combs,"vs."), `[`, 2))), "vs.", sapply(strsplit(pgs_diff_alspac$pair_combs,"vs. "), `[`, 1))

## Make long dataframe
# rename pair_combs to pair_combs_p12 so everything is in consistent naming style
pgs_diff_alspac <- rename(pgs_diff_alspac, pair_combs_p12 = pair_combs)
pgs_diff_alspac$id <- 1:nrow(pgs_diff_alspac)
pgs_diff_alspac_long <- reshape(pgs_diff_alspac, 
                         direction="long",
                         v.names=c("pair_combs", "log_odds_diff", "se_diff", "low_ci", "high_ci"),
                         varying=list(c("pair_combs_p12", "pair_combs_p21"), 
                                      c("log_odds_diff_p12", "log_odds_diff_p21"),
                                      c("se_diff_p12", "se_diff_p21"),
                                      c("low_ci_p12", "low_ci_p21"),
                                      c("high_ci_p12", "high_ci_p21")),
                         idvar="id")

## Plot positive differences only (so the PGS with the larger effect is shown first)
library(stringr)
pgs_diff_alspac_long <- pgs_diff_alspac_long[order(pgs_diff_alspac_long[,'pair_combs']), ]
pgs_diff_alspac_long <- pgs_diff_alspac_long[pgs_diff_alspac_long$log_odds_diff>=0,]

## Add p-values
# p value for a 2-sided test (see: https://www.bmj.com/content/343/bmj.d2304)
pgs_diff_alspac_long$z <- pgs_diff_alspac_long$log_odds_diff / pgs_diff_alspac_long$se_diff
pgs_diff_alspac_long$p <- 2*(1-pnorm(pgs_diff_alspac_long$z))

## Prepare dataframe for plotting
# make paircombs an ordered factor
pgs_diff_alspac_long$pair_combs <- factor(pgs_diff_alspac_long$pair_combs, levels = pgs_diff_alspac_long$pair_combs)

# add var indicating if significant difference or not 
pgs_diff_alspac_long$sig <- ifelse(pgs_diff_alspac_long$p<=0.05, 0, 1)

## Order dataframe by significant result or not
pgs_diff_alspac_long <- pgs_diff_alspac_long[order(pgs_diff_alspac_long$sig, pgs_diff_alspac_long$pair_combs),] 

## Remove capitalisation from second named PGS (except ADHD)
pgs_diff_alspac_long$pair_combs <- as.character(paste0(sapply(strsplit((as.character(pgs_diff_alspac_long$pair_combs)), 
                                                          " vs. "), "[", 1), " vs. ", 
                                  tolower(sapply(strsplit((as.character(pgs_diff_alspac_long$pair_combs)), " vs. "), "[", 2))))
pgs_diff_alspac_long$pair_combs[pgs_diff_alspac_long$pair_combs=="Depression vs. adhd " ] <- "Depression vs. ADHD"
```

## ABCD - Specify SEM

``` r
pgs_ace_model <- '
## Regressions
# maltreatment
maltreatment_r ~ a1*ADHD_PGS_r + b1*alcohol_PGS_r + c1*antisocial_PGS_r + d1*anxiety_PGS_r + e1*autism_PGS_r + f1*bipolar2019_PGS_r + g1*depression_PGS_r + h1*schizophrenia_PGS_r + sex
# domestic violence
domestic_violence ~ a2*ADHD_PGS_r + b2*alcohol_PGS_r + c2*antisocial_PGS_r + d2*anxiety_PGS_r + e2*autism_PGS_r + f2*bipolar2019_PGS_r + g2*depression_PGS_r + h2*schizophrenia_PGS_r + sex
# parental substance abuse
parental_substance ~ a3*ADHD_PGS_r + b3*alcohol_PGS_r + c3*antisocial_PGS_r + d3*anxiety_PGS_r + e3*autism_PGS_r + f3*bipolar2019_PGS_r + g3*depression_PGS_r + h3*schizophrenia_PGS_r + sex
# parental psychopathology
parental_psychopathology ~ a4*ADHD_PGS_r + b4*alcohol_PGS_r + c4*antisocial_PGS_r + d4*anxiety_PGS_r + e4*autism_PGS_r + f4*bipolar2019_PGS_r + g4*depression_PGS_r + h4*schizophrenia_PGS_r + sex
# parental criminality
parental_criminality ~ a5*ADHD_PGS_r + b5*alcohol_PGS_r + c5*antisocial_PGS_r + d5*anxiety_PGS_r + e5*autism_PGS_r + f5*bipolar2019_PGS_r + g5*depression_PGS_r + h5*schizophrenia_PGS_r + sex
# parental separation
parental_separation ~ a6*ADHD_PGS_r + b6*alcohol_PGS_r + c6*antisocial_PGS_r + d6*anxiety_PGS_r + e6*autism_PGS_r + f6*bipolar2019_PGS_r + g6*depression_PGS_r + h6*schizophrenia_PGS_r + sex

# Variances and covariances
maltreatment_r ~~ cvy12* domestic_violence #covariance between ACE_1 and ACE_2
maltreatment_r ~~ cvy13* parental_substance #covariance between ACE_1 and ACE_3
maltreatment_r ~~ cvy14*parental_psychopathology #covariance between ACE_1 and ACE_4
maltreatment_r ~~ cvy15*parental_criminality #covariance between ACE_1 and ACE_5
maltreatment_r ~~ cvy16*parental_separation #covariance between ACE_1 and ACE_6

domestic_violence ~~ cvy23*parental_substance #covariance between ACE_2 and ACE_3
domestic_violence ~~ cvy24*parental_psychopathology #covariance between ACE_2 and ACE_4
domestic_violence ~~ cv25*parental_criminality #covariance between ACE_2 and ACE_5
domestic_violence ~~ cvy26*parental_separation #covariance between ACE_2 and ACE_6

parental_substance ~~ cvy34*parental_psychopathology #covariance between ACE_3 and ACE_4
parental_substance ~~ cvy35*parental_criminality #covariance between ACE_3 and ACE_5
parental_substance ~~ cvy36*parental_separation #covariance between ACE_3 and ACE_6

parental_psychopathology ~~ cvy45*parental_criminality #covariance between ACE_4 and ACE_5
parental_psychopathology ~~ cvy46*parental_separation #covariance between ACE_4 and ACE_6

parental_criminality ~~ cvy56*parental_separation #covariance between ACE_5 and ACE_6

# Average effects per PGS
Av_ADHD:=(a1 + a2 + a3 + a4 + a5 + a6)/6
Av_alcohol:=(b1 + b2 + b3 + b4 + b5 + b6)/6
Av_antisocial:=(c1 + c2 + c3 + c4 + c5 + c6)/6
Av_anxiety:=(d1 + d2 + d3 + d4 + d5 + d6)/6
Av_autism:=(e1 + e2 + e3 + e4 + e5 + e6)/6
Av_bipolar:=(f1 + f2 + f3 + f4 + f5 + f6)/6
Av_depression:=(g1 + g2 + g3 + g4 + g5 + g6)/6
Av_schizophrenia:=(h1 + h2 + h3 + h4 + h5 + h6)/6
'
```

## Fit model and run Wald test to assess differences between polygenic scores

``` r
pgs_diff_abcd <- semTools::runMI(pgs_ace_model, data=a.out_abcd$imputations, fun="lavaan", estimator = "WLSMV",
                          ordered=c("maltreatment_r", "domestic_violence",
                                    "parental_substance", "parental_psychopathology", 
                                    "parental_criminality", "parental_separation"))
summary(pgs_diff_abcd)

## Test whether there are differences across PGSs using Wald test
p_any <- lavTestWald.mi(pgs_diff_abcd, constraints = c("Av_ADHD == Av_alcohol", "Av_alcohol == Av_antisocial",
                                                 "Av_antisocial == Av_anxiety", "Av_anxiety == Av_autism",
                                                 "Av_autism == Av_bipolar", "Av_bipolar == Av_depression",
                                                 "Av_depression == Av_schizophrenia"))
p_any["pvalue"]
```

## Run pairwise comparisons to test which polygenic scores differ in ABCD

``` r
# Get each combination of polygenic score effects to compare in pairwise comparisons
pair_combs_Wald <- as.vector(mapply(paste, sep = " == ", combs[c(T,F),], combs[c(F,T),])) #https://stackoverflow.com/questions/37901142/paste-multiple-rows-together-in-r

# Apply function to run pairwise Wald tests examining which PGSs differ 
pgs_comparison_abcd <- lapply(pair_combs_Wald, pairwise_Wald, model=pgs_diff_abcd)
# Make dataframe with pairwise comparison p-values
abcd_wald_pairwise_comp <- data.frame(matrix(unlist(pgs_comparison_abcd), nrow=length(pgs_comparison_abcd), 
                                               byrow=TRUE, dimnames=list(NULL, c("PGS_comparison", "pvalue"))))
abcd_wald_pairwise_comp$pvalue <- as.numeric(as.character(abcd_wald_pairwise_comp$pvalue)) # convert p-value to numeric

# Show significant differences according to Wald test
sig_abcd <- abcd_wald_pairwise_comp %>% filter(pvalue<=0.05)
sig_abcd

## Test whether pairwise differences fall within the equivalence bounds
# Extract results
results_pgs_abcd <- summary(pgs_diff_abcd)
#Make dataframe including PGS, probit estimate, and probit SE, and log odds estimate and log odds SE
pgs_data_abcd <- data.frame(PGS = results_pgs_abcd[148:155,1],
                              est = results_pgs_abcd[148:155,5], 
                              se = results_pgs_abcd[148:155,6])
pgs_data_abcd$log_odds = pgs_data_abcd$est*1.8
pgs_data_abcd$log_se = pgs_data_abcd$se*1.8

# Get list of PGSs to compare in log odds difference
combs_PGS <- combn(c("ADHD", "alcohol dependence", "antisocial behaviour","anxiety", "autism", "bipolar disorder", "depression", "schizophrenia"), 2)

# Edit labels for pairwise comparisons
library(Hmisc)
pair_combs_PGS <- capitalize(as.vector(mapply(paste, sep = " vs. ", combs_PGS[c(T,F),], combs_PGS[c(F,T),])))

## Calculate pairwise log odds differences
log_odds_combs_abcd <- combn(pgs_data_abcd$log_odds, 2) # get combinations of log odds ratios between PGSs

# Calculate differences between PGS 1 - PGS2
log_odds_diff_p12 <- mapply(diffs_odds, log_odds_combs_abcd[1,], log_odds_combs_abcd[2,])# get differences between
log_odds_diff_p12

# Calculate differences between PGS 2 - PGS1 (i.e., other way around)
log_odds_diff_p21 <- mapply(diffs_odds, log_odds_combs_abcd[2,], log_odds_combs_abcd[1,])# get differences between
log_odds_diff_p21

# # get combinations of SEs between PGSs
se_combs_abcd <- combn(pgs_data_abcd$log_se, 2)
# Calculate SE for differences between PGS 1 - PGS2
se_diff_p12  <- mapply(diffs_se, se_combs_abcd[1,], se_combs_abcd[2,])
# Calculate SE for differences between PGS 2 - PGS1 (i.e., other way around)
se_diff_p21 <- mapply(diffs_se, se_combs_abcd[2,], se_combs_abcd[1,])

# Make dataframe
pgs_diff_abcd <- data.frame(pair_combs_PGS, log_odds_diff_p12, log_odds_diff_p21, se_diff_p12, se_diff_p21) # log odds diffs and SEs are same

# Add 90% CIs
pgs_diff_abcd$low_ci_p12 <- pgs_diff_abcd$log_odds_diff_p12 - (1.645*pgs_diff_abcd$se_diff_p12) # 90% low CI
pgs_diff_abcd$high_ci_p12 <- pgs_diff_abcd$log_odds_diff_p12 + (1.645*pgs_diff_abcd$se_diff_p12) # 90% upper CI
# Add 90% CIs for differences between PGS 2 - PGS1
pgs_diff_abcd$low_ci_p21 <- pgs_diff_abcd$log_odds_diff_p21 - (1.645*pgs_diff_abcd$se_diff_p21) # 90% low CI
pgs_diff_abcd$high_ci_p21 <- pgs_diff_abcd$log_odds_diff_p21 + (1.645*pgs_diff_abcd$se_diff_p21) # 90% upper CI
pgs_diff_abcd

# Add pair combs column for log_odds_diff_p21 results
pgs_diff_abcd$pair_combs_p21 <- paste(capitalize(trimws(sapply(strsplit(pgs_diff_abcd$pair_combs,"vs."), `[`, 2))), "vs.", sapply(strsplit(pgs_diff_abcd$pair_combs,"vs. "), `[`, 1))

## Make long dataframe
# rename pair_combs to pair_combs_p12 so everything is in consistent naming style
pgs_diff_abcd <- rename(pgs_diff_abcd, pair_combs_p12 = pair_combs_PGS)
pgs_diff_abcd$id <- 1:nrow(pgs_diff_abcd)
pgs_diff_abcd_long <- reshape(pgs_diff_abcd, 
                         direction="long",
                         v.names=c("pair_combs", "log_odds_diff", "se_diff", "low_ci", "high_ci"),
                         varying=list(c("pair_combs_p12", "pair_combs_p21"), 
                                      c("log_odds_diff_p12", "log_odds_diff_p21"),
                                      c("se_diff_p12", "se_diff_p21"),
                                      c("low_ci_p12", "low_ci_p21"),
                                      c("high_ci_p12", "high_ci_p21")),
                         idvar="id")

## Plot positive differences only (so the PGS with the larger effect is shown first)
pgs_diff_abcd_long <- pgs_diff_abcd_long[order(pgs_diff_abcd_long[,'pair_combs']), ]
pgs_diff_abcd_long <- pgs_diff_abcd_long[pgs_diff_abcd_long$log_odds_diff>=0,]

## Add p-values
# p value for a 2-sided test (see: https://www.bmj.com/content/343/bmj.d2304)
pgs_diff_abcd_long$z <- pgs_diff_abcd_long$log_odds_diff / pgs_diff_abcd_long$se_diff
pgs_diff_abcd_long$p <- 2*(1-pnorm(pgs_diff_abcd_long$z))

## Prepare dataframe for plotting
# make paircombs an ordered factor
pgs_diff_abcd_long$pair_combs <- factor(pgs_diff_abcd_long$pair_combs, levels = pgs_diff_abcd_long$pair_combs)

# add var indicating if significant difference or not 
pgs_diff_abcd_long$sig <- ifelse(pgs_diff_abcd_long$p<=0.05, 0, 1)

## Order dataframe by significant result or not
pgs_diff_abcd_long <- pgs_diff_abcd_long[order(pgs_diff_abcd_long$sig, pgs_diff_abcd_long$pair_combs),] 

## Remove capitalisation from second named PGS 
pgs_diff_abcd_long$pair_combs <- as.character(paste0(sapply(strsplit((as.character(pgs_diff_abcd_long$pair_combs)), 
                                                          " vs. "), "[", 1), " vs. ", 
                                  tolower(sapply(strsplit((as.character(pgs_diff_abcd_long$pair_combs)), " vs. "), "[", 2))))
```

## Combine ALSPAC and ABCD figures

``` r
library(metafor)
dev.off()

# Prepare to save as EPS
setwd(Figures)
setEPS(width=6.3, height=9)
postscript("Hyp1b_ALSPAC_ABCD_StahlBipolar.eps")

# Set location of where to put plots
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1))

par(mar = c(3, 4, 2, 2))
# Plot ALSPAC results
forest(pgs_diff_alspac_long$log_odds_diff, ci.lb=pgs_diff_alspac_long$low_ci, ci.ub=pgs_diff_alspac_long$high_ci,
       cex=0.7,
       slab=paste(pgs_diff_alspac_long$pair_combs),
       xlab = "Difference in log odds ratio", cex.axis=0.7,
       rows = c(29:23, 20:0),
       ylim=c(0,33),
       xlim=c(-0.5, 0.65),
       alim=c(-0.1,0.3),
       at=c(-1, 0, 0.1, 0.2, 0.3),
       psize=1,
       ilab = format(round(pgs_diff_alspac_long$p,4), nsmall=4),
       ilab.xpos = c(0.53),
       ilab.pos = c(4),
       textpos = c(-0.5, 0.5))
par(font=2)
text(c(-0.355, 0.41, 0.57), 32, c("Polygenic score comparison", 
                                "Log odds diff. (90% CI)",
                                "p-value"), cex=0.7)
segments(x0=0.1, y0=-1, x1=0.1, y1=31,col="red", lty = 5)
segments(x0=-0.1, y0=-1, x1=-0.1, y1=31,col="red", lty = 5)
par(font=4)       
text(-0.5, c(30, 21), pos=4, c("Statistically significant differences",
                               "Statistically non-significant differences"), cex=0.7)

par(mar = c(4, 4, 2, 2))
# Plot ABCD results
forest(pgs_diff_abcd_long$log_odds_diff, ci.lb=pgs_diff_abcd_long$low_ci, ci.ub=pgs_diff_abcd_long$high_ci,
       cex=0.7,
       slab=paste(pgs_diff_abcd_long$pair_combs),
       xlab = "Difference in log odds ratio", cex.axis=0.7,
       rows = c(29:15, 12:0),
       ylim=c(0,33),
       xlim=c(-0.5, 0.65),
       alim=c(-0.1,0.3),
       at=c(-1, 0, 0.1, 0.2, 0.3),
       psize=1,
       ilab = format(round(pgs_diff_abcd_long$p,4), nsmall=4),
       ilab.xpos = c(0.53),
       ilab.pos = c(4),
       textpos = c(-0.5, 0.5))
par(font=2)
text(c(-0.355, 0.41, 0.57), 32, c("Polygenic score comparison", 
                                "Log odds diff. (90% CI)",
                                "p-value"), cex=0.7)
segments(x0=0.1, y0=-1, x1=0.1, y1=31,col="red", lty = 5)
segments(x0=-0.1, y0=-1, x1=-0.1, y1=31,col="red", lty = 5)
par(font=4)       
text(-0.5, c(30, 13), pos=4, c("Statistically significant differences",
                               "Statistically non-significant differences"), cex=0.7)
dev.off()
```

# Hypothesis 1C - ALSPAC

## Specify SEM

``` r
ace_diffs_model <- '
## Regressions
# maltreatment
maltreatment_0_9.5yrs ~ a1*ADHD_PGS_r + b1*alcohol_PGS_r + c1*antisocial_PGS_r + d1*anxiety_PGS_r + e1*autism_PGS_r + f1*bipolar2019_PGS_r + g1*depression_PGS_r + h1*schizophrenia_PGS_r + sex
# domestic violence
violence_between_parents_0_9.5yrs ~ a2*ADHD_PGS_r + b2*alcohol_PGS_r + c2*antisocial_PGS_r + d2*anxiety_PGS_r + e2*autism_PGS_r + f2*bipolar2019_PGS_r + g2*depression_PGS_r + h2*schizophrenia_PGS_r + sex
# parental substance abuse
substance_household_0_9.5yrs ~ a3*ADHD_PGS_r + b3*alcohol_PGS_r + c3*antisocial_PGS_r + d3*anxiety_PGS_r + e3*autism_PGS_r + f3*bipolar2019_PGS_r + g3*depression_PGS_r + h3*schizophrenia_PGS_r + sex
# parental mental illness
mental_health_problems_or_suicide_0_9.5yrs ~ a4*ADHD_PGS_r + b4*alcohol_PGS_r + c4*antisocial_PGS_r + d4*anxiety_PGS_r + e4*autism_PGS_r + f4*bipolar2019_PGS_r + g4*depression_PGS_r + h4*schizophrenia_PGS_r + sex
# parental criminality
parent_convicted_offence_0_9.5yrs ~ a5*ADHD_PGS_r + b5*alcohol_PGS_r + c5*antisocial_PGS_r + d5*anxiety_PGS_r + e5*autism_PGS_r + f5*bipolar2019_PGS_r + g5*depression_PGS_r + h5*schizophrenia_PGS_r + sex
# parental separation
parental_separation_0_9.5yrs ~ a6*ADHD_PGS_r + b6*alcohol_PGS_r + c6*antisocial_PGS_r + d6*anxiety_PGS_r + e6*autism_PGS_r + f6*bipolar2019_PGS_r + g6*depression_PGS_r + h6*schizophrenia_PGS_r + sex 

# Variances and covariances
maltreatment_0_9.5yrs ~~ cvy12* violence_between_parents_0_9.5yrs #covariance between ACE_1 and ACE_2
maltreatment_0_9.5yrs ~~ cvy13* substance_household_0_9.5yrs #covariance between ACE_1 and ACE_3
maltreatment_0_9.5yrs ~~ cvy14*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_1 and ACE_4
maltreatment_0_9.5yrs ~~ cvy15*parent_convicted_offence_0_9.5yrs #covariance between ACE_1 and ACE_5
maltreatment_0_9.5yrs ~~ cvy16*parental_separation_0_9.5yrs #covariance between ACE_1 and ACE_6

violence_between_parents_0_9.5yrs ~~ cvy23*substance_household_0_9.5yrs #covariance between ACE_2 and ACE_3
violence_between_parents_0_9.5yrs ~~ cvy24*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_2 and ACE_4
violence_between_parents_0_9.5yrs ~~ cv25*parent_convicted_offence_0_9.5yrs #covariance between ACE_2 and ACE_5
violence_between_parents_0_9.5yrs ~~ cvy26*parental_separation_0_9.5yrs #covariance between ACE_2 and ACE_6

substance_household_0_9.5yrs ~~ cvy34*mental_health_problems_or_suicide_0_9.5yrs #covariance between ACE_3 and ACE_4
substance_household_0_9.5yrs ~~ cvy35*parent_convicted_offence_0_9.5yrs #covariance between ACE_3 and ACE_5
substance_household_0_9.5yrs ~~ cvy36*parental_separation_0_9.5yrs #covariance between ACE_3 and ACE_6

mental_health_problems_or_suicide_0_9.5yrs ~~ cvy45*parent_convicted_offence_0_9.5yrs #covariance between ACE_4 and ACE_5
mental_health_problems_or_suicide_0_9.5yrs ~~ cvy46*parental_separation_0_9.5yrs #covariance between ACE_4 and ACE_6

parent_convicted_offence_0_9.5yrs ~~ cvy56*parental_separation_0_9.5yrs #covariance between ACE_5 and ACE_6

# Average effects per ACE
Av_maltreatment:= (a1 + b1 + c1 + d1 + e1 + f1 + g1 + h1)/8
Av_dom_violence:= (a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2)/8
Av_par_sub:= (a3 + b3 + c3 + d3 + e3 + f3 + g3 + h3)/8
Av_par_psych:= (a4 + b4 + c4 + d4 + e4 + f4 + g4 + h4)/8
Av_par_crim:= (a5 + b5 + c5 + d5 + e5 + f5 + g5 + h5)/8
Av_par_sep:= (a6 + b6 + c6 + d6 + e6 + f6 + g6 + h6)/8
'
```

## Fit model and run Wald test

``` r
aces_diff_alspac <- runMI(ace_diffs_model, data=a.out_alspac$imputations, fun="lavaan", estimator = "WLSMV",
                    ordered=c("maltreatment_0_9.5yrs", "violence_between_parents_0_9.5yrs",
                              "substance_household_0_9.5yrs", "mental_health_problems_or_suicide_0_9.5yrs", 
                              "parent_convicted_offence_0_9.5yrs", "parental_separation_0_9.5yrs"))
summary(aces_diff_alspac)

## Test whether there are differences across PGSs using Wald test
p_any <- lavTestWald.mi(aces_diff_alspac, constraints =  c("Av_maltreatment == Av_dom_violence", 
                                                     "Av_dom_violence == Av_par_sub",
                                                     "Av_par_sub == Av_par_psych", 
                                                     "Av_par_psych == Av_par_crim",
                                                     "Av_par_crim == Av_par_sep"))
p_any["pvalue"]
```

# Hypothesis 1C - ABCD

## Specify SEM

``` r
ace_diffs_model_abcd <- '
## Regressions
# maltreatment
maltreatment_r ~ a1*ADHD_PGS_r + b1*alcohol_PGS_r + c1*antisocial_PGS_r + d1*anxiety_PGS_r + e1*autism_PGS_r + f1*bipolar2019_PGS_r + g1*depression_PGS_r + h1*schizophrenia_PGS_r + sex
# domestic violence
domestic_violence ~ a2*ADHD_PGS_r + b2*alcohol_PGS_r + c2*antisocial_PGS_r + d2*anxiety_PGS_r + e2*autism_PGS_r + f2*bipolar2019_PGS_r + g2*depression_PGS_r + h2*schizophrenia_PGS_r + sex
# parental substance abuse
parental_substance ~ a3*ADHD_PGS_r + b3*alcohol_PGS_r + c3*antisocial_PGS_r + d3*anxiety_PGS_r + e3*autism_PGS_r + f3*bipolar2019_PGS_r + g3*depression_PGS_r + h3*schizophrenia_PGS_r + sex
# parental mental illness
parental_psychopathology ~ a4*ADHD_PGS_r + b4*alcohol_PGS_r + c4*antisocial_PGS_r + d4*anxiety_PGS_r + e4*autism_PGS_r + f4*bipolar2019_PGS_r + g4*depression_PGS_r + h4*schizophrenia_PGS_r + sex
# parental criminality
parental_criminality ~ a5*ADHD_PGS_r + b5*alcohol_PGS_r + c5*antisocial_PGS_r + d5*anxiety_PGS_r + e5*autism_PGS_r + f5*bipolar2019_PGS_r + g5*depression_PGS_r + h5*schizophrenia_PGS_r + sex
# parental separation
parental_separation ~ a6*ADHD_PGS_r + b6*alcohol_PGS_r + c6*antisocial_PGS_r + d6*anxiety_PGS_r + e6*autism_PGS_r + f6*bipolar2019_PGS_r + g6*depression_PGS_r + h6*schizophrenia_PGS_r + sex 

# Variances and covariances
maltreatment_r ~~ cvy12* domestic_violence #covariance between ACE_1 and ACE_2
maltreatment_r ~~ cvy13* parental_substance #covariance between ACE_1 and ACE_3
maltreatment_r ~~ cvy14*parental_psychopathology #covariance between ACE_1 and ACE_4
maltreatment_r ~~ cvy15*parental_criminality #covariance between ACE_1 and ACE_5
maltreatment_r ~~ cvy16*parental_separation #covariance between ACE_1 and ACE_6

domestic_violence ~~ cvy23*parental_substance #covariance between ACE_2 and ACE_3
domestic_violence ~~ cvy24*parental_psychopathology #covariance between ACE_2 and ACE_4
domestic_violence ~~ cv25*parental_criminality #covariance between ACE_2 and ACE_5
domestic_violence ~~ cvy26*parental_separation #covariance between ACE_2 and ACE_6

parental_substance ~~ cvy34*parental_psychopathology #covariance between ACE_3 and ACE_4
parental_substance ~~ cvy35*parental_criminality #covariance between ACE_3 and ACE_5
parental_substance ~~ cvy36*parental_separation #covariance between ACE_3 and ACE_6

parental_psychopathology ~~ cvy45*parental_criminality #covariance between ACE_4 and ACE_5
parental_psychopathology ~~ cvy46*parental_separation #covariance between ACE_4 and ACE_6

parental_criminality ~~ cvy56*parental_separation #covariance between ACE_5 and ACE_6

# Average effects per ACE
Av_maltreatment:=(a1 + b1 + c1 + d1 + e1 + f1 + g1 + h1)/8
Av_dom_violence:=(a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2)/8
Av_par_sub:=(a3 + b3 + c3 + d3 + e3 + f3 + g3 + h3)/8
Av_par_psych:=(a4 + b4 + c4 + d4 + e4 + f4 + g4 + h4)/8
Av_par_crim:=(a5 + b5 + c5 + d5 + e5 + f5 + g5 + h5)/8
Av_par_sep:=(a6 + b6 + c6 + d6 + e6 + f6 + g6 + h6)/8
'
```

## Fit model and run Wald test

``` r
aces_diff_abcd <- runMI(ace_diffs_model_abcd, data=a.out_abcd$imputations, fun="lavaan", estimator = "WLSMV",
                    ordered=c("maltreatment_r", "domestic_violence",
                              "parental_substance", "parental_psychopathology", 
                              "parental_criminality", "parental_separation", 
                              "household.income", "high.educ"))
summary(aces_diff_abcd)

## Test whether there are differences across PGSs using Wald test
p_any <- lavTestWald.mi(aces_diff_abcd, constraints =  c("Av_maltreatment == Av_dom_violence", 
                                                     "Av_dom_violence == Av_par_sub",
                                                     "Av_par_sub == Av_par_psych", 
                                                     "Av_par_psych == Av_par_crim",
                                                     "Av_par_crim == Av_par_sep"))
p_any["pvalue"]
```

``` r
# Clear workspace
rm(abcd_wald_pairwise_comp, ace_diffs_model, ace_diffs_model_abcd,  aces_diff_abcd, aces_diff_alspac,
   alspac_wald_pairwise_comp, combs,  combs_PGS, diffs_odds, diffs_se, log_odds_combs_abcd, 
   log_odds_combs_alspac, log_odds_diff_p12, log_odds_diff_p21, p_any, pair_combs,
   pair_combs_PGS , pair_combs_Wald, pairwise_Wald, pgs_ace_model, pgs_comparison_abcd, 
   pgs_comparison_alspac, pgs_data_abcd, pgs_data_alspac, pgs_diff_abcd, pgs_diff_abcd_long,
   pgs_diff_alspac, pgs_diff_alspac_long, results_pgs_abcd, results_pgs_alspac, se_combs_abcd,
   se_combs_alspac, se_diff_p12, se_diff_p21, sig_abcd, sig_alspac)
```

# Hypothesis 2A

# Specify SEM

``` r
multi_PGS_model = 
  '# Outcome is regressed onto polygenic scores (mediators) and ACE
    MH ~ b1*ADHD_PGS_r + b2*alcohol_PGS_r + b3*antisocial_PGS_r + b4*anxiety_PGS_r + b5*autism_PGS_r + b6*bipolar2019_PGS_r + b7*depression_PGS_r + b8*schizophrenia_PGS_r + cp * ACE_1
    # Polygenic scores are regressed onto ACEs
    ADHD_PGS_r ~ a1 * ACE_1
    alcohol_PGS_r ~ a2 * ACE_1
    antisocial_PGS_r ~ a3 * ACE_1
    anxiety_PGS_r ~ a4 * ACE_1
    autism_PGS_r ~ a5 * ACE_1
    bipolar2019_PGS_r ~ a6 * ACE_1
    depression_PGS_r ~ a7 * ACE_1
    schizophrenia_PGS_r ~ a8 * ACE_1
    
    # Covariances between polygenic scores
    ADHD_PGS_r ~~ cv12*alcohol_PGS_r
    ADHD_PGS_r ~~ cv13*antisocial_PGS_r
    ADHD_PGS_r ~~ cv14*anxiety_PGS_r
    ADHD_PGS_r ~~ cv15*autism_PGS_r
    ADHD_PGS_r ~~ cv16*bipolar2019_PGS_r
    ADHD_PGS_r ~~ cv17*depression_PGS_r
    ADHD_PGS_r ~~ cv18*schizophrenia_PGS_r
    alcohol_PGS_r ~~ cv23*antisocial_PGS_r
    alcohol_PGS_r ~~ cv24*anxiety_PGS_r
    alcohol_PGS_r ~~ cv25*autism_PGS_r
    alcohol_PGS_r ~~ cv26*bipolar2019_PGS_r
    alcohol_PGS_r ~~ cv27*depression_PGS_r
    alcohol_PGS_r ~~ cv28*schizophrenia_PGS_r
    antisocial_PGS_r ~~ cv34*anxiety_PGS_r
    antisocial_PGS_r ~~ cv35*autism_PGS_r
    antisocial_PGS_r ~~ cv36*bipolar2019_PGS_r
    antisocial_PGS_r ~~ cv37*depression_PGS_r
    antisocial_PGS_r ~~ cv38*schizophrenia_PGS_r
    anxiety_PGS_r ~~ cv45*autism_PGS_r
    anxiety_PGS_r ~~ cv46*bipolar2019_PGS_r
    anxiety_PGS_r ~~ cv47*depression_PGS_r
    anxiety_PGS_r ~~ cv48*schizophrenia_PGS_r
    autism_PGS_r ~~ cv56*bipolar2019_PGS_r
    autism_PGS_r ~~ cv57*depression_PGS_r
    autism_PGS_r ~~ cv58*schizophrenia_PGS_r
    bipolar2019_PGS_r ~~ cv67*depression_PGS_r
    bipolar2019_PGS_r ~~ cv68*schizophrenia_PGS_r
    depression_PGS_r ~~ cv78*schizophrenia_PGS_r
    
    # Variances
    MH ~~ start(1)*MH
    ADHD_PGS_r ~~ start(1)*ADHD_PGS_r
    alcohol_PGS_r ~~ start(1)*alcohol_PGS_r
    antisocial_PGS_r ~~ start(1)*antisocial_PGS_r
    anxiety_PGS_r ~~ start(1)*anxiety_PGS_r
    autism_PGS_r ~~ start(1)*autism_PGS_r
    bipolar2019_PGS_r ~~ start(1)*bipolar2019_PGS_r
    depression_PGS_r ~~ start(1)*depression_PGS_r
    schizophrenia_PGS_r ~~ start(1)*schizophrenia_PGS_r
    
    ## Estimate the indirect effects for each ACE
    indirect_PGS:= (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8)
    
    ## Estimate the proportion of the association mediated through the PGS for each ACE
    prop_PGSs:= ((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8))/
    ((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + cp)
    total:=((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + cp)
    adjusted:=total-indirect_PGS'
```

## Define function to extract results

``` r
extract_res <- function(fit_maltreatment, 
                        fit_dom_violence,
                        fit_par_psych,
                        fit_par_substance,
                        fit_par_crim,
                        fit_par_sep, 
                        nrow,
                        label) {
  est <- c(summary(fit_maltreatment, ci=TRUE)[nrow, 5], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 5], 
              summary(fit_par_psych, ci=TRUE)[nrow, 5],
              summary(fit_par_substance, ci=TRUE)[nrow, 5],
              summary(fit_par_crim, ci=TRUE)[nrow, 5],
              summary(fit_par_sep, ci=TRUE)[nrow, 5])
  low_ci <- c(summary(fit_maltreatment, ci=TRUE)[nrow, 10], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 10], 
              summary(fit_par_psych, ci=TRUE)[nrow, 10],
              summary(fit_par_substance, ci=TRUE)[nrow, 10],
              summary(fit_par_crim, ci=TRUE)[nrow, 10],
              summary(fit_par_sep, ci=TRUE)[nrow, 10])
  up_ci <- c(summary(fit_maltreatment, ci=TRUE)[nrow, 11], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 11], 
              summary(fit_par_psych, ci=TRUE)[nrow, 11],
              summary(fit_par_substance, ci=TRUE)[nrow, 11],
              summary(fit_par_crim, ci=TRUE)[nrow, 11],
              summary(fit_par_sep, ci=TRUE)[nrow, 11])
  ACE <- c("Maltreatment", "Domestic violence", "Parental mental illness", 
           "Parental substance abuse", "Parental criminality", "Parental separation")
  df <- data.frame(ACE, est, low_ci, up_ci)
  colnames(df)[2:4] <- paste0(label, "_", colnames(df))[2:4] # label columns by the type of result
  return(df)
}
```

## ALSPAC - Run models for internalising problems

``` r
# Specify that MH = internalising problems, with sex regressed out
# For each ACE, transform the multiply imputed dataset (a.out_alspac) to specify that ACE_1 = the particular ACE
## Maltreatment
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(internalising10 ~ sex, na.action=na.exclude))),
                   ACE_1 = maltreatment_0_9.5yrs)
fit_int_maltreatment <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_maltreatment)

### Domestic violence
a.out_alspac <- transform(a.out_alspac, ACE_1 = violence_between_parents_0_9.5yrs)
fit_int_dom_violence <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_dom_violence)

### Parental substance abuse
a.out_alspac <- transform(a.out_alspac, ACE_1 = substance_household_0_9.5yrs)
fit_int_par_substance <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_par_substance)

### Parental psychopathology
a.out_alspac <- transform(a.out_alspac, ACE_1 = mental_health_problems_or_suicide_0_9.5yrs)
fit_int_par_psych <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_par_psych)

### Parental criminality
a.out_alspac <- transform(a.out_alspac, ACE_1 = parent_convicted_offence_0_9.5yrs)
fit_int_par_crim <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_par_crim)

### Parental separation
a.out_alspac <- transform(a.out_alspac, ACE_1 = parental_separation_0_9.5yrs)
fit_int_par_sep <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_int_par_sep)
```

## ALSPAC - Extract results for internalising problems into a dataframe

``` r
## Extract results (estimate, low CI, upper CI [plus SE for proportions])

# Proportion explained by polygenic scores
prop <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                        fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 57, "prop")
# Adjusted effect
adjusted <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                            fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 59, "adjusted")
# Genetic confounding
confound <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                            fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 56, "confound") 
# Total effect
total <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                         fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 58, "total") 
# Generate dataframe
hyp2a_int_alsp <- data.frame(prop, total, confound, adjusted)
hyp2a_int_alsp <- subset(hyp2a_int_alsp, select = -c(ACE.1, ACE.2, ACE.3)) # drop extra ACE variables
```

### ALSPAC - Get correlation between ACEs

``` r
# The correlation between ACEs is necessary to specify in the aggregate model (to run later) as the level of dependency between effect sizes 
library(tidyverse)
library(broom)
library(polycor)

# Combine imputed datasets into one big data frame with bind_rows(), following guidance: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
all_imputations_alspac <- bind_rows(unclass(a.out_alspac$imputations), .id = "m") %>%
  group_by(m) 

# Get correlation between ACE variables
ace_cor_alspac <- hetcor(all_imputations_alspac[,c("maltreatment_0_9.5yrs", 
                                     "violence_between_parents_0_9.5yrs", 
                                     "mental_health_problems_or_suicide_0_9.5yrs", 
                                     "substance_household_0_9.5yrs", 
                                     "parent_convicted_offence_0_9.5yrs", 
                                     "parental_separation_0_9.5yrs")])$correlations
# Get the mean ACE correlation excluding the diagonal
ace_cor_alspac_nodiag <- ace_cor_alspac
diag(ace_cor_alspac_nodiag) <- NA
mean_ace_cor_alspac <- mean(ace_cor_alspac_nodiag, na.rm=TRUE)
mean_ace_cor_alspac
```

## Aggregate results for proportions explained by polygenic scores

``` r
hyp2a_int_alsp$var <- ((hyp2a_int_alsp$prop_up_ci - hyp2a_int_alsp$prop_low_ci)/3.92)^2 # get variance
hyp2a_int_alsp$ni <- 6411 # specify sample size
hyp2a_int_alsp$id <- 1

# check mean of raw proportions
mean(hyp2a_int_alsp$prop_est)

## Aggregate effect sizes based on non-transformed data
library(MAd)
agg_2a <- agg(id=id, es=prop_est, var=var, method = "BHHR", cor = mean_ace_cor_alspac, mod=NULL, data=hyp2a_int_alsp) 
## Extract p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
round((c(est_model, lowci_model, upci_model)),4)*100
```

## Run models for externalising problems

``` r
## Maltreatment
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(externalising10 ~ sex, na.action=na.exclude))), # MH = externalising, sex regressed out
                   ACE_1 = maltreatment_0_9.5yrs)
fit_ext_maltreatment <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_maltreatment)

### Domestic violence
a.out_alspac <- transform(a.out_alspac, ACE_1 = violence_between_parents_0_9.5yrs)
fit_ext_dom_violence <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_dom_violence)

### Parental psychopathology
a.out_alspac <- transform(a.out_alspac, ACE_1 = mental_health_problems_or_suicide_0_9.5yrs)
fit_ext_par_psych <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_par_psych)

### Parental substance abuse
a.out_alspac <- transform(a.out_alspac, ACE_1 = substance_household_0_9.5yrs)
fit_ext_par_substance <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_par_substance)

### Parental criminality
a.out_alspac <- transform(a.out_alspac, ACE_1 = parent_convicted_offence_0_9.5yrs)
fit_ext_par_crim <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_par_crim)

### Parental separation
a.out_alspac <- transform(a.out_alspac, ACE_1 = parental_separation_0_9.5yrs)
fit_ext_par_sep <- runMI(multi_PGS_model, data=a.out_alspac$imputations, fun="lavaan")
summary(fit_ext_par_sep)
```

## ALSPAC - Extract results for externalising problems into a dataframe

``` r
## Extract results (estimate, low CI, upper CI [plus SE for proportions])

# Proportion explained by polygenic scores
prop <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                        fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 57, "prop")
# Adjusted effect
adjusted <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                            fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 59, "adjusted")
# Genetic confounding
confound <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                            fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 56, "confound") 
# Total effect
total <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                         fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 58, "total") 
# Generate dataframe
hyp2a_ext_alsp <- data.frame(prop, total, confound, adjusted)
hyp2a_ext_alsp <- subset(hyp2a_ext_alsp, select = -c(ACE.1, ACE.2, ACE.3)) # drop extra ACE variables
```

## ALSPAC - Aggregate results for proportions explained by polygenic scores

``` r
hyp2a_ext_alsp$var <- ((hyp2a_ext_alsp$prop_up_ci - hyp2a_ext_alsp$prop_low_ci)/3.92)^2 # get variance
hyp2a_ext_alsp$ni <- 6411 # sample size
hyp2a_ext_alsp$id <- 1

mean(hyp2a_ext_alsp$prop_est)

## Aggregate effect sizes based on non-transformed data
agg_2a <- agg(id=id, es=prop_est, var=var, method = "BHHR", cor = mean_ace_cor_alspac, mod=NULL, data=hyp2a_ext_alsp) 
## Extract p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
round((c(est_model, lowci_model, upci_model)),4)*100
```

## Remove models saved for ALSPAC data

``` r
rm(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych, fit_int_par_substance, fit_int_par_crim, fit_int_par_sep,
   fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych, fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep,
   confound, int, ext, nc_int, nc_ext, prop, total)
```

# ABCD - Specify SEM

``` r
multi_PGS_model_abcd = 
  '# Outcome is regressed onto polygenic scores (mediators) and ACE
    MH ~ b1*ADHD_PGS_r + b2*alcohol_PGS_r + b3*antisocial_PGS_r + b4*anxiety_PGS_r + b5*autism_PGS_r + b6*bipolar2019_PGS_r + b7*depression_PGS_r + b8*schizophrenia_PGS_r + cp * ACE_1
    # Polygenic scores are regressed onto ACEs
    ADHD_PGS_r ~ a1 * ACE_1
    alcohol_PGS_r ~ a2 * ACE_1
    antisocial_PGS_r ~ a3 * ACE_1
    anxiety_PGS_r ~ a4 * ACE_1
    autism_PGS_r ~ a5 * ACE_1
    bipolar2019_PGS_r ~ a6 * ACE_1
    depression_PGS_r ~ a7 * ACE_1
    schizophrenia_PGS_r ~ a8 * ACE_1
    
    # Covariances between polygenic scores
    ADHD_PGS_r ~~ cv12*alcohol_PGS_r
    ADHD_PGS_r ~~ cv13*antisocial_PGS_r
    ADHD_PGS_r ~~ cv14*anxiety_PGS_r
    ADHD_PGS_r ~~ cv15*autism_PGS_r
    ADHD_PGS_r ~~ cv16*bipolar2019_PGS_r
    ADHD_PGS_r ~~ cv17*depression_PGS_r
    ADHD_PGS_r ~~ cv18*schizophrenia_PGS_r
    alcohol_PGS_r ~~ cv23*antisocial_PGS_r
    alcohol_PGS_r ~~ cv24*anxiety_PGS_r
    alcohol_PGS_r ~~ cv25*autism_PGS_r
    alcohol_PGS_r ~~ cv26*bipolar2019_PGS_r
    alcohol_PGS_r ~~ cv27*depression_PGS_r
    alcohol_PGS_r ~~ cv28*schizophrenia_PGS_r
    antisocial_PGS_r ~~ cv34*anxiety_PGS_r
    antisocial_PGS_r ~~ cv35*autism_PGS_r
    antisocial_PGS_r ~~ cv36*bipolar2019_PGS_r
    antisocial_PGS_r ~~ cv37*depression_PGS_r
    antisocial_PGS_r ~~ cv38*schizophrenia_PGS_r
    anxiety_PGS_r ~~ cv45*autism_PGS_r
    anxiety_PGS_r ~~ cv46*bipolar2019_PGS_r
    anxiety_PGS_r ~~ cv47*depression_PGS_r
    anxiety_PGS_r ~~ cv48*schizophrenia_PGS_r
    autism_PGS_r ~~ cv56*bipolar2019_PGS_r
    autism_PGS_r ~~ cv57*depression_PGS_r
    autism_PGS_r ~~ cv58*schizophrenia_PGS_r
    bipolar2019_PGS_r ~~ cv67*depression_PGS_r
    bipolar2019_PGS_r ~~ cv68*schizophrenia_PGS_r
    depression_PGS_r ~~ cv78*schizophrenia_PGS_r
    
    # Variances
    MH ~~ start(1)*MH
    ADHD_PGS_r ~~ start(1)*ADHD_PGS_r
    alcohol_PGS_r ~~ start(1)*alcohol_PGS_r
    antisocial_PGS_r ~~ start(1)*antisocial_PGS_r
    anxiety_PGS_r ~~ start(1)*anxiety_PGS_r
    autism_PGS_r ~~ start(1)*autism_PGS_r
    bipolar2019_PGS_r ~~ start(1)*bipolar2019_PGS_r
    depression_PGS_r ~~ start(1)*depression_PGS_r
    schizophrenia_PGS_r ~~ start(1)*schizophrenia_PGS_r
    
    ## Estimate the indirect effects for each ACE
    indirect_PGS:= (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8)
    
    ## Estimate the proportion of the association mediated through the PGS for each ACE
    prop_PGSs:= ((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8))/
    ((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + cp)
    total:=((a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + cp)
    adjusted:=total-indirect_PGS'
```

## ABCD - Run models for internalising problems

``` r
# Specify that MH = internalising problems, with sex regressed out
# For each ACE, transform the multiply imputed dataset (a.out) to specify that ACE_1 = the particular ACE

## Maltreatment
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_internalising ~ sex, na.action=na.exclude))),
                   ACE_1 = maltreatment_r)
fit_int_maltreatment <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan", ordered="ACE_1")
# note: specify ordered=ACE_1 to avoid convergence problems
summary(fit_int_maltreatment) 

### Domestic violence
a.out_abcd <- transform(a.out_abcd, ACE_1 = domestic_violence)
fit_int_dom_violence <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_int_dom_violence) 

### Parental psychopathology
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_psychopathology)
fit_int_par_psych <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_int_par_psych) 

### Parental substance abuse
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_substance)
fit_int_par_substance <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_int_par_substance) 

### Parental criminality
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_criminality)
fit_int_par_crim <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan", ordered="ACE_1")
# note: specify ordered=ACE_1 to avoid convergence problems
summary(fit_int_par_crim) 

### Parental separation
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_separation)
fit_int_par_sep <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_int_par_sep)
```

## Define function to extract results (necessary to re-define function as maltreatment and parental criminality used ordered=ACE)

``` r
extract_res <- function(fit_maltreatment, 
                        fit_dom_violence,
                        fit_par_psych,
                        fit_par_substance,
                        fit_par_crim,
                        fit_par_sep, 
                        nrow,
                        nrow_orderedACE, # number row when ACE is declared as ordered (use for maltreatment and par crim)
                        label) {
  est <- c(summary(fit_maltreatment, ci=TRUE)[nrow_orderedACE, 5], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 5], 
              summary(fit_par_psych, ci=TRUE)[nrow, 5],
              summary(fit_par_substance, ci=TRUE)[nrow, 5],
              summary(fit_par_crim, ci=TRUE)[nrow_orderedACE, 5],
              summary(fit_par_sep, ci=TRUE)[nrow, 5])
  low_ci <- c(summary(fit_maltreatment, ci=TRUE)[nrow_orderedACE, 10], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 10], 
              summary(fit_par_psych, ci=TRUE)[nrow, 10],
              summary(fit_par_substance, ci=TRUE)[nrow, 10],
              summary(fit_par_crim, ci=TRUE)[nrow_orderedACE, 10],
              summary(fit_par_sep, ci=TRUE)[nrow, 10])
  up_ci <- c(summary(fit_maltreatment, ci=TRUE)[nrow_orderedACE, 11], 
              summary(fit_dom_violence, ci=TRUE)[nrow, 11], 
              summary(fit_par_psych, ci=TRUE)[nrow, 11],
              summary(fit_par_substance, ci=TRUE)[nrow, 11],
              summary(fit_par_crim, ci=TRUE)[nrow_orderedACE, 11],
              summary(fit_par_sep, ci=TRUE)[nrow, 11])
  ACE <- c("Maltreatment", "Domestic violence", "Parental mental illness", 
           "Parental substance abuse", "Parental criminality", "Parental separation")
  df <- data.frame(ACE, est, low_ci, up_ci)
  colnames(df)[2:4] <- paste0(label, "_", colnames(df))[2:4] # label columns by the type of result
  return(df)
}
```

## ABCD - Extract results for internalising problems into a dataframe

``` r
## Extract results (estimate, low CI, upper CI [plus SE for proportions])

# Proportion explained by polygenic scores
prop <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                        fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 57, 67, "prop")
# Adjusted effect
adjusted <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                            fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 59, 69, "adjusted")
# Genetic confounding
confound <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                            fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 56, 66, "confound") 
# Total effect
total <- extract_res(fit_int_maltreatment, fit_int_dom_violence, fit_int_par_psych,
                         fit_int_par_substance, fit_int_par_crim, fit_int_par_sep, 58, 68, "total") 
# Generate dataframe
int_abcd <- data.frame(prop, total, confound, adjusted)
int_abcd <- subset(int_abcd, select = -c(ACE.1, ACE.2, ACE.3)) # drop extra ACE variables
int_abcd
```

### ABCD - Get correlation between ACEs for aggregate model

``` r
library(tidyverse)
library(broom)
# Combine imputed datasets into one big data frame with bind_rows(), following guidance: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
all_imputations_abcd <- bind_rows(unclass(a.out_abcd$imputations), .id = "m") %>%
  group_by(m) 

# Run correlation on ACE variables
library(polycor)
ace_cor_abcd <- hetcor(all_imputations_abcd[,c("maltreatment_r", 
                                     "domestic_violence", 
                                     "parental_psychopathology", 
                                     "parental_substance", 
                                     "parental_criminality", 
                                     "parental_separation")])$correlations
# Get the mean ACE correlation excluding the diagonal
ace_cor_nodiag_abcd <- ace_cor_abcd
diag(ace_cor_nodiag_abcd) <- NA
mean_ace_cor_abcd <- mean(ace_cor_nodiag_abcd, na.rm=TRUE) # r=0.31
mean_ace_cor_abcd
```

## ABCD - Aggregate results for proportions explained by polygenic scores

``` r
int_abcd$var <- ((int_abcd$prop_up_ci - int_abcd$prop_low_ci)/3.92)^2 # get variance
int_abcd$ni <- nrow(a.out_abcd$imputations$imp1) # specify sample size
int_abcd$id <- 1

# check mean of raw proportions
mean(int_abcd$prop_est)

## Aggregate effect sizes 
agg_2a <- agg(id=id, es=prop_est, var=var, method = "BHHR", cor = mean_ace_cor_abcd, mod=NULL, data=int_abcd) 
## Extract p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
round((c(est_model, lowci_model, upci_model)),4)*100

# Save results
hyp2a_int_abcd <- int_abcd
```

## ABCD - Run models for externalising problems

``` r
## Maltreatment
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_externalising ~ sex, na.action=na.exclude))), # MH = externalising, sex regressed out
                   ACE_1 = maltreatment_r)
fit_ext_maltreatment <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan", ordered="ACE_1") 
# note: specify ordered=ACE_1 to avoid convergence problems
summary(fit_ext_maltreatment)

### Domestic violence
a.out_abcd <- transform(a.out_abcd, ACE_1 = domestic_violence)
fit_ext_dom_violence <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_ext_dom_violence)

### Parental psychopathology
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_psychopathology)
fit_ext_par_psych <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_ext_par_psych)

### Parental substance abuse
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_substance)
fit_ext_par_substance <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_ext_par_substance)

### Parental criminality
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_criminality)
fit_ext_par_crim <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan", ordered="ACE_1")
# note: specify ordered=ACE_1 to avoid convergence problems
summary(fit_ext_par_crim)

### Parental separation
a.out_abcd <- transform(a.out_abcd, ACE_1 = parental_separation)
fit_ext_par_sep <- runMI(multi_PGS_model_abcd, data=a.out_abcd$imputations, fun="lavaan")
summary(fit_ext_par_sep)
```

## ABCD - Extract results for externalising problems into a dataframe

``` r
## Extract results (estimate, low CI, upper CI [plus SE for proportions])
# Proportion explained by polygenic scores
prop <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                        fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 57, 67, "prop")
# Adjusted effect
adjusted <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                            fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 59, 69, "adjusted")
# Genetic confounding
confound <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                            fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 56, 66, "confound") 
# Total effect
total <- extract_res(fit_ext_maltreatment, fit_ext_dom_violence, fit_ext_par_psych,
                         fit_ext_par_substance, fit_ext_par_crim, fit_ext_par_sep, 58, 68, "total") 
# Generate dataframe
ext_abcd <- data.frame(prop, total, confound, adjusted)
ext_abcd <- subset(ext_abcd, select = -c(ACE.1, ACE.2, ACE.3)) # drop extra ACE variables
ext_abcd
```

## ABCD -Aggregate results for proportions explained by polygenic scores

``` r
ext_abcd$var <- ((ext_abcd$prop_up_ci - ext_abcd$prop_low_ci)/3.92)^2 # get variance
ext_abcd$ni <- nrow(a.out_abcd$imputations$imp1) # specify sample size
ext_abcd$id <- 1

## Aggregate effect sizes 
agg_2a <- agg(id=id, es=prop_est, var=var, method = "BHHR", cor = mean_ace_cor_abcd, data=ext_abcd) 
## Extract p-value and other estimates
est_model <- agg_2a$es
var_model <- agg_2a$var
se_model <- sqrt(agg_2a$var)
lowci_model <- est_model - 1.96*se_model
upci_model <- est_model + 1.96*se_model
round((c(est_model, lowci_model, upci_model)),4)*100

# Save results
hyp2a_ext_abcd <- ext_abcd
```

# Hypothesis 2B

## Define Gsens function for proportions

``` r
gsensY_prop <- function (rxy, rgx, rgy, n, h2, constrain = NULL, print = TRUE) 
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
                             pe[pe$label == "prop", ]))[, 5:10] # Extract proportion
  results = results %>% mutate_if(is.numeric, round, 3)
  rownames(results) = c("Adjusted Bxy", "Genetic confounding", 
                        "Total effect", "Prop")
  results
}
```

## Define functions for formatting Gsens results

``` r
## Function to put results into a table 
format_gsens_res <- function(gsens_mal, gsens_dom_vi, gsens_par_psych,
                             gsens_par_sub, gsens_par_crim, gsens_par_sep) {
  gsens_res <- rbind(gsens_mal[1:4,c(1:2,5:6)],
                     gsens_dom_vi[1:4,c(1:2,5:6)],
                     gsens_par_psych[1:4,c(1:2,5:6)],
                     gsens_par_sub[1:4,c(1:2,5:6)],
                     gsens_par_crim[1:4,c(1:2,5:6)],
                     gsens_par_sep[1:4,c(1:2,5:6)])
  gsens_res$ACE <-  c(rep("maltreatment", 4),
                      rep("domestic_violence",4),
                      rep("par_psych", 4),
                      rep("par_substance", 4),
                      rep("par_criminal", 4),
                      rep("par_sep", 4))
  # Convert to wide dataframe
  gsens_res <- setDT(gsens_res, keep.rownames = TRUE)[] # make rownames a variable
  gsens_res$rn <- gsub('[[:digit:]]+', '', gsens_res$rn)
  gsens_res <- reshape(gsens_res, idvar = "ACE", timevar = "rn", direction = "wide")
  return(gsens_res)
}

#### Function to aggregate proportions across ACEs
agg_res <- function(data, corr_aces, N) {
  library(metafor)
  # derive variance of the proportion
  data$Prop_var <- data$se.Prop^2
  # Derive variable indexing N of sample
  data$ni <- N
  # Derive ID variable (for later aggregation)
  data$id <- 1
  
  # Aggregate effect sizes 
  agg_2b <- agg(id = id, es = est.Prop, var = Prop_var, method = "BHHR", cor = corr_aces, mod = NULL, data = data) 
  
  # Extract transformed estimates
  est_model <- agg_2b$es
  var_model <- agg_2b$var
  se_model <- sqrt(agg_2b$var)
  lowci_model <- est_model - 1.96*se_model
  upci_model <- est_model + 1.96*se_model
  
  return(c(est_model, lowci_model, upci_model))
}
```

## ALSPAC - Step 1. Obtain correlation between the ACE and the mental health outcome (c path)

``` r
# To run Gsens, need to obtain 3 sets of correlations:
# 1. Correlation between the ACE and the mental health outcome
# 2. Correlation between the observed polygenic scores and the ACE
# 3. Correlation between the observed polygenic scores and the mental health outcome

# To get the correlation between the ACE and mental health outcome, take 2 steps:
# - 1. Run a linear regression model predicting the mental health outcome from the ACE 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the ACE
# Note. All models will be estimated using the lavaan package, accounting for sex and principal components. 

# Specify lavaan model
reg_mh_ace <- '
# Regression
MH ~ ACE
# Variance in outcome
MH ~~ MH
# Intercept
MH ~ 1'

#### Internalising problems - make MH = internalising problems with age and sex regressed out
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(internalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))
# Maltreatment analyses
a.out_alspac <- transform(a.out_alspac, ACE = maltreatment_0_9.5yrs) #transform so MH = externalising & ACE = maltreatment
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Domestic violence analyses
a.out_alspac <- transform(a.out_alspac, ACE = violence_between_parents_0_9.5yrs) #transform so ACE = domestic violence
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental psychopathology analyses
a.out_alspac <- transform(a.out_alspac, ACE = mental_health_problems_or_suicide_0_9.5yrs) #transform so ACE = parental psychopathology
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental substance abuse analyses
a.out_alspac <- transform(a.out_alspac, ACE = substance_household_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental criminality analyses
a.out_alspac <- transform(a.out_alspac, ACE = parent_convicted_offence_0_9.5yrs) #transform so ACE = parental criminality
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental separation analyses
a.out_alspac <- transform(a.out_alspac, ACE = parental_separation_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[6,5] # extract results

r2_int.df <- data.frame(ACE = c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"),
                        r2 = c(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep)) # make dataframe with results

rm(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep) # remove r2 objects

#### Repeat for externalising
# Specify that MH = externalising problems (with sex + 10 PCs regressed out)
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(externalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))

# Maltreatment analyses
a.out_alspac <- transform(a.out_alspac, ACE = maltreatment_0_9.5yrs) #transform so MH = externalising & ACE = maltreatment
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Domestic violence analyses
a.out_alspac <- transform(a.out_alspac, ACE = violence_between_parents_0_9.5yrs) #transform so ACE = domestic violence
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental psychopathology analyses
a.out_alspac <- transform(a.out_alspac, ACE = mental_health_problems_or_suicide_0_9.5yrs) #transform so ACE = parental psychopathology
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental substance abuse analyses
a.out_alspac <- transform(a.out_alspac, ACE = substance_household_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental criminality analyses
a.out_alspac <- transform(a.out_alspac, ACE = parent_convicted_offence_0_9.5yrs) #transform so ACE = parental criminality
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental separation analyses
a.out_alspac <- transform(a.out_alspac, ACE = parental_separation_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[6,5] # extract results

# make dataframe with results
r2_ext.df <- data.frame(ACE = c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"),
                        r2 = c(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep)) 

# Combine dataframes for the associations between ACEs and internalising and externalising problems 
ace_mh_alspac <- rbind(r2_int.df, r2_ext.df)
ace_mh_alspac$outcome <- c(rep("int", 6), rep("ext", 6)) # add outcome variable
ace_mh_alspac$r <- sqrt(ace_mh_alspac$r2) # calculate r from r2
ace_mh_alspac

rm(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep, r2_ext.df, r2_int.df) # remove r2 objects
```

## ALSPAC - Step 2. Obtain correlation between observed polygenic scores for mental health problems and the ACE (a path)

``` r
# - 1. Run a probit regression model predicting the ACE from the polygenic scores 
# - 2. Take the square root of the R2 value reflecting the variance in the (latent-response) ACE variable explained by the polygenic scores

# Specify lavaan model
reg_ace_pgs <- "
  # Regression  
  ACE ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar2019_PGS_r + depression_PGS_r + schizophrenia_PGS_r
  "

# Standardise polygenic scores (which are already residualised for sex and 10 PCs in imputed data)
a.out_alspac <- transform(a.out_alspac, ADHD_PGS_r = scale(ADHD_PGS_r), 
                   alcohol_PGS_r = scale(alcohol_PGS_r),
                   antisocial_PGS_r = scale(antisocial_PGS_r),
                   anxiety_PGS_r = scale(anxiety_PGS_r),
                   autism_PGS_r = scale(autism_PGS_r),
                   bipolar2019_PGS_r = scale(bipolar2019_PGS_r),
                   depression_PGS_r = scale(depression_PGS_r),
                   schizophrenia_PGS_r = scale(schizophrenia_PGS_r)) 

#### Run across ACE types

# Maltreatment analyses
a.out_alspac <- transform(a.out_alspac, ACE = maltreatment_0_9.5yrs) #transform so ACE = maltreatment
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Domestic violence analyses
a.out_alspac <- transform(a.out_alspac, ACE = violence_between_parents_0_9.5yrs) #transform so ACE = domestic violence
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental substance abuse analyses
a.out_alspac <- transform(a.out_alspac, ACE = substance_household_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental psychopathology analyses
a.out_alspac <- transform(a.out_alspac, ACE = mental_health_problems_or_suicide_0_9.5yrs) #transform so ACE = parental psychopathology
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental criminality analyses
a.out_alspac <- transform(a.out_alspac, ACE = parent_convicted_offence_0_9.5yrs) #transform so ACE = parental criminality
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental separation analyses
a.out_alspac <- transform(a.out_alspac, ACE = parental_separation_0_9.5yrs) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Return results
pgs_r2 <- c(r2_mal, r2_domvi, r2_parpsych, r2_parsub, r2_parcrim, r2_parsep)
pgs_ace_alspac <- data.frame(ACE=c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"), pgs_r2)
pgs_ace_alspac$r <- sqrt(pgs_ace_alspac$pgs_r2)
```

## ALSPAC - Step 3. Obtain correlation between observed polygenic scores and the mental health outcome (b path)

``` r
# - 1. Run a linear regression model predicting the mental health outcome from the polygenic scores for mental health problems 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the polygenic scores

# Specify lavaan model
reg_mh_pgs <- '
# Regression  
MH ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar2019_PGS_r + depression_PGS_r + schizophrenia_PGS_r
# Variance
MH ~~ MH
# Intercept
MH ~ 1
'

## Run model for PGSs and internalising problems
# Specify that MH = internalising problems (with sex + 10 PCs regressed out)
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(internalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude)))) # regress sex + 10 PCs out of internalising
fit <- runMI(reg_mh_pgs, data=a.out_alspac$imputations, fun="lavaan", ordered="ACE") # fit lavaan model
r2_int <- summary(fit, rsquare=TRUE)[55,5] # extract results

## Run model for PGSs and externalising problems
# Specify that MH = externalising problems (with sex + 10 PCs regressed out)
a.out_alspac <- transform(a.out_alspac, MH = scale(residuals(lm(externalising10 ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude)))) # regress sex + 10 PCs out of externalising
fit <- runMI(reg_mh_pgs, data=a.out_alspac$imputations, fun="lavaan") # fit lavaan model
r2_ext <- summary(fit, rsquare=TRUE)[55,5] # extract results

# Combine results into dataframe
pgs_mh_alspac <- data.frame(outcome = c("int", "ext"), mh_pgs_r2 = c(r2_int, r2_ext))
pgs_mh_alspac$r <- sqrt(pgs_mh_alspac$mh_pgs_r2)
pgs_mh_alspac
```

## ALSPAC - Run Gsens for internalising problems

``` r
# Maltreatment
gsens_int_mal <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="maltreatment" & ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="maltreatment"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Domestic violence 
gsens_int_dom_vi <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="domestic_violence" & ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="domestic_violence"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental psychopathology
gsens_int_par_psych <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_psych" &  ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_psych"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental substance abuse
gsens_int_par_sub <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_substance" & ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_substance"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental criminality
gsens_int_par_crim <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_criminal" & ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_criminal"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental separation
gsens_int_par_sep <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_sep" & ace_mh_alspac$outcome=="int"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_sep"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="int"],
    n = 6411,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 
```

## ALSPAC - Run Gsens for externalising problems

``` r
# Maltreatment
gsens_ext_mal <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="maltreatment" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="maltreatment"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Domestic violence
gsens_ext_dom_vi <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="domestic_violence" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="domestic_violence"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental psychopathology
gsens_ext_par_psych <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_psych" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_psych"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental substance abuse
gsens_ext_par_sub <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_substance" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_substance"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental criminality
gsens_ext_par_crim <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_criminal" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_criminal"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental separation
gsens_ext_par_sep <- 
  gsensY_prop(
    rxy = ace_mh_alspac$r[ace_mh_alspac$ACE=="par_sep" & ace_mh_alspac$outcome=="ext"],
    rgx = pgs_ace_alspac$r[pgs_ace_alspac$ACE=="par_sep"],
    rgy = pgs_mh_alspac$r[pgs_mh_alspac$outcome=="ext"],
    n = 6411,
    h2 = 0.09, 
    constrain = 'bgx*bgy < bgx*bgy+bxy') 
```

## ALSPAC - Pool gsens results using all PGSs and format for table/plot

``` r
## Internalising problems
# Apply function to extract gsens results into a table
gsens_int_alspac <- format_gsens_res(gsens_int_mal, gsens_int_dom_vi, gsens_int_par_psych,
                              gsens_int_par_sub, gsens_int_par_crim, gsens_int_par_sep)

# Apply function to aggregate gsens results
round(agg_res(gsens_int_alspac, mean_ace_cor_alspac, 6411),4)*100 


## Externalising problems
# Apply function to extract gsens results into a table
gsens_ext_alspac <- format_gsens_res(gsens_ext_mal, gsens_ext_dom_vi, gsens_ext_par_psych,
                              gsens_ext_par_sub, gsens_ext_par_crim, gsens_ext_par_sep)
# Apply function to aggregate gsens results
round(agg_res(gsens_ext_alspac, mean_ace_cor_alspac, 6411),4)*100
```

## ABCD - Repeat Gsens analyses in ABCD using all PGSs

``` r
# Remove objects and results derived in ALSPAC
rm(fit, r2_mal, r2_domvi, r2_parcrim, r2_parpsych, r2_parsep, r2_parsub, r2_int, r2_ext,
   reg_ace_pgs, reg_mh_ace, reg_mh_pgs,
   pgs_r2,
   gsens_int_mal, gsens_int_dom_vi, gsens_int_par_psych, gsens_int_par_sub, 
   gsens_int_par_crim, gsens_int_par_sep,
   gsens_ext_mal, gsens_ext_dom_vi, gsens_ext_par_psych, gsens_ext_par_sub, 
   gsens_ext_par_crim, gsens_ext_par_sep)
```

### ABCD - Get correlation between ACEs for aggregate model

``` r
# Combine imputed datasets into one big data frame with bind_rows(), following guidance: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
all_imputations_abcd <- bind_rows(unclass(a.out_alspac_abcd$imputations), .id = "m") %>%
  group_by(m) 

# Run correlation on ACE variables
ace_cor_abcd <- hetcor(all_imputations_abcd[,c("maltreatment_r", 
                                     "domestic_violence", 
                                     "parental_psychopathology", 
                                     "parental_substance", 
                                     "parental_criminality", 
                                     "parental_separation")])$correlations
# Get the mean ACE correlation excluding the diagonal
ace_cor_nodiag_abcd <- ace_cor_abcd
diag(ace_cor_nodiag_abcd) <- NA
mean_ace_cor_abcd <- mean(ace_cor_nodiag_abcd, na.rm=TRUE) # r=0.31
mean_ace_cor_abcd
```

## ABCD - Step 1. Obtain correlation between the ACE and the mental health outcome

``` r
# - 1. Run a linear regression model predicting the mental health outcome from the ACE 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the ACE
# Note. All models will be estimated using the lavaan package, accounting for sex and principal components. 

# Specify lavaan model
reg_mh_ace <- '
# Regression
MH ~ ACE
# Variance in outcome
MH ~~ MH
# Intercept
MH ~ 1'

#### Internalising problems - make MH = internalising problems with age and sex regressed out
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_internalising ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))
# Maltreatment analyses
a.out_abcd <- transform(a.out_abcd, ACE = maltreatment_r) #transform so MH = externalising & ACE = maltreatment
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Domestic violence analyses
a.out_abcd <- transform(a.out_abcd, ACE = domestic_violence) #transform so ACE = domestic violence
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental psychopathology analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_psychopathology) #transform so ACE = parental psychopathology
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental substance abuse analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_substance) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental criminality analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_criminality) #transform so ACE = parental criminality
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental separation analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_separation) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[6,5] # extract results

# make dataframe with results
r2_int.df <- data.frame(ACE = c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"),
                        r2 = c(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep)) 
r2_int.df
rm(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep) # remove r2 objects

#### Repeat for externalising
# Specify that MH = externalising problems (with sex + 10 PCs regressed out)
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_externalising ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude))))

# Maltreatment analyses
a.out_abcd <- transform(a.out_abcd, ACE = maltreatment_r) #transform so MH = externalising & ACE = maltreatment
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Domestic violence analyses
a.out_abcd <- transform(a.out_abcd, ACE = domestic_violence) #transform so ACE = domestic violence
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental psychopathology analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_psychopathology) #transform so ACE = parental psychopathology
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental substance abuse analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_substance) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental criminality analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_criminality) #transform so ACE = parental criminality
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[6,5] # extract results

# Parental separation analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_separation) #transform so ACE = parental substance abuse
fit <- runMI(reg_mh_ace, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[6,5] # extract results

# make dataframe with results
r2_ext.df <- data.frame(ACE = c("maltreatment", "domestic_violence", "par_psych", "par_substance", "par_criminal", "par_sep"),
                        r2 = c(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep)) 
r2_ext.df
rm(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep) # remove r2 objects

# Combine dataframes for the associations between ACEs and internalising and externalising problems 
ace_mh_abcd <- rbind(r2_int.df, r2_ext.df)
ace_mh_abcd$outcome <- c(rep("int", 6), rep("ext", 6)) # add outcome variable
ace_mh_abcd$r <- sqrt(ace_mh_abcd$r2) # calculate r from r2
ace_mh_abcd
```

## ABCD - Step 2. Obtain correlation between observed polygenic scores for mental health problems and the ACE (a path)

``` r
# - 1. Run a probit regression model predicting the ACE from the polygenic scores 
# - 2. Take the square root of the R2 value reflecting the variance in the (latent-response) ACE variable explained by the polygenic scores

# Specify lavaan model
reg_ace_pgs <- "
  # Regression  
  ACE ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar2019_PGS_r + depression_PGS_r + schizophrenia_PGS_r
  "

# Standardise polygenic scores (which are already residualised for sex and 10 PCs in imputed data)
a.out_abcd <- transform(a.out_abcd, ADHD_PGS_r = scale(ADHD_PGS_r), 
                   alcohol_PGS_r = scale(alcohol_PGS_r),
                   antisocial_PGS_r = scale(antisocial_PGS_r),
                   anxiety_PGS_r = scale(anxiety_PGS_r),
                   autism_PGS_r = scale(autism_PGS_r),
                   bipolar2019_PGS_r = scale(bipolar2019_PGS_r),
                   depression_PGS_r = scale(depression_PGS_r),
                   schizophrenia_PGS_r = scale(schizophrenia_PGS_r)) 

#### Run across ACE types

# Maltreatment analyses
a.out_abcd <- transform(a.out_abcd, ACE = maltreatment_r) #transform so ACE = maltreatment
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_mal <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Domestic violence analyses
a.out_abcd <- transform(a.out_abcd, ACE = domestic_violence) #transform so ACE = domestic violence
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_domvi <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental psychopathology analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_psychopathology) #transform so ACE = parental psychopathology
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parpsych <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental substance abuse analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_substance) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsub <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental criminality analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_criminality) #transform so ACE = parental criminality
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parcrim <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Parental separation analyses
a.out_abcd <- transform(a.out_abcd, ACE = parental_separation) #transform so ACE = parental substance abuse
fit <- runMI(reg_ace_pgs, data=a.out_abcd$imputations, fun="lavaan", ordered=c("ACE"), estimator = "WLSMV") # fit lavaan model
r2_parsep <- summary(fit, rsquare=TRUE)[57,5] # extract results

# Return results
pgs_r2 <- c(r2_mal, r2_domvi, r2_parsub, r2_parpsych, r2_parcrim, r2_parsep)
pgs_ace_abcd <- data.frame(ACE=c("maltreatment", "domestic_violence", "par_substance", "par_psych", "par_criminal", "par_sep"), pgs_r2)
pgs_ace_abcd$r <- sqrt(pgs_ace_abcd$pgs_r2)
pgs_ace_abcd
rm(r2_mal, r2_domvi, r2_parpsych, r2_parsub,  r2_parcrim, r2_parsep) # remove r2 objects
```

## ABCD - Step 3. Obtain correlation between observed polygenic scores and the mental health outcome (b path)

``` r
# - 1. Run a linear regression model predicting the mental health outcome from the polygenic scores for mental health problems 
# - 2. Take the square root of the R2 value reflecting the variance in the mental health outcome explained by the polygenic scores

# Specify lavaan model
reg_mh_pgs <- '
# Regression  
MH ~ ADHD_PGS_r + alcohol_PGS_r + antisocial_PGS_r + anxiety_PGS_r + autism_PGS_r + bipolar2019_PGS_r + depression_PGS_r + schizophrenia_PGS_r
# Variance
MH ~~ MH
# Intercept
MH ~ 1
'

## Run model for PGSs and internalising problems
# Specify that MH = internalising problems (with sex + 10 PCs regressed out)
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_internalising ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude)))) # regress sex + 10 PCs out of internalising
fit <- runMI(reg_mh_pgs, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_int <- summary(fit, rsquare=TRUE)[55,5] # extract results

## Run model for PGSs and externalising problems
# Specify that MH = externalising problems (with sex + 10 PCs regressed out)
a.out_abcd <- transform(a.out_abcd, MH = scale(residuals(lm(cbcl_externalising ~ sex + PC1 + PC2 + PC3 + PC4 + 
                                                    PC5 + PC6 + PC7 + PC8 + PC9 + PC10, na.action=na.exclude)))) # regress sex + 10 PCs out of externalising
fit <- runMI(reg_mh_pgs, data=a.out_abcd$imputations, fun="lavaan") # fit lavaan model
r2_ext <- summary(fit, rsquare=TRUE)[55,5] # extract results

# Combine results into dataframe
pgs_mh_abcd <- data.frame(outcome = c("int", "ext"),
                          mh_pgs_r2 = c(r2_int, r2_ext))
pgs_mh_abcd$r <- sqrt(pgs_mh_abcd$mh_pgs_r2)
pgs_mh_abcd
```

## ABCD - Run Gsens for internalising problems

``` r
# Maltreatment 
gsens_int_mal <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="maltreatment" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="maltreatment"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Domestic violence
gsens_int_dom_vi <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="domestic_violence" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="domestic_violence"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental psychopathology
gsens_int_par_psych <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_psych" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_psych"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental substance abuse
gsens_int_par_sub <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_substance" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_substance"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental criminality
gsens_int_par_crim <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_criminal" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_criminal"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental separation
gsens_int_par_sep <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_sep" & ace_mh_abcd$outcome=="int"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_sep"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="int"],
    n = 4996,
    h2 = 0.06,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 
```

## ABCD - Run Gsens for externalising problems

``` r
# Maltreatment
gsens_ext_mal <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="maltreatment" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="maltreatment"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Domestic violence
gsens_ext_dom_vi <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="domestic_violence" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="domestic_violence"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental psychopathology
gsens_ext_par_psych <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_psych" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_psych"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental substance abuse
gsens_ext_par_sub <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_substance" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_substance"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental criminality
gsens_ext_par_crim <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_criminal" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_criminal"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 

# Parental separation
gsens_ext_par_sep <- 
  gsensY_prop(
    rxy = ace_mh_abcd$r[ace_mh_abcd$ACE=="par_sep" & ace_mh_abcd$outcome=="ext"],
    rgx = pgs_ace_abcd$r[pgs_ace_abcd$ACE=="par_sep"],
    rgy = pgs_mh_abcd$r[pgs_mh_abcd$outcome=="ext"],
    n = 4996,
    h2 = 0.09,
    constrain = 'bgx*bgy < bgx*bgy+bxy') 
```

## ABCD - Pool gsens results

``` r
## Internalising problems
# Apply function to extract gsens results into a table
gsens_int_abcd <- format_gsens_res(gsens_int_mal, gsens_int_dom_vi, gsens_int_par_psych,
                              gsens_int_par_sub, gsens_int_par_crim, gsens_int_par_sep)

# Apply function to aggregate gsens results
round(agg_res(gsens_int_abcd, mean_ace_cor_abcd, 4996),4)*100 

## Externalising problems
# Apply function to extract gsens results into a table
gsens_ext_abcd <- format_gsens_res(gsens_ext_mal, gsens_ext_dom_vi, gsens_ext_par_psych,
                              gsens_ext_par_sub, gsens_ext_par_crim, gsens_ext_par_sep)
# Apply function to aggregate gsens results
round(agg_res(gsens_ext_abcd, mean_ace_cor_abcd, 4996),4)*100
```
