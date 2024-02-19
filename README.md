Adverse childhood experiences and mental health: A genetically
informative study
================

# Description

The following documentation provides scripts for all analytical steps
involved in the Registered Report on “Adverse childhood experiences and
mental health: A genetically informative study”. The scripts can be run
in R and shell.

## Table of Contents

1.  [QC of GWAS summary statistics](#qc_sumstats)
2.  [QC ALSPAC genetic data and derive polygenic scores](#qc_alspac)
3.  [QC ABCD genetic data and derive polygenic scores](#qc_abcd)
4.  [Derive ACE measures in ALSPAC](#ace_alspac)
5.  [Derive mental health measures in ALSPAC](#mh_alspac)
6.  [Derive all measures in the ABCD Study](#measures_abcd)
7.  [Multiple imputation of ALSPAC data](#imputation_alspac)
8.  [Multiple imputation of ABCD data](#imputation_abcd)
9.  [Main analysis on ALSPAC and ABCD - imputed data](#main_analysis)
10. [Sensitivity analysis on ALSPAC and ABCD - complete
    cases](#complete_cases)
11. [Sensitivity analysis on ALSPAC and ABCD using alternate GWAS
    summary statistics](#stah_bipolarPGS)

# [1. QC of GWAS summary statistics](#qc_sumstats)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/1_QC_SumStats_20220205.md)
performs quality control (QC) on the GWAS summary statistics used to
derive polygenic scores for this study.

# [2. QC ALSPAC genetic data and derive polygenic scores](#qc_alspac)

Two scripts are used to QC ALSPAC genetic data and derive polygenic scores. The first [script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/2_ALSPAC_QC_genetic_20220204.sh)
performs QC of the ALSPAC genotype data (for children only), using Plink
and King software. The second [script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/2b_ALSPAC_derive_PGS_20210607.sh) derives polygenic scores for mental health problems and negative controls.

# [3. QC ABCD genetic data and derive polygenic scores](#qc_abcd)

Two scripts are used to QC ABCD genetic data and derive polygenic scores.This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/3_ABCD_QC_genetic_20220204.sh)
performs QC of the ABCD genotype data (release 3), using Plink and King
software. The second [script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/3b_ABCD_derive_PGS_20220811.sh) derives polygenic scores for mental health problems and negative controls.

# [4. Derive ACE measures in ALSPAC](#ace_alspac)

Two scripts are used to derive ACE measures in ALSPAC. The first
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/4a_ALSPAC_DeriveACEs_script1_20220204.R)
recodes all items used to derive ACE measures into binary constructs.
The second
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/4b_ALSPAC_DeriveACEs_script2_20220204.R)
generates the binary ACE variables used as the main exposures in the
study and as auxiliary variables in the imputation model.

# [5. Derive mental health measures in ALSPAC](#mh_alspac)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/5_ALSPAC_DeriveMHmeasures_20220204.R)
derives measures of internalising and externalising problems in ALSPAC
children using items from the DAWBA.

# [6. Derive all measures in the ABCD Study](#measures_abcd)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/6_ABCD_DeriveMeasures_20220203.R)
derives measures of ACEs, internalising and externalising problems, and
auxiliary variables in the ABCD Study.

# [7. Multiple imputation of ALSPAC data](#imputation_alspac)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/7_ALSPAC_imputation_20220204.R)
performs multiple imputation on ALSPAC data.

# [8. Multiple imputation of ABCD data](#imputation_abcd)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/8_ABCD_MultipleImputation_20220203.R)
performs multiple imputation on ABCD data.

# [9. Main analysis on ALSPAC and ABCD - imputed data](#main_analysis)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/9_Imputed_analysis_ALSPAC_ABCD_20220131.md)
performs the main analysis (testing hypotheses 1a, 1b, 1c, 2a, and 2b)
on the imputed ALSPAC and ABCD Study data.

# [10. Sensitivity analysis on ALSPAC and ABCD - complete cases](#complete_cases)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/10_completeCase_ALSP_ABCD_20211222.md)
repeats the main analysis on the ALSPAC and ABCD Study complete case
(non-imputed) samples.

# [11. Sensitivity analysis on ALSPAC and ABCD using alternate GWAS summary statistics](#stah_bipolarPGS)

This
[script](https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/11_Imputed_Bipolar2019_ALSPAC_ABCD_20211220.md)
repeats the main analysis using alterantive GWAS summary statistics -
namely the Stahl et al. (2019) GWAS to derive the polygenic score for
bipolar disorder (instead of Mullins et al., 2021).
