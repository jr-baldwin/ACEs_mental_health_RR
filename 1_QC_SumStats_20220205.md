Quality control for GWAS summary statistics
================
Jessie Baldwin

-   [Access the cluster and open R](#access-the-cluster-and-open-r)
-   [Define function for QC of summary
    stats](#define-function-for-qc-of-summary-stats)
-   [Begin QC of GWAS summary
    statistics](#begin-qc-of-gwas-summary-statistics)
    -   [Major depressive disorder](#major-depressive-disorder)
    -   [Anxiety disorder](#anxiety-disorder)
    -   [Bipolar disorder (new)](#bipolar-disorder-new)
    -   [Bipolar disorder (old)](#bipolar-disorder-old)
    -   [Autism](#autism)
    -   [ADHD](#adhd)
    -   [Alcohol use disorder](#alcohol-use-disorder)
    -   [Antisocial behaviour](#antisocial-behaviour)
    -   [Schizophrenia](#schizophrenia)
    -   [Handedness](#handedness)
    -   [Cataracts](#cataracts)
-   [Create table summarising GWAS summary statistics QC
    process](#create-table-summarising-gwas-summary-statistics-qc-process)

## Access the cluster and open R

QC of summary stats will be performed on the Myriad cluster, but most of
the analyses will be in R.

``` bash
# ======= Connect via UCL VPN ================
cd /opt/cisco/anyconnect/bin
open vpn
# Type: "connect"
# copy the following: "connect vpn.ucl.ac.uk" 

# ======== Connect to Filezilla ==============
cd /Users/jessie/Dropbox/Programs #change to my directory
open -a FileZilla
# use the following host in FIlezilla:
# sftp://myriad.rc.ucl.ac.uk
# then fill username and password and port 22

# ======== Start interactive session ========
ssh ucjujb6@myriad.rc.ucl.ac.uk

# ======== Load R module ========
module unload -f compilers mpi gcc-libs
module load beta-modules
module load r/new

# ======== Open R and load packages========
R
library("data.table")
```

## Define function for QC of summary stats

This function first QCs the summary statistics, and then creates a table
reporting the information and SNPs removed at each stage. The function
requires 6 pieces of information:

-   **sumstats**: dataframe with the GWAS summary statistics
-   **outputs**: vector containing the file path to export results to
-   **pheno**: name of the phenotype
-   **SNP\_h2**: SNP heritability reported in the paper
-   **effect\_allele**: name of the effect allele, i.e., A1 or A2
-   **md5checksum**: md5checksum string for the GWAS summary statistics
    file

The QC of summary statistics includes the following steps:

-   Remove non-autosomal variants
-   Remove SNPs with INFO &lt; 0.8
-   Remove SNPs with MAF &lt;.01
-   Remove ambiguous SNPs
-   Remove variants with duplicate SNP IDs

The dataframe created for each GWAS summary statistic file contains the
following information:

-   Phenotype name
-   SNP heritability reported in the paper
-   Name of the effect allele
-   md5checksum code
-   Number of SNPs before QC
-   Number of non-autosomal SNPs removed
-   Number of SNPs with INFO&lt;.08 removed
-   Number of SNPs with MAF&lt;.01 removed
-   Number of ambiguous SNPs removed
-   Number of duplicate SNPs removed
-   Number of SNPs remaining after QC

Individual dataframes can be combined into one [overall summary
table](#table)

``` bash
QC_fun <- function(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum) {
  
# Keep only autosomal variants
if ("CHR" %in% colnames(sumstats)) {
      sumstats_aut <- sumstats[which(sumstats$CHR >= 1 & sumstats$CHR  <= 22),]
      print("CHR provided. Sex chromosomes removed")
    } else {
      print("Warning: CHR not provided. Please check that summary statistics do not contain sex chromosomes")
      sumstats_aut <- sumstats
    }
    
# Remove SNPs with INFO < .8 
if ("INFO" %in% colnames(sumstats_aut)) {
      sumstats_info <- sumstats_aut [which(sumstats_aut$INFO > 0.8),]
      print("INFO provided. SNPs with INFO < 0.8 removed")
    } else {
      print("Warning: INFO not provided. Please note info filter applied by GWAS authors")
      sumstats_info <- sumstats_aut
    }
    
# Filter on MAF <.01 (and MAF >.99 as A1 not minor allele)
if ("MAF" %in% colnames(sumstats_info)) {
      sumstats_maf <- sumstats_info [which(sumstats_info$MAF > 0.01 & sumstats_info$MAF < 0.99),]
      print("MAF provided. SNPs with MAF > 0.01 and < 0.99 removed")
    } else {
      print("Warning: MAF not provided. Please note MAF filter applied by GWAS authors")
      sumstats_maf <- sumstats_info
    }

# Remove ambiguous SNPs
sumstats_amb <- sumstats_maf[!(sumstats_maf$A1 == "A" & sumstats_maf$A2 == "T" |
                sumstats_maf$A1 == "T" & sumstats_maf$A2 =="A" | 
                sumstats_maf$A1 == "C" & sumstats_maf$A2 == "G" | 
                sumstats_maf$A1 == "G" & sumstats_maf$A2 =="C"),]

# Remove variants with duplicate SNP ids
sumstats_dedub <- sumstats_amb[!duplicated(sumstats_amb$SNP),] 

# Save QC'd substats
write.table(sumstats_dedub, paste0(outputs, "/", pheno, ".txt"), col.names=T, row.names=F,quote=F) # save as .txt

# Create dataframe summarising QC steps and noting SNPs removed at each step of QC
table <- matrix(NA, nrow = 1,ncol = 11)
colnames(table) <- c("phenotype", "heritability", "effect_allele", "md5checksum", 
              "nSPSs_pre_QC", "not_autosomal", "INFO<.08", "MAF<.01", 
              "ambiguous_SNPs", "duplicate_SNPs", "QC_positive_SNPs")
# Add information to the table on phenotype, SNP heritability, effect allele, md5checksum, and number of SNPs removed at each step
table[1,1:11] <- c(pheno, SNP_h2, effect_allele, md5checksum, 
              nrow(sumstats_raw), # no. of SNPs before QC (SPSs_pre_QC)
              nrow(sumstats_raw) - nrow(sumstats_aut), # no. of non-autosomal SNPs removed
              nrow(sumstats_aut) - nrow(sumstats_info), # no. of SNPs with INFO<.08 removed
              nrow(sumstats_info) - nrow(sumstats_maf), # no. of SNPs with MAF<.01 removed
              nrow(sumstats_maf) - nrow(sumstats_amb), # no. of ambiguous SNPs removed
              nrow(sumstats_amb) - nrow(sumstats_dedub), # no. of duplicateSNPs removed
              nrow(sumstats_dedub)) # no. of SNPs left after QC (QC positive_SNPs)
write.table(table, paste0(outputs, pheno, ".csv"), row.names = F, col.names = T)
return(table)
}
```

## Begin QC of GWAS summary statistics

QC will be conducted for the following summary statistics:

-   Major depressive disorder
-   Anxiety disorder
-   Bipolar disorder
-   Autism
-   ADHD
-   Alcohol use disorder
-   Antisocial behaviour
-   Schizophrenia
-   Handedness (negative control)
-   Cataracts (negative control)

For each phenotype, need to:

1.  Set file shortcuts
2.  Read in summary statistics
3.  Check datafile conforms to standard formatting (headers,
    capitalisation of allele names) <br/> 3a. If necessary, rename
    headers <br/> 3b. If necessary, capitalise allele names <br/>
4.  Report SNP heritability (reported in the original paper)
5.  Report the effect allele
6.  Run md5checksum to check file corruption
7.  Apply QC function to remove SNPs and write output to a table

### Major depressive disorder

Summary statistics are from [Howard et
al. (2019)](https://datashare.is.ed.ac.uk/handle/10283/3203).
Genome-wide summary statistics are from a meta-analysis of the 33
cohorts of the Psychiatric Genomics Consortium (excluding UK Biobank and
23andMe data) as described in [Wray et
al. (2018)](https://doi.org/10.1038/s41588-018-0090-3) and the broad
depression phenotype in the full release of the UK Biobank as described
in [Howard et al. (2018)](https://doi.org/10.1038/s41467-018-03819-3).
The total number of individuals in this data is 500,199 (170,756 cases
and 329,443 controls).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Depression" 
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/PGC_UKB_depression_genome-wide.txt")
pheno <- "Depression_Howard_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
# Missing: BP, CHR, MAF, INFO, Z, N
# Re-order and drop column "Freq"
sumstats <- sumstats_raw[,c('MarkerName', 'A1', 'A2', 'P', 'LogOR', 'StdErrLogOR')]
names(sumstats) <-c('SNP', 'A1', 'A2', 'P', 'BETA', 'SE') # rename (note log OR named as 'beta')
head(sumstats)

# 3b. Capitalise alleles if provided in small caps: 
# If not "A", "T", "C" or "G" subsequent code to remove ambigious variants will not work.
sumstats$A1 <- toupper(sumstats$A1)
sumstats$A2 <- toupper(sumstats$A2)
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: The SNP-based heritability of depression...was 0.089 
SNP_h2 <- 0.089

# 5. Report effect allele 
# Effect allele is reported to be A1 in the readme
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/PGC_UKB_depression_genome-wide.txt")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Anxiety disorder

Summary statistics are from [Purves et
al. (2019)](https://www.nature.com/articles/s41380-019-0559-1). The
total number of individuals in this data is 83,566 (25,453 cases and
58,113 controls).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Anxiety" 
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/TotAnx_effect_sumstats.gz")
pheno <- "Anxiety_Purves_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
# Note that effect refers to log OR, so name as beta
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('SNP', 'pos', 'chr', 'A1', 'A2', 'info', 'P', 'effect', 'SE')]
names(sumstats) <-c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'BETA', 'SE') # rename (note log OR named as 'beta')
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: Estimates of SNP heritability (h2SNP) converted to the liability threshold are 0.26 
SNP_h2 <- 0.26

# 5. Report effect allele 
# Effect allele is reported to be A2 ("test allele) in the readme
effect_allele <- "A2"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/TotAnx_effect_sumstats.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Bipolar disorder (new)

(new) summary statistics are from [Mullins et
al. (2021)](https://www.nature.com/articles/s41588-021-00857-4), with
the file found
[here](https://figshare.com/articles/dataset/PGC3_bipolar_disorder_GWAS_summary_statistics/14102594).
The total number of individuals in this data is 413,466 (41,917 cases
and 371,549 controls) with an effective N of 101,962.

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Bipolar"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/pgc-bip2021-all.vcf.tsv.gz")
pheno <- "Bipolar_Mullins_2021"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)
colnames(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
sumstats <- sumstats_raw[,c('ID', 'POS', 'X.CHROM', 'A1', 'A2', 'IMPINFO', 'PVAL', 'BETA', 'SE')]
names(sumstats) <-c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'BETA', 'SE') # rename (note log OR named as 'beta')
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report:  the ℎ2SNP of BD was estimated to be 18.6%... the liability scale
SNP_h2 <-  0.186

# 5. Report effect allele 
# Asked Niamh Mullins; Effect allele is reported to be A1 ("test allele) 
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/pgc-bip2021-all.vcf.tsv.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Bipolar disorder (old)

The older bipolar disorder GWAS summary statistics are from [Stahl et
al. (2019)](https://www.nature.com/articles/s41588-019-0397-8), with the
file found
[here](https://www.med.unc.edu/pgc/download-results/bip/?choice=Bipolar+Disorder+%28BIP%29Bipolar+Disorder+%28BIP%29).
The total number of individuals in this data is 51,710 (20,352 cases and
31,358 controls). Note that the ReadMe is not available on the PGC
website.

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Bipolar"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/daner_PGC_BIP32b_mds7a_0416a")
pheno <- "Bipolar_Stahl_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)
colnames(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
sumstats <- sumstats_raw[,c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'OR', 'SE')]
colnames(sumstats)
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: The LD score regression SNP heritability estimates for BD were 0.17–0.23 on the liability scale
SNP_h2 <- 0.17

# 5. Report effect allele 
# Effect allele is reported to be A1 ("test allele) in the readme
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/daner_PGC_BIP32b_mds7a_0416a")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Autism

Summary statistics are from [Grove et
al. (2019)](https://www.nature.com/articles/s41588-019-0344-8). The
total number of individuals in this data is 46,350 (18,381 cases and
27,969 controls).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Autism"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/iPSYCH-PGC_ASD_Nov2017.gz")
pheno <- "ASD_Grove_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'OR', 'SE')]
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: . The SNP heritability was estimated to be 0.118 
SNP_h2 <- 0.12

# 5. Report effect allele 
# Effect allele is reported to be A1 ("test allele) in the readme
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/iPSYCH-PGC_ASD_Nov2017.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### ADHD

Summary statistics are from [Demontis et
al. (2019)](https://www.nature.com/articles/s41588-018-0269-7.pdf?origin=ppub).
The total number of individuals in this data is 53,293 (19,099 cases and
34,194 controls, from the European sample only).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/ADHD"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/adhd_eur_jun2017.gz")
pheno <- "ADHD_Demontis_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'OR', 'SE')]
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: We estimated the liability-scale SNP heritability as h2SNP = 0.216
SNP_h2 <- 0.22

# 5. Report effect allele 
# Effect allele is reported to be A1 ("test allele) in the readme
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/adhd_eur_jun2017.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Alcohol use disorder

Summary statistics are from [Sanchez-Roige et
al. (2019)](https://ajp.psychiatryonline.org/doi/pdf/10.1176/appi.ajp.2018.18040369).
The total number of individuals in this data is 121,604.

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Alcohol"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/AUDIT_UKB_2018_AJP.txt.gz")
pheno <- "Alcohol_SanchezRoige_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))
# Note: Warning message: In fread(sumstats) : Discarded single-line footer: <<12 rs838905 A G 0.975078 -0.0042

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
# We will focus on the total AUDIT score: "beta_T", "se_T", and "p_T"
# Note: no BP provided; ReadMe states a_1 is A1 and a_0 is A2
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('rsid', 'chr', 'a_1', 'a_0', 'info', 'p_T', 'beta_T', 'se_T', 'N')]
names(sumstats) <-c('SNP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'BETA', 'SE', 'N')
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In main text, authors report: We estimated the SNP heritability of AUDIT total score to be 12%
SNP_h2 <- 0.12

# 5. Report effect allele 
# Effect allele is reported to be A1 ("test allele) in the readme
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/AUDIT_UKB_2018_AJP.txt.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Antisocial behaviour

Summary statistics are from Jorim Tielbeeks new GWAS (unpublished as of
05 February 2022). The total number of individuals in this data is
83,674.

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/AntisocialBehaviour"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/BroadABCphase2_exclALSPAC_20_11_2020_withRS.TBL")
pheno <- "AntisocialBehaviour_Tielbeek_2020"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
sumstats <- sumstats_raw[,c('RS', 'BP', 'CHR', 'A1', 'A2', 'P.value', 'Beta', 'Zscore', 'N')]
names(sumstats) <-c('SNP', 'BP', 'CHR', 'A1', 'A2', 'P', 'BETA', 'Z', 'N')
head(sumstats)

# 3b. Capitalise alleles if provided in small caps: 
sumstats$A1 <- toupper(sumstats$A1)
sumstats$A2 <- toupper(sumstats$A2)
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# Jorim said in email that heritability is 8.4% (although this includes ALSPAC)
SNP_h2 <- 0.084

# 5. Report effect allele 
# Jorim said in email the effect allele is A1
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/BroadABCphase2_exclALSPAC_20_11_2020_withRS.TBL")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Schizophrenia

Summary statistics are from [Pardiñas et
al. (2018)](https://www.nature.com/articles/s41588-018-0059-2). The
total number of individuals in this data is 105,318 (40,675 cases and
64,643 controls).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Schizophrenia"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/clozuk_SCHZ_SumStats")
pheno <- "Schizophrenia_Pardiñas_2018"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
#so if e.g the effect size is named 'EFFECT', BP is named 'pos' or allele freq is named 'af', rename these.
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('SNP', 'BP', 'CHR', 'A1', 'A2', 'P', 'OR', 'SE')]
head(sumstats$SNP)

# Note that the SNP variable is in IMPUTE2 format, which includes the rsID first, then position, then A2 allele
# Some SNPs (at beginning) do not have an rsID, so they have chromosome number first 
sumstats$SNP <- sapply(strsplit(sumstats$SNP, ":"), `[`, 1)
head(sumstats$SNP, 4000)
tail(sumstats$SNP, 1000)
head(sumstats)

# 3b. Capitalise alleles if provided in small caps (is it ok that A2 has multiple letters?)
sumstats$A1 <- toupper(sumstats$A1)
sumstats$A2 <- toupper(sumstats$A2)
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In supplementary table 15, meta-analysis of liability-scale SNP heritability is reported to be 20%
SNP_h2 <- 0.20

# 5. Report effect allele 
# Effect allele is reported to be A1 ("test allele) in the readme:https://walters.psycm.cf.ac.uk
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/clozuk_SCHZ_SumStats")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Handedness

Summary statistics are from [De Kovel et
al. (2019)](https://www.nature.com/articles/s41598-019-42515-0). The
total number of individuals in this data is 331,037 (31,856
left-handers, 299,181 right handers).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Handedness"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/DeKovel_PMID30980028_ambi_pval_maf01.assoc.gz")
pheno <- "Handedness_deKovel_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
summary(sumstats_raw)

# 3a. Rename headers 
# This script will use the following convention for header names: 
# SNP, BP, CHR, A1, A2, MAF, INFO, P, OR, BETA, Z, SE, N
# In ReadMe it says:
# a_0   The character code for the first allele (a string).
# a_1   The character code for the second allele (a string).
# beta: the beta coefficient refers to the effect of having an extra copy of the second allele.
# So a_1 is the effect allele (and will be renamed to A2 later)
# It also says:
# af    The allele frequency of the second allele (if the second allele is the minor allele, this is the MAF).
# Suggests that af does not always mean minor allele frequency
# SNPs have been filtered for minor allele frequencies > 0.01 for ambidextrous and minor allele frequencies > 0.001 for left and right
colnames(sumstats_raw)
sumstats <- sumstats_raw[,c('rsid', 'pos', 'chr', 'a_0', 'a_1', 'info', 'pvalue', 'beta', 'se')]
head(sumstats)
# Rename using standard col names 
names(sumstats) <-c('SNP', 'BP', 'CHR', 'A1', 'A2', 'INFO', 'P', 'BETA', 'SE')

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# In text, it says: Analysis with LDSC indicated a SNP-based heritability for left-handedness h2=0.0251 
SNP_h2 <- 0.025

# 5. Report effect allele 
# Effect allele is reported to be A2 in the readme
# "beta:    the beta coefficient refers to the effect of having an extra copy of the second allele."
effect_allele <- "A2"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(paste0(filepath, "/DeKovel_PMID30980028_ambi_pval_maf01.assoc.gz")))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

### Cataracts

Summary statistics were from [Watanabe et
al. (2019)](https://www.nature.com/articles/s41588-019-0481-0). The
total number of individuals in this data is 127,603 (UKB) (11,986 cases
and 115,617 controls).

``` bash
# 1. Set shortcuts
filepath <- "/home/ucjujb6/Scratch/SumStats/Cataracts"
outputs <- paste0(filepath,"/QC_SumStats")
sumstats <- paste0(filepath, "/6148_4_logistic.EUR.sumstats.MACfilt.txt.gz")
pheno <- "Cataracts_Watanabe_2019"

# 2. Read in summary stats
sumstats_raw <- data.frame(fread(sumstats))

# 3. Check data conforms to standard formatting
head(sumstats_raw)
dim(sumstats_raw)

# Note: no RS IDs are given in the "SNP" column, although rsIDs are given in the "SNPID_UKB" column
# However, not sure if I can use the rsIDs from the "SNPID_UKB" column because they may not match
# given that the A1_UKB is not a perfect match for A2, and A2_UKB not a perfect match for A1
identical(sumstats_raw$A1, sumstats_raw$A2_UKB)
identical(sumstats_raw$A2, sumstats_raw$A1_UKB)

# Therefore, obtain rs numbers and merge files
# .bim file with rs numbers was extracted from: https://sites.google.com/site/qutsgel/software/seca-local-version/ld-clumping-tutorial
# Version was: 1000G PhaseI v3 CEU (minimac names if no rsID) - duplicate variant names removed

library("genio")
rs_numbers <- as.data.frame(read_bim(paste0(filepath, "/1000G_20101123_v3_GIANT_chr1_23_minimacnamesifnotRS_CEU_MAF0.01.bim"), verbose = TRUE))

head(rs_numbers)
tail(rs_numbers)
dim(rs_numbers)

# Details of .bim format here: https://www.cog-genomics.org/plink/1.9/formats
# chr = chromosome
# id = SNP ID
# posg = position in morgans/centigrams
# pos = base-pair co-ordinate
# ref = Allele 1
# alt = Allele 2

# Rename columns in RS Numbers to be consistent iwth sumstats raw
names(rs_numbers)
names(rs_numbers) <- c('CHR', 'RS_ID', 'POSG', 'BP', 'A1', 'A2')

# Merge files (join by  CHR, BP, A1, A2 )
dfs <- list(sumstats_raw, rs_numbers)
colnames(sumstats_raw)
colnames(rs_numbers)
library("plyr")
merged <- join_all(dfs, by = NULL, type = "left", match = "all") # Joining by CHR, BP, A1, A2 
head(merged) 
dim(merged) #9,572,429 SNPs

# Subset data and rename columns
sumstats <- merged[,c('RS_ID', 'BP', 'CHR', 'A1', 'A2', 'P', 'OR', 'SE')]
names(sumstats) <- c('SNP', 'BP', 'CHR', 'A1', 'A2', 'P', 'OR', 'SE')
head(sumstats)

# 4. Report SNP heritability 
# Chip heritability needs to be >0.05
# On GWAS atlas, heritability reported as 0.019 https://atlas.ctglab.nl/traitDB/3543
SNP_h2 <- 0.019

# 5. Report effect allele 
# Effect allele is reported to be A1: https://atlas.ctglab.nl/documentation#2
effect_allele <- "A1"

# 6. Run md5checksum to check file corruption
md5checksum <- as.vector(tools::md5sum(sumstats))

# 7. Apply QC function
QC_fun(sumstats, outputs, pheno, SNP_h2, effect_allele, md5checksum)
```

## Create table summarising GWAS summary statistics QC process

``` bash
# Sumstats folder containing sub-folders
parent.folder <- "/home/ucjujb6/Scratch/SumStats"

# Sub-folders (select only folders which contain tables)
sub.folders <- list.dirs(parent.folder, recursive=TRUE)[!grepl("/QC_SumStats", list.dirs(parent.folder, recursive=TRUE))]
sub.folders <- sub.folders[-1]

# CSV file paths
tables <- list.files(sub.folders, pattern=".csv")

# Remove old sumstats (bipolar, Stahl) no longer using
tables <- tables[-7]

# Create one table with all the sumstats info
dataset <- do.call("rbind", lapply(paste0(sub.folders, "/",  tables), FUN=function(files){read.table(files, header=TRUE)}))

# Save dataset
write.table(dataset, "QCd_sumstats.csv", row.names=F)
```

<a name="table"></a>

``` r
# Note: displaying the table was done in R locally (rather than on the cluster) due to issues running (evaluating) the Bash script when knitting in R Markdown. The "QCd_sumstats_csv" file needs to be transferred to the local machine.
library(kableExtra)
library(textclean)
library(Hmisc)
library(stringr)
sumstats <- read.table("~/Desktop/QCd_sumstats.csv", header=TRUE)
# Format phenotype column
sumstats$phenotype <- str_replace_all(sumstats$phenotype, c("_" = " ", "2018" = "2018)", "2019" = "2019)", 
                                                            "2020" = "2020)", "2021" = "2021)"))
sumstats$phenotype <- str_replace(sumstats$phenotype, word(sumstats$phenotype, 2), paste0("(", word(sumstats$phenotype, 2), 
                                                                                          " et al.," ))                            

kable(sumstats, align = c('l', rep('c', 10)), col.names = capitalize(mgsub(names(sumstats), 
                                             c("_", "..", "heritability", "phenotype"), 
                                             c(" ", " <.", "SNP heritability", "Phenotype (ref)")))) %>%
  kable_styling(font_size = 11) %>%
 column_spec(1:11, color="black") 
```

<table class="table" style="font-size: 11px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Phenotype (ref)
</th>
<th style="text-align:center;">
SNP heritability
</th>
<th style="text-align:center;">
Effect allele
</th>
<th style="text-align:center;">
Md5checksum
</th>
<th style="text-align:center;">
NSPSs pre QC
</th>
<th style="text-align:center;">
Not autosomal
</th>
<th style="text-align:center;">
INFO &lt;.08
</th>
<th style="text-align:center;">
MAF &lt;.01
</th>
<th style="text-align:center;">
Ambiguous SNPs
</th>
<th style="text-align:center;">
Duplicate SNPs
</th>
<th style="text-align:center;">
QC positive SNPs
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;color: black !important;">
ADHD (Demontis et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.220
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
a5d78e3eb9852dff9c01513a10fa6d03
</td>
<td style="text-align:center;color: black !important;">
8094094
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1127287
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
6966807
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Alcohol (SanchezRoige et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.120
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
2f77db06a7942c9817ea68812e3d6f44
</td>
<td style="text-align:center;color: black !important;">
15765039
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
3031555
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1770725
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
10962759
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
AntisocialBehaviour (Tielbeek et al., 2020)
</td>
<td style="text-align:center;color: black !important;">
0.084
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
dbfe48f79460dcedba4b85134b5f5e19
</td>
<td style="text-align:center;color: black !important;">
6660300
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
900914
</td>
<td style="text-align:center;color: black !important;">
101
</td>
<td style="text-align:center;color: black !important;">
5759285
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Anxiety (Purves et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.260
</td>
<td style="text-align:center;color: black !important;">
A2
</td>
<td style="text-align:center;color: black !important;">
9d94a9e22799f7a8568d533bb2b4183c
</td>
<td style="text-align:center;color: black !important;">
7926782
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
170
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1213020
</td>
<td style="text-align:center;color: black !important;">
57577
</td>
<td style="text-align:center;color: black !important;">
6656015
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
ASD (Grove et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.120
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
5ca46780db3b37038bd02bd20c38c85c
</td>
<td style="text-align:center;color: black !important;">
9112386
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
632948
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1178059
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
7301379
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Bipolar (Mullins et al., 2021)
</td>
<td style="text-align:center;color: black !important;">
0.186
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
02e610aaf630e0c869a22fe179d37067
</td>
<td style="text-align:center;color: black !important;">
7608183
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
435343
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1087298
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
6085542
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Cataracts (Watanabe et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.019
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
ccbd166c43155928e1917fd9325f65dd
</td>
<td style="text-align:center;color: black !important;">
9572429
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1444183
</td>
<td style="text-align:center;color: black !important;">
2073406
</td>
<td style="text-align:center;color: black !important;">
6054840
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Depression (Howard et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.089
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
ed4597a4e7fa168fb96970e3286a0b31
</td>
<td style="text-align:center;color: black !important;">
8483301
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1297781
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
7185520
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Handedness (deKovel et al., 2019)
</td>
<td style="text-align:center;color: black !important;">
0.025
</td>
<td style="text-align:center;color: black !important;">
A2
</td>
<td style="text-align:center;color: black !important;">
ccbd166c43155928e1917fd9325f65dd
</td>
<td style="text-align:center;color: black !important;">
9859367
</td>
<td style="text-align:center;color: black !important;">
4435631
</td>
<td style="text-align:center;color: black !important;">
5
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
737970
</td>
<td style="text-align:center;color: black !important;">
4563
</td>
<td style="text-align:center;color: black !important;">
4681198
</td>
</tr>
<tr>
<td style="text-align:left;color: black !important;">
Schizophrenia (Pardiñas et al., 2018)
</td>
<td style="text-align:center;color: black !important;">
0.200
</td>
<td style="text-align:center;color: black !important;">
A1
</td>
<td style="text-align:center;color: black !important;">
8518db4ae2549eeecfd0fe483a8a61d9
</td>
<td style="text-align:center;color: black !important;">
8171061
</td>
<td style="text-align:center;color: black !important;">
102992
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
0
</td>
<td style="text-align:center;color: black !important;">
1162816
</td>
<td style="text-align:center;color: black !important;">
3416
</td>
<td style="text-align:center;color: black !important;">
6901837
</td>
</tr>
</tbody>
</table>
