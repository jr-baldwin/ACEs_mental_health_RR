---
title: "Derive polygenic scores"
author: "Jessie Baldwin"
date: "16/03/2021"
output: 
 html_document:
    toc: true
---

```{r setup, include=FALSE}
knitr::opts_hooks$set(eval = function(options) {
  if (options$engine == "bash") {
    options$eval <- FALSE
  }
options
})
```

## Access the cluster
Polygenic scores will be derived using PRSice, via the UCL Myriad cluster.
```{bash, eval=FALSE}
# ======= Connect via UCL VPN ================
cd /opt/cisco/anyconnect/bin
open vpn
# Type: "connect"
# copy the following: "connect vpn.ucl.ac.uk" (don't forget the connect)

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

# Specify path for target data
alspac_data="/lustre/scratch/scratch/ucjujb6/B3219/genetics/1000G_2021-01-25/all1/data/Merged_chromosomes" 
```

## PRSice commands 

The next sections will derive functions for deriving polygenic scores in PRSice software, following this [tutorial](https://choishingwan.github.io/PRS-Tutorial/prsice/) from Sam Choi. Relevant commands are detailed below:

#### General
- prsice: informs PRSice of the location of the PRSice software

#### Base file (i.e., GWAS summary statistics) commands
- base: informs PRSice of the name of the file containing the GWAS summary statistic
- A1: column in base file containing the effective allele (usually A1, but sometimes A2)
- A2: column in base file containing the non-effective allele (usually A2, but sometimes A1)
- snp: column in base file containing the SNP ID
- bp: column in base file containing the base pair coordinate of SNPs
- chr: column in base file containing the chromosome information
- stat: column in base file containing the effect size (either OR or BETA)
- beta: flag to indicate if GWAS test statistic is a beta
- or: flag to indicate if GWAS test statistic is an odds ratio
- pvalue: column in base file containing p-value for association

#### Target file (i.e., ALSPAC genetic data) commands
- target: informs PRsice of the name of the genotype file (e.g., for QCd ALSPAC data)

#### Clumping 
- clump-kb: the distance for clumping in kb. Clumping removes SNPs in LD with eachother (default: 250kb)
- clump-r2: the r2 threshold for clumping (default=0.1)
- clump-p: the p-value threshold for clumping (default=1)
- ld: the LD reference file. If not provided, will use post-filtered target genotype for LD calculation
- note: commands of pheno, cov and binary-target are not needed as we are only generating PGS and not testing their associations with outcomes
- note: we will use the default values for clumping so will not include these commands in the function

#### P-value thresholding
- bar-levels: level of barchart to be plotted (1=all p-values)
- fastscore: only calculate threshold stated in bar-levels
- no-regress: do not perform regression analysis and output all PRS (include if only generating PRS)

#### Miscellaneous
- thread max: use the maximum number of threads for speed

## Define function for deriving polygenic scores based on summary statistics reporting beta coefficients
```{bash}
prsice_beta () { 
cd /home/ucjujb6/Scratch/Prsice_Files/PRSice_linux_Nov/
Rscript PRSice.R --dir .\
    --prsice PRSice_linux \
    --base /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats/$SumStatGWAS_file \
    --a1 $A1 \
    --a2 $A2 \
    --snp SNP \
    --stat BETA \
    --beta \
    --pvalue P \
    --target $alspac_data/8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01 \
    --bar-levels 1\
    --fastscore \
    --no-regress \
    --thread max \
    --out /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/$Pheno_GWAS/$SumStatGWAS_file$date
}
```

## Define function for deriving polygenic scores based on summary statistics reporting odds ratios 
```{bash}
prsice_or () { 
cd /home/ucjujb6/Scratch/Prsice_Files/PRSice_linux_Nov/ 
Rscript PRSice.R --dir .\
    --prsice PRSice_linux \
    --base /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats/$SumStatGWAS_file \
    --a1 $A1 \
    --a2 $A2 \
    --snp SNP \
    --stat OR \
    --or \
    --pvalue P \
    --target $alspac_data/8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01 \
    --bar-levels 1\
    --fastscore \
    --no-regress \
    --thread max \
    --out /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/$Pheno_GWAS/$SumStatGWAS_file$date
}
```

#### Notes on deriving polygenic scores:

For each PGS, need to specify:

- $Pheno_GWAS (i.e., phenotype name)
- $SumStatGWAS_file (i.e., file name with QCd sumstats)
- $A1 (whether effect allele is A1 or A2; check in csv table)
- $A2 (whether non-effect allele is A1 or A2; check in csv table)
- $date (the date)

```{bash}
# Set date
date="20210607"
echo "$date"
```

## Derive polygenic scores

Polygenic scores will be derived for the following outcomes:

- Major depressive disorder
- Anxiety disorder
- Bipolar disorder
- Autism
- ADHD
- Alcohol use disorder
- Antisocial behaviour
- Schizophrenia
- Handedness (negative control)
- Cataracts (negative control)

For each phenotype, need to:

1. Set file shortcuts
2. Check if GWAS summary statistics contain betas or odds ratios
3. Check whether effect allele is A1 or A2
4. Specify A1 and A2 parameters
5. Make a directory to store the output in
6. Generate the polygenic score

### Depression
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
1. Set file shortcuts
Pheno_GWAS="Depression"
SumStatGWAS_file="Depression_Howard_2019.txt"
SumStatsQC_table="QC_SumStatsDepression_Howard_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (beta)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file.txt

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Anxiety disorder
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Anxiety"
SumStatGWAS_file="Anxiety_Purves_2019.txt"
SumStatsQC_table="QC_SumStatsAnxiety_Purves_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (beta)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A2)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A2"
A2="A1"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 

```

### Bipolar disorder 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Bipolar"
SumStatGWAS_file="Bipolar_Mullins_2021.txt"
SumStatsQC_table="QC_SumStatsBipolar_Mullins_2021.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (OR)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Autism 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Autism"
SumStatGWAS_file="ASD_Grove_2019.txt"
SumStatsQC_table="QC_SumStatsASD_Grove_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (OR)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_or "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### ADHD 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="ADHD"
SumStatGWAS_file="ADHD_Demontis_2019.txt"
SumStatsQC_table="QC_SumStatsADHD_Demontis_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (OR)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_or "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Alcohol use disorder 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Alcohol"
SumStatGWAS_file="Alcohol_SanchezRoige_2019.txt"
SumStatsQC_table="QC_SumStatsAlcohol_SanchezRoige_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (beta)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Antisocial behaviour 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="AntisocialBehaviour"
SumStatGWAS_file="AntisocialBehaviour_Tielbeek_2020.txt"
SumStatsQC_table="QC_SumStatsAntisocialBehaviour_Tielbeek_2020.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (beta)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Schizophrenia 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Schizophrenia"
SumStatGWAS_file="Schizophrenia_Pardiñas_2018.txt"
SumStatsQC_table="QC_SumStatsSchizophrenia_Pardiñas_2018.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (OR)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_or "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Handedness 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Handedness"
SumStatGWAS_file="Handedness_deKovel_2019.txt"
SumStatsQC_table="QC_SumStatsHandedness_deKovel_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (beta)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A2)
# note: readme says the beta coefficient refers to the effect of having an extra copy of the second allele.
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A2"
A2="A1"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_beta "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

### Cataracts 
```{bash}
cd /lustre/scratch/scratch/ucjujb6/SumStats
# 1. Set file shortcuts
Pheno_GWAS="Cataracts"
SumStatGWAS_file="Cataracts_Watanabe_2019.txt"
SumStatsQC_table="QC_SumStatsCataracts_Watanabe_2019.csv"

# 2. Check if GWAS summary statistics contain betas or odds ratios (OR)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS/QC_SumStats
head $SumStatGWAS_file

# 3. Check whether effect allele is A1 or A2 (A1)
cd /lustre/scratch/scratch/ucjujb6/SumStats/$Pheno_GWAS
cat $SumStatsQC_table | awk '{ print $1, $3 }'

# 4. Specify A1 and A2 parameters
A1="A1"
A2="A2"

# 5. Make a directory to store the output in
cd /home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/
mkdir $Pheno_GWAS

# 6. Generate the polygenic score
prsice_or "A1" "A2" "date" "Pheno_GWAS" "SumStatGWAS_file" 
```

## Merge all polygenic scores and principal components into one file 
```{bash}
R
library(data.table)
library(tidyverse)
setwd("/home/ucjujb6/Scratch/Prsice_Files/Polygenic_Scores/")

##########################################################################
#################### Combine all polygenic scores ########################
##########################################################################

# Load datasets
ADHD <- fread("ADHD/ADHD_Demontis_2019.txt20210323.all_score")
alcohol <- fread("Alcohol/Alcohol_SanchezRoige_2019.txt20210323.all_score")
antisocial <- fread("AntisocialBehaviour/AntisocialBehaviour_Tielbeek_2020.txt20210323.all_score")
anxiety <- fread("Anxiety/Anxiety_Purves_2019.txt20210323.all_score")
autism <- fread("Autism/ASD_Grove_2019.txt20210323.all_score")
bipolar_2019 <- fread("Bipolar/Bipolar_Stahl_2019.txt20210323.all_score")
bipolar <- fread("Bipolar/Bipolar_Mullins_2021.txt20210607.all_score")
cataracts <- fread("Cataracts/Cataracts_Watanabe_2019.txt20210323.all_score")
depression <- fread("Depression/Depression_Howard_2019.txt20210323.all_score")
handedness <- fread("Handedness/Handedness_deKovel_2019.txt20210323.all_score")
schizophrenia <- fread("Schizophrenia/Schizophrenia_Pardiñas_2018.txt20210323.all_score")

# Label PGS variable in each file
dfs <- c("ADHD", "alcohol", "antisocial", "anxiety",
          "autism", "bipolar_2019", "bipolar", "cataracts", 
          "depression", "handedness", "schizophrenia")

for(df in dfs) {
  df.tmp <- get(df)
  colnames(df.tmp)[3] <- paste0(df, "_PGS")
  assign(df, df.tmp)
}

# Check names
list(ADHD, alcohol, antisocial, anxiety,
      autism, bipolar_2019, bipolar, cataracts, 
      depression, handedness, schizophrenia) %>%
    lapply( colnames )
    
# Merge different PGS datafiles into one dataset
PGS_all <- list(ADHD, alcohol, antisocial, anxiety,autism, bipolar_2019, bipolar, cataracts, 
      depression, handedness, schizophrenia) %>% reduce(left_join, by = c("FID", "IID"))
    
head(PGS_all)
dim(PGS_all)

##########################################################################
############## Combine with 10 Principal Components ######################
##########################################################################
PCs <- fread("/lustre/scratch/scratch/ucjujb6/B3219/principal_components_10.eigenvec")
head(PCs)
# Note: columns are not named, but V1 and V2 are FID and IID which are identical, and the others are PCs:
identical(PCs[['V1']], PCs[['V2']])
names(PCs) <- c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10") 

# Merge with PGS file
PGS_PCs <- list(PGS_all, PCs) %>% reduce(left_join, by = c("FID", "IID"))
head(PGS_PCs)   
dim(PGS_PCs)   

# Save file
write.csv(PGS_PCs, file="/lustre/scratch/scratch/ucjujb6/Prsice_Files/Polygenic_Scores/all_PGS_PCs_20210607.csv", row.names=FALSE)

q(save="no")
```
