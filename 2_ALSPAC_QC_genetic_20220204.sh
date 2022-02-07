################################################################################
###################### ALSPAC - QC of genetic data #############################
################################################################################

# This script runs QC on the ALSPAC genetic data (children only)

# QC of ALSPAC genetic data is conducted on the Myriad cluster
# Original datasets are stored at /lustre/scratch/scratch/ucjujb6/B3219/genetics/1000G_2021-01-25/all1

# ======= Connect to Myriad via UCL VPN ================
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

# ======== Start interactive session =========
ssh ucjujb6@myriad.rc.ucl.ac.uk

# ======= Load file paths ==============
# Load variables / directory paths
cd /lustre/scratch/scratch/ucjujb6/paths

# ======= Create variables linking to files =======
childGeneticData="/lustre/scratch/scratch/ucjujb6/B3219/genetics/1000G_2021-01-25/all1" #makes the dataset a vector in R
childPhenoData="/lustre/scratch/scratch/ucjujb6/B3219/phenotypic_data" #makes the dataset a vector in R (need to update with my data file)
software="/lustre/scratch/scratch/ucjujb6/software" #makes the dataset a vector in R

# =========== Load required software ================
module avail # check the available software that's on the cluster 
# Load R module
module unload -f compilers mpi gcc-libs
module load beta-modules
module load r/new

cd $software
# Load plink
module load plink/1.90b3.40
alias plink='$software/plink'

# Change directory to my file
cd /lustre/scratch/scratch/ucjujb6

###### Convert chromosome files in .bgen format to binary plink format (.bed, .bim, .fam)

#### Look at files
cd $childGeneticData/data 
mkdir Converted_chomosomes
wc -l data.sample # N=17818, note this includes mums and children
head data.sample 
tail data.sample

#### Remove categorical phenotype (Ms for mothers)
#Note that the files include children and mothers, and plink will return an error that there is an 
# "Invalid categorical phenotype '3611M' on line 8887, column 5 of .sample". 
# This is because of the "M" in column 5 indicating the rows are for mothers. 
# Therefore, as a workaround, delete Ms from the IDs of column 5.
awk '{ gsub(/M/,"", $5); print } ' data.sample > M_deleted5_data.sample  # delete "M" in column 5
head M_deleted5_data.sample
tail M_deleted5_data.sample
tail data.sample
wc -l M_deleted5_data.sample  # 17818 matches data.sample
wc -l data.sample

#### Convert files to binary plink format
for chr in {01..23}; do
plink2 --bgen $childGeneticData/data/data_chr${chr}.bgen snpid-chr \
--make-bed \
--sample $childGeneticData/data/M_deleted5_data.sample \
--missing-code -9,0 \
--out $childGeneticData/data/Converted_chromosomes/chr${chr} ;
done

#### Get the list of files to merge
cd $childGeneticData/data/Converted_chromosomes
ls *.bim | sed 's/.bim//g' > merge_list #sed is a text extraction tool
head merge_list
tail merge_list

#### Get the list of problematic SNPs with 3+ alleles present
# Make a new directory for Merged chromosomes
mkdir Merged_chromosomes

# Get the list of problematic SNPs (This command will fail and generate the list of SNPs to remove)
plink --merge-list merge_list --make-bed --out $childGeneticData/data/Merged_chromosomes/ALSP_merged

# If binary merging fails because at least one variant would have more than two alleles, a list of offending variant(s) will be written to plink.missnp.]
# the file 'ALSPAC_merged-merge.missnp' is created => containing all triallelic sites


#### Convert again but exclude SNPs with 3+ alleles 
cd $childGeneticData/data/Converted_chromosomes

# Make a directory 'Chromosomes triallelic excluded' to store the chromosomes in (with SNPs with 3+ alleles excluded)
mkdir Chromosomes_triallelic_excluded

# Repeat conversion process again
for chr in {01..23}; do
plink \
--bfile chr${chr} \
--make-bed \
--exclude $childGeneticData/data/Merged_chromosomes/ALSP_merged-merge.missnp \
--out $childGeneticData/data/Converted_chromosomes/Chromosomes_triallelic_excluded/chr_clean${chr} ;
done

#### Merge chromosome files
cd $childGeneticData/data/Converted_chromosomes/Chromosomes_triallelic_excluded
ls chr_clean*.bim | sed 's/.bim//g' > merge_list_clean #Made a new file with a list of all the clean chromosomes
ls
plink --merge-list merge_list_clean \
--make-bed \
--out $childGeneticData/data/Merged_chromosomes/ALSP_merged

head $childGeneticData/data/Merged_chromosomes/ALSP_merged.bim 

#### Check number of SNPs and individuals before QC
wc -l ALSP_merged.bim ## 28634576 included SNPs before QC
wc -l ALSP_merged.fam ## 17816 included individuals before QC

## Quality control steps summary
#The quality control steps will proceed in the following order:

# 0. Prune SNPs to produce a subset of those in linkage equilibrium
# 1. Remove siblings and individuals of non-white ethnicity 
# 2. Remove autosomal SNPs
# 3. Remove SNPs with low call rate
# 4. Remove samples with low call rate
# 5. Remove samples with outlying heterozygosity
# 6. Remove related individuals
# 7. Remove sex mismatches
# 8. Remove SNPs deviating from Hardy-Weinberg Equilibrium
# 9. Remove SNPs deviating with MAF < 1%

### 0. Prune SNPs to produce a subset of those in linkage equilibrium
# Pruning generates a subset of SNPs that are in linkage equilibrium with eachother. 
# This method uses the strength of LD between SNPs within a specific region of the chromosome and selects SNPs that are uncorrelated.
# To prune SNPs, we use the command --indep-pairwise which requires 3 parameters:

#- 1. a window size in variant count or kilobase units (e.g., 250 = 250 SNPs)
#- 2. a variant count to shift the window at the end of each step (e.g., 50 = 50 SNPs)
#- 3. a pairwise r2 threshold (e.g., 0.1): at each step, pairs of variants in the current window with squared correlation greater than the threshold are noted, and variants are greedily pruned from the window until no such pairs remain

plink --bfile ALSP_merged \
--indep-pairwise 250 50 0.1 \
--out pruned_indepPW_250_50_0.1 

### 1. Remove siblings and individuals of non-white ethnicity 
# Note: the ALSPAC study already removed "Samples that clustered outside the CEU HapMap population using 
# multidimensional scaling of genome-wide IBS pairwise distances". 
# Here we will remove siblings and individuals with non-white ethnicity.


#### Use R to generate table of IDs of individuals with white ethnicity and non siblings
R # Launch R

# Load packages
library(foreign)
library(gmodels)
library(dplyr)

# Set file paths
childGeneticData <- file.path("/lustre/scratch/scratch/ucjujb6/B3219/genetics/1000G_2021-01-25/all1/data")
childPhenoData <- file.path("/lustre/scratch/scratch/ucjujb6/B3219/phenotypic_data")

# Select only white ethnicity and non-siblings in phenotype file 
setwd(childPhenoData)

# Read in phenotypic data
datALSP_all <- read.spss("Baldwin_08Jan21.sav", use.value.labels=FALSE, to.data.frame=TRUE)

# Subset dataset to include only those without missing data on ID variable
datALSP_rm1 <- datALSP_all[complete.cases(datALSP_all$cidB3219), ] 

# Subset to select only subjects of white ethnicity [# Ethnic group c800:1 = white]
datALSP_rm <- subset(datALSP_rm1, c800 == 1) #
table(datALSP_rm$c800) # Shows only white participants (N=12k)

# Create a new variable called sibling_ID indexing birth order within pregnancy 
datALSP_rm1$sibling_ID <- as.factor(datALSP_rm1$qlet) 
table(datALSP_rm1$sibling_ID) ## shows 15,239 singletons and 203 Twin As

# Create ID variable indexing the family ID and sibling ID, which matches the ID variable in the .fam file
datALSP_rm$IID <- paste0(datALSP_rm$cidB3219, datALSP_rm$qlet) 
datALSP_rm$IID_F <- as.factor(datALSP_rm$IID)
head(datALSP_rm$IID_F)
head(datALSP_rm$cidB3219)
head(datALSP_rm$qlet)

# Read in sample file from the genetic data
setwd("/lustre/scratch/scratch/ucjujb6/B3219/genetics/1000G_2021-01-25/all1/data")
SampleFile <- read.table("data.sample", header=TRUE) # Sample file that corresponds to the .bgen files
head(SampleFile)
dim(SampleFile) #17817 participants (mums and children)

# Subset sample file to include only those with complete data on the ID variable
SampleFile_rm <- subset(SampleFile, complete.cases(SampleFile$ID_1) & ID_1!=0)
head(SampleFile_rm$ID_1) # Shows mums are at the beginning (finishing with "M")
tail(SampleFile_rm$ID_1) # Children are at the end (finishing with "A")
dim(SampleFile_rm) #17816 (dropped one row with ID_1=0)

# Merges the phenotypic sample with the genetic sample (from sample.file) 
# Note: at this stage the mothers should get dropped, because they are not represented in the phenotypic data
length(grep("M", SampleFile_rm$ID_1)) # There are 8884 mothers included in the genetic sample file
length(grep("M", datALSP_rm$IID_F)) # There are 0 mothers included in the phenotypic file

datALSP <- merge(SampleFile_rm, datALSP_rm, by.x="ID_1", by.y= "IID_F")
dim(datALSP) #7861, only children with both phenotypic and genetic data

# Double check if mothers are represented
length(grep("M", datALSP$ID_1))

datALSP$IID <- datALSP$ID_1
head(datALSP$IID)

# Exclude siblings 
head(datALSP$sibling_ID)
ID_sibling_ethnicity_clean <- subset(datALSP, sibling_ID == "A") # Select only non-siblings or the first-born child
ID_sibling_ethnicity_clean_selected <- select(ID_sibling_ethnicity_clean, IID, ID_1) #Select only the ID variables which should be the same
head(ID_sibling_ethnicity_clean_selected)
length(ID_sibling_ethnicity_clean_selected$IID) # N=7792 non-siblings

# Save table with IDs of all the included non-siblings and white individuals
write.table(ID_sibling_ethnicity_clean_selected, 
            file = paste0(childGeneticData, "/Merged_chromosomes/ID_sibling_ethnicity_clean.txt"),
            col.names = F, row.names = F,quote=F, sep = "\t")

# Quit R          
q(save = "no") 

#### Remove siblings and participants with non-white ethnicity from sample
cd $childGeneticData/data/Merged_chromosomes

# Check number of individuals to be included
wc -l ID_sibling_ethnicity_clean.txt #7792 
head ID_sibling_ethnicity_clean.txt

# Make a bed file with white participants without siblings only
plink --bfile ALSP_merged \
--keep ID_sibling_ethnicity_clean.txt \
--make-bed --out 1_ALSP_merged_ExclEthnicity_ExclSibling 

# Check number of individuals before and after siblings and non-white participants were removed
wc -l ALSP_merged.fam # n=17816 included individuals BEFORE removal of non-white, mothers and siblings
wc -l 1_ALSP_merged_ExclEthnicity_ExclSibling.fam # n=7792 included individuals AFTER removal of mothers, non-white and sibling #Mine has 7801?
wc -l 1_ALSP_merged_ExclEthnicity_ExclSibling.bim # 28.634.576 included SNPs

### 2. Remove autosomal SNPs
#Here we generate bfile with autosomal SNPs only (i.e., from chromosomes 1 to 22)

# Generate a text file with list of SNPs from chromosomes 1 to 22
awk '{ if ($1 >= 1 && $1 <= 22) print $2 }' 1_ALSP_merged_ExclEthnicity_ExclSibling.bim > snp_1_22.txt 
wc -l 1_ALSP_merged_ExclEthnicity_ExclSibling.bim # 28634576 SNPs
wc -l snp_1_22.txt # 27384358 SNPs (i.e., fewer, because chromosome 23 is missing)

# Select SNPs on chromosomes 1-22 and make a new bed file 
plink --bfile 1_ALSP_merged_ExclEthnicity_ExclSibling \
--extract snp_1_22.txt \
--make-bed \
--out 2_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23

wc -l 2_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23.bim ## 27384358 SNPs (on chromosomes 1-22, consistent with the count from the list above)

### 3. Remove SNPs with low call rate
# This excludes SNPs that are missing in a large proportion of the subjects (5%). 
# Note that SNP filtering should be performed before individual filtering
plink --bfile 2_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23 \
--geno 0.05 \
--make-bed \
--out 3_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr

### 4. Remove samples with low call rate
# This step uses the plink command 'mind' removes people with high rates of genotype missingness. 
# Individuals with low genotype calls (< 3%) are removed.

plink --bfile 3_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr \
--mind 0.03 \
--make-bed \
--out 4_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr

### 5. Remove samples with outlying heterozygosity
# This step removes individuals with high or low heterozygosity rates. 
# These individuals need to be removed because deviations can introduce sample contamination or inbreeding. 
# Here we will remove individuals who deviate by 3 SD from the sample mean. 
# Note: Checks for heterozygosity are performed on a subset of SNPs which are uncorrelated. 

##### Create heterogzygosity file with the number of homozygous genotypes per individual
plink --bfile 4_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr \
--extract pruned_indepPW_250_50_0.1.prune.in \
--het \
--out heterozygosity

head -n10 heterozygosity.het # the third column denotes the observed number of homozygous genotypes [O(Hom)] and the fifth column denotes the number of non-missing genotypes [N(NM)] per individual.
wc -l heterozygosity.het # check number (7793 individuals)

##### In R, remove individuals with heterozygosity > 3 SD from the mean
R
setwd("childGeneticData/Merged_chromosomes")

# Read in heterozygosity data
het <- read.table("heterozygosity.het", header=TRUE) ## sample file that corresponds to the .bgen files

# Generate a variable indexing heterozygosity rate
het$HET_RATE <- (het$"N.NM." - het$"O.HOM.")/het$"N.NM."

# Generate a dataset including those with heterozygosity > 3 SD from the mean
het_fail <- subset(het, (het$HET_RATE < mean(het$HET_RATE)-3*sd(het$HET_RATE)) | (het$HET_RATE > mean(het$HET_RATE)+3*sd(het$HET_RATE)))
dim(het_fail)
het_fail$HET_DST = (het_fail$HET_RATE - mean(het$HET_RATE))/sd(het$HET_RATE)
head(het_fail)

# Make graph showing heterozygosity rate
pdf("heterozygosity.pdf")
hist(het$HET_RATE, xlab="Heterozygosity Rate", ylab="Frequency", main= "Heterozygosity Rate") 
dev.off()

# Generate text file listing participants with heterozygosity > 3 SD from the mean
write.table(het_fail, "fail-het-qc.txt", row.names=FALSE)
q(save = "no")

##### Check the heterozygosity file and adapt it for Plink
# In this step, we will remove all quotation marks from the file and selecting only the first two columns.
# View file
cd $childGeneticData/data/Merged_chromosomes
head -n300 fail-het-qc.txt
wc -l fail-het-qc.txt

# Make the heterozygosity file compatible for PLINK, 
sed 's/"// g' fail-het-qc.txt | awk '{print$1, $2}'> het_fail_ind.txt
head -n300 het_fail_ind.txt
wc -l het_fail_ind.txt

##### Remove heterozygosity rate outliers (n=64)
plink --bfile 4_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr \
--remove het_fail_ind.txt \
--make-bed \
--out 5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity

### 6. Remove related individuals
# In this step we remove one person from each pair of individuals with IBD > 0.1875. 
# The person removed is the one with the highest levels of missingness in each related pair.

##### Use KING to assess relatedness
cd $software
wget http://people.virginia.edu/~wc9c/KING/Linux-king.tar.gz
tar -xzvf Linux-king.tar.gz

# Check relatedness
# This saves a file (related_3rd_degree.seg) which contains the IBD values of pairs related with IBD proportion > 0.0884 (over 3rd degree relatives)
$software/king  -b $childGeneticData/data/Merged_chromosomes/5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity.bed \
                                    --ibdseg \
                                    --related \
                                    --degree 3 \
                                    --prefix related_3rd_degree
                                    
# Use the below to summarise relatedness. Then remove those with prop IBD > 0.1875 using R script                               
$software/king  -b $childGeneticData/data/Merged_chromosomes/5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity.bed \
                                    --related \
                                    --degree 3 \
                                    --prefix related_3rd_degree
                                    
#Each family consists of one individual.
#Relationship inference across families starts at Tue Mar 23 12:11:01 2021
#18 CPU cores are used...Inference ends at Tue Mar 23 13:13:55 2021

#Relationship summary (total relatives: 0 by pedigree, 557 by inference)
              #  MZ      PO      FS      2nd
  #=====================================================
  #Inference     0       0       33      28


#Between-family relatives (kinship >= 0.04419) saved in file related_3rd_degree.kin0

# 560 relatives over 3rd degree (kinship >= 0.04419) 
wc -l related_3rd_degree.kin0
head  related_3rd_degree.kin0  
tail  related_3rd_degree.kin0  
# FS = full sibling. IBD for that pair is 0.4558
# Now subset this file to those with IBD > 0.1875 and then remove 1 in each pair
# Import into R and do it in there

R # Launch R
# Load imiss and genome files
ibd <- read.table("related_3rd_degree.kin0", header=TRUE)
imiss <- read.table("missingness_ALSP_children.imiss", header=TRUE)
head(ibd)
head(imiss)
ibd_0.1875 <- subset(ibd, PropIBD>=0.1875)
dim(ibd_0.1875)
[1] 56 14 # 56 pairs with IBD > 0.1875
ibd_0.1875

# Find the individual in the pair with the lowest call rate and exclude
low_call_rate_fid = low_call_rate_iid = vector(length = nrow(ibd_0.1875))
for (i in 1:nrow(ibd_0.1875)) {
  # ind1 is the row in imiss corresponding to the IID1 individual in genome
  # ind2 is the row in imiss corresponding to the IID2 individual in genome
  ind1 = imiss[imiss$IID == as.character(ibd_0.1875[i,'ID1']),]
  ind2 = imiss[imiss$IID == as.character(ibd_0.1875[i,'ID2']),]
  if (ind1$F_MISS > ind2$F_MISS) {
    low_call_rate_fid[i] = as.character(ind1$FID)
    low_call_rate_iid[i] = as.character(ind1$IID)
  } else {
    low_call_rate_fid[i] = as.character(ind2$FID)
    low_call_rate_iid[i] = as.character(ind2$IID)
  }
}

low_call_rate = data.frame(FID = unique(low_call_rate_fid), IID = unique(low_call_rate_iid))
dim(low_call_rate)

write.table(low_call_rate, file = "IBD0.1875_KING.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

q(save = "no")

##### Remove related individuals
wc -l IBD0.1875_KING.txt

plink --bfile 5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity \
--remove IBD0.1875_KING.txt \
--make-bed \
--out 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875

### 7. Remove sex mismatches
# This steps removes individuals whose assigned sex mismatches with their genotype. 
# A discrepancy likely points to sample mix‐ups in the lab. 
# Note: this test can only be conducted when SNPs on the sex chromosomes (X and Y) have been assessed. 
# Therefore use file type: 1_ALSP_merged_ExclEthnicity_ExclSibling

cd $childGeneticData/data/Merged_chromosomes
plink --bfile 1_ALSP_merged_ExclEthnicity_ExclSibling \
--check-sex \
--out check_sex_discrepancy

head check_sex_discrepancy.sexcheck

# Produce a list of individuals with discordant sex data (i.e., if Plink has recorded it as "PROBLEM") 
grep PROBLEM check_sex_discrepancy.sexcheck > check_sex_discrepancy.sexprobs 
head check_sex_discrepancy.sexprobs

# Because no sex discordant individuals were removed, retain file derived after IBD, named "wc -l 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875"

### 8. Remove SNPs deviating from Hardy-Weinberg Equilibrium
# SNPs which deviate from Hardy–Weinberg equilibrium with p-value of 5e-7 will be removed. Deviations from HWE are likely to reflect genotyping errors

plink --bfile 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875 \
--hwe 5e-7 \
--make-bed \
--out 7_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7

### 9. Remove SNPs deviating with MAF < 1%
plink --bfile 7_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7 \
--maf 0.01 \
--make-bed \
--out 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01

## Summarise all QC steps

# There is a total of 6,968,314 SNPs and 7,189 individuals included after QC. The numbers at different steps are shown below.

##### Summarise number of included individuals 
# Before QC (removing mothers [with 'M' in ID], N=8932)
awk '{print $1}' ALSP_merged.fam | grep -v 'M' | wc -l 

# 1. Remove individuals of non-white ethnicity and siblings (7792)
wc -l 1_ALSP_merged_ExclEthnicity_ExclSibling.fam

# 2. Remove autosomal SNPs (7792 individuals)
wc -l 2_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23.fam

# 3. Remove SNPs with low call rate (7792)
wc -l 3_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr.fam

# 4. Remove samples with low call rate (7792)
wc -l 4_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr.fam

# 5. Remove samples with outlying heterozygosity (7728 individuals)
wc -l 5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity.fam

# 6. Remove related individuals (7675 individuals)
wc -l 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875.fam

# 7. Remove sex mismatches (7675 individuals)
wc -l 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875.fam

# 8. Remove SNPs deviating from Hardy-Weinberg Equilibrium (7675 individuals)
wc -l 7_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7.fam

# 9. Remove SNPs deviating with MAF < 1% (7675 individuals)
wc -l 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.fam

##### Summarise number of included SNPs 
# 1. Remove individuals of non-white ethnicity and siblings 
wc -l 1_ALSP_merged_ExclEthnicity_ExclSibling.bim #28634576 SNPs

# 2. Remove autosomal SNPs 
wc -l 2_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23.bim #27384358 SNPs

# 3. Remove SNPs with low call rate
wc -l 3_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr.bim #25715823 SNPs

# 4. Remove samples with low call rate 
wc -l 4_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr.bim #25715823 SNPs

# 5. Remove samples with outlying heterozygosity 
wc -l 5_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity.bim #25715823 SNPs

# 6. Remove related individuals 
wc -l 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875.bim #25715823 SNPs

# 7. Remove sex mismatches (7189 individuals)
wc -l 6_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875.bim #25715823 SNPs

# 8. Remove SNPs deviating from Hardy-Weinberg Equilibrium 
wc -l 7_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7.bim #25,715,739 SNPs

# 9. Remove SNPs deviating with MAF < 1%
wc -l 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.bim #6,967,960 SNPs


## Principal component analysis
# The basic concept behind calculating principal components of genetic ancestry is to identify axes (or components) of variability. 
# The components of variability, rather amazingly, correspond with ethnicity or the evolutionary history of populations. 
# We will derive 10 principal components through the plink command "pca 10". Two files are formed from this command:

#- .eigenval: eigenvalues explained by each principal component, ordered from highest to lowest, by default, plink estimates 20 but here we will estimate 10
#- .eigenvec: actual principal components for each individual

# Note that you should used pruned SNPs for PCA (using the extract option).
plink --bfile 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01 \
--extract pruned_indepPW_250_50_0.1.prune.in \
--pca 10 \
--out 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01

##### Examine files created
#-.eigenval contains the eigenvalues explained by each of the 10 principal components
#- .eigenvec contains the actual 10 principal components for each individual in the data set
# .eigenval 
cat 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01
wc -l 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.eigenval

# .eigenvec
head 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.eigenvec 

##### Save PCs in overall B3219 folder to merge with the phenotypic data
cp 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.eigenvec /lustre/scratch/scratch/ucjujb6/B3219
cd /lustre/scratch/scratch/ucjujb6/B3219
# Rename file
mv 8_ALSP_merged_ExclEthnicity_ExclSibling_ExclChr23_ExclSNPcr_ExclSampcr_exclHeterozygosity_ExclIBD0.1875_ExclSexDis_hweP5e7_maf01.eigenvec principal_components_10.eigenvec
ls



