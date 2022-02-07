################################################################################
################## ABCD Study - QC of genetic data #############################
################################################################################

# This script runs QC on the ABCD genetic data (release 3)

# ======= Connect to the Myriad cluster via UCL VPN ================
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

# ======== Start interactive session =========
ssh ucjujb6@myriad.rc.ucl.ac.uk

# ======= Load file paths ==============
# Load variables / directory paths
cd /lustre/scratch/scratch/ucjujb6/paths

# ======= Create variables linking to files =======
childGeneticData="/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed" #makes the dataset a vector in R
software="/lustre/scratch/scratch/ucjujb6/software" #makes the dataset a vector in R

# =========== Load required software ================
module avail # check the available software that's on the cluster 
# Load R module
module unload -f compilers mpi gcc-libs
module load beta-modules
module load r/new

# Load plink
module load plink/1.90b3.40

# Change directory to my file
cd /lustre/scratch/scratch/ucjujb6

# =========== Unzip VCF files ================
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03
# Unzip files to get individual VCFs for each chromosome, started at 16.50 on 25 oct
unzip ABCD_imputed.zip

# Unzip the info files
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed
for i in {1..22}; do 
gunzip chr${i}.info.gz; done 

# =========== Extract SNPs with MAF > 1% and imputation r2 > 0.4 ===============

# In R, make a list of SNPs with MAF > 1% and imputation > 0.4
R
library(data.table)
setwd("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed")
for (i in 1:22){
a = read.table(paste0("chr", i, ".info"), header = T)
a$Rsq = as.numeric(as.character(a$Rsq))
b = subset(a, Rsq > 0.4)
b = subset(b, ALT_Frq > 0.01 & ALT_Frq < 0.99)
write.table(b[,1], file = paste0("chr", i, "extract.txt"), row.names = F, col.names = T, quote = F)
}
q (save="no")

# Made bed file with only extracted SNPs (MAF > 1% and imputation R2 > 0.4)
module load plink/1.90b3.40
plink --vcf ${IN}chr${CHR}.dose.vcf.gz \
--make-bed \
--out ${OUT}chr${CHR} \
--extract ${IN}chr${CHR}extract.txt \
--const-fid 0

# =========== Get rsIDs for SNPs from bed file ================
# The SNPs are labelled as in chr:position:A1:A2 rather than having rsIDs (needed to derive polygenic scores)
# To get the rsIDs, we use BED files which include chr name, start and end positions, and rsIDs
# This follows advice and a script from Gustavo Sudre: https://github.com/gsudre/lab_notes/blob/master/031-PRS_for_ABCD.md#2021-06-25-103217

# Download BED files with rsID info for GrCh38
for c in {1..22}; do
    wget https://ftp.ncbi.nih.gov/snp/organisms/human_9606_b151_GRCh38p7/BED/bed_chr_${c}.bed.gz;
done

# Map rsID to ABCD chromosome files
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
for i in {1..22}; do
    echo $i;
    # grab just the variant name
    cut -f 2 chr${i}.bim > tmp_name.txt; # takes the variant name from column 2 and makes a new file called tmp_name.txt
    # grab only chr and position 
    cut -d":" -f 1,2 tmp_name.txt > tmp_name2.txt; # here he splits the SNP name from chr15:19589482:AG:A at the colon and keeps the first and second bit (chr:position)
    # create file to map between variant with alleles and just chr:pos 
    paste tmp_name.txt tmp_name2.txt > update_snps1.txt; # makes a file tmp_name2 with chr:position and then update_snps1 with chr:position:A1:A2 & chr:position as diff columns
    plink --bfile chr${i} --update-name update_snps1.txt \
        --make-bed --write-snplist --out tmp2; # renames the SNP name to just chr:position in tmp2 new file
    # remove any duplicated SNPs
    cat tmp2.snplist | sort | uniq -d > duplicated_snps.snplist; # makes list of duplicated SNPs
    plink --bfile tmp2 --exclude duplicated_snps.snplist --out tmp_nodups \
        --make-bed --noweb; # makes new binary files excluding duplicated SNPs
    # update chr:pos to rsid
    # use end position at first
    zcat bed_chr_${i}.bed.gz | tail -n +2 | awk \
        '{ print $1":"$3, $4 }' > rsids.txt; # creates a file "rsids.txt" with chr:position for that chromosome and then matching rsID
    # filter out any chr:pos that is repeated (reduces the number of SNPs)
    awk '!seen[$1]++' rsids.txt > rsids_clean.txt; 
    cut -d" " -f 1 rsids_clean.txt > keep_vars.txt; # this creates file "keep_vars.txt" with chr:position of the SNPs to keep (non-duplicates)
    plink --bfile tmp_nodups --extract keep_vars.txt --make-bed --out tmp3;
    plink --bfile tmp3 --update-name rsids_clean.txt --make-bed \
        --out chr${i}_rsIDfromBED;
done

# Have a look at files with rsIDs (chr1 as an example)
head chr1_rsIDfromBED.bim
wc -l chr1_rsIDfromBED.bim

# ======= Convert plink binary to map and ped format ==============
# Because the ABCD genotype data is in Gr38 build, we need to liftover to hg19 
# To liftover, we first need to convert binary files to map and ped format
for i in {1..22}; do plink \
--bfile chr${i}_rsIDfromBED \
--recode --out chr${i}_rsIDfromBED_prelift; done

# ======= Run liftover to hg19 ==============
# This step converts the ABCD files from Gr38 to hg19 format
# This is for consistency with GWAS summary statistics, which are in hg19
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
for i in {1..22}; do python2 liftOverPlink.py \
-m chr${i}_rsIDfromBED_prelift.map \
-p chr${i}_rsIDfromBED_prelift.ped \
-o chr${i}_rsIDfromBED_hg19 \
-e liftOver \
-c hg38ToHg19.over.chain.gz; done

# Convert hg19 file back to bed format
module load plink/1.90b3.40
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
for i in {1..22}; do plink --file chr${i}_rsIDfromBED_hg19 --make-bed --out chr${i}_rsIDfromBED_hg19 --allow-extra-chr; done

# ======= Ensure each chromosome is named correctly ==============
# The chromosome names are not always correct, so ensure first column
# For each chromosome correctly labels the chromosome (e.g. chr1 should be "1")
R
library(data.table)
for (i in 1:22){
  data1 = fread(paste0("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/chr",i,"_rsIDfromBED_hg19.bim"))
  data1$V1 = i
  write.table(data1, file = paste0("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/chr",i,"_rsIDfromBED_hg19_update.bim"), row.names = F, col.names = F, quote = F)
}

q(save="no")

cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
# Check number of SNPs is the same before and after renaming chromosome
wc -l chr22_rsIDfromBED_hg19_update.bim #
wc -l chr22_rsIDfromBED_hg19.bim #(151088)

# ======= Combine all chromosomes into one file ==============
# Note: all bim files from the last step were moved into folder "hg19_15Dec" and were renamed to "rsIDfromBED_hg19.bim" 
# for consistency with bed and fam files (to be moved into the folder next)

# Copy all bed files into folder "hg19_15Dec"
cp chr1_rsIDfromBED_hg19.bed chr2_rsIDfromBED_hg19.bed chr3_rsIDfromBED_hg19.bed chr4_rsIDfromBED_hg19.bed chr5_rsIDfromBED_hg19.bed \
chr6_rsIDfromBED_hg19.bed chr7_rsIDfromBED_hg19.bed chr8_rsIDfromBED_hg19.bed chr9_rsIDfromBED_hg19.bed chr10_rsIDfromBED_hg19.bed hg19_15Dec
cp chr11_rsIDfromBED_hg19.bed chr12_rsIDfromBED_hg19.bed chr13_rsIDfromBED_hg19.bed chr14_rsIDfromBED_hg19.bed chr15_rsIDfromBED_hg19.bed \
chr16_rsIDfromBED_hg19.bed chr17_rsIDfromBED_hg19.bed chr18_rsIDfromBED_hg19.bed chr19_rsIDfromBED_hg19.bed chr20_rsIDfromBED_hg19.bed \
chr21_rsIDfromBED_hg19.bed chr22_rsIDfromBED_hg19.bed hg19_15Dec

# Copy all fam files into folder "hg19_15Dec"
cp chr1_rsIDfromBED_hg19.fam chr2_rsIDfromBED_hg19.fam chr3_rsIDfromBED_hg19.fam chr4_rsIDfromBED_hg19.fam chr5_rsIDfromBED_hg19.fam \
chr6_rsIDfromBED_hg19.fam chr7_rsIDfromBED_hg19.fam chr8_rsIDfromBED_hg19.fam chr9_rsIDfromBED_hg19.fam chr10_rsIDfromBED_hg19.fam \
chr11_rsIDfromBED_hg19.fam chr12_rsIDfromBED_hg19.fam chr13_rsIDfromBED_hg19.fam chr14_rsIDfromBED_hg19.fam chr15_rsIDfromBED_hg19.fam \
chr16_rsIDfromBED_hg19.fam chr17_rsIDfromBED_hg19.fam chr18_rsIDfromBED_hg19.fam chr19_rsIDfromBED_hg19.fam chr20_rsIDfromBED_hg19.fam \
chr21_rsIDfromBED_hg19.fam chr22_rsIDfromBED_hg19.fam hg19_15Dec

# Make list of files
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/hg19_15Dec
ls *.bim | sed 's/.bim//g' > merge_list #sed is a text extraction tool
head merge_list
tail merge_list

# Get a list of potentially problematic SNPs 
# If binary merging fails because at least one variant would have more than two alleles, a list of offending variant(s) 
# will be written to merged_chroms_15Dec.missnp.
plink --merge-list merge_list --make-bed --out /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/merged_chroms_15Dec
head merged_chroms_15Dec.missnp
# no such file, there are no problematic SNPs with more than 2 alleles

# ======= Check number of SNPs and individuals before later stages of QC ==============
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
wc -l merged_chroms_15Dec.bim ## 10,995,918 included SNPs before QC (28,634,576 in ALSPAC) 
wc -l merged_chroms_15Dec.fam ## 11,101 included individuals before QC

# ======= Prune SNPs to produce a subset of SNPs in linkage equilibrium  ==============
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
plink --bfile merged_chroms_15Dec \
--indep-pairwise 250 50 0.1 \
--out pruned_indepPW_250_50_0.1 

# ======= Update family IDs ==============
# Note: the ID variable (2nd column) combines the sample collection ID (beginning AB) and the ABCD study ID (beginning NDAR)
# This step will split the IDs so that FID = sample collection ID and IID = ABCD study ID

# First generate file in R indicating what the old and new FID and IID are
R # Launch R
library(data.table) # Load packages
library(tidyr)
abcd_genetic <- fread("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/merged_chroms_15Dec.fam")
head(abcd_genetic)

abcd_genetic$oldFID <- abcd_genetic$V1
abcd_genetic$oldIID <- abcd_genetic$V2

# Seperate V2 into FID and IID
abcd_genetic <- abcd_genetic %>% separate(V2, into = c('FID', 'IID'), sep = 10)
abcd_genetic$FID <- substr(abcd_genetic$FID,1,nchar(abcd_genetic$FID)-1)
head(abcd_genetic)

setwd("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files")
write.table(abcd_genetic[,c("oldFID", "oldIID", "FID", "IID")], file = "updatefamnames.txt", row.names = F, col.names = F, quote = F)
q(save="no")

# Update family names in plink file based on "update famnames" instruction file generated in R
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
plink --bfile merged_chroms_15Dec \
--update-ids updatefamnames.txt \
--make-bed \
--out merged_chroms_famNames15Dec

# ======= Quality control steps for removing samples and SNPs ==============
### QC Steps will include the following
# Exclude non-European ancestry
# Exclude related individuals (1 individual per pair with IBD >= 0.20)
# Exclude HWE P < 1e-6 
# Generate principal components

# Note that the ABCD Study Genetics team already conducted some QC on the data, including:
# removing samples with a low call rate (>10% (saliva samples) >20% (blood samples)
# gender mismatches (X-chromosome F < 0.5 for male or > 0.5 for female)
# removing SNPs with a low call rate (>10%)

# ======= Exclude non-European ancestry (>90% proportion European) ==============
# In this step we will exclude individuals with non-European genetic ancestry
# European ancestry is defined as a proportion of >90% European ancestry
# Proportion of European ancestry was calculated by the ABCD Study and the variable info is available: https://nda.nih.gov/data_structure.html?short_name=acspsw03
# We will check the concordance between those with European genetic ancestry and self-reported white race

R # Launch R
# Load packages
library(foreign)
library(gmodels)
library(dplyr)
library(data.table)

# Set file paths
childGeneticData <- file.path("/lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files")
childPhenoData <- file.path("/lustre/scratch/scratch/ucjujb6/ABCD_phenotypic_data")

# Read in phenotypic data with self-reported race data
setwd(childPhenoData)
abcd_pheno <- readRDS("ABCD_preImput.rds")
dim(abcd_pheno)

# Read in file with genetic ancestry proportions (https://nda.nih.gov/data_structure.html?short_name=acspsw03)
abcd_genAn <- fread("Package_1193821/acspsw03.txt")
head(abcd_genAn)
# Remove first row (variable description)
abcd_genAn <- abcd_genAn[-1,]
table(abcd_genAn$eventname) #contains data for baseline and 1 year follow-up

# Check genetic ancestry proportions
library(psych)
abcd_genAn$genetic_af_european <- as.numeric(abcd_genAn$genetic_af_european)
describe(abcd_genAn$genetic_af_european[abcd_genAn$eventname=="baseline_year_1_arm_1"]) # mean = 74% euro
describe(as.numeric(abcd_genAn$genetic_af_african)[abcd_genAn$eventname=="baseline_year_1_arm_1"]) # mean = 17% african
describe(as.numeric(abcd_genAn$genetic_af_east_asian)[abcd_genAn$eventname=="baseline_year_1_arm_1"]) # mean = 4% east Asian
describe(as.numeric(abcd_genAn$genetic_af_american)[abcd_genAn$eventname=="baseline_year_1_arm_1"]) # mean = 5% American

# Generate IDs of people who have >= 90% European genetic ancestry
abcd_genAn$euro_ancestry <- NA
abcd_genAn$euro_ancestry[abcd_genAn$genetic_af_european >= 0.90] <- 1
abcd_genAn$euro_ancestry[abcd_genAn$genetic_af_european < 0.90] <- 0
table(abcd_genAn$euro_ancestry) # 6103 have 90% European ancestry
abcd_euro_gen <- subset(abcd_genAn, euro_ancestry==1) # Sample with proportion of >=90% European ancestry
dim(abcd_euro_gen) # N=6,103

# Subset phenotypic data to select only subjects of white ethnicity [# Ethnic group c800:1 = white]
abcd_white_ethnic <- subset(abcd_pheno, race == "White")#
table(abcd_white_ethnic$race) # Shows only white participants (N=7,525)

# Check if those with >90% European ancestry are white
check <- abcd_euro_gen$src_subject_id %in% abcd_white_ethnic$src_subject_id
table(check)["TRUE"] # 5905 of the 6103 (96.8%) with >90% european ancestry self-report white ethnicity

# Create ID variable in European genetic ancestry subset indexing the indiviual ID, which matches the ID variable in the .fam file
# this will mean we can merge the file with the genotype .fam file in later steps
abcd_euro_gen$IID <- as.factor(paste0(noquote(abcd_euro_gen$src_subject_id)))
head(abcd_euro_gen$IID)

# Read in fam file from the genetic data
setwd(childGeneticData)
abcd_genetic <- fread("merged_chroms_famNames15Dec.fam")

head(abcd_genetic)
head(abcd_euro_gen)

# Merge the phenotypic sample with the genetic sample file 
datABCD <- merge(abcd_genetic, abcd_euro_gen, by.x="V2", by.y= "IID")
dim(datABCD) # 6103 children with European ancestry
head(datABCD)

# Drop unnecessary variables
datABCD_fam <- subset(datABCD, select=c(V1, V2, V3, V4, V5, V6))
head(datABCD_fam)

# Save table with IDs of all the included white individuals
write.table(datABCD_fam, 
            file = paste0(childGeneticData, "/ABCD_euroAncestry16Dec.fam"),
            col.names = F, row.names = F,quote=F, sep = "\t")

# Quit R          
q(save = "no")

# Restrict the sample to those with European genetic ancestry (N=6103)
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
plink --bfile merged_chroms_famNames15Dec \
--keep ABCD_euroAncestry16Dec.fam  \
--make-bed \
--out merged_chroms_famNames_euro16Dec \
--threads 15
wc -l merged_chroms_famNames_euro16Dec.fam #6103

# ======= Remove related individuals (IBD > 0.20) ==============
cd $software

# Check relatedness using KING
# This saves a file (related_3rd_degree16Dec.seg) which contains the IBD values of pairs related with IBD proportion > 0.0884 (over 3rd degree relatives)
cd /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files
$software/king  -b /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/merged_chroms_famNames_euro16Dec.bed \
                                    --ibdseg \
                                    --related \
                                    --degree 3 \
                                    --prefix related_3rd_degree16Dec

#Total length of 39 chromosomal segments usable for IBD segment analysis is 2681.6 Mb.
#Information of these chromosomal segments can be found in file related_3rd_degree16Decallsegs.txt

#Each family consists of one individual.
#Relationship inference across families starts at Thu Dec 16 16:29:37 2021
#18 CPU cores are used...Inference ends at Thu Dec 16 16:46:07 2021

#Relationship summary (total relatives: 0 by pedigree, 1252 by inference)
               # MZ      PO      FS      2nd
  #=====================================================
  #Inference     253     0       865     26


#Between-family relatives (kinship >= 0.04419) saved in file related_3rd_degree16Dec.kin0

#Total length of 39 chromosomal segments usable for IBD segment analysis is 2681.6 Mb.Information of these chromosomal segments can be found in file related_3rd_degree16Decallsegs.txt

#IBD segment analysis starts at Thu Dec 16 16:46:07 2021
#18 CPU cores are used for autosome inference...ends at Thu Dec 16 17:35:08 2021

#Note with relationship inference as the primary goal, the following filters are applied:
#Sample pairs without any long IBD segments (>10Mb) are excluded.
#Short IBD segments (<3Mb) are not reported/utilized.
#Summary statistics of IBD segments for individual pairs saved in file related_3rd_degree16Dec.seg
#IBD segments saved in a gzipped file related_3rd_degree16Dec.segments.gz
#KING ends at Thu Dec 16 17:35:08 2021
                                 
ls
head related_3rd_degree16Dec.seg
wc -l related_3rd_degree16Dec.seg #1261 pairs with IBD greater than > 0.0884

# Subset this file to those with IBD > 0.20 and then remove 1 in each pair 
# In R, generate a list of IDs to remove, by selecting one individual from each pair with IBD > 0.20
R # Launch R
# Load imiss and genome files
getwd()
ibd <- read.table("related_3rd_degree16Dec.seg", header=TRUE)
head(ibd)
ibd_0.20 <- subset(ibd, PropIBD>=0.20)
dim(ibd_0.20) # 1143 pairs with IBD > 20
head(ibd_0.20)
library(psych)
describe(ibd_0.20$PropIBD)

# Make dataframe with the IDs to exclude based on IBD > 0.20 (select ID1)
ibd_ids <- subset(ibd_0.20, select=c(FID1, ID1))
dim(ibd_ids)
head(ibd_ids)
ibd_ids <- unique(ibd_ids) # remove duplicated IDs
dim(ibd_ids) #1107 unique IDs
head(ibd_ids)

# Save list of IDs to exclude
write.table(ibd_ids, file = "IBD20_KING.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
q(save = "no")

### Remove related individuals (1107 people)
wc -l IBD20_KING.txt
head IBD20_KING.txt

plink --bfile merged_chroms_famNames_euro16Dec \
--remove IBD20_KING.txt \
--make-bed \
--out merged_chroms_famNames_euro_exclIBD20_16Dec

wc -l merged_chroms_famNames_euro_exclIBD20_16Dec.fam #4996 left

# ======= Exclude HWE P > 1e-6 ==============
plink --bfile merged_chroms_famNames_euro_exclIBD20_16Dec \
--hwe 1e-6 \
--make-bed \
--out merged_chroms_famNames_euro_exclIBD20_hweP5e7_16Dec

# ======= Summarise all QC steps ==============

##### Summarise number of included individuals 
## Before QC
wc -l merged_chroms_15Dec.fam # 11,101
## Exclude individuals with non-European ancestry
wc -l merged_chroms_famNames_euro16Dec.fam # 6103
## Exclude based on IBD >= 0.20
wc -l merged_chroms_famNames_euro_exclIBD20_16Dec.fam #4996
## Exclude HWE P < 1e-6 
wc -l merged_chroms_famNames_euro_exclIBD20_hweP5e7_16Dec.fam # 4996

##### Summarise number of included SNPs 
## Before QC
wc -l merged_chroms_15Dec.bim # 10,995,918
## Exclude individuals with non-European ancestry
wc -l merged_chroms_famNames_euro16Dec.bim # 10,995,918
## Exclude based on IBD >= 0.20
wc -l merged_chroms_famNames_euro_exclIBD20_16Dec.bim #10,995,918
## Exclude HWE P < 1e-6 
wc -l merged_chroms_famNames_euro_exclIBD20_hweP5e7_16Dec.bim # 10,987,366

#There is a total of 10,987,366 SNPs and 5,859 individuals included after QC. 

# Files saved in folder: /lustre/scratch/scratch/ucjujb6/ABCD_genetic_data/genomics_sample03/imputed/binary_files/abcd_qcd_16Dec21

# ======= Generate 10 Principal components ==============
plink --bfile merged_chroms_famNames_euro_exclIBD20_hweP5e7_16Dec \
--extract pruned_indepPW_250_50_0.1.prune.in \
--pca 10 \
--out merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca

### Examine files created
#-.eigenval contains the eigenvalues explained by each of the 10 principal components
#- .eigenvec contains the actual 10 principal components for each individual in the data set
```{bash, eval=FALSE, engine="sh"}
# .eigenval 
head merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenval
wc -l merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenval # 10

# .eigenvec
head merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenvec
wc -l merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenvec # 4996

##### Save PCs in overall ABCD folder to merge with the phenotypic data
cp merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenvec /lustre/scratch/scratch/ucjujb6/ABCD_phenotypic_data
cd /lustre/scratch/scratch/ucjujb6/ABCD_phenotypic_data
# Rename file
mv merged_chroms_famNames_euro_exclIBD20_hweP5e7_pca.eigenvec principal_components_10_16Dec.eigenvec
ls