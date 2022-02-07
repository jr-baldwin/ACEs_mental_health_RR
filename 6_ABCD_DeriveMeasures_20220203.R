################################################################################
##################### ABCD Study - Derive measures #############################
################################################################################

#### Load libraries
library(psych)

## ============== Import all datasets (from release 3 data) =================
setwd("/Users/jessie/Desktop")
# ksads PTSD
ksads_ptsd <- read.delim("Package_1188169/abcd_ptsd01.txt", header = TRUE, sep = "\t", dec = ".")
# Family Environment Scale - parent reported
fam_env_par <- read.delim("Package_1188169/fes02.txt", header = TRUE, sep = "\t", dec = ".")
# Family Environment Scale - youth reported
fam_env_youth <- read.delim("Package_1188169/abcd_fes01.txt", header = TRUE, sep = "\t", dec = ".")
# Children's Report of Parental Behavioral Inventory (download)
par_behav_invent_youth <- read.delim("Package_1188169/crpbi01.txt", header = TRUE, sep = "\t", dec = ".")
# Family History Assessment Part 1
fha_1 <- read.delim("Package_1188169/fhxp102.txt", header = TRUE, sep = "\t", dec = ".")
# Family History Assessment Part 2
fha_2 <- read.delim("Package_1188169/fhxp201.txt", header = TRUE, sep = "\t", dec = ".")
# Parent Life Events
ple <- read.delim("Package_1188169/abcd_ple01.txt", header = TRUE, sep = "\t", dec = ".")
# ABCD Longitudinal Parent Demographics Survey
long_par_dem <- read.delim("Package_1188169/abcd_lpds01.txt", header = TRUE, sep = "\t", dec = ".")
# Parent Demographics Survey
par_dem <- read.delim("Package_1188169/pdem02.txt", header = TRUE, sep = "\t", dec = ".")
# CBCL summary scores
cbcl <- read.delim("Package_1188169/abcd_cbcls01.txt", header = TRUE, sep = "\t", dec = ".")
# CBCL raw
cbcl_raw <-  read.delim("Package_1188169/abcd_cbcl01.txt", header = TRUE, sep = "\t", dec = ".")
# Child victimisation
vic <- read.delim("Package_1188169/abcd_peq01.txt", header = TRUE, sep = "\t", dec = ".")
# Parent CBCL sum scores
asrs <- read.delim("Package_1188169/abcd_asrs01.txt", header = TRUE, sep = "\t", dec = ".")
# Parent CBCL raw scores
rasr <- read.delim("Package_1188169/pasr01.txt", header = TRUE, sep = "\t", dec = ".")
# ABCD Developmental History Questionnaire
dhx01 <- read.delim("Package_1188169/dhx01.txt", header = TRUE, sep = "\t", dec = ".")

## ============== Format datasets before merging into one =================
## Combine dataframes into a list
List <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

tables <- List(asrs, cbcl, cbcl_raw, fam_env_par, fam_env_youth,
               fha_1, fha_2, ksads_ptsd, long_par_dem,
               par_behav_invent_youth, par_dem, ple, rasr, vic, dhx01)
names(tables)
len.tables=length(tables)

# Sometimes the "eventname" column in datasets is called "visit" 
# Check if this is the case in any dataframes
for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt) ){
    print(names(tables)[p]) # if visit is present, print the name of the dataframe
      }
}

# Rename "visit" to "eventname" in fha_2 and dhx01
tables$fha_2$eventname <- tables$fha_2$visit
tables$dhx01$eventname <- tables$dhx01$visit

# The first row in each spreadsheet is the element description, so remove from all dataframes
tables <- lapply(tables, function(x) x=x[-1,])
nrow(tables$asrs) # check that the number of rows is 1 less than the number shown in rasr (example dataset) in global environment

# Check if number of rows for participants with baseline data is 11878 in all dataframes; if not list exceptions
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (table(dt$eventname)["baseline_year_1_arm_1"] != 11878 & !is.na(table(dt$eventname)["baseline_year_1_arm_1"]) ){
    print(names(tables)[p]) # if number of rows is more than 11878, print name of dataframe
  }
}

# fam_env_par and fha_1 have more than 11878 rows, check levels in "eventname" variable in these datasets
table(fam_env_par$eventname[fam_env_par$eventname=="baseline_year_1_arm_1"]) # 23756 (double the number of rows) 
table(fha_1$eventname[fha_1$eventname=="baseline_year_1_arm_1"]) #23756 (double the number of rows) 

# fam_env_par and fha_1 have double the number of rows - this is because each subject in these 
# datasets has 2 dataset IDs (either 34384 or 34291)

# This was established with the below code: 
# Check for duplicate IDs in datasets with multiple rows per event (assessment)
fam_env_par_baseline <- subset(fam_env_par, eventname=="baseline_year_1_arm_1") # subset to only baseline assessment
fam_env_par_baseline <- fam_env_par_baseline[order(fam_env_par_baseline$src_subject_id),]  # order by subject ID
head(fam_env_par_baseline) # view dataset
length(unique(fam_env_par_baseline$src_subject_id)) #11878 unique IDs
n_occur <- data.frame(table(fam_env_par_baseline$src_subject_id)) # check if subject ID is duplicated in dataset
nrow(n_occur[n_occur$Freq > 1,]) # all subject IDs are duplicated
# each subject has 2 dataset IDs (either 34384 or 34291), so remove one 
# NDA confirmed that the duplicated data were in error, so fine to select one row per subject ID
tables$fam_env_par <- subset(tables$fam_env_par, dataset_id=="34384")
table(tables$fam_env_par$eventname) #11878

# Check why duplicates in fha_1 (because there are 2 dataset IDs for each person and each assessment)
tables$fha_1 <- tables$fha_1[order(tables$fha_1$src_subject_id),] 
head(tables$fha_1)
# each subject has 2 dataset IDs (either 34384 or 34403), so remove one 
tables$fha_1 <- subset(tables$fha_1, dataset_id=="34598")
table(tables$fha_1$eventname) #11878

#Drop columns introduced by NDA, they are not required in the resulting table.
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(names(dt) %in% c("collection_id", "collection_title", "promoted_subjectkey", 
                              "subjectkey", "study_cohort_name"))]
  tables[[p]] = dt
}

# Put individual dataframes in list back as separate objects in global environment
list2env(tables,.GlobalEnv)

## ============== Merge datafiles to create one ABCD dataset =================
abcd <- Reduce(function(x,y) merge(x,y,by=c("src_subject_id","eventname", "sex"), all=TRUE), #Outer join - preserves all rows
                    list(ksads_ptsd, 
                         fam_env_par, #edited so 1 row per participant assessment
                         fam_env_youth,
                         par_behav_invent_youth,
                         fha_1,  #edited so 1 row per participant assessment
                         fha_2,
                         ple,
                         long_par_dem,
                         par_dem,
                         cbcl,
                         cbcl_raw,
                         asrs,
                         rasr,
                         vic,
                         dhx01))
colnames(abcd)
dim(abcd) #29684 rows, because there are 3 possible assessments and the dataset is in long format
table(abcd$eventname)
head(abcd)
tail(abcd)

# Remove individual datafiles
rm(ksads_ptsd, fam_env_youth, fam_env_par, fam_env_par_baseline,
   par_behav_invent_youth, fha_1, fha_2, ple, long_par_dem,
   par_dem, cbcl, cbcl_raw, vic, asrs, rasr, dhx01,
   tables, dt, n_occur, len.tables, p)

####################################################################################
#--------------------- Derive mental health measures ------------------------------#
####################################################################################

#------------------------- Derive raw CBCL internalising -----------------------------------------#
### This measure includes the following items:
## Anxious/depressed
#cbcl_q14_p = Cries a lot
#cbcl_q29_p = Fears certain animals, situations, or places, other than school
#cbcl_q30_p = Fears going to school 
#cbcl_q31_p = Fears he/she might think or do something bad
#cbcl_q32_p = Feels he/she has to be perfect
#cbcl_q33_p = Feels or complains that no one loves him/her
#cbcl_q35_p = Feels worthless or inferior 
#cbcl_q45_p = Nervous, highstrung, or tense 
#cbcl_q50_p = Too fearful or anxious
#cbcl_q52_p = Feels too guilty
#cbcl_q71_p = Self-conscious or easily embarrassed
#cbcl_q91_p = Talks about killing self 
#cbcl_q112_p = Worries 

## Withdrawn/depressed
#cbcl_q05_p = There is very little he/she enjoys 
#cbcl_q42_p = Would rather be alone than with others
#cbcl_q65_p = Refuses to talk 
#cbcl_q69_p = Secretive, keeps things to self
#cbcl_q75_p = Too shy or timid 
#cbcl_q102_p = Underactive, slow moving, or lacks energy 
#cbcl_q103_p = Unhappy, sad, or depressed 
#cbcl_q111_p = Withdrawn, doesn't get involved with others 

## Somatic complaints
#cbcl_q47_p = Nightmares 
#cbcl_q49_p = Constipated, doesn't move bowels
#cbcl_q51_p = Feels dizzy or lightheaded 
#cbcl_q54_p = Overtired without good reason
#cbcl_q56a_p = Aches or pains (not stomach or headaches) 
#cbcl_q56b_p = Headaches 
#cbcl_q56c_p = Nausea, feels sick 
#cbcl_q56d_p = Problems with eyes (not if corrected by glasses)
#cbcl_q56e_p = Rashes or other skin problems 
#cbcl_q56f_p = Stomachaches 
#cbcl_q56g_p = Vomiting, throwing up 

# Convert item variables included in measure from factor to numeric
internalising_cols <- c("cbcl_q14_p", "cbcl_q29_p", "cbcl_q30_p", "cbcl_q31_p", 
                        "cbcl_q32_p", "cbcl_q33_p", "cbcl_q35_p", "cbcl_q45_p", 
                        "cbcl_q50_p", "cbcl_q52_p", "cbcl_q71_p", "cbcl_q91_p",
                        "cbcl_q112_p", "cbcl_q05_p", "cbcl_q42_p", "cbcl_q65_p",
                        "cbcl_q69_p", "cbcl_q75_p", "cbcl_q102_p", "cbcl_q103_p",
                        "cbcl_q111_p", "cbcl_q47_p", "cbcl_q49_p", "cbcl_q51_p", 
                        "cbcl_q54_p", "cbcl_q56a_p", "cbcl_q56b_p", "cbcl_q56c_p",
                        "cbcl_q56d_p", "cbcl_q56e_p", "cbcl_q56f_p", "cbcl_q56g_p") 
abcd[,internalising_cols] <- lapply(abcd[,internalising_cols], function(x) as.numeric(as.character(x)))

# Derive raw CBCL internalising sum score
abcd$cbcl_internalising <- apply(abcd [,internalising_cols], 1, sum, na.rm=T) # sum items
abcd$cbcl_internalising[apply(apply(abcd[,internalising_cols],2,is.na),1,sum) > 16] <- NA # set to missing if more than 50% missing

# Check against pre-derived CBCL internalising raw score
psych::describe(abcd$cbcl_internalising)  # derived measure
psych::describe(as.numeric(abcd$cbcl_scr_syn_internal_r))# original measure

#------------------------- Derive raw CBCL externalising -----------------------------------------#
# Note this sum score will include the following items, from the subscales of 
# rule-Breaking Behavior, Aggressive Behavior and Attention Problems

## Rule-Breaking Behavior
#cbcl_q02_p = Drinks alcohol without parents' approval
#cbcl_q26_p = Doesn't seem to feel guilty after misbehaving 
#cbcl_q28_p = Breaks rules at home, school or elsewhere
#cbcl_q39_p = Hangs around with others who get in trouble
#cbcl_q43_p = Lying or cheating
#cbcl_q63_p = Prefers being with older kids 
#cbcl_q67_p = Runs away from home
#cbcl_q72_p = Sets fires
#cbcl_q73_p = Sexual problems
#cbcl_q81_p = Steals at home 
#cbcl_q82_p = Steals outside the home
#cbcl_q90_p = Swearing or obscene language
#cbcl_q96_p = Thinks about sex too much
#cbcl_q99_p = Smokes, chews, or sniffs tobacco
#cbcl_q101_p = Truancy, skips school 
#cbcl_q105_p = Uses drugs for non medical purposes (don't include alcohol or tobacco) 
#cbcl_q106_p = Vandalism

## Aggressive Behavior
#cbcl_q03_p = Argues a lot 
#cbcl_q16_p = Cruelty, bullying, or meanness to others
#cbcl_q19_p = Demands a lot of attention
#cbcl_q20_p = Destroys his/her own things 
#cbcl_q21_p = Destroys things belonging to his/her family or others
#cbcl_q22_p = Disobedient at home 
#cbcl_q23_p = Disobedient at school 
#cbcl_q37_p = Gets in many fights
#cbcl_q57_p = Physically attacks people
#cbcl_q68_p = Screams a lot 
#cbcl_q86_p = Stubborn, sullen, or irritable
#cbcl_q87_p = Sudden changes in mood or feelings 
#cbcl_q88_p = Sulks a lot 
#cbcl_q89_p = Suspicious
#cbcl_q94_p = Teases a lot
#cbcl_q95_p = Temper tantrums or hot temper 
#cbcl_q97_p = Threatens people
#cbcl_q104_p = Unusually loud

## Attention problems
#cbcl_q01_p = Acts too young for his/her age 
#cbcl_q04_p = Fails to finish things he/she starts 
#cbcl_q08_p = Can't concentrate, can't pay attention for long
#cbcl_q10_p = Can't sit still, restless, or hyperactive 
#cbcl_q13_p = Confused or seems to be in a fog 
#cbcl_q17_p = Daydreams or gets lost in his/her thoughts
#cbcl_q41_p = Impulsive or acts without thinking
#cbcl_q61_p = Poor school work 
#cbcl_q78_p = Inattentive or easily distracted 
#cbcl_q80_p = Stares blankly

# Convert variables from factor to numeric 
externalising_cols <- c("cbcl_q02_p", "cbcl_q26_p","cbcl_q28_p", "cbcl_q39_p",
                        "cbcl_q43_p", "cbcl_q63_p", "cbcl_q67_p", "cbcl_q72_p", 
                        "cbcl_q73_p", "cbcl_q81_p", "cbcl_q82_p", "cbcl_q90_p", 
                        "cbcl_q96_p", "cbcl_q99_p", "cbcl_q101_p", "cbcl_q105_p", 
                        "cbcl_q106_p", "cbcl_q03_p", "cbcl_q16_p","cbcl_q19_p", 
                        "cbcl_q20_p", "cbcl_q21_p", "cbcl_q22_p", "cbcl_q23_p", 
                        "cbcl_q37_p", "cbcl_q57_p","cbcl_q68_p", "cbcl_q86_p", 
                        "cbcl_q87_p", "cbcl_q88_p", "cbcl_q89_p", "cbcl_q94_p",
                        "cbcl_q95_p", "cbcl_q97_p","cbcl_q104_p", 
                        "cbcl_q01_p", "cbcl_q04_p", "cbcl_q08_p", "cbcl_q10_p",
                        "cbcl_q13_p", "cbcl_q17_p", "cbcl_q41_p", "cbcl_q61_p",
                        "cbcl_q78_p", "cbcl_q80_p")

abcd[,externalising_cols] <- lapply(abcd[,externalising_cols], function(x) as.numeric(as.character(x)))

# Derive raw CBCL externalising sum score
abcd$cbcl_externalising <- apply(abcd [,externalising_cols], 1, sum, na.rm=T) # sum items
abcd$cbcl_externalising[apply(apply(abcd[,externalising_cols],2,is.na),1,sum) > 22] <- NA #set missing if >50% items NA

# check against pre-derived CBCL externalising raw score 
# note: the distributions are different because the pre-derived CBCL externalising does not include attention problems
psych::describe(abcd$cbcl_externalising)  # derived measure
psych::describe(as.numeric(abcd$cbcl_scr_syn_external_r)) # original measure

####################################################################################
#-------------------------- Derive ACEs measures ----------------------------------#
####################################################################################

#------------------------- Maltreatment - recode KSADS items ------------------------------------#
# note: items are largely from the ABCD Parent Diagnostic Interview for DSM-5 (KSADS) Traumatic Events
# https://nda.nih.gov/data_structure.html?short_name=abcd_ptsd01
# note: many component items from KSADS have empty responses of ""; so code to missing
# physical abuse
abcd$ksads_ptsd_raw_762_p[abcd$ksads_ptsd_raw_762_p==""] <- NA
abcd$ksads_ptsd_raw_763_p[abcd$ksads_ptsd_raw_763_p==""] <- NA

# sexual abuse
abcd$ksads_ptsd_raw_767_p[abcd$ksads_ptsd_raw_767_p==""] <- NA
abcd$ksads_ptsd_raw_768_p[abcd$ksads_ptsd_raw_768_p==""] <- NA
abcd$ksads_ptsd_raw_769_p[abcd$ksads_ptsd_raw_769_p==""] <- NA

# emotional abuse
abcd$ksads_ptsd_raw_765_p[abcd$ksads_ptsd_raw_765_p==""] <- NA
abcd$ksads_ptsd_raw_764_p[abcd$ksads_ptsd_raw_764_p==""] <- NA

#------------------------- Emotional neglect -----------------------------------------#
# Note: emotional neglect items are from the ABCD Children's Report of Parental Behavioral Inventory
# https://nda.nih.gov/data_structure.html?short_name=crpbi01

# code responses of "" to missing
abcd$crpbi_parent1_y[abcd$crpbi_parent1_y==""] <- NA
abcd$crpbi_parent2_y[abcd$crpbi_parent2_y==""] <- NA
abcd$crpbi_parent3_y[abcd$crpbi_parent3_y==""] <- NA
abcd$crpbi_parent4_y[abcd$crpbi_parent4_y==""] <- NA
abcd$crpbi_parent5_y[abcd$crpbi_parent5_y==""] <- NA

# convert emotional neglect items to numeric
abcd$crpbi_parent1_y <- as.numeric(as.character(abcd$crpbi_parent1_y))
abcd$crpbi_parent2_y <- as.numeric(as.character(abcd$crpbi_parent2_y))
abcd$crpbi_parent3_y <- as.numeric(as.character(abcd$crpbi_parent3_y))
abcd$crpbi_parent4_y <- as.numeric(as.character(abcd$crpbi_parent4_y))
abcd$crpbi_parent5_y <- as.numeric(as.character(abcd$crpbi_parent5_y))

# Code each individual item so a score of 1 indicates neglect
# then if the child reports more than 2 items, code as overall neglect

# First caregiver makes me feel better after talking over my worries with him/her
# 1=not like him /her
table(abcd$crpbi_parent1_y[abcd$eventname=="baseline_year_1_arm_1"])
abcd$crpbi_parent1_y_r <- ifelse(abcd$crpbi_parent1_y==1, c(1), c(0)) # code to 1 if not like him/her
table(abcd$crpbi_parent1_y_r[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# First caregiver smiles at me very often
# 1=not like him /her
table(abcd$crpbi_parent2_y[abcd$eventname=="baseline_year_1_arm_1"])
abcd$crpbi_parent2_y_r <- ifelse(abcd$crpbi_parent2_y==1, c(1), c(0)) # code to 1 if not like him/her
table(abcd$crpbi_parent2_y_r[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# First caregiver is able to make me feel better when I am upset
# 1=not like him /her
table(abcd$crpbi_parent3_y[abcd$eventname=="baseline_year_1_arm_1"])
abcd$crpbi_parent3_y_r <- ifelse(abcd$crpbi_parent3_y==1, c(1), c(0)) # code to 1 if not like him/her
table(abcd$crpbi_parent3_y_r[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# First caregiver believes in showing his/her love for me
# 1=not like him /her
table(abcd$crpbi_parent4_y[abcd$eventname=="baseline_year_1_arm_1"])
abcd$crpbi_parent4_y_r <- ifelse(abcd$crpbi_parent4_y==1, c(1), c(0)) # code to 1 if not like him/her
table(abcd$crpbi_parent4_y_r[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# First caregiver is easy to talk to
# 1=not like him /her
table(abcd$crpbi_parent5_y[abcd$eventname=="baseline_year_1_arm_1"])
abcd$crpbi_parent5_y_r <- ifelse(abcd$crpbi_parent5_y==1, c(1), c(0)) # code to 1 if not like him/her
table(abcd$crpbi_parent5_y_r[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

## Generate overall emotional neglect sum score 
abcd$emotional_neglect_sum <- NA
abcd$emotional_neglect_sum <- apply(abcd [,c("crpbi_parent1_y_r", "crpbi_parent2_y_r",
                                          "crpbi_parent3_y_r", "crpbi_parent4_y_r", 
                                          "crpbi_parent5_y_r") ], 1, sum, na.rm=T)
abcd$emotional_neglect_sum[apply(apply(abcd[,c("crpbi_parent1_y_r", "crpbi_parent2_y_r",
                                            "crpbi_parent3_y_r", "crpbi_parent4_y_r", 
                                            "crpbi_parent5_y_r")],2,is.na),1,sum) > 2] <- NA
describe(abcd$emotional_neglect_sum[abcd$eventname=="baseline_year_1_arm_1"])
table(abcd$emotional_neglect_sum[abcd$eventname=="baseline_year_1_arm_1"]) # A score of 2 or more indicates emotional neglect

#------------------------- Maltreatment - derive composite measure ------------------------------------#
# Defined as evidence of any: 
# sexual abuse (was touched by/touched an adult, did sexual things with an adult, or was forced by a peer into sexual activities)
# physical abuse (shot, stabbed, or beaten brutally by an adult in the home, beaten to the point of having bruises by an adult in the home)
# emotional abuse (threatened to be killed by a family member or non-family member), or
# emotional neglect (>=2 of the following: caregiver does not believe in showing love for the child; does not make child feel 
# better when upset; or when talking over worries; is not easy to talk to; does not smile at child very often)

abcd$maltreatment <- 0
abcd$maltreatment[abcd$ksads_ptsd_raw_762_p==1 | abcd$ksads_ptsd_raw_763_p == 1 | # code exposed if evidence of physical abuse 
                      abcd$ksads_ptsd_raw_767_p==1 | abcd$ksads_ptsd_raw_768_p == 1 | abcd$ksads_ptsd_raw_769_p == 1 | # code exposed if evidence of sexual abuse 
                      abcd$ksads_ptsd_raw_764_p==1  | abcd$ksads_ptsd_raw_765_p==1 | # code exposed if evidence of emotional abuse 
                      abcd$emotional_neglect_sum >= 2] <- 1 # code exposed if evidence of emotional neglect 
abcd$maltreatment[apply(apply(abcd[,c("ksads_ptsd_raw_762_p", "ksads_ptsd_raw_763_p", 
                                        "ksads_ptsd_raw_767_p", "ksads_ptsd_raw_768_p", "ksads_ptsd_raw_769_p",
                                        "ksads_ptsd_raw_765_p", "emotional_neglect_sum")],2,is.na),1,sum) > 3] <- NA # code missing if missing 3/8 items
table(abcd$maltreatment[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") # 318 missing
prop.table(table(abcd$maltreatment[abcd$eventname=="baseline_year_1_arm_1"])) #5.3%
abcd$maltreatment <- as.factor(abcd$maltreatment)

# Check missingness in component maltreatment measures 
library(finalfit)
library(dplyr)
abcd[abcd$eventname=="baseline_year_1_arm_1",c("ksads_ptsd_raw_762_p", "ksads_ptsd_raw_763_p", 
        "ksads_ptsd_raw_767_p", "ksads_ptsd_raw_768_p", "ksads_ptsd_raw_769_p",
        "ksads_ptsd_raw_765_p", "emotional_neglect_sum")]  %>% 
  missing_pattern
# this shows that N=314 are missing all KSADS measures (of abuse); N=30 missing emotional neglect only

#------------------------- Maltreatment - derive individual subtypes ------------------------------------#
# Note: maltreatment subtypes variables will be used in the multiple imputation model 
# Physical abuse
abcd$physical_abuse <- 0
abcd$physical_abuse[abcd$ksads_ptsd_raw_762_p==NA & abcd$ksads_ptsd_raw_763_p==NA] <- NA
abcd$physical_abuse[abcd$ksads_ptsd_raw_762_p==1 | abcd$ksads_ptsd_raw_763_p==1] <- 1
abcd$physical_abuse <- as.factor(abcd$physical_abuse)
table(abcd$physical_abuse[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Sexual abuse
abcd$sexual_abuse <- 0
abcd$sexual_abuse[abcd$ksads_ptsd_raw_767_p==1 | abcd$ksads_ptsd_raw_768_p==1 | abcd$ksads_ptsd_raw_769_p==1] <- 1
abcd$sexual_abuse[apply(apply(abcd[,c("ksads_ptsd_raw_767_p", "ksads_ptsd_raw_768_p",
                                           "ksads_ptsd_raw_769_p")],2,is.na),1,sum) > 1] <- NA
abcd$sexual_abuse <- as.factor(abcd$sexual_abuse)
table(abcd$sexual_abuse[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Emotional abuse
abcd$emotional_abuse <- 0
abcd$emotional_abuse[abcd$ksads_ptsd_raw_765_p==NA & abcd$ksads_ptsd_raw_764_p==NA] <- NA
abcd$emotional_abuse[abcd$ksads_ptsd_raw_765_p==1 | abcd$ksads_ptsd_raw_764_p==1] <- 1
abcd$emotional_abuse <- as.factor(abcd$emotional_abuse)
table(abcd$emotional_abuse[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Emotional neglect
abcd$emotional_neglect <- 0
abcd$emotional_neglect[abcd$emotional_neglect_sum >=2] <- 1
abcd$emotional_neglect[abcd$emotional_neglect_sum == NA] <- NA
abcd$emotional_neglect <- as.factor(abcd$emotional_neglect)
table(abcd$emotional_neglect[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

#------------------------- Domestic violence -----------------------------------------#
# Items included are the following:
# ksads_ptsd_raw_766_p = Witness the grownups in the home push, shove or hit one another
# fam_enviro6_p = Family members sometimes hit each other (parent-report)
# fes_youth_q6 = Family members sometimes hit each other (child-report)

# Code component items with response of "" to missing
abcd$ksads_ptsd_raw_766_p[abcd$ksads_ptsd_raw_766_p==""] <- NA
abcd$fam_enviro6_p[abcd$fam_enviro6_p==""] <- NA
abcd$fes_youth_q6[abcd$fes_youth_q6==""] <- NA

# Check reports of exposure for each item
table(abcd$ksads_ptsd_raw_766_p[abcd$eventname=="baseline_year_1_arm_1"]) # Witness the grownups in the home push, shove or hit one another
table(abcd$fam_enviro6_p[abcd$eventname=="baseline_year_1_arm_1"]) # Family members sometimes hit each other (parent-report)
table(abcd$fes_youth_q6[abcd$eventname=="baseline_year_1_arm_1"]) # Family members sometimes hit each other (child-report)

# Check type of variable (character)
class(abcd$ksads_ptsd_raw_766_p[abcd$eventname=="baseline_year_1_arm_1"])
class(abcd$fam_enviro6_p[abcd$eventname=="baseline_year_1_arm_1"])
class(abcd$fes_youth_q6[abcd$eventname=="baseline_year_1_arm_1"])

# Derive domestic violence variable (missing if 1 or more of 3 values are missing)
abcd$domestic_violence <- 0
abcd$domestic_violence[abcd$ksads_ptsd_raw_766_p==1 | abcd$fam_enviro6_p == 1 | abcd$fes_youth_q6 == 1] <- 1
abcd$domestic_violence[apply(apply(abcd[,c("ksads_ptsd_raw_766_p", "fam_enviro6_p", "fes_youth_q6")],2,is.na),1,sum) >= 1] <- NA
abcd$domestic_violence <- as.factor(abcd$domestic_violence)
table(abcd$domestic_violence[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") # 341 missing
prop.table(table(abcd$domestic_violence[abcd$eventname=="baseline_year_1_arm_1"], useNA="always"))

#------------------------- Parental psychopathology -----------------------------------------#
### Items included are the following:
## Adult Self-Report (ASR) t-score scales:
# asr_scr_depress_t = Depressive Problems ASR DSM-5-Oriented Scale (t-score)
# asr_scr_anxdisord_t = Anxiety Problems ASR DSM-5-Oriented Scale (t-score)
# asr_scr_adhd_t = AD/H Problems ASR DSM-5-Oriented Scale (t-score)
## Family History variables:
# fam_history_6_yes_no - Has ANY blood relative of your child ever suffered from depression,
# fam_history_7_yes_no - Has ANY blood relative of your child ever had a period of time when others were concerned because they suddenly became more active day and night and seemed not to need any sleep and talked much more than usual for them?
# fam_history_8_yes_no - Has ANY blood relative of your child ever had a period lasting six months when they saw visions or heard voices or thought people were spying on them or plotting against them? 
# fam_history_13_yes_no - Has ANY blood relative of your child ever attempted or committed suicide? 
## Family history specifiers - relating to mother or father
# fam_history_q6a_depression - biological father - depression
# fam_history_q6d_depression - biological mother - depression
# fam_history_q7a_mania==1 - biological father - mania
# fam_history_q7d_mania==1 - biological mother - mania 
# fam_history_q8a_visions==1 - biological father - visions
# fam_history_q8d_visions==1 - biological mother - visions
# fam_history_q13a_suicide==1 - biological father - suicide
# fam_history_q13d_suicide==1 - biological mother - suicide

# Code Adult Self-Report (ASR) component items with response of "" to missing
abcd$asr_scr_depress_t[abcd$asr_scr_depress_t==""] <- NA
abcd$asr_scr_anxdisord_t[abcd$asr_scr_anxdisord_t==""] <- NA
abcd$asr_scr_adhd_t[abcd$asr_scr_adhd_t==""] <- NA

# Code ASR variables to numeric
asr_vars <- c("asr_scr_depress_t", "asr_scr_anxdisord_t", "asr_scr_adhd_t") 
class(asr_vars)
abcd[,asr_vars] <- lapply(abcd[,asr_vars], function(x) as.numeric(as.character(x)))

# Generate clinical cut-off scores for ASR variables
# asr_scr_depress_t = Depressive Problems ASR DSM-5-Oriented Scale (t-score)
abcd$asr_scr_depress_t_cutoff <- ifelse(abcd$asr_scr_depress_t > 63, 1, 0)
# asr_scr_anxdisord_t = Anxiety Problems ASR DSM-5-Oriented Scale (t-score)
abcd$asr_scr_anxdisord_t_cutoff <- ifelse(abcd$asr_scr_anxdisord_t > 63, 1, 0)
# asr_scr_adhd_t = ADHD Problems ASR DSM-5-Oriented Scale (t-score)
abcd$asr_scr_adhd_t_cutoff <- ifelse(abcd$asr_scr_adhd_t > 63, 1, 0)

# Check prevalence of ASR variables
table(abcd$asr_scr_depress_t_cutoff[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
table(abcd$asr_scr_anxdisord_t_cutoff[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
table(abcd$asr_scr_adhd_t_cutoff[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Check prevalence family history variables
table(abcd$fam_history_6_yes_no[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") # Has ANY blood relative of your child ever suffered from depression,
table(abcd$fam_history_7_yes_no[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") # Has ANY blood relative of your child ever had a period of time when others were concerned because they suddenly became more active day and night and seemed not to need any sleep and talked much more than usual for them?
table(abcd$fam_history_8_yes_no[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") #Has ANY blood relative of your child ever had a period lasting six months when they saw visions or heard voices or thought people were spying on them or plotting against them? 
table(abcd$fam_history_13_yes_no[abcd$eventname=="baseline_year_1_arm_1"], useNA="always") # Has ANY blood relative of your child ever attempted or committed suicide? 

# Recode family history variables to missing if values are 999 or 7
abcd$fam_history_6_yes_no[abcd$fam_history_6_yes_no=="999" | abcd$fam_history_6_yes_no=="7"] <- NA
abcd$fam_history_7_yes_no[abcd$fam_history_7_yes_no=="999" | abcd$fam_history_7_yes_no=="7"] <- NA
abcd$fam_history_8_yes_no[abcd$fam_history_8_yes_no=="999" | abcd$fam_history_8_yes_no=="7"] <- NA
abcd$fam_history_13_yes_no[abcd$fam_history_13_yes_no=="999" | abcd$fam_history_13_yes_no=="7"] <- NA

## Derive parental psychopathology composite variable
abcd$parental_psychopathology <- 0
abcd$parental_psychopathology[
  # Code as exposed if biological mother or father has depression, manic episode, psychotic experiences or suicide attempt
  abcd$fam_history_6_yes_no==1 & abcd$fam_history_q6a_depression==1 |  # a = biological father, d = biological mother
    abcd$fam_history_6_yes_no==1 & abcd$fam_history_q6d_depression==1 | 
    abcd$fam_history_7_yes_no==1 & abcd$fam_history_q7a_mania==1 | 
    abcd$fam_history_7_yes_no==1 & abcd$fam_history_q7d_mania==1 | 
    abcd$fam_history_8_yes_no==1 & abcd$fam_history_q8a_visions==1 | 
    abcd$fam_history_8_yes_no==1 & abcd$fam_history_q8d_visions==1 |
    abcd$fam_history_13_yes_no==1 & abcd$fam_history_q13a_suicide==1 | 
    abcd$fam_history_13_yes_no==1 & abcd$fam_history_q13d_suicide==1 |
    # Code as exposed if parent has ASR score above cut-off (63) for depression, anxiety, or ADHD
    abcd$asr_scr_depress_t_cutoff==1 | 
    abcd$asr_scr_anxdisord_t_cutoff == 1 |
    abcd$asr_scr_adhd_t_cutoff == 1 ] <- 1
abcd$parental_psychopathology[apply(apply(abcd[,c("fam_history_6_yes_no", "fam_history_7_yes_no", 
                                      "fam_history_8_yes_no", "fam_history_13_yes_no", "asr_scr_depress_t_cutoff",
                                      "asr_scr_anxdisord_t_cutoff", "asr_scr_adhd_t_cutoff")],2,is.na),1,sum) > 3] <- NA # code missing if missing 3/8 core items
abcd$parental_psychopathology <- as.factor(abcd$parental_psychopathology)
table(abcd$parental_psychopathology[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
prop.table(table(abcd$parental_psychopathology[abcd$eventname=="baseline_year_1_arm_1"])) # 37%

#------------------------- Parental substance abuse -----------------------------------------#
## Items include the following:
# asr_q126_p = In the past 6 months, on how many days did you use drugs for nonmedical purposes (including marijuana, cocaine, and other drugs, except alcohol and nicotine)? 
# famhx_4_p	4 = Has ANY blood relative of your child ever had any problems due to alcohol?
# fam_history_5_yes_no = Has ANY blood relative of your child ever had any problems due to drugs?

## Recode component variables
# asr_q126_p = In the past 6 months, on how many days did you use drugs for nonmedical purposes (including marijuana, cocaine, and other drugs, except alcohol and nicotine)? 
table(abcd$asr_q126_p)
abcd$asr_q126_p[abcd$asr_q126_p==""] <- NA # code ASR component items with response of "" to missing
class(abcd$asr_q126_p)
abcd$asr_q126_p <- as.numeric(abcd$asr_q126_p)  # convert to numeric
describe(abcd$asr_q126_p[abcd$eventname=="baseline_year_1_arm_1"]) 
# Assess number who used drugs multiple times per week for past 6 months: 
# 26 weeks equates to 6 months. So multiple times per week for 6 months would be >=52 times (on average 2 or more times per week)
psych::describe(abcd$asr_q126_p[abcd$eventname=="baseline_year_1_arm_1" & abcd$asr_q126_p>=52]) #131 reported that

# famhx_4_p	4 = Has ANY blood relative of your child ever had any problems due to alcohol?
abcd$famhx_4_p[abcd$famhx_4_p=="" | abcd$famhx_4_p=="7" | abcd$famhx_4_p=="999"] <- NA # code items with response of "", 7 or 999 to missing
table(abcd$famhx_4_p[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# fam_history_5_yes_no = Has ANY blood relative of your child ever had any problems due to drugs?
table(abcd$fam_history_5_yes_no)
abcd$fam_history_5_yes_no[abcd$fam_history_5_yes_no=="" | 
                            abcd$fam_history_5_yes_no=="7" | 
                            abcd$fam_history_5_yes_no=="999"] <- NA # code items with response of "", 7 or 999 to missing
table(abcd$fam_history_5_yes_no[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Derive parental substance abuse composite variable
abcd$parental_substance <- 0
abcd$parental_substance[abcd$asr_q126_p>=52 | # used drugs multiple times weekly in past 6 months
                          # biological father had problems due to alcohol
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___1==1 | # marital problems
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___2==1 | # work problems
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___3==1 | # arrests/DUI
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___4==1 | # alcohol treatment programme
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___6==1 | # isolated self, arguments, drunk a lot 
                          abcd$famhx_4_p==1 & abcd$famhx4a_p___7==1 | # health problems
                          # biological mother had problems due to alcohol
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___1==1 | # marital problems
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___2==1 | # work problems
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___3==1 | # arrests/DUI
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___4==1 | # alcohol treatment programme
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___6==1 | # isolated self, arguments, drunk a lot 
                          abcd$famhx_4_p==1 & abcd$famhx_4d_p___7==1 | # health problems
                          # biological father had problems due to drugs
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___1==1 | # marital problems
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___2==1 | # work problems
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___3==1 | # arrests/DUI
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___4==1 | # drug treatment programme
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___6==1 | # isolated self, arguments, drunk a lot 
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___7==1 | # health problems
                          # biological mother had problems due to drugs
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5d_drugs___1==1 | # marital problems
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5d_drugs___2==1 | # work problems
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5d_drugs___3==1 | # arrests/DUI
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5d_drugs___4==1 | # drug treatment programme
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5d_drugs___6==1 | # isolated self, arguments, drunk a lot 
                          abcd$fam_history_5_yes_no==1 & abcd$fam_history_q5a_drugs___7==1] <- 1
abcd$parental_substance[apply(apply(abcd[,c("asr_q126_p", "famhx_4_p", 
                                                  "fam_history_5_yes_no")],2,is.na),1,sum) > 1] <- NA # code missing if missing >1/3 items
abcd$parental_substance <- as.factor(abcd$parental_substance)
table(abcd$parental_substance[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
prop.table(table(abcd$parental_substance[abcd$eventname=="baseline_year_1_arm_1"])) #20%

#------------------------- Parental criminality -----------------------------------------#
## Component items include the following:
# ple_law_p = Parents/caregiver got into trouble with the law?
# ple_law_past_yr_p	NO(0) = Did this happen in the past year? 
# ple_jail_p = One of the parents/caregivers went to jail?
# ple_jail_past_yr_p NO(0) = Did this happen in the past year? 
# ple_arrest_p = Someone in the family was arrested?
# ple_arrest_past_yr_p	NO(0) = Did this happen in the past year? 

# code component items with response of "" to missing
table(abcd$ple_law_p)
abcd$ple_law_p[abcd$ple_law_p==""] <- NA
table(abcd$ple_arrest_p)
abcd$ple_arrest_p[abcd$ple_arrest_p==""] <- NA
table(abcd$ple_jail_p)
abcd$ple_jail_p[abcd$ple_jail_p==""] <- NA

# Derive parental criminality variable (assessed at 1 year follow-up)
abcd$parent_criminality <- 0
abcd$parent_criminality[abcd$ple_arrest_p==1 & abcd$ple_arrest_past_yr_p==0 | # family member was arrested and was not in past year
                          abcd$ple_law_p==1 & abcd$ple_law_past_yr_p==0 | # parent in trouble with law and was not in past year
                          abcd$ple_jail_p==1 & abcd$ple_jail_past_yr_p==0 ] <- 1 # parent in jail and was not in past year
abcd$parent_criminality[apply(apply(abcd[,c("ple_arrest_p", "ple_law_p", "ple_jail_p")],2,is.na),1,sum) > 1] <- NA # code to missing if missing more than 1/3 items
abcd$parent_criminality <- as.factor(abcd$parent_criminality)
table(abcd$parent_criminality[abcd$eventname=="1_year_follow_up_y_arm_1"], useNA="always")
prop.table(table(abcd$parent_criminality[abcd$eventname=="1_year_follow_up_y_arm_1"])) #3.5%

#------------------------- Parental separation -----------------------------------------#
## Component items include the following:
# demo_prnt_marital_v2 = Are you now married, widowed, divorced, separated, never married or living with a partner? 
# demo_prnt_prtnr_v2 = Do you have a partner?
# demo_prnt_prtnr_bio = Is your partner the child's biological parent? 
# demo_prnt_prtnr_adopt = Is your partner the child's adoptive parent? 
# ple_separ_p = Parents separated or divorced? 
# ple_separ_past_yr_p = Did this happen in the past year?

# Code component items with response of "" to missing
table(abcd$demo_prnt_marital_v2)
abcd$demo_prnt_marital_v2[abcd$demo_prnt_marital_v2=="" | abcd$demo_prnt_marital_v2=="777"] <- NA
table(abcd$demo_prnt_prtnr_v2)
abcd$demo_prnt_prtnr_v2[abcd$demo_prnt_prtnr_v2=="" | abcd$demo_prnt_prtnr_v2=="777"] <- NA
table(abcd$ple_separ_p)
abcd$ple_separ_p[abcd$ple_separ_p==""] <- NA
table(abcd$ple_separ_p[abcd$eventname=="1_year_follow_up_y_arm_1"])

# Convert variables used to index parental separation from character to factor
table(abcd$demo_prnt_marital_v2)
abcd$demo_prnt_marital_v2 <- as.factor(abcd$demo_prnt_marital_v2)
table(abcd$demo_prnt_prtnr_v2)
abcd$demo_prnt_prtnr_v2 <- as.factor(abcd$demo_prnt_prtnr_v2)
table(abcd$demo_prnt_prtnr_bio)
abcd$demo_prnt_prtnr_bio <- as.factor(abcd$demo_prnt_prtnr_bio)
table(abcd$demo_prnt_prtnr_adopt)
abcd$demo_prnt_prtnr_adopt <- as.factor(abcd$demo_prnt_prtnr_adopt)
table(abcd$ple_separ_p)
abcd$ple_separ_p <- as.factor(abcd$ple_separ_p)
table(abcd$ple_separ_past_yr_p)
abcd$ple_separ_past_yr_p <- as.factor(abcd$ple_separ_past_yr_p)

# Parental separation was assessed in the demographic survey at baseline and 1-year follow-up assessment
# Therefore the variable will be created when the ABCD dataset has been converted from long format to wide format

####################################################################################
#--------------- Derive auxiliary variables for imputation ------------------------#
####################################################################################

## Socio-demographic indicators
# Child sex = sex
# Child race = demo_race_a_p___10
# Parental employment = demo_prnt_empl_v2
# Parental highest education qualification = demo_prnt_ed_v2
# Family income = demo_prnt_income_v2
# Household size = demo_roster_v2_refuse
# Maternal age at birth = devhx_3_p
## Adversity exposure before birth
# Birthweight = birth_weight_lbs
# Premature birth = devhx_12a_p
# Maternal smoking during pregnancy = devhx_8_tobacco
# Maternal alcohol consumption during pregnancy = devhx_9_alcohol
# Pregnancy complications = devhx_14a3_p
## Other forms of adversity
# Difficulty affording food = demo_fam_exp1_v2
# Difficulty affording gas/electricity/oil = demo_fam_exp5_v2
# Evicted from home because could not pay rent/mortgage = demo_fam_exp4_v2
# Bullying victimisation = socialdev_cvict_c2
# Parent AESBA mental health scores:
# Anxious/Depressed scale = asr_scr_anxdep_t
# Withdrawn scale = asr_scr_withdrawn_t
# Somatic Complaints scale = asr_scr_somatic_t
# Thought Problems scale = asr_scr_thought_t
# Attention Problems scale = asr_scr_attention_t
# Aggressive Behaviour scale = asr_scr_aggressive_t
# Rule-Breaking Behaviour scale = asr_scr_rulebreak_t

######### Check demographic variables and their levels of missingness, and association with outcomes
# Race - ABCD Parent Demographics Survey - demo_race_a_p___10: https://nda.nih.gov/data_structure.html?short_name=pdem02
abcd$race <- NA
abcd$race[abcd$demo_race_a_p___10==1] <- "White"
abcd$race[abcd$demo_race_a_p___11==1] <- "Black/African American"
abcd$race[abcd$demo_race_a_p___12==1] <- "Native American"
abcd$race[abcd$demo_race_a_p___13==1] <- "Alaska Native"
abcd$race[abcd$demo_race_a_p___14==1] <- "Native Hawaiian"
abcd$race[abcd$demo_race_a_p___15==1] <- "Guamanian"
abcd$race[abcd$demo_race_a_p___16==1] <- "Samoan"
abcd$race[abcd$demo_race_a_p___17==1] <- "Other Pacific Islander"
abcd$race[abcd$demo_race_a_p___18==1] <- "Asian Indian"
abcd$race[abcd$demo_race_a_p___19==1] <- "Chinese"
abcd$race[abcd$demo_race_a_p___20==1] <- "Filipino"
abcd$race[abcd$demo_race_a_p___21==1] <- "Japanese"
abcd$race[abcd$demo_race_a_p___22==1] <- "Korean"
abcd$race[abcd$demo_race_a_p___23==1] <- "Vietnamese"
abcd$race[abcd$demo_race_a_p___24==1] <- "Other Asian"
abcd$race[abcd$demo_race_a_p___25==1] <- "Other Race"
table(abcd$race[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
class(abcd$race)
abcd$race <- as.factor(abcd$race)

# Parent employment	- ABCD Parent Demographics Survey	- demo_prnt_empl_v2: https://nda.nih.gov/data_structure.html?short_name=pdem02
abcd$employment <- as.character(abcd$demo_prnt_empl_v2)
abcd$employment[abcd$employment==1] <- "Working now"
abcd$employment[abcd$employment==2] <- "Temporarily laid off"
abcd$employment[abcd$employment==9] <- "Sick leave"
abcd$employment[abcd$employment==10] <- "Maternity leave"
abcd$employment[abcd$employment== 11] <- "Unemployed not looking for work"
abcd$employment[abcd$employment==3] <- "Looking for work"
abcd$employment[abcd$employment==4] <- "Retired"
abcd$employment[abcd$employment==5] <- "Disabled"
abcd$employment[abcd$employment==6] <- "Stay at home parent"
abcd$employment[abcd$employment==7] <- "Student"
abcd$employment[abcd$employment==8] <- "Other"
abcd$employment[abcd$employment==777 | abcd$employment==""] <- NA
table(abcd$employment)
abcd$employment <- as.factor(abcd$employment)

# Parental highest education qualification
# Derive var with 5 different levels that correspond to the numbers published by the American Community Survey (ACS), following example: https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/core_demographics3.0.R
high.educ1 = abcd$demo_prnt_ed_v2
high.educ2 = abcd$demo_prtnr_ed_v2
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
abcd$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )
table(abcd$high.educ[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

### Household income
# Derived following: https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/core_demographics3.0.R
household.income <- abcd$demo_comb_income_v2
household.income[abcd$demo_comb_income_v2 == "1"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "2"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "3"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "4"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "5"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "6"] = 1 # "[<50K]"
household.income[abcd$demo_comb_income_v2 == "7"] = 2 # "[>=50K & <100K]"
household.income[abcd$demo_comb_income_v2 == "8"] = 2 # "[>=50K & <100K]"
household.income[abcd$demo_comb_income_v2 == "9"] = 3 # "[>=100K]"
household.income[abcd$demo_comb_income_v2 == "10"] = 3 # "[>=100K]"
household.income[abcd$demo_comb_income_v2 == "777"] = NA
household.income[abcd$demo_comb_income_v2 == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
abcd$household.income <- factor( household.income, levels= 1:3, labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]") )
table(abcd$household.income, useNA="always")

# Household size 
table(abcd$demo_roster_v2)
abcd$demo_roster_v2[abcd$demo_roster_v2==""] <- NA
abcd$household_size <- as.numeric(as.character(abcd$demo_roster_v2))
table(abcd$household_size, useNA="always")

# Maternal age at birth	
table(as.character(abcd$devhx_3_p))
abcd$mat_age_birth <- abcd$devhx_3_p
abcd$mat_age_birth[abcd$mat_age_birth==""] <- NA
table(as.character(abcd$mat_age_birth))
abcd$mat_age_birth <- as.numeric(as.character(abcd$mat_age_birth))

# Birthweight = birth_weight_lbs (missing for everyone - do not include)
describe(abcd$birth_weight_lbs[abcd$eventname=="baseline_year_1_arm_1"])
class(abcd$birth_weight_lbs)
abcd$birth_weight_lbs <- as.numeric(levels(abcd$birth_weight_lbs))[abcd$birth_weight_lbs]
summary(abcd$birth_weight_lb)
describe(as.numeric(abcd$birth_weight_oz))
summary(abcd$devhx_1_p)
summary(abcd$devhx_12a_p)
levels(abcd$devhx_2_p_dk)
abcd$birth_weight_oz <- as.numeric(levels(abcd$birth_weight_oz))[abcd$birth_weight_oz]

# Premature birth = devhx_12a_p 
table(abcd$devhx_12a_p)
abcd$premature_birth <- NA
abcd$premature_birth[abcd$devhx_12a_p==1] <- 1
abcd$premature_birth[abcd$devhx_12a_p==0] <- 0
abcd$premature_birth <- as.factor(abcd$premature_birth)
table(abcd$premature_birth[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Maternal smoking during pregnancy 
# devhx_8_tobacco = smoking before knowing of pregnancy
# devhx_9_tobacco = smoking knowing of pregnancy
table(abcd$devhx_8_tobacco)
abcd$devhx_8_tobacco[abcd$devhx_8_tobacco=="" | abcd$devhx_8_tobacco==999] <- NA
table(as.character(abcd$devhx_8_tobacco)[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
abcd$devhx_9_tobacco[abcd$devhx_9_tobacco=="" | abcd$devhx_9_tobacco==999] <- NA
table(as.character(abcd$devhx_9_tobacco)[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Rename variables to "maternal_smoking_bef_preg" and "maternal_smoking_aft_preg"
abcd$maternal_smoking_bef_preg <- abcd$devhx_8_tobacco
abcd$maternal_smoking_aft_preg <- abcd$devhx_9_tobacco

# Maternal alcohol consumption during pregnancy 
# devhx_8_alcohol = alcohol before knowing of pregnancy
# devhx_9_alcohol = alcohol knowing of pregnancy
table(abcd$devhx_8_alcohol)
abcd$devhx_8_alcohol[abcd$devhx_8_alcohol=="" | abcd$devhx_8_alcohol==999] <- NA
table(as.character(abcd$devhx_8_alcohol)[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
abcd$devhx_9_alcohol[abcd$devhx_9_alcohol=="" | abcd$devhx_9_alcohol==999] <- NA
table(as.character(abcd$devhx_9_alcohol)[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Rename variables to "maternal_alcohol_bef_preg" and "maternal_alcohol_aft_preg"
abcd$maternal_alcohol_bef_preg <- as.factor(abcd$devhx_8_alcohol)
abcd$maternal_alcohol_aft_preg <- as.factor(abcd$devhx_9_alcohol)

# Pregnancy complications = devhx_14a3_p 
table(abcd$devhx_14a3_p)
abcd$devhx_14a3_p[abcd$devhx_14a3_p=="" | abcd$devhx_14a3_p==999] <- NA
abcd$preg_complications <- as.factor(abcd$devhx_14a3_p)
table(as.character(abcd$preg_complications)[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

## Other forms of adversity
# Difficulty affording food = demo_fam_exp1_v2
table(abcd$demo_fam_exp1_v2)
abcd$diff_afford_food <- as.character(abcd$demo_fam_exp1_v2)
abcd$diff_afford_food[abcd$diff_afford_food=="" | abcd$diff_afford_food=="777"] <- NA
abcd$diff_afford_food <- as.factor(abcd$diff_afford_food)
table(abcd$diff_afford_food, useNA="always")

# Difficulty affording gas/electricity/oil = demo_fam_exp5_v2
table(abcd$demo_fam_exp5_v2)
abcd$diff_afford_gas <- as.character(abcd$demo_fam_exp5_v2)
abcd$diff_afford_gas[abcd$diff_afford_gas=="" | abcd$diff_afford_gas=="777"] <- NA
abcd$diff_afford_gas <- as.factor(abcd$diff_afford_gas)
table(abcd$diff_afford_gas, useNA="always")

# Evicted from home because could not pay rent/mortgage = demo_fam_exp4_v2
table(abcd$demo_fam_exp4_v2)
abcd$evicted_home <- as.character(abcd$demo_fam_exp4_v2)
abcd$evicted_home[abcd$evicted_home=="" | abcd$evicted_home=="777"] <- NA
abcd$evicted_home <- as.factor(abcd$evicted_home)
table(abcd$evicted_home, useNA="always")

# Bullying victimisation 
# peq_left_out_vic = Some kids left me out of an activity or conversation I really wanted to be included in.
# peq_chase_vic = A kid chased me like he or she was really trying to hurt me.
# peq_rumor_vic = A kid tried to damage my social reputation by spreading rumors about me
# peq_invite_vic = A kid did not invite me to a party or social event though they knew that I wanted to go.
# peq_exclude_vic = A kid left me out of what they were doing.
# peq_gossip_vic = Another kid gossiped about me so others would not like me.
# peq_threat_vic = A kid threatened to hurt or beat me up.
# peq_loser_vic = Another kid said mean things about me so that people would think I was a loser.
# peq_hit_vic = A kid hit, kicked, or pushed me in a mean way.

# Convert item variables from factor to numeric
bullying_cols <- c("peq_left_out_vic", "peq_chase_vic", "peq_rumor_vic", "peq_invite_vic", 
                        "peq_exclude_vic", "peq_gossip_vic", "peq_threat_vic", "peq_loser_vic", 
                        "peq_hit_vic") 
abcd[,bullying_cols] <- lapply(abcd[,bullying_cols], function(x) as.numeric(as.character(x)))

# Derive bullying continuous sum score
abcd$bullying_vic <- apply(abcd [,bullying_cols], 1, sum, na.rm=T)
abcd$bullying_vic[apply(apply(abcd[,bullying_cols],2,is.na),1,sum) > 4] <- NA #code NA is missing >=50% items
psych::describe(abcd$bullying_vic)
psych::describe(abcd$bullying_vic[abcd$eventname=="2_year_follow_up_y_arm_1"]) # note: bullying was assessed only at the 2 year follow-up

# Parent AESBA mental health scores
# Convert measures to numeric
# Anxious/Depressed scale = asr_scr_anxdep_t
abcd$par_anxious_depressed <- as.numeric(as.character(abcd$asr_scr_anxdep_t))
# Withdrawn scale = asr_scr_withdrawn_t
abcd$par_withdrawn <- as.numeric(as.character(abcd$asr_scr_withdrawn_t))
# Somatic Complaints scale = asr_scr_somatic_t
abcd$par_somatic_complaints <- as.numeric(as.character(abcd$asr_scr_somatic_t))
# Thought Problems scale = asr_scr_thought_t
abcd$par_thought_problems <- as.numeric(as.character(abcd$asr_scr_thought_t))
# Attention Problems scale = asr_scr_attention_t
abcd$par_attention_problems <- as.numeric(as.character(abcd$asr_scr_attention_t))
# Aggressive Behaviour scale = asr_scr_aggressive_t
abcd$par_aggress_behav <- as.numeric(as.character(abcd$asr_scr_aggressive_t))

####################################################################################
#-------------- Select all the variables needed for imputation --------------------#
####################################################################################

required_vars <- c("src_subject_id", "eventname", 
         # ACEs
         "maltreatment", 
         "physical_abuse", 
         "sexual_abuse", 
         "emotional_abuse",
         "emotional_neglect",
         "domestic_violence", 
         "parental_psychopathology", 
         "parental_substance", 
         "parent_criminality",
         # parental separation items (note: will derive variable in the wide dataset as it's from 2 time points)
         "demo_prnt_marital_v2", # Are you now married, widowed, divorced, separated, never married or living with a partner? 
         "demo_prnt_prtnr_v2", # Do you have a partner?
         "demo_prnt_prtnr_bio", # Is your partner the child's biological parent? 
         "demo_prnt_prtnr_adopt", # Is your partner the child's adoptive parent? 
         "ple_separ_p", # Parents separated or divorced? 
         "ple_separ_past_yr_p", # Did this happen in the past year?"
         # Outcome variables
         "cbcl_internalising",
         "cbcl_externalising",
         # Auxiliary variables
         "sex",
         "race", # Race
         "employment", # Parental employment
         "high.educ", # Parental highest educational achievement
         "household.income", # Household income
         "household_size", # Household size
         "mat_age_birth", # Maternal age at birth	
         "premature_birth", # Premature birth
         "maternal_alcohol_bef_preg", # alcohol before knowing of pregnancy
         "maternal_alcohol_aft_preg", # alcohol knowing of pregnancy
         "preg_complications", # Pregnancy complications
         "diff_afford_food", # Difficulty affording food
         "diff_afford_gas", # Difficulty affording gas/electricity/oil 
         "evicted_home", # Evicted from home because could not pay rent/mortgage
         "bullying_vic", # Bullying
         # Parent AESBA mental health scores
         "par_anxious_depressed",  # Anxious/Depressed scale
         "par_withdrawn",# Withdrawn scale 
         "par_somatic_complaints", # Somatic Complaints scale
         "par_thought_problems", # Thought Problems scale
         "par_attention_problems", # Attention Problems scale
         "par_aggress_behav") # Aggressive Behaviour scale

# Subset to a reduced dataset only including the required variables for imputation 
abcd_small <- subset(abcd, select=required_vars)
head(abcd_small)

# Check variables are the correct class
str(abcd_small)
abcd_small$sex <- as.factor(abcd_small$sex) # convert sex to factor

#-------------------------  Reshape dataset from long to wide format -----------------------------------------#
# Note: the existing ABCD dataset is in long format, with multiple rows for each participant 
# according to the time at assessment (eventname variable), e.g. "baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", 
# or "2_year_follow_up_y_arm_1" 
# Because some ACEs span multiple assessment times (e.g., parental separation), we need to convert the dataset
# to wide so each participant has one row, and variable names reference the time point of assessment 

# Reshape from long to wide
abcd_wide <- reshape(data=abcd_small, idvar="src_subject_id", 
         timevar="eventname", direction="wide", 
         v.names=required_vars[-c(1,2)]) # variable names = all variables except subject ID and eventname

head(abcd_wide)   
colnames(abcd_wide) # Note: now each variable has the time of assessment listed at the end
# e.g. "baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1", 

#### Check the reshaping to wide format worked and that all columns are labelled correctly
## All ACEs except parental criminality and parental separation should have data for baseline_year_1_arm_1 because they were assessed then
## ACEs
aces_long <- c("maltreatment", "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect",
               "domestic_violence", "parental_psychopathology", "parental_substance")

old <- lapply(abcd[abcd$eventname=="baseline_year_1_arm_1",aces_long], function(x) table(x))
new <- lapply(abcd_wide[,paste0(aces_long, ".baseline_year_1_arm_1")], function(x) table(x))
merge(old, new) # prevalence of exposed and unexposed is the same in the wide dataset as in the long dataset (subsetting to baseline assessment)


### Check prevalence of parental separation items
## Items assessed at baseline
# demo_prnt_marital_v2 = Are you now married, widowed, divorced, separated, never married or living with a partner? 
table(as.character(abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1)) # wide dataset
table(as.character(abcd$demo_prnt_marital_v2[abcd$eventname=="baseline_year_1_arm_1"])) # long dataset
# demo_prnt_prtnr_v2 = Do you have a partner?
table(as.character(abcd_wide$demo_prnt_prtnr_v2.baseline_year_1_arm_1)) # wide dataset
table(as.character(abcd$demo_prnt_prtnr_v2[abcd$eventname=="baseline_year_1_arm_1"])) # long dataset
# demo_prnt_prtnr_bio = Is your partner the child's biological parent? NOTE: RECODE 777 AND 999
table(as.character(abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1)) # wide dataset
table(as.character(abcd$demo_prnt_prtnr_bio[abcd$eventname=="baseline_year_1_arm_1"])) # long dataset
table(as.character(abcd$demo_prnt_prtnr_bio[abcd$eventname=="1_year_follow_up_y_arm_1"])) # check
abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1[abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1==999 | 
                                                      abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1==777 |
                                                      abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1==""] <- NA # Recode
table(as.character(abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1), useNA="always")
# demo_prnt_prtnr_adopt = Is your partner the child's adoptive parent? 
table(as.character(abcd_wide$demo_prnt_prtnr_adopt.baseline_year_1_arm_1)) # wide dataset
table(as.character(abcd$demo_prnt_prtnr_adopt[abcd$eventname=="baseline_year_1_arm_1"])) # long dataset

## Items assessed at the 1 year follow-up
# ple_separ_p = Parents separated or divorced? 
table(as.character(abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1)) # wide dataset
table(as.character(abcd$ple_separ_p[abcd$eventname=="1_year_follow_up_y_arm_1"])) # long dataset
# ple_separ_past_yr_p = Did this happen in the past year?
table(as.character(abcd_wide$ple_separ_past_yr_p.1_year_follow_up_y_arm_1)) # wide dataset
table(as.character(abcd$ple_separ_past_yr_p[abcd$eventname=="1_year_follow_up_y_arm_1"])) # long dataset

#---------------Derive parental separation variable from baseline and follow-up data ----------------------------------#
abcd_wide$parental_separation <- 0
abcd_wide$parental_separation[abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1 == 3 | # parent divorced
                                abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1 == 4 | # parent separated
                                abcd_wide$demo_prnt_prtnr_v2.baseline_year_1_arm_1 == 1 & # parent has a partner
                                abcd_wide$demo_prnt_prtnr_bio.baseline_year_1_arm_1 == 2 | #  but the partner is are not child's biological parent
                                abcd_wide$demo_prnt_prtnr_v2.baseline_year_1_arm_1 == 1 & # parent has a partner 
                                abcd_wide$demo_prnt_prtnr_adopt.baseline_year_1_arm_1 == 2 | # who is not child's adoptive parent
                                abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1 == 1 & # parents separated and it wasn't in past year
                                abcd_wide$ple_separ_past_yr_p.1_year_follow_up_y_arm_1 == 0] <- 1
abcd_wide$parental_separation[apply(apply(abcd_wide[,c("demo_prnt_marital_v2.baseline_year_1_arm_1", 
                                                  "demo_prnt_prtnr_v2.baseline_year_1_arm_1", 
                                                  "ple_separ_p.1_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA
table(abcd_wide$parental_separation, useNA="always")
prop.table(table(abcd_wide$parental_separation, useNA="always")) #28%

#--------------- Check prevalences of ACEs ----------------------------------#
# Maltreatment
prop.table(table(abcd_wide$maltreatment.baseline_year_1_arm_1)) #5.3%
# Domestic violence
prop.table(table(abcd_wide$domestic_violence.baseline_year_1_arm_1)) #34.0%
# Parental psychopathology
prop.table(table(abcd_wide$parental_psychopathology.baseline_year_1_arm_1)) #37.3%
# Parental substance abuse
prop.table(table(abcd_wide$parental_substance.baseline_year_1_arm_1)) #20.2%
# Parental criminality
prop.table(table(abcd_wide$parent_criminality.1_year_follow_up_y_arm_1)) #3.5%
# Parental separation
prop.table(table(abcd_wide$parental_separation)) #27.8%

####################################################################################
#-------------- Create dataset for imputation -------------------------------------#
####################################################################################

# Subset to variables at the correct time points 
# Note: All variables should be assessed at baseline except:
# parental separation (assessed baseline and 1y follow-up), 
# parental criminality (assessed 1y follow-up) 
# bullying victimisation (assessed 2y follow-up)

head(abcd_wide)

# Make list of required variables at Baseline assessment + parental separation
required_vars <- c("src_subject_id", 
                   # ACEs
                   "maltreatment", 
                   "physical_abuse", 
                   "sexual_abuse", 
                   "emotional_abuse",
                   "emotional_neglect",
                   "domestic_violence", 
                   "parental_psychopathology", 
                   "parental_substance", 
                   "parental_separation", 
                    # Outcome variables
                   "cbcl_internalising",
                   "cbcl_externalising",
                   # Auxiliary variables
                   "sex", # Sex of the child
                   "race", # Race
                   "employment", # Parental employment
                   "high.educ", # Parental highest educational achievement
                   "household.income", # Household income
                   "household_size", # Household size
                   "mat_age_birth", # Maternal age at birth	
                   "premature_birth", # Premature birth
                   "maternal_alcohol_bef_preg", # alcohol before knowing of pregnancy
                   "maternal_alcohol_aft_preg", # alcohol knowing of pregnancy
                   "preg_complications", # Pregnancy complications
                   "diff_afford_food", # Difficulty affording food
                   "diff_afford_gas", # Difficulty affording gas/electricity/oil 
                   "evicted_home", # Evicted from home because could not pay rent/mortgage
                   # Parent AESBA mental health scores
                   "par_anxious_depressed",  # Anxious/Depressed scale
                   "par_withdrawn",# Withdrawn scale 
                   "par_somatic_complaints", # Somatic Complaints scale
                   "par_thought_problems", # Thought Problems scale
                   "par_attention_problems", # Attention Problems scale
                   "par_aggress_behav") # Aggressive Behaviour scale

# Remove ".baseline_year_1_arm_1" from variable names to match up to the names in the required vars list
names(abcd_wide) <- gsub(".baseline_year_1_arm_1", "", names(abcd_wide))

# Subset to required variables (at baseline + parental_separation) plus parental criminality (1y follow-up) & bullying
abcd_wide_small <- subset(abcd_wide, select=c(required_vars, 
                                              "parent_criminality.1_year_follow_up_y_arm_1", 
                                              "bullying_vic.2_year_follow_up_y_arm_1"))
head(abcd_wide_small)
dim(abcd_wide_small)

# Rename parental criminality and bullying to remove the time of assessment
abcd_wide_small$parental_criminality <- abcd_wide_small$parent_criminality.1_year_follow_up_y_arm_1
abcd_wide_small$bullying_vic <- abcd_wide_small$bullying_vic.2_year_follow_up_y_arm_1
abcd_wide_small <- subset(abcd_wide_small, select=-c(parent_criminality.1_year_follow_up_y_arm_1, bullying_vic.2_year_follow_up_y_arm_1)) # remove duplicate variables

#### Save dataset
saveRDS(abcd_wide_small, file = "ABCD_preImput_20211108.rds")

####################################################################################
#------------ Check sample size of children with >10% of ACE items ----------------#
####################################################################################
# Note: criteria for imputation is that the child must have at least
# 10% of questionnaire responses used to define exposure to ACEs between birth and age 9/10 

aces_vars <- c("ksads_ptsd_raw_762_p", "ksads_ptsd_raw_763_p",  # physical abuse
               "ksads_ptsd_raw_767_p", "ksads_ptsd_raw_768_p", "ksads_ptsd_raw_769_p", # sexual abuse
               "ksads_ptsd_raw_765_p", "ksads_ptsd_raw_764_p", # emotional abuse 
               "crpbi_parent1_y", "crpbi_parent2_y", "crpbi_parent3_y",
               "crpbi_parent4_y", "crpbi_parent5_y", # emotional neglect
               "ksads_ptsd_raw_766_p", "fam_enviro6_p", "fes_youth_q6", # domestic violence
               "asr_scr_depress_t_cutoff", "asr_scr_anxdisord_t_cutoff", "asr_scr_adhd_t_cutoff", # parental psychopathology ASR
               "fam_history_6_yes_no", "fam_history_7_yes_no", "fam_history_8_yes_no", "fam_history_13_yes_no", # parental psych fam hist
               "asr_q126_p", "famhx_4_p", "fam_history_5_yes_no", # parental substance abuse
               "ple_arrest_p", "ple_law_p", "ple_jail_p", # parental criminality
               "demo_prnt_marital_v2", "demo_prnt_prtnr_v2", "ple_separ_p") # parental separation

length(aces_vars) # 31 variables

# Subset to only include ACEs vars
abcd_aces_small <- subset(abcd, select=c("src_subject_id", "eventname", aces_vars))

## Convert to wide dataset with ACEs variables
abcd_aces_wide <- reshape(data=abcd_aces_small, idvar="src_subject_id", 
                          timevar="eventname", direction="wide", 
                          v.names=colnames(abcd_aces_small[-c(1,2)])) # variable names = all variables except subject ID and eventname

head(abcd_aces_wide)   
dim(abcd_aces_wide)
colnames(abcd_aces_wide)

## Variables needed in wide dataset
aces_vars_wide <- c("ksads_ptsd_raw_762_p.baseline_year_1_arm_1", "ksads_ptsd_raw_763_p.baseline_year_1_arm_1",  # physical abuse, baseline
                    "ksads_ptsd_raw_767_p.baseline_year_1_arm_1", "ksads_ptsd_raw_768_p.baseline_year_1_arm_1", "ksads_ptsd_raw_769_p.baseline_year_1_arm_1", # sexual abuse baseline
                    "ksads_ptsd_raw_765_p.baseline_year_1_arm_1", "ksads_ptsd_raw_764_p.baseline_year_1_arm_1", # emotional abuse  baseline
                    "crpbi_parent1_y.baseline_year_1_arm_1", "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent3_y.baseline_year_1_arm_1",
                    "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent5_y.baseline_year_1_arm_1", # emotional neglect baseline
                    "ksads_ptsd_raw_766_p.baseline_year_1_arm_1", "fam_enviro6_p.baseline_year_1_arm_1", "fes_youth_q6.baseline_year_1_arm_1", # domestic violence baseline
                    "asr_scr_depress_t_cutoff.baseline_year_1_arm_1", "asr_scr_anxdisord_t_cutoff.baseline_year_1_arm_1", "asr_scr_adhd_t_cutoff.baseline_year_1_arm_1", # parental psychopathology ASR baseline
                    "fam_history_6_yes_no.baseline_year_1_arm_1", "fam_history_7_yes_no.baseline_year_1_arm_1", "fam_history_8_yes_no.baseline_year_1_arm_1", "fam_history_13_yes_no.baseline_year_1_arm_1", # parental psych fam hist baseline
                    "asr_q126_p.baseline_year_1_arm_1", "famhx_4_p.baseline_year_1_arm_1", "fam_history_5_yes_no.baseline_year_1_arm_1", # parental substance abuse baseline
                    "ple_arrest_p.1_year_follow_up_y_arm_1", "ple_law_p.1_year_follow_up_y_arm_1", "ple_jail_p.1_year_follow_up_y_arm_1", # parental criminality 1 yr follow-up
                    "demo_prnt_marital_v2.baseline_year_1_arm_1", "demo_prnt_prtnr_v2.baseline_year_1_arm_1", # parental separation baseline
                    "ple_separ_p.1_year_follow_up_y_arm_1") # parental separation 1 year follow-up
length(aces_vars_wide)
# child should have data for at least 3 items on ACEs to be included in the imputation 
31*0.10  # because 31 is number of ACE items, multiplied by 10%
abcd_aces_wide$aces_10percent <- 0 # code to zero by default
abcd_aces_wide$aces_10percent[apply(apply(abcd_aces_wide[,c(aces_vars_wide)],2,is.na),1,sum) > 27] <- 1 # code 1 if missing more than 27 of 31 (90%)
table(abcd_aces_wide$aces_10percent) # 11,878 (all) have at least 10% of data on ACEs

