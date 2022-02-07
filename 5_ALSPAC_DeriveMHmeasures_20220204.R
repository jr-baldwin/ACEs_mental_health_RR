################################################################################
######## ALSPAC - Derive internalising & externalising measures ################
################################################################################

# This script generates the DAWBA internalising & externalising measures for age 7, 10 and 12
# Note: Participants with a full measure at age 7, 10 or 12 will be included in the imputed model
# And Participants with a full measure at age 10 will not have DAWBA data imputed

## ================== Load packages ====================================
library(foreign)
library(dplyr)
library(psych)

## ================== Load data ====================================
# Note: use.value.labels=FALSE because otherwise labels get coded to different numbers than the code used below

data <- read.spss("/Users/jessie/Desktop/Baldwin_08Jan21.sav", use.value.labels=FALSE, to.data.frame=TRUE)

# Subset to DAWBA variables needed 
# Note on codes: KR = measures at age 7.6 (91 months); KV = age 10; TB = age 13 measures
dawba_vars <- c("kr199b", "kr201", "kr202", "kr203", "kr204", "kr205", "kr206", 
                 "kr207", "kr208", "kr209", "kr210", "kr250", "kr251", "kr252", 
                 "kr253", "kr254", "kr255", "kr256", "kr355", "kr356", "kr357", 
                 "kr358", "kr359", "kr360", "kr361", "kr362", "kr363", "kr371", 
                 "kr372", "kr373", "kr374", "kr375", "kr376", "kr390", "kr397", 
                 "kr404", "kr410", "kr411", "kr412", "kr413", "kr414", "kr415", 
                 "kr416", "kr417", "kr418", "kr419", "kr420", "kr421", "kr409", 
                 "kr436", "kr437", "kr438", "kr439", "kr440", "kr441", "kr442", 
                 "kr443", "kr444", "kr448", "kr449", "kr450", "kr451", "kr452", 
                 "kr453", "kr454", "kr455", "kr456", "kr480", "kr481", "kr482", 
                 "kr483", "kr484", "kr485", "kr486", "kr487", "kr488", "kr489", 
                 "kr503", "kr505", "kr507", "kr509", "kr511", "kr513", "kr515", 
                 "kv4000", "kv4020", "kv4021", "kv4022", "kv4023", "kv4024", "kv4025", 
                 "kv4026", "kv4027", "kv4028", "kv4029", "kv5000", "kv5010", "kv5011", 
                 "kv5012", "kv5013", "kv5014", "kv5015", "kv6500", "kv6510", "kv6520", 
                 "kv6521", "kv6522", "kv6523", "kv6524", "kv6525", "kv6526", "kv6550", 
                 "kv6551", "kv6552", "kv6553", "kv6554", "kv6555", "kv7000", "kv7010", 
                 "kv7020", "kv7031", "kv7032", "kv7033", "kv7034", "kv7035", "kv7036", 
                 "kv7037", "kv7038", "kv7039", "kv7040", "kv7041", "kv7042", "kv7510", 
                 "kv7511", "kv7512", "kv7513", "kv7514", "kv7520", "kv7521", "kv7522", 
                 "kv7523", "kv7530", "kv7531", "kv7532", "kv7533", "kv7534", "kv7535", 
                 "kv7536", "kv7537", "kv7538", "kv8000", "kv8010", "kv8011", "kv8012", 
                 "kv8013", "kv8014", "kv8015", "kv8016", "kv8017", "kv8018", "kv8080", 
                 "kv8084", "kv8088", "kv8092", "kv8100", "tb4000", "tb4020", "tb4021", 
                 "tb4022", "tb4023", "tb4024", "tb4025", "tb4026", "tb4027", "tb4028", 
                 "tb4029", "tb5000", "tb5010", "tb5011", "tb5012", "tb5013", "tb5014", 
                 "tb5015", "tb6500", "tb6510", "tb6520", "tb6521", "tb6522", "tb6523", 
                 "tb6524", "tb6525", "tb6526", "tb6550", "tb6551", "tb6552", "tb6553", 
                 "tb6554", "tb6555", "tb7000", "tb7010", "tb7020", "tb7031", "tb7032", 
                 "tb7033", "tb7034", "tb7035", "tb7036", "tb7037", "tb7038", "tb7039", 
                 "tb7040", "tb7041", "tb7042", "tb7510", "tb7511", "tb7512", "tb7513", 
                 "tb7514", "tb7520", "tb7521", "tb7522", "tb7523", "tb7530", "tb7531", 
                 "tb7532", "tb7533", "tb7534", "tb7535", "tb7536", "tb7537", "tb7538", 
                 "tb8000", "tb8010", "tb8011", "tb8012", "tb8013", "tb8014", "tb8015", 
                 "tb8016", "tb8017", "tb8018", "tb8080", "tb8082", "tb8084", "tb8086", 
                 "tb8088", "tb8090", "tb8092")

dim(dawba_vars)

##########################################################################################
#==================== Part 1 - Derive internalising problems measures ====================
##########################################################################################

## =======================================================================================
## ========== Define functions to derive internalising problems measure ==================
## =======================================================================================

## To derive the DAWBA internalising problems at each age, the function does the following:
# 1. Calculate the mean for each subscale for participants with data for 50% of the items
# 2. Standardise the subscale scores
# 3. Sum the scores across anxiety sub-scores and standardise the measure
# 4. Sum anxiety and depression scores
# 5. Standardise overall internalising problems measure

## Define function to recode individual internalising subscales for
## separation anxiety, social anxiety, generalised anxiety, and depression
dawba_int_subscales <- function(data, sep_anx, sep_anx1_gateway, sep_anx2, sep_anx3, sep_anx4, sep_anx5, #Separation anxiety items
                                sep_anx6, sep_anx7, sep_anx8, sep_anx9, sep_anx10, sep_anx11,  #Separation anxiety items 
                                soc_anx, soc_anx1, soc_anx2, soc_anx3, soc_anx4, soc_anx5, soc_anx6, #Social anxiety items
                                gen_anx, gen_anx1_gateway, gen_anx2, gen_anx3, gen_anx4, gen_anx5, #Generalised anxiety items
                                gen_anx6, gen_anx7, gen_anx8, gen_anx9, #Generalised anxiety items
                                dep, dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, #Major depression items
                                dep10, dep11, dep12, dep13, dep14, dep15, dep16_gateway) #Major depression items (dep16 is check, kr409)
{
  ## Separation anxiety subscale
  sep_anx <- apply(data[ ,c(sep_anx2, sep_anx3, sep_anx4, sep_anx5, 
                            sep_anx6, sep_anx7, sep_anx8, sep_anx9, sep_anx10, sep_anx11)], 1, mean, na.rm=T)  # Mean score 
  sep_anx[apply(apply(data[,c(sep_anx2, sep_anx3, sep_anx4, sep_anx5, 
                              sep_anx6, sep_anx7, sep_anx8, sep_anx9, sep_anx10, sep_anx11)],2,is.na),1,sum) >5] <- NA #make NA when missing more than 50% items 
  sep_anx[data$sep_anx1_gateway==2] <- 0 # code to zero if not particularly attached to anyone
  sep_anx <- scale(sep_anx) # standardise score
  
  ## Social anxiety subscale
  soc_anx <- apply(data[ ,c(soc_anx1, soc_anx2, soc_anx3, soc_anx4, soc_anx5, soc_anx6)],1, mean, na.rm=T)  # Mean score 
  soc_anx[apply(apply(data[,c(soc_anx1, soc_anx2, soc_anx3, soc_anx4, soc_anx5, soc_anx6)],
                      2,is.na),1,sum) >3] <- NA #make NA when missing more than 50% items 
  soc_anx <- scale(soc_anx) # standardise score
  
  ## Generalised anxiety subscale
  gen_anx <- apply(data[ ,c(gen_anx2, gen_anx3, gen_anx4, gen_anx5, 
                            gen_anx6, gen_anx7, gen_anx8, gen_anx9)],1, mean, na.rm=T)  # Mean score 
  gen_anx[apply(apply(data[,c(gen_anx2, gen_anx3, gen_anx4, gen_anx5, 
                              gen_anx6, gen_anx7, gen_anx8, gen_anx9)],
                      2,is.na),1,sum) >4] <- NA #make NA when missing more than 50% items
  gen_anx[gen_anx1_gateway==2 | gen_anx1_gateway=="No"] <- 0 # code to zero if child never worries (gateway question)
  gen_anx <- scale(gen_anx) # standardise score
  
  ## Major depression subscale
  dep <- apply(data[ ,c(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, #Major depression items
                        dep10, dep11, dep12, dep13, dep14, dep15)], 1, mean, na.rm=T)  # Mean score
  dep[apply(apply(data[,c(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, #Major depression items
                          dep10, dep11, dep12, dep13, dep14, dep15)],2,is.na),1,sum) >7 
      & data$dep16_gateway!=3] <- NA #make NA when missing more than 50% items & kr409 (for age 7) was not answered in error
  dep[data$dep16_gateway==2] <- 0 # code to zero if said no to gateway question
  dep <- scale(dep)
  
  return(list(sep_anx, soc_anx, gen_anx, dep)) # return multiple variables as list
}

#### Define function to generate overall anxiety measure
dawba_anx <- function(data, sep_anx, soc_anx, gen_anx, overall_anx) {
  overall_anx <- apply(data[ ,c(sep_anx, soc_anx, gen_anx)], 1, mean, na.rm=T)  # Mean score 
  overall_anx[apply(apply(data[,c(sep_anx, soc_anx, gen_anx)],2, is.na),1,sum) >1] <- NA #NA if missing more than 50%
  overall_anx <- scale(overall_anx)
  return(overall_anx)
}

#### Define function to generate overall internalising problems measure
dawba_internalising <- function(data, overall_anx, overall_dep, overall_int) {
  overall_int <- apply(data[ ,c(overall_anx, overall_dep)], 1, sum, na.rm=T)  # Sum score 
  overall_int[apply(apply(data[,c(overall_anx, overall_dep)],2, is.na),1,sum) >1] <- NA #NA if missing more than 50%
  overall_int <- scale(overall_int)
  return(overall_int)
}

## ================== Re-code response levels in items - age 7 ====================================

## ================== Separation anxiety - age 7 ====================================

#kr199b	DV: Any attachment figure (derived variable, gateway question)
#kr201	C3a: Degree to which child worried about something unpleasant happening to special people in past month relative to peers
#kr202	C3b: Degree to which child worried about being taken away from special people in past month relative to peers
#kr203	C3c: Degree to which child was unwilling to go to school in case something nasty happened to special people in past month relative to peers
#kr204	C3d: Degree to which child worried about sleeping alone in past month relative to peers
#kr205	C3e: Degree to which child left bedroom at night to check on special people in past month relative to peers
#kr206	C3f: Degree to which child worried about sleeping in a strange place in past month relative to peers
#kr207	C3g: Degree to which child was afraid of being alone in a room at home without special people in past month relative to peers
#kr208	C3h: Degree to which child had repeated nightmares about being separated from special people in past month relative to peers
#kr209	C3i: Degree to which child was ill when leaving special people in past month relative to peers
#kr210	C3j: Degree to which child had tantrums when separated from special people in past month relative to peers

cols <- c("kr199b", "kr201", "kr202","kr203","kr204", "kr205", 
          "kr206","kr207", "kr208", "kr209","kr210")
str(data[,cols])

data <- data %>% 
  dplyr::mutate_at(c("kr201", "kr202","kr203", "kr204", "kr205", 
                     "kr206","kr207", "kr208", "kr209",
                     "kr210"), funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
psych::describe(data[,cols])

## ================== Social phobia - age 7 ====================================

# The below vars are coded 1=no; 2=a little; 3=a lot; 4=not in the last month
#kr251	E2a: Degree to which child was afraid of meeting new people in past month 
#kr252	E2b: Degree to which child was afraid of meeting lots of people in past month
#kr253	E2c: Degree to which child was afraid of speaking in class in past month
#kr254	E2d: Degree to which child was afraid of reading out loud before others in past month
#kr255	E2e: Degree to which child was afraid of writing in front of others in past month
#kr256	E2f: Degree to which child was afraid of eating in front of others in past month

cols <- c("kr251", "kr252", "kr253", "kr254", "kr255", "kr256")
str(data[,cols])
psych::describe(data[,cols])

#### Recode social phobia items kr251-k256 as follows: 1=0; 4=0, 2=1; 3=2
# Note original coding: 4="not in the last month"; 1="no"; 2="a little"; 3="a lot"
data <- data %>% 
  dplyr::mutate_at(c("kr251", "kr252", "kr253", "kr254", 
                     "kr255", "kr256"), funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, `4`=0, default = NaN)))
psych::describe(data[,cols])

## ================== Generalised anxiety - age 7 ====================================
#kr355	H1: Study child ever worries (only include as 0 for those who said no)
#kr356	H2: Child has had severe general anxieties in past 6 months
#kr357	H3a: Frequency child worries about past behaviour
#kr358	H3b: Frequency child worries about schoolwork/homework
#kr359	H3c: Frequency child worries about disasters
#kr360	H3d: Frequency child worries about health
#kr361	H3e: Frequency child worries about bad things happening to others
#kr362	H3f: Frequency child worries about the future
#kr363	H3g: Frequency child worries about other things

cols <- c("kr355", "kr356", "kr357", "kr358", "kr359",
          "kr360", "kr361", "kr362", "kr363")
str(data[,cols])
psych::describe(data[,cols])

# Variables coded in 4 types of ways:
#kr355	H1: Study child ever worries: 1:2 (only include as 0 for those who said no)
#kr356: Child has had severe general anxieties in past 6 months: coded 1 (definitely), 2 (perhaps), 3(no)
#kr356: recode to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
#kr357:kr363 1:3 (RECODE 3=2, 2=1, 1=0)

# Recode kr356 "Child has had severe general anxieties in past 6 months" 
# to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
data <- data %>% 
  dplyr::mutate_at(c("kr356"), 
                   funs(dplyr::recode(., `3`=0, `2`=1, `1`=2, default = NaN)))

## Recode frequency generalised anxiety items (kr357-363x) as 3=2, 2=1, 1=0
# Note original coding: 4="not in the last month"; 1="no"; 2="a little"; 3="a lot"
data <- data %>% 
  dplyr::mutate_at(c("kr357", "kr358", "kr359",
                     "kr360", "kr361", "kr362", "kr363"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0, default = NaN)))
psych::describe(data[,cols])

## ================== Major depression - age 7 ====================================
# Note: all items coded yes or no, with yes=1, no=2, so recode so higher score = more depression
#kr390	J1: Child has been very sad/tearful/miserable in past month (yes=1, no=2)
#kr397	J2: Child has been very grumpy/irritable in past month (yes=1, no=2)
#kr404	J4: Child has suffered from interest-loss in past month (yes=1, no=2)
#kr410	J5a: Child lacked energy in past month
#kr411	J5b: Child ate much more/less than usual in past month
#kr412	J5c: Child lost/gained a lot of weight in past month
#kr413	J5d: Child found it hard to get to sleep in past month
#kr414	J5e: Child slept too much in past month
#kr415	J5f: Child was frequently agitated for a period in past month
#kr416	J5g: Child frequently felt worthless/guilty for a period in past month
#kr417	J5h: Child found it unusually hard to concentrate for a period in past month
#kr418	J5i: Child thought about death a lot in past month
#kr419	J5j: Child talked about harming/killing self in past month
#kr420	J5k: Child tried to harm/kill self in past month
#kr421	J5l: Child tried to harm/kill self in lifetime
#kr409	J5: Child has been miserable, irritable or suffered from interest-loss in past month 
# GATEWAY/CHECK = only include to give 0 if J5=2 as these individuals didn't answer next questions)

cols <- c("kr390", "kr397", "kr404", "kr409", "kr410", "kr411",
          "kr412", "kr413", "kr414", "kr415", "kr416",
          "kr417", "kr418", "kr419", "kr420", "kr421")
str(data[,cols])
psych::describe(data[,cols])

## Recode all depression items so 2=0 (not depressed), 1=1 (depressed)
data <- data %>% 
  dplyr::mutate_at(c("kr390", "kr397", "kr404", "kr410", "kr411",
                     "kr412", "kr413", "kr414", "kr415", "kr416",
                     "kr417", "kr418", "kr419", "kr420", "kr421"), 
                   funs(dplyr::recode(., `2`=0, `1`=1,  default = NaN)))
psych::describe(data[,cols])
# Note: kr410: kr421 only answered if the child was depressed in the last month (yes to kr409, use as gateway only?)

## =======================================================================================
## ================== Apply function to derive internalising problems at age 7 measure ===
## =======================================================================================

## Apply Function to recode individual internalising subscales for
## separation anxiety, social anxiety, generalised anxiety, and depression

# Generate empty subscales variables
data$sep_anx7 <- NA 
data$soc_anx7 <- NA 
data$gen_anx7 <- NA  
data$dep7 <- NA  

# Apply function to return separate subscales variables
vars7 <- dawba_int_subscales(data, "sep_anx7", "kr199b", "kr201", "kr202","kr203", #Separation anxiety items
                            "kr204", "kr205","kr206","kr207","kr208", "kr209","kr210",
                            "soc_anx7", "kr251", "kr252", "kr253", "kr254", "kr255", "kr256", #Social anxiety items
                            "gen_anx7", "kr355", "kr356", "kr357", "kr358", "kr359", # Generalised anxiety items
                            "kr360", "kr361", "kr362", "kr363", # Generalised anxiety items
                            "dep7", "kr390", "kr397", "kr404", "kr410", "kr411", # Major depression items
                            "kr412", "kr413", "kr414", "kr415", "kr416","kr417", # Major depression items
                            "kr418", "kr419", "kr420", "kr421", "kr409")# Major depression items
data$sep_anx7 <- unlist(vars7[1]) # Separation anxiety
psych::describe(data$sep_anx7)
data$soc_anx7 <- unlist(vars7[2]) # Social anxiety
psych::describe(data$soc_anx7)
data$gen_anx7 <- unlist(vars7[3]) # Generalised anxiety
psych::describe(data$gen_anx7)
data$dep7 <- unlist(vars7[4]) # Depression
psych::describe(data$dep7)

## Apply function to generate overall anxiety measure

# Generate empty overall anxiety variable
data$overall_anx7 <- NA  
# Apply function to derive overall anxiety variable
data$overall_anx7 <- dawba_anx(data, "sep_anx7", "soc_anx7", "gen_anx7", "overall_anx7")
psych::describe(data$overall_anx7)

### Apply Function to generate overall internalising problems measure

# Generate empty overall internalising variable
data$internalising7 <- NA
# Apply function to derive overall internalising variable
data$internalising7 <- dawba_internalising(data, "overall_anx7", "dep7", "internalising7")
psych::describe(data$internalising7)

## ================== Re-code response levels in items - age 10 ====================================

## ================== Separation anxiety - age 10 ====================================
#kv4000	D1a: Child is particularly attached to Mother or Mother figure (2= no one)
#kv4020	D3a: In past month child has often worried about something unpleasant happening to or losing special person
#kv4021	D3b: In past month child has often worried about being taken away from special person
#kv4022	D3c: In past month child has not wanted to go to school in case something bad happened to special person
#kv4023	D3d: In past month child has worried about sleeping alone
#kv4024	D3e: In past month child has left bed at night to check on or to sleep near special person
#kv4025	D3f: In past month child has worried about sleeping in a strange place
#kv4026	D3g: In past month child has been afraid  to be alone in room without special person
#kv4027	D3h: In past month child has had bad dreams about separation from special person
#kv4028	D3i: In past month child has felt ill when had to leave special person
#kv4029	D3j: In past month child has become upset at being apart from special person

cols <- c("kv4000", "kv4020", "kv4021","kv4022","kv4023", "kv4024", 
          "kv4025","kv4026", "kv4027", "kv4028","kv4029")
str(data[,cols]) # kv4000 is coded differently as gateway question, 2 means attached to no one
psych::describe(data[,cols])

data <- data %>% 
  dplyr::mutate_at(c("kv4020", "kv4021","kv4022","kv4023", "kv4024", 
                     "kv4025","kv4026", "kv4027", "kv4028","kv4029"), funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
psych::describe(data[,cols])

## ================== Social phobia - age 10 ====================================
#kv5010	F2a: In past month child has been afraid of meeting new people
#kv5011	F2b: In past month child has been afraid of meeting a lot of people
#kv5012	F2c: In past month child has been afraid of speaking in class
#kv5013	F2d: In past month child has been afraid of reading out loud in front of others
#kv5014	F2e: In past month child has been afraid of writing in front of others
#kv5015	F2f: In past month child has been afraid of eating in front of others

cols <- c("kv5010", "kv5011", "kv5012", "kv5013", "kv5014", "kv5015")
str(data[,cols])
psych::describe(data[,cols])

#### Recode social phobia items kr251-k256 as follows: 1=0; 4=0, 2=1; 3=2
# Note original coding: 4="not in the last month"; 1="no"; 2="a little"; 3="a lot"
data <- data %>% 
  dplyr::mutate_at(c("kv5010", "kv5011", "kv5012", "kv5013", "kv5014", "kv5015"), 
                   funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, `4`=0, default = NaN)))
psych::describe(data[,cols])

## ================== Generalised anxiety - age 10 ====================================
#kv6500	J1: Child worries
#kv6510	J2: Child has had other worries in past 6 months that have interfered with life
#kv6520	J3a: Child worries about past behaviour
#kv6521	J3b: Child worries about school work
#kv6522	J3c: Child worries about disasters
#kv6523	J3d: Child worries about own health
#kv6524	J3e: Child worries about bad things happening to others
#kv6525	J3f: Child worries about the future
#kv6526	J3g: Child worries about other things

cols <- c("kv6500", "kv6510", "kv6520", 
          "kv6521", "kv6522","kv6523", 
          "kv6524", "kv6525", "kv6526")
str(data[,cols])
psych::describe(data[,cols])

# Variables coded in 3 types of ways:
#kv6500	J1: Child worries: 1:2 (for those who answered 2 [no], the other questions were skipped so code 2=0 in overall measure)
#kv6510: Child has had other worries in past 6 months that have interfered with life: coded 1 (definitely), 2 (perhaps), 3(no)
#kv6510: recode to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
#kv6520:kv6526 1:3 (RECODE 3 (often) =2, 2 (sometimes) =1, 1 (not at all)=0)

# Recode kv6500 to numeric
data <- data %>% 
  dplyr::mutate_at(c("kv6500"), 
                   funs(dplyr::recode(., `2`=2, `1`=1, default = NaN)))

# Recode kv6510 "Child has had other worries in past 6 months that have interfered with life" 
# to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
data <- data %>% 
  dplyr::mutate_at(c("kv6510"), 
                   funs(dplyr::recode(., `3`=0, `2`=1, `1`=2, default = NaN)))

## Recode frequency generalised anxiety items (kr357-363x) as 3=2, 2=1, 1=0
# Note original coding: 3="often"; 2="sometimes"; 1="not at all"
data <- data %>% 
  dplyr::mutate_at(c("kv6520", "kv6521", "kv6522",
                     "kv6523", "kv6524", "kv6525", "kv6526"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0, default = NaN)))

psych::describe(data[,cols])

## ================== Major depression - age 10 ====================================
# Note: all items coded yes or no, with yes=1, no=2, so recode so higher score = more depression
#kv7000	K1: In past month child has been very sad
#kv7010	K2: In past month child has been grumpy or irritable in a way that was out of character
#kv7020	K4: In past month there have been times when child lost interest in everything
#kv7031	K5a: In past month child seemed tired all the time
#kv7032	K5b: In past month child was eating much more or less
#kv7033	K5c: In past month child either lost or gained a lot of weight
#kv7034	K5d: In past month child found sleeping hard
#kv7035	K5e: In past month child slept too much
#kv7036	K5f: In past month there was a period when child was agitated or restless
#kv7037	K5g: In past month there was a period when child felt worthless or unnecessarily guilty
#kv7038	K5h: In past month there was a period when child found it hard to concentrate
#kv7039	K5i: In past month child thought about death a lot
#kv7040	K5j: In past month child talked about harming or killing themselves
#kv7041	K5k: In past month child tried to harm or kill themselves
#kv7042	K5l: Child has ever tried to harm or kill self
# Note: kv7030 K5 is GATEWAY/CHECK = only include to give 0 if K5=2 as these individuals didn't answer next questions)

cols <- c("kv7000", "kv7010", "kv7020", "kv7031", "kv7032", "kv7033",
          "kv7034", "kv7035", "kv7036", "kv7037", "kv7038", "kv7039",
          "kv7040", "kv7041", "kv7042")
str(data[,cols])
psych::describe(data[,cols])

## Recode all depression items so 2=0 (not depressed), 1=1 (depressed)
data <- data %>% 
  dplyr::mutate_at(c("kv7000", "kv7010", "kv7020", "kv7031", "kv7032", "kv7033",
                     "kv7034", "kv7035", "kv7036", "kv7037", "kv7038", "kv7039",
                     "kv7040", "kv7041", "kv7042"), 
                   funs(dplyr::recode(., `2`=0, `1`=1, `3`=NaN, default = NaN)))
psych::describe(data[,cols])
# Note: kv7030: kv7031-7042 only answered if the child was depressed in the last month (yes to kv7030, use as gateway only?)


## =======================================================================================
## ================== Apply function to derive internalising problems at age 10 measure ===
## =======================================================================================

## Apply Function to recode individual internalising subscales for
## separation anxiety, social anxiety, generalised anxiety, and depression

# Generate empty subscales variables
data$sep_anx10 <- NA 
data$soc_anx10 <- NA 
data$gen_anx10 <- NA  
data$dep10 <- NA  

# Apply function to return separate subscales variables
vars10 <- dawba_int_subscales(data, "sep_anx10", "kv4000", "kv4020", "kv4021", "kv4022",
                            "kv4023", "kv4024","kv4025","kv4026", "kv4027", "kv4028","kv4029", #Separation anxiety items
                            "soc_anx10","kv5010", "kv5011", "kv5012", "kv5013", "kv5014", "kv5015",#Social anxiety items
                            "gen_anx10", data$kv6500, "kv6510", "kv6520",  # Generalised anxiety items
                            "kv6521", "kv6522","kv6523", # Generalised anxiety items
                            "kv6524", "kv6525", "kv6526", # Generalised anxiety items
                            "dep10", "kv7000", "kv7010", "kv7020", "kv7031", "kv7032", "kv7033",
                            "kv7034", "kv7035", "kv7036", "kv7037", "kv7038", "kv7039",
                            "kv7040", "kv7041", "kv7042", "kv7030")# Major depression items
data$sep_anx10 <- unlist(vars10[1]) # Separation anxiety
psych::describe(data$sep_anx10)
data$soc_anx10 <- unlist(vars10[2]) # Social anxiety
psych::describe(data$soc_anx10)
data$gen_anx10 <- unlist(vars10[3]) # Generalised anxiety
psych::describe(data$gen_anx10)
data$dep10 <- unlist(vars10[4]) # Depression
psych::describe(data$dep10)

# Generate empty overall anxiety variable
data$overall_anx10 <- NA  

# Apply function to derive overall anxiety variable
data$overall_anx10 <- dawba_anx(data, "sep_anx10", "soc_anx10", "gen_anx10", "overall_anx10")
psych::describe(data$overall_anx10) 

### Apply Function to generate overall internalising problems measure

# Generate empty overall internalising variable
data$internalising10 <- NA

# Apply function to derive overall internalising variable
data$internalising10 <- dawba_internalising(data, "overall_anx10", "dep10", "internalising10")
psych::describe(data$internalising10)

## ================== Re-code response levels in items - age 13 ====================================

## ================== Separation anxiety - age 13 ====================================
#tb4000	D1a: Child is particularly  attached to mum/mother figure
#tb4020	D3a: Degree to which child worried about something unpleasant happening to special people in past month relative to peers
#tb4021	D3b: Degree to which child worried about being taken away from special people in past month relative to peers
#tb4022	D3c: Degree to which child often not wanted to go to school in case something nasty happened to special people in past month relative to peers
#tb4023	D3d: Degree to which child worried about sleeping alone in past month relative to peers
#tb4024	D3e: Degree to which child has left bedroom at night to check on/sleep near special people in past month relative to peers
#tb4025	D3f: Degree to which child worried about sleeping in a strange place in past month relative to peers
#tb4026	D3g: Degree to which child been afraid of being alone at home without special people(even if they/respondent close by) in past month relative to peers
#tb4027	D3h: Degree to which child had repeated nightmares/bad dreams about being separated from special people in past month relative to peers
#tb4028	D3i: Degree to which child felt ill when had to leave special person/knew this was about to happen in past month relative to peers
#tb4029	D3j: Degree to which being apart/thought of being apart from special person led to worry/crying/tantrums/clinginess/misery in past month relative to peers
cols <- c("tb4000", "tb4020", "tb4021","tb4022","tb4023", "tb4024", 
          "tb4025","tb4026", "tb4027", "tb4028","tb4029")
str(data[,cols]) # tb4000 is coded differently as gateway question, 2 means attached to no one
psych::describe(data[,cols])

data <- data %>% 
  dplyr::mutate_at(c("tb4020", "tb4021","tb4022","tb4023", "tb4024", 
                     "tb4025","tb4026", "tb4027", "tb4028","tb4029"), 
                   funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
psych::describe(data[,cols])

## ================== Social phobia - age 13 ====================================
#tb5010	F2a: Degree to which child particularly afraid of meeting new people during last month
#tb5011	F2b: Degree to which child particularly afraid of meeting a lot people during last month
#tb5012	F2c: Degree to which child particularly afraid of speaking in class during last month
#tb5013	F2d: Degree to which child particularly afraid of reading a loud in front of others during last month
#tb5014	F2e: Degree to which child particularly afraid of writing in front of others during last month
#tb5015	F2f: Degree to which child particularly afraid of eating in front of others during last month
cols <- c("tb5010", "tb5011", "tb5012", "tb5013", "tb5014", "tb5015")
str(data[,cols])
psych::describe(data[,cols])

#### Recode social phobia items tb5010-tb5015 as follows: 1=0; 4=0, 2=1; 3=2
# Note original coding: 4="not in the last month"; 1="no"; 2="a little"; 3="a lot"
data <- data %>% 
  dplyr::mutate_at(c("tb5010", "tb5011", "tb5012", "tb5013", "tb5014", "tb5015"), 
                   funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, `4`=0, default = NaN)))
psych::describe(data[,cols])

## ================== Generalised anxiety - age 13 ====================================
#tb6500	J1: Child ever worries
#tb6510	J2: Ignoring specific anxieties previously mentioned, child worried so much about so many things it really upset/interfered with their life during last six months
#tb6520	J3a: Frequency child worries a lot about past behaviour (e.g. they did something wrong/upset someone/someone has forgiven them)
#tb6521	J3b: Frequency child worries a lot about school work/homework/tests/examinations
#tb6522	J3c: Frequency child worries a lot about disasters (e.g. burglaries/muggings/fires/bombs)
#tb6523	J3d: Frequency child worries a lot about their own health
#tb6524	J3e: Frequency child worries a lot about bad things happening to others (e.g. family/friends/pets/the world)
#tb6525	J3f: Frequency child worries a lot about the future (e.g. changing school/growing up/getting job)
#tb6526	J3g: Frequency child has any other worries (tick & describe)

cols <- c("tb6500", "tb6510", "tb6520", 
          "tb6521", "tb6522","tb6523", 
          "tb6524", "tb6525", "tb6526")
str(data[,cols])
psych::describe(data[,cols])

# Variables coded in 3 types of ways:
#tb6500	J1: Child worries: 1:2 (for those who answered 2 [no], the other questions were skipped so code 2=0 in overall measure)
#tb6510: child worried so much about so many things it really upset/interfered with their life: coded 1 (definitely), 2 (perhaps), 3(no)
#kv6510: recode to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
#tb6520:tb6526 1:3 (RECODE 3 (often) =2, 2 (sometimes) =1, 1 (not at all)=0)

# Recode kv6500 to numeric
data <- data %>% 
  dplyr::mutate_at(c("tb6500"), 
                   funs(dplyr::recode(., `2`=2, `1`=1, default = NaN)))

# Recode tb6510 "Child has had other worries in past 6 months that have interfered with life" 
# to 3=0 (no), 2=1 (perhaps), 1=3(definitely)
data <- data %>% 
  dplyr::mutate_at(c("tb6510"), 
                   funs(dplyr::recode(., `3`=0, `2`=1, `1`=2, default = NaN)))

## Recode frequency generalised anxiety items (tb6520:tb6526) as 3=2, 2=1, 1=0
# Note original coding: 3="often"; 2="sometimes"; 1="not at all"
data <- data %>% 
  dplyr::mutate_at(c("tb6520", 
                     "tb6521", "tb6522","tb6523", 
                     "tb6524", "tb6525", "tb6526"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0, default = NaN)))

psych::describe(data[,cols])

## ================== Major depression - age 13 ====================================
# Note: all items coded yes or no, with yes=1, no=2, so recode so higher score = more depression
#tb7000	K1: Child has been very sad/miserable/unhappy/tearful during past month
#tb7010	K2: Child been grumpy/irritable that was out of character in past month
#tb7020	K4: At times child lost interest in everything/nearly everything normally enjoys in past month
#tb7031	K5a: Child had no energy/seemed tired all the time in past month
#tb7032	K5b: Child was eating much more/less than usual in past month
#tb7033	K5c: Child lost/gained a lot of weight in past month
#tb7034	K5d: Child had difficulty getting to sleep in past month
#tb7035	K5e: Child slept too much in past month
#tb7036	K5f: Period where child was agitated/restless much of the time in past month
#tb7037	K5g: Period where child felt worthless/unnecessarily guilty much of time in past month
#tb7038	K5h: Period where child found it unusually hard to concentrate/think things out in past month
#tb7039	K5i: Child thought about death a lot in past month
#tb7040	K5j: Child ever talked about harming/killing them self in past month
#tb7041	K5k: Child ever tried to harm/kill them self in past month
#tb7042	K5l: Child ever tried to harm/kill them self during their lifetime
# Note: tb7030 K5 is GATEWAY/CHECK = only include to give 0 if K5=2 as these individuals didn't answer next questions)

cols <- c("tb7000", "tb7010", "tb7020", "tb7031", "tb7032", "tb7033",
          "tb7034", "tb7035", "tb7036", "tb7037", "tb7038", "tb7039",
          "tb7040", "tb7041", "tb7042")
str(data[,cols])
psych::describe(data[,cols])

## Check coding of
# tb7000 (1=yes, 2=no)
# tb7010 (1=yes, 2=no)
# tb7020 (1=yes, 2=no)
data <- data %>% 
  dplyr::mutate_at(c("tb7000", "tb7010", "tb7020"), 
                   funs(dplyr::recode(., `2`=0, `1`=1, default = NaN)))
psych::describe(data[,cols])
# Check if tb7030 K5: Child has been miserable/irritable/lacked interest in things usually enjoys in past month
# should be included (yes, gateway check)

## Note: tb7031-tb7042 coded 1:9; 1=Yes; 2=No; 9=Don't know

## Recode all depression items so 2=0 (not depressed), 1=1 (depressed)
data <- data %>% 
  dplyr::mutate_at(c("tb7031", "tb7032", "tb7033", "tb7034", "tb7035", 
                     "tb7036", "tb7037", "tb7038", "tb7039",
                     "tb7040", "tb7041", "tb7042"), 
                   funs(dplyr::recode(., `2`=0, `1`=1, `9`=NaN, default = NaN)))
psych::describe(data[,cols])
# Note: tb7030: tb7031-7042 only answered if the child was depressed in the last month (yes to tb7030, use as gateway only?)

## =======================================================================================
## ================== Apply function to derive internalising problems at age 13 measure ===
## =======================================================================================

## Apply Function to recode individual internalising subscales for
## separation anxiety, social anxiety, generalised anxiety, and depression

# Generate empty subscales variables
data$sep_anx13 <- NA 
data$soc_anx13 <- NA 
data$gen_anx13 <- NA  
data$dep13 <- NA  

# Apply function to return separate subscales variables
vars13 <- dawba_int_subscales(data, "sep_anx13", "tb4000", "tb4020", "tb4021","tb4022","tb4023", "tb4024", 
                            "tb4025","tb4026", "tb4027", "tb4028","tb4029", #Separation anxiety items
                            "soc_anx13", "tb5010", "tb5011", "tb5012", "tb5013", "tb5014", "tb5015", #Social anxiety items
                            "gen_anx13", data$tb6500, "tb6510", "tb6520", #note may need to change tb6500 to data$tb6500,  
                            "tb6521", "tb6522","tb6523", "tb6524", "tb6525", "tb6526",# Generalised anxiety items
                            "dep13", "tb7000", "tb7010", "tb7020", "tb7031", "tb7032", "tb7033",
                            "tb7034", "tb7035", "tb7036", "tb7037", "tb7038", "tb7039",
                            "tb7040", "tb7041", "tb7042", "tb7030")# Major depression items
data$sep_anx13 <- unlist(vars13[1]) # Separation anxiety
psych::describe(data$sep_anx13)
data$soc_anx13 <- unlist(vars13[2]) # Social anxiety
psych::describe(data$soc_anx13)
data$gen_anx13 <- unlist(vars13[3]) # Generalised anxiety
psych::describe(data$gen_anx13)
data$dep13 <- unlist(vars13[4]) # Depression
psych::describe(data$dep13)

# Generate empty overall anxiety variable
data$overall_anx13 <- NA  

# Apply function to derive overall anxiety variable
data$overall_anx13 <- dawba_anx(data, "sep_anx13", "soc_anx13", "gen_anx13", "overall_anx13")
psych::describe(data$overall_anx13) 

### Apply Function to generate overall internalising problems measure

# Generate empty overall internalising variable
data$internalising13 <- NA

# Apply function to derive overall internalising variable
data$internalising13 <- dawba_internalising(data, "overall_anx13", "dep13", "internalising13")
psych::describe(data$internalising13)

##########################################################################################
#==================== Part 2 - Derive externalising problems measures ====================
##########################################################################################

## =======================================================================================
## =============== Define functions to derive externalising problems measure ============
## =======================================================================================

# The below function does the following: 
# 1. Calculate the mean for each subscale for participants with data for 50% of the items
# 2. Standardise the subscale scores (hyperkinesis/ADHD and conduct/oppositional disorders)

## Function to recode individual externalising subscales for ADHD and conduct/oppositional disorders
dawba_ext_subscales <- function(data, adhd, adhd1, adhd2, adhd3, #ADHD items
                                adhd4, adhd5, adhd6, adhd7, adhd8, #ADHD items
                                adhd9, adhd10, adhd11, adhd12, adhd13, #ADHD items
                                adhd14, adhd15, adhd16, adhd17, adhd18, #ADHD items
                                conduct, conduct1, conduct2, conduct3, #conduct items
                                conduct4, conduct5, conduct6, conduct7, conduct8, #conduct items
                                conduct9, conduct10, conduct11, conduct12, conduct13, #conduct items
                                conduct14, conduct15, conduct16, conduct17) 
{
  ## ADHD subscale
  adhd <- apply(data[ ,c(adhd1, adhd2, adhd3, adhd4, adhd5, 
                         adhd6, adhd7, adhd8, adhd9, adhd10, 
                         adhd11, adhd12, adhd13, adhd14, adhd15, 
                         adhd16, adhd17, adhd18)], 1,  mean, na.rm=T) # calculate mean 
  adhd[apply(apply(data[,c(adhd1, adhd2, adhd3, adhd4, adhd5, 
                           adhd6, adhd7, adhd8, adhd9, adhd10, 
                           adhd11, adhd12, adhd13, adhd14, adhd15, 
                           adhd16, adhd17, adhd18)],
                   2,is.na),1,sum) >9] <- NA #make NA when missing more than 50% items 
  adhd <- scale(adhd) # standardise score
  
  ## Conduct/oppositional subscale
  conduct <- apply(data[ ,c(conduct1, conduct2, conduct3, conduct4, conduct5, 
                            conduct6, conduct7, conduct8, conduct9, conduct10, 
                            conduct11, conduct12, conduct13, conduct14, conduct15, 
                            conduct16, conduct17)], 1, mean, na.rm=T) # calculate mean
  conduct[apply(apply(data[,c(conduct1, conduct2, conduct3, conduct4, conduct5, 
                              conduct6, conduct7, conduct8, conduct9, conduct10, 
                              conduct11, conduct12, conduct13, conduct14, conduct15, 
                              conduct16, conduct17)],
                      2,is.na),1,sum) >8] <- NA #make NA when missing more than 50% items 
  conduct <- scale(conduct) # standardise score
  
  return(list(adhd, conduct)) # return multiple variables as list
}

#### Function to generate overall externalising problems measure
# This function does the following: 
# 1. Sum hyperkinesis/ADHD and conduct/oppositional disorders subscales
# 2. Standardise overall externalising problems measure
dawba_externalising <- function(data, adhd, conduct, overall_ext) {
  overall_ext <- apply(data[ ,c(adhd, conduct)], 1, sum, na.rm=T)  # Sum score 
  overall_ext[apply(apply(data[,c(adhd, conduct)],2, is.na),1,sum) >1] <- NA #NA if missing more than 50%
  overall_ext <- scale(overall_ext)
  return(overall_ext)
}

## ================== Re-code response levels in items - age 7 ====================================

## ================== ADHD - age 7 ====================================
#kr436	K2a: Degree to which child often fidgeted in past 6 months relative to peers
#kr437	K2b: Degree to which child found it hard to sit down for long in past 6 months relative to peers
#kr438	K2c: Degree to which child ran or climbed about illicitly in past 6 months relative to peers
#kr439	K2d: Degree to which child found it hard to play quietly in past 6 months relative to peers
#kr440	K2e: Degree to which child found it hard to calm down in past 6 months relative to peers
#kr441	K3a: Degree to which child often blurted out answers in past 6 months relative to peers
#kr442	K3b: Degree to which child found it hard to wait own turn in past 6 months relative to peers
#kr443	K3c: Degree to which child often butted into conversations/games in past 6 months relative to peers
#kr444	K3d: Degree to which child often went on talking when asked to stop in past 6 months relative to peers
#kr448	K4a: Degree to which child often made careless mistakes in past 6 months relative to peers
#kr449	K4b: Degree to which child often lost interest in activities in past 6 months relative to peers
#kr450	K4c: Degree to which child often didn't listen when addressed in past 6 months relative to peers
#kr451	K4d: Degree to which child often didn't finish a job properly in past 6 months relative to peers
#kr452	K4e: Degree to which child often found it hard to get organised in past 6 months relative to peers
#kr453	K4f: Degree to which child often tried to get out of activities involving thought in past 6 months relative to peers
#kr454	K4g: Degree to which child often lost things needed for school in past 6 months relative to peers
#kr455	K4h: Degree to which child was easily distracted in past 6 months relative to peers
#kr456	K4i: Degree to which child was often forgetful in past 6 months relative to peers

cols <- c("kr436", "kr437", "kr438", "kr439", "kr440",
          "kr441", "kr442", "kr443", "kr444", "kr448", 
          "kr449", "kr450", "kr451", "kr452", "kr453", 
          "kr454", "kr455", "kr456")
str(data[,cols])
describe(data[,cols])

# Note: all ADHD variables coded from 1:3, 1="no", 2="a little more than others", 3="a lot more than others"
# Re-code variables from 0-2, with 2 indicating symptoms: so 3=2, 2=1, 1=0

data <- data %>% 
  dplyr::mutate_at(c("kr436", "kr437", "kr438", "kr439", "kr440",
                     "kr441", "kr442", "kr443", "kr444", "kr448", 
                     "kr449", "kr450", "kr451", "kr452", "kr453", 
                     "kr454", "kr455", "kr456"), funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
describe(data[,cols])

## ================== Conduct/oppositional disorders - age 7 ====================================
#kr480	L1: Degree of child's awkward behaviour in past 6 months relative to peers
#kr481	L2a: Degree to which child has had severe temper tantrums in past 6 months relative to peers
#kr482	L2b: Degree to which child has argued with grown-ups in past 6 months relative to peers
#kr483	L2c: Degree to which child has ignored rules / been disobedient in past 6 months relative to peers
#kr484	L2d: Degree to which child has deliberately annoyed people in past 6 months relative to peers
#kr485	L2e: Degree to which child has blamed others for own mistakes in past 6 months relative to peers
#kr486	L2f: Degree to which child has been easily annoyed in past 6 months relative to peers
#kr487	L2g: Degree to which child has been angry & resentful in past 6 months relative to peers
#kr488	L2h: Degree to which child has been spiteful in past 6 months relative to peers
#kr489	L2i: Degree to which child has tried to get revenge on people in past 6 months relative to peers
#kr503	L9a: Child has told lies to obtain objects/favours or to avoid duties in past 12 months
#kr505	L9b: Frequency child has started fights with non-siblings in past 12 months
#kr507	L9c: Frequency child has bullied/threatened people in past 12 months
#kr509	L9d: Frequency child has stayed out later than allowed in past 12 months
#kr511	L9e: Child has stolen things in past 12 months
#kr513	L9f: Frequency child has run away / stayed away all night without permission in past 12 months
#kr515	L9g: Child has often played truant in past 12 months

cols <- c("kr480", "kr481", "kr482", "kr483", "kr484", 
          "kr485", "kr486", "kr487", "kr488", "kr489", 
          "kr503", "kr505", "kr507", "kr509", "kr511", 
          "kr513", "kr515")
str(data[,cols])
describe(data[,cols])

# Note: the variables are all coded from 1:3, but coded in 4 ways:
# kr480 is coded as 3="more troublesome than average", 2="about average", 1="less troublesome than average"
# kr481:kr489 are coded as 1=no, 2=more than others, 3=a lot more than others
# kr503, kr511 and kr515 is coded as 3="definitely", 2="perhaps", 1="no
# kr505: kr509 is coded as 3="often", 2="sometimes", 1="never"
# kr513 is coded as 3="> once", 2="once only", 1="never"
# With all of the items, 3=worst symptom, 1=least symptom, recode from 0:2 so 1=0, 2=1, 3=2

data <- data %>% 
  dplyr::mutate_at(c("kr480", "kr481", "kr482", "kr483", "kr484", 
                     "kr485", "kr486", "kr487", "kr488", "kr489", 
                     "kr503", "kr505", "kr507", "kr509", "kr511", 
                     "kr513", "kr515"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0,  default = NaN)))
describe(data[,cols])

## =======================================================================================
## ================== Apply function to derive externalising problems at age 7 measure ===
## =======================================================================================

## Apply Function to recode individual internalising subscales for
## ADHD and conduct

# Generate empty subscales variables
data$adhd7 <- NA 
data$conduct7 <- NA 

# Apply function to return separate subscales variables
vars7 <- dawba_ext_subscales(data, "adhd7", "kr436", "kr437", "kr438", #ADHD vars
                             "kr439", "kr440","kr441", "kr442", #ADHD vars
                             "kr443", "kr444", "kr448", "kr449", #ADHD vars
                             "kr450", "kr451", "kr452", "kr453", #ADHD vars
                             "kr454", "kr455", "kr456", #ADHD vars
                             "conduct7", "kr480", "kr481", "kr482", #conduct vars
                             "kr483", "kr484", "kr485", "kr486", #conduct vars
                             "kr487", "kr488", "kr489","kr503", #conduct vars
                             "kr505", "kr507", "kr509", "kr511", #conduct vars
                             "kr513", "kr515") #conduct vars

data$adhd7 <- unlist(vars7[1]) # ADHD
psych::describe(data$adhd7)
data$conduct7 <- unlist(vars7[2]) # conduct problems
psych::describe(data$conduct7)

### Apply Function to generate overall externalising problems measure

# Generate empty overall externalising variable
data$externalising7 <- NA  

# Apply function to derive overall externalising variable
data$externalising7 <- dawba_externalising(data, "adhd7", "conduct7", "externalising7")
psych::describe(data$internalising7)

## ================== Re-code response levels in items - age 10 ====================================

## ================== ADHD - age 10 ====================================
#kv7510	L2a: In last 6 months child often fidgets
#kv7511	L2b: In last 6 months child found it hard to sit still for long
#kv7512	L2c: In last 6 months child ran or climbed around when should not
#kv7513	L2d: In last 6 months child found it hard to take part in activities without making noise
#kv7514	L2e: In last 6 months child found it hard to calm down when asked to
#kv7520	L3a: In last 6 months child blurted out answers before hearing questions properly
#kv7521	L3b: In last 6 months child found it hard to wait for turn
#kv7522	L3c: In last 6 months child often butted in on others conversations or games
#kv7523	L3d: In last 6 months child often went on talking even if told to stop or no one is listening
#kv7530	L4a: In last 6 months child makes careless mistakes or doesn't pay attention
#kv7531	L4b: In last 6 months child seems to lose interest in what child is doing
#kv7532	L4c: In last 6 months child does not listen to people
#kv7533	L4d: In last 6 months child often does not complete jobs
#kv7534	L4e: In last 6 months child has had difficulty organising themselves
#kv7535	L4f: In last 6 months child often tried to avoid things involving thought
#kv7536	L4g: In last 6 months child often lost things needed for school
#kv7537	L4h: In last 6 months child has been easily distracted
#kv7538	L4i: In last 6 months child was often forgetful

cols <- c("kv7510", "kv7511", "kv7512", "kv7513", "kv7514",
          "kv7520", "kv7521", "kv7522", "kv7523", "kv7530", 
          "kv7531", "kv7532", "kv7533", "kv7534", "kv7535",
          "kv7536", "kv7537", "kv7538")
str(data[,cols])
describe(data[,cols])

# Note: all ADHD variables coded from 1:3, 1="no", 2="a little more than others", 3="a lot more than others"
# Re-code variables from 0-2, with 2 indicating symptoms: so 3=2, 2=1, 1=0

data <- data %>% 
  dplyr::mutate_at(c("kv7510", "kv7511", "kv7512", "kv7513", "kv7514",
                     "kv7520", "kv7521", "kv7522", "kv7523", "kv7530", 
                     "kv7531", "kv7532", "kv7533", "kv7534", "kv7535",
                     "kv7536", "kv7537", "kv7538"), 
                   funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
describe(data[,cols])

## ================== Conduct/oppositional disorders - age 10 ====================================
#kv8000	M1: Mothers assessment of how child's awkward behaviour compares with other children
#kv8010	M2a: In past 6 months child has had severe tantrums
#kv8011	M2b: In past 6 months child has argued with grown-ups
#kv8012	M2c: In past 6 months child has taken no notice of rules or refused to do as told
#kv8013	M2d: In past 6 months child has done things to annoy others
#kv8014	M2e: In past 6 months child has blamed others for own mistakes
#kv8015	M2f: In past 6 months child has been touchy or easily annoyed
#kv8016	M2g: In past 6 months child has been angry or resentful
#kv8017	M2h: In past 6 months child has been spiteful
#kv8018	M2i: In past 6 months child has tried to get own back on others
#kv8080	M9a: In past year child has told lies to get favours or to get out of things
#kv8082	m9b: in past year child has often started fights
#kv8084	M9c: In past year child has bullied or threatened people
#kv8086	m9d: in past year child has stayed out much later than was supposed to
#kv8088	M9e: In past year child has stolen things
#kv8090	m9f: in past year child has run away from home or stayed out all night without p
#kv8092	M9g: In past year child has often played truant

cols <- c("kv8000", "kv8010", "kv8011", "kv8012",
          "kv8013", "kv8014", "kv8015", "kv8016", 
          "kv8017", "kv8018", "kv8080", "kv8082", 
          "kv8084","kv8086","kv8088", "kv8090", "kv8092")
str(data[,cols])
describe(data[,cols])

# Note: the variables are all coded from 1:3, but coded in 4 ways:
# kv8000 is coded as 3="more troublesome than average", 2="about average", 1="less troublesome than average"
# kv8010:kv8018 are coded as 3=a lot more than average, 2=a little more than average,  1=no more than average
# kv8080, kv8088 and kv8092 is coded as 3="definitely", 2="perhaps", 1="no
# kv8082: kv8086 is coded as 3="often", 2="sometimes", 1="no"
# With all of the items, 3=worst symptom, 1=least symptom, recode from 0:2 so 1=0, 2=1, 3=2

data <- data %>% 
  dplyr::mutate_at(c("kv8000", "kv8010", "kv8011", "kv8012",
                     "kv8013", "kv8014", "kv8015", "kv8016", 
                     "kv8017", "kv8018", "kv8080", "kv8082", 
                     "kv8084","kv8086","kv8088", "kv8090", "kv8092"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0,  default = NaN)))
describe(data[,cols])

## =======================================================================================
## ================== Apply function to derive externalising problems at age 10 measure ===
## =======================================================================================

## Apply Function to recode individual externalising subscales for
## ADHD and conduct

# Generate empty subscales variables
data$adhd10 <- NA 
data$conduct10 <- NA 

# Apply function to return separate subscales variables
vars10 <- dawba_ext_subscales(data, "adhd10", "kv7510", "kv7511", #ADHD vars
                              "kv7512", "kv7513", "kv7514", "kv7520", #ADHD vars
                              "kv7521", "kv7522", "kv7523", "kv7530", #ADHD vars
                              "kv7531", "kv7532", "kv7533", "kv7534", #ADHD vars
                              "kv7535", "kv7536", "kv7537", "kv7538", #ADHD vars
                              "conduct10", "kv8000", "kv8010", "kv8011", "kv8012",
                              "kv8013", "kv8014", "kv8015", "kv8016", #conduct vars
                              "kv8017", "kv8018", "kv8080", "kv8082",#conduct vars
                              "kv8084","kv8086","kv8088", "kv8090", "kv8092") #conduct vars

data$adhd10 <- unlist(vars10[1]) # ADHD
psych::describe(data$adhd10)
data$conduct10 <- unlist(vars10[2]) # conduct problems
psych::describe(data$conduct10)

### Apply Function to generate overall externalising problems measure

# Generate empty overall externalising variable
data$externalising10 <- NA  
# Apply function to derive overall externalising variable
data$externalising10 <- dawba_externalising(data, "adhd10", "conduct10", "externalising10")
psych::describe(data$internalising10)

## ================== Re-code response levels in items - age 13 ====================================

## ================== ADHD - age 13 ====================================
#tb7510	L2a: Degree to which child often fidgets in past 6 months relative to peers
#tb7511	L2b: Degree to which child finds it hard to stay sitting down for long in past 6 months relative to peers
#tb7512	L2c: Degree to which child runs/climbs about when shouldn't in past 6 months relative to peers
#tb7513	L2d: Degree to which child finds it hard to play/take part in other leisure activities without making noise in past 6 months relative to peers
#tb7514	L2e: Degree to which child finds it hard to calm down if rushing about when asked to in past 6 months relative to peers
#tb7520	L3a: Degree to which child often blurts out answer before heard question properly in past 6 months relative to peers
#tb7521	L3b: Degree to which child finds it hard to wait their turn in past 6 months relative to peers
#tb7522	L3c: Degree to which child often butt's in on other people's conversation/games in past 6 months relative to peers
#tb7523	L3d: Degree to which child often goes on talking even if asked to stop/no one else listening in past 6 months relative to peers
#tb7530	L4a: Degree to which child often makes careless mistakes/fails to pay attention in past 6 months relative to peers
#tb7531	L4b: Degree to which child often seems to loose interest in what their doing in past 6 months relative to peers
#tb7532	L4c: Degree to which child often not listening to what people saying to them in past 6 months relative to peers
#tb7533	L4d: Degree to which child often not finish a job properly in past 6 months relative to peers
#tb7534	L4e: Degree to which often hard for child to get organised to do something in past 6 months relative to peers
#tb7535	L4f: Degree to which child often try's to get out of things they would have to think about such as homework in past 6 months relative to peers
#tb7536	L4g: Degree to which child often looses things needed for school/PE in past 6 months relative to peers
#tb7537	L4h: Degree to which child easily distracted in past 6 months relative to peers
#tb7538	L4i: Degree to which child often forgetful in past 6 months relative to peers

cols <- c("tb7510", "tb7511", "tb7512", "tb7513", "tb7514",
          "tb7520", "tb7521", "tb7522", "tb7523", "tb7530",
          "tb7531", "tb7532", "tb7533", "tb7534", "tb7535",
          "tb7536", "tb7537", "tb7538")
str(data[,cols])
describe(data[,cols])

# Note: all ADHD variables coded from 1:3, 1="not at all", 2="a little more than others", 3="a lot more than others"
# Re-code variables from 0-2, with 2 indicating symptoms: so 3=2, 2=1, 1=0

data <- data %>% 
  dplyr::mutate_at(c("tb7510", "tb7511", "tb7512", "tb7513", "tb7514",
                     "tb7520", "tb7521", "tb7522", "tb7523", "tb7530",
                     "tb7531", "tb7532", "tb7533", "tb7534", "tb7535",
                     "tb7536", "tb7537", "tb7538"), funs(dplyr::recode(., `1`=0, `2`=1, `3`=2, default = NaN)))
describe(data[,cols])

## ================== Conduct/oppositional disorders - age 13 ====================================
#tb8000	M1: Overall degree to which child's awkward & troublesome behaviour compares in past 6 months relative to peers
#tb8010	M2a: Degree to which child had severe temper tantrums in past 6 months relative to peers
#tb8011	M2b: Degree to which child argued with grown-ups in past 6 months relative to peers
#tb8012	M2c: Degree to which child taken no notice of rules/refused to do as told in past 6 months relative to peers
#tb8013	M2d: Degree to which child seemed to do things to annoy people on purpose in past 6 months relative to peers
#tb8014	M2e: Degree to which child blamed others for own mistakes/bad behaviour in past 6 months relative to peers
#tb8015	M2f: Degree to which child been touchy/easily annoyed in past 6 months relative to peers
#tb8016	M2g: Degree to which child been angry/resentful in past 6 months relative to peers
#tb8017	M2h: Degree to which child been spiteful in past 6 months relative to peers
#tb8018	M2i: Degree to which child tried to get their own back on people in past 6 months relative to peers
#tb8080	M9a: Child told lies to get things/favours from others/to get out of things supposed to do over past 12 months
#tb8082	M9b: Child often started fights other than brother's & sisters over past 12 months
#tb8084	M9c: Child bullied/threatened people over past 12 months
#tb8086	M9d: Child stayed out much later than supposed to over past 12 months
#tb8088	M9e: Child has stolen things from house/other people's houses/shops/school over past 12 months
#tb8090	M9f: Child has run away form home/ever stayed away all night without respondents permission over past 12 months
#tb8092	M9g: Child often played truant (bunked off) from school over past 12 months

cols <- c("tb8000", "tb8010", "tb8011", "tb8012",
          "tb8013", "tb8014", "tb8015", "tb8016", 
          "tb8017", "tb8018", "tb8080", "tb8082",
          "tb8084", "tb8086", "tb8088", "tb8090", "tb8092")
str(data[,cols])
describe(data[,cols])

# Note: the variables are all coded from 1:3, but coded in 4 ways:
# tb8000 is coded as 3="more troublesome than average", 2="about average", 1="less troublesome than average"
# tb8010:tb8018 are coded as 3=a lot more than others, 2=a little more than others,  1=no more than others
# tb8080, tb8088 and tb8092 is coded as 3="definitely", 2="perhaps", 1="no
# tb8082: tb8086 is coded as 3="often", 2="sometimes", 1="no"
# tb8090 is coded as 3="> once", 2="once", 1="no"
# With all of the items, 3=worst symptom, 1=least symptom, recode from 0:2 so 1=0, 2=1, 3=2

data <- data %>% 
  dplyr::mutate_at(c("tb8000", "tb8010", "tb8011", "tb8012",
                     "tb8013", "tb8014", "tb8015", "tb8016", 
                     "tb8017", "tb8018", "tb8080", "tb8082",
                     "tb8084", "tb8086", "tb8088", "tb8090", "tb8092"), 
                   funs(dplyr::recode(., `3`=2, `2`=1, `1`=0,  default = NaN)))
describe(data[,cols])

## =======================================================================================
## ================== Apply function to derive externalising problems at age 13 measure ===
## =======================================================================================

## Apply Function to recode individual externalising subscales for
## ADHD and conduct

# Generate empty subscales variables
data$adhd13 <- NA 
data$conduct13 <- NA 

# Apply function to return separate subscales variables
vars13 <- dawba_ext_subscales(data, "adhd13", "tb7510", "tb7511", 
                              "tb7512", "tb7513", "tb7514",
                              "tb7520", "tb7521", "tb7522", "tb7523", "tb7530",
                              "tb7531", "tb7532", "tb7533", "tb7534", "tb7535",
                              "tb7536", "tb7537", "tb7538", #ADHD vars
                              "conduct13", "tb8000", "tb8010", "tb8011", "tb8012",
                              "tb8013", "tb8014", "tb8015", "tb8016", 
                              "tb8017", "tb8018", "tb8080", "tb8082",
                              "tb8084", "tb8086", "tb8088", "tb8090", "tb8092") #conduct vars

data$adhd13 <- unlist(vars13[1]) # ADHD
psych::describe(data$adhd13)
data$conduct13 <- unlist(vars13[2]) # conduct problems
psych::describe(data$conduct13)

### Apply Function to generate overall externalising problems measure

# Generate empty overall externalising variable
data$externalising13 <- NA  

# Apply function to derive overall externalising variable
data$externalising13 <- dawba_externalising(data, "adhd13", "conduct13", "externalising13")
psych::describe(data$externalising13)


