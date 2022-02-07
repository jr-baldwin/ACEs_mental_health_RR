################################################################################
### Derive ACE measures in ALSPAC - script 1 - recode binary constructs ########
################################################################################

## Note: this script was adapted from one by Lotte Houtepen to derive ACE measures in ALSPAC

############################################################ 
# Minimum required at start:
#1. Excel file with description variables 
#   - 'SI_data1_ACE_definitions_overtime.xlsx' 
#
# 2. File with ALSPAC data ('alspac.table_ACE.RData'),  labelled "ACEswith CIDB.csv"
# The columns in this dataframe needs to include at least five groups of variables:
#
#   i. all variables in the column 'variable' of the excel sheet 
#      (SI_data1_ACE_definitions_overtime.xlsx)
#
#  ii. all variables in the column 'followup_inc_other_question' 
#      of the excel sheet (SI_data1_ACE_definitions_overtime.xlsx) 
#
# iii. variables needed to derive household class: 
#       "pa_sc_p","pb_sc_p", "b_sc_m","c_sc_ptnr_pp", "c_sc_m", "pd_sc_p",
#       "f_sc_ptnr_pp", "f_sc_m","pe_sc_p", "g_sc_ptnr_pp", "g_sc_m", 
#       "h_sc_ptnr_pp", "h_sc_m", "j_sc_ptnr_pp", "j_sc_m","pl_sc_p"
#
#  iv. Auxiliary imputation variables:
#      "c804", "kz030", "kz029", "dw002", "dw042", "a006", "mz028b", 
#      "b032", "a525", "c645a", "c666a", "pb325a", "pb342a", "kz021", 
#      "b608", "b593", "c472", "c520", "c522", "pb183", "b370", "c600", 
#      "pb260", "b106", "b107", "b122", "b123", "b597", "c093", "c101", 
#      "d152", "d169", "d170", "pa172", "pa189", "pa190", "pb187", "a600", 
#      "b598", "pb188a", "b578", "b587", "pb168", "pb177", 
#      "d790", "pb130", "b701", "b702", "b714", "d167", "d168", "pa187", 
#      "pa188", "pb098", "t3336", "t3337", "fa3333", "fa3334", "t3322", 
#      "t1360", "t1362", "fa3322", "fa1360", "fa1362", "t5404", "t3327", 
#      "t3255", "t5300", "t5305", "t5320", "t5340", "t5345", "t5360", 
#      "t5365", "t5380", "t5385", "fa5404", "fa3327", "t2035", "fa2035", 
#      "t3328", "fa3328", "t3308", "t3316", "fa3308", "fa3316", "t3325", 
#      "t3326", "fa3325", "fa3326", "t5402", "t5406", "t5410", "t5412", 
#      "t5510", "fa5402", "fa5406", "fa5410", "fa5411", "fa5510", "ypa5005_dup", 
#      "ypa5007_dup", "ypa5009_dup", "ypa5011_dup", "ypa5013_dup", "ypa5015_dup", 
#      "ypa5017_dup", "ypa5050", "t3321", "fa3321"
#
#   v. ALSPAC do file variables (used for merging with other ALSPAC data)
#      'aln','qlet','mz001','mz010','mz010a','mz013','mz014','mz028b',
#      'a006','a525','b032','b650','b663','b665','b667',
#      'c645a','c755','c765',paste0('c80',0:4),'bestgest',
#      'kz011b','kz021','kz030','mult',
#      'in_core','in_alsp','in_phase2','in_phase3','tripquad'
############################################################  

######################
# Code block 1: Read in data, check all necessary variables are present
# A.	Read in alspac data
# B. 	Read in dataframe with info on ACE variables 
#     	(sheet called 'ACE variables' in  SI_data1_ACE_definitions_overtime.xlsx)
# C. 	Specify which variables should be in the ALSPAC datafile
# D. 	Check if all necessary variables are present in ALSPAC datafile 

######################
## A. Read in alspac data
alspac.table_ACE <- read.csv("/Users/jessie/Desktop/ACEswith CIDB.csv", na.strings = c("NA", " ", ""))
ls() #Should now have a dataframe called 'alspac.table_ACE' in environment
class(alspac.table_ACE)
dim(alspac.table_ACE) #15445 participants with 813 variables # there are 812 variables

# Copy ALSPAC ID variable to "aln" (ID used in these scripts)
alspac.table_ACE$aln <- as.numeric(alspac.table_ACE$cidB3219)
# check aln is numeric
class(alspac.table_ACE$aln)

## B. Read in dataframe with info on ACE variables
require(readxl)  
setwd("/Users/jessie/Dropbox/Wellcome fellowship/Registered Report/R Scripts/ACE scripts")
adv_description <- data.frame(readxl::read_excel(
  'SI_data1_ACE_definitions_overtime.xlsx', sheet = 'ACE variables',col_names=TRUE))

## C. Specify which variables should be in the ALSPAC datafile
#i. ACE variables
ACE_var <- unique(tolower(adv_description$variable_unique))

# ii.For ACE variables that are part of skips in questionnaire,
# need to include previous question which determined 
# whether someone answered our question of interest 
# (eg Were your personal belongings stolen? 
# followed up by our question of interest 
# How often were your personal belongings stolen?) 
quest_follow_up <- unique(unlist(strsplit(
  gsub('OR','_',adv_description$followup_inc_other_question[
    which(!is.na(adv_description$followup_inc_other_question)& 
            !adv_description$followup_inc_other_question=='no')]),
  split='_')))

# iii. vector ALSPAC variables needed to derive household social class
org_sc_vars <- c("pa_sc_p","pb_sc_p", "b_sc_m","c_sc_ptnr_pp", "c_sc_m",
               "pd_sc_p","f_sc_ptnr_pp", "f_sc_m","pe_sc_p","g_sc_ptnr_pp", "g_sc_m",
               "h_sc_ptnr_pp", "h_sc_m","j_sc_ptnr_pp", "j_sc_m","pl_sc_p")

# iv. vector ALSPAC variables that will be used as auxiliary imputation variables
impute_ACE <-
  c("c804", "kz030", "kz029", "dw002", "dw042", "a006", "mz028b", 
    "b032", "a525", "c645a", "c666a", "pb325a", "pb342a", "kz021", 
    "b608", "b593", "c472", "c520", "c522", "pb183", "b370", "c600", 
    "pb260", "b106", "b107", "b122", "b123", "b597", "c093", "c101", 
    "d152", "d169", "d170", "pa172", "pa189", "pa190", "pb187", "a600", 
    "b598", "pb188a", "b578", "b587", "pb168", "pb177", "sc_household_18wgest", 
    "d790", "pb130", "b701", "b702", "b714", "d167", "d168", "pa187", 
    "pa188", "pb098", "t3336", "t3337", "fa3333", "fa3334", "t3322", 
    "t1360", "t1362", "fa3322", "fa1360", "fa1362", "t5404", "t3327", 
    "t3255", "t5300", "t5305", "t5320", "t5340", "t5345", "t5360", 
    "t5365", "t5380", "t5385", "fa5404", "fa3327", "t2035", "fa2035", 
    "t3328", "fa3328", "t3308", "t3316", "fa3308", "fa3316", "t3325", 
    "t3326", "fa3325", "fa3326", "t5402", "t5406", "t5410", "t5412", 
    "t5510", "fa5402", "fa5406", "fa5410", "fa5411", "fa5510", "ypa5005_dup", 
    "ypa5007_dup", "ypa5009_dup", "ypa5011_dup", "ypa5013_dup", "ypa5015_dup", 
    "ypa5017_dup", "ypa5050", "t3321", "fa3321","b665","b667","c482","e178")

# v. vector of variables that are part of alspac do files, 
# added to enable merge with other alspac data
alspac_do_file <- c('aln','qlet','mz001','mz010','mz010a','mz013','mz014','mz028b',
                  'a006','a525','b032','b650','b663','b665','b667',
                  'c645a','c755','c765',paste0('c80',0:4),'bestgest',
                  'kz011b','kz021','kz030','mult',
                  'in_core','in_alsp','in_phase2','in_phase3','tripquad')

check_in_alspac_data <- c(alspac_do_file,ACE_var[!grepl('sc_household|dup',ACE_var)],
                         quest_follow_up,org_sc_vars,
                         impute_ACE[!grepl('sc_household|dup',impute_ACE)])

## D. Run check variables in file
all(check_in_alspac_data%in%names(alspac.table_ACE))
# If FALSE, next line should show which variables are missing
check_in_alspac_data [!check_in_alspac_data %in%names(alspac.table_ACE)]

##################################################################
# Code block 2: Calculate highest social class 
##################################################################
# derived from combination mum&partner report at same time

## A. First recode levels of SES variables to right order 
# Note: they are currently coded alphabetically in csv, so skilled manual is before skilled non-manual (wrong)
sapply(alspac.table_ACE[,org_sc_vars], levels)
labs_orig <- c("I - Professional", "II - Mangerial and technical", "IIINM - Skilled non-manual", 
          "IIIM - Skilled manual", "IV - Partly skilled", "V - Unskileld") #keep consistent spelling mistakes as original version
alspac.table_ACE[,org_sc_vars] <- lapply(alspac.table_ACE[,org_sc_vars], factor,
                                         levels = labs_orig)
sapply(alspac.table_ACE[,org_sc_vars], levels) # Check levels again

## B. Make numeric instead of factor (enables use matrixStats)
sc_household_numeric <- sapply(alspac.table_ACE[,org_sc_vars],as.numeric)

labs <- c("I - Professional", "II - Managerial and technical", "IIINM - Skilled non-manual", 
               "IIIM - Skilled manual", "IV - Partly skilled", "V - Unskilled")

## C. Per time point create highest social class household
require(matrixStats)

alspac.table_ACE$sc_household_12wgest <- factor(sc_household_numeric[,"pa_sc_p"], labels=labs)
alspac.table_ACE$sc_household_18wgest <- rowMins(sc_household_numeric[,c("pb_sc_p", "b_sc_m")],na.rm=T)
alspac.table_ACE$sc_household_18wgest[is.infinite(alspac.table_ACE$sc_household_18wgest)] <- NA
alspac.table_ACE$sc_household_18wgest <- factor(alspac.table_ACE$sc_household_18wgest,labels=labs)

alspac.table_ACE$sc_household_32wgest <- rowMins(sc_household_numeric[,c("c_sc_ptnr_pp", "c_sc_m")], na.rm=T)
alspac.table_ACE$sc_household_32wgest[is.infinite(alspac.table_ACE$sc_household_32wgest)] <- NA
alspac.table_ACE$sc_household_32wgest <- factor(alspac.table_ACE$sc_household_32wgest,labels=labs)

alspac.table_ACE$sc_household_8m <- rowMins(sc_household_numeric[,c("pd_sc_p","f_sc_ptnr_pp", "f_sc_m")], na.rm=T)
alspac.table_ACE$sc_household_8m[is.infinite(alspac.table_ACE$sc_household_8m)] <- NA
alspac.table_ACE$sc_household_8m <- factor(alspac.table_ACE$sc_household_8m,labels=labs)

alspac.table_ACE$sc_household_2yr <- rowMins(sc_household_numeric[,c("pe_sc_p","g_sc_ptnr_pp", "g_sc_m")], na.rm=T)
alspac.table_ACE$sc_household_2yr[is.infinite(alspac.table_ACE$sc_household_2yr)] <- NA
alspac.table_ACE$sc_household_2yr <- factor(alspac.table_ACE$sc_household_2yr,labels=labs)

alspac.table_ACE$sc_household_3yr <- rowMins(sc_household_numeric[,c("h_sc_ptnr_pp", "h_sc_m")], na.rm=T)
alspac.table_ACE$sc_household_3yr[is.infinite(alspac.table_ACE$sc_household_3yr)] <- NA
alspac.table_ACE$sc_household_3yr <- factor(alspac.table_ACE$sc_household_3yr,labels=labs)

alspac.table_ACE$sc_household_4yr <- rowMins(sc_household_numeric[,c("j_sc_ptnr_pp", "j_sc_m")], na.rm=T)
alspac.table_ACE$sc_household_4yr[is.infinite(alspac.table_ACE$sc_household_4yr)] <- NA
alspac.table_ACE$sc_household_4yr <- factor(alspac.table_ACE$sc_household_4yr,labels=labs)

# Check levels
table(alspac.table_ACE$sc_household_4yr)
table(alspac.table_ACE$j_sc_ptnr_pp, alspac.table_ACE$j_sc_m)

########################################################################################
# Code block 3: Create binary maternal smoking during pregnancy variables (for imputation)
########################################################################################
#   Added on 20180501, binary maternal smoking during pregnancy variables
#                      using original variables leads to convergence errors (small nrs)
alspac.table_ACE$matsmok_tri1 <- ifelse(alspac.table_ACE$b665=='N',0,1)
alspac.table_ACE$matsmok_tri2 <- ifelse(alspac.table_ACE$b667=='N',0,1)
alspac.table_ACE$matsmok_tri3_c <- ifelse(alspac.table_ACE$c482==0,0,1)
alspac.table_ACE$matsmok_tri3_e <- ifelse(alspac.table_ACE$e178=='Not at all',0,1)
# remove old and add dichotomous new
length(impute_ACE)#115
impute_ACE <- impute_ACE[!impute_ACE%in%c('b665','b667','c482','e178')]
length(impute_ACE)#111
impute_ACE <- c(impute_ACE,grep('matsmok',names(alspac.table_ACE),value=T))
length(impute_ACE)#115

########################################################################################
# Code block 4: Prepare data for recoding:
# A.	Duplicate variables encoding two different exposure time periods
# B.	Factor levels in correct order
# C.	Add in non-exposed for variables with skips
# D.	Ensure trauma exposure is highest level
# E.	Check data is either factor or numeric
########################################################################################
## A. add duplicate variables (variable encoding two different time periods)
table(duplicated(ACE_var)) #unique id
alspac.table_ACE <- cbind(alspac.table_ACE,
                        alspac.table_ACE[,gsub('_dup','',adv_description$variable_unique[
                          duplicated(gsub('_dup','',adv_description$variable_unique))])])
#    add _dup to names of variables encoding 2 time periods
names(alspac.table_ACE)[duplicated(names(alspac.table_ACE))] <-
  paste0(names(alspac.table_ACE)[duplicated(names(alspac.table_ACE))],'_dup')
names(alspac.table_ACE)[names(alspac.table_ACE)%in% gsub('_dup','',
 adv_description$variable_unique [!adv_description$variable_unique%in%names(alspac.table_ACE)])] <- 
  paste0(names(alspac.table_ACE)[names(alspac.table_ACE)%in% gsub('_dup','',adv_description$variable_unique[!adv_description$variable_unique%in%names(alspac.table_ACE)])], '_dup')

## B. Variables need to be ordered properly 
# Jessie: Change for ALL variables, as all are in the wrong order due to csv format
all_var <- adv_description$variable_unique

# State the change in the log
cat('\n\n',paste0(Sys.time()),
     'Start changing levels for variables with factor levels in wrong order 
     (this applies to all variables due to csv file format changing order from R data original file)\n') 

# Loop to change variable names
for(var in all_var){ # loop through all variables
  
  cat('Before',var,length(levels(factor(alspac.table_ACE[[var]]))),'levels:',
      paste0(levels(factor(alspac.table_ACE[[var]])),sep=','))
    recode <- unlist(strsplit(
    adv_description[which(adv_description$variable_unique==var),'recode_ACE'],
    '];'))
  recode
  adv_description[which(adv_description$variable_unique==var),'recode_ACE'] <-
    if(length(recode)==3) {
      paste0(recode[[3]],';',recode[[1]],'];',recode[[2]]) } else 
        if(length(recode)==4) {
          paste0(recode[[3]],';',recode[[1]],'];',recode[[2]],';',recode[[4]])
        } else 
          if(length(recode)==2) {
            paste0(recode[[1]],'];',recode[[2]]) 
            } else
              if(length(recode)==1) {
                paste0(recode)
                }
  recode <- unlist(strsplit(
    gsub(']','',adv_description[which(adv_description$variable_unique==var),'recode_ACE']),
    ';'))
  recode
  alspac.table_ACE[[var]] <- as.character(alspac.table_ACE[[var]]) #to be able to adapt levels
  alspac.table_ACE[[var]] <- factor(alspac.table_ACE[[var]],levels = recode)
  if(grepl('yes',adv_description[which(adv_description$variable_unique==var),'reverse_scale'])){
    adv_description[which(adv_description$variable_unique==var),'reverse_scale']<-'yes'
  } else {
   adv_description[which(adv_description$variable_unique==var),'reverse_scale']<-'no'
  }
  cat('\nAfter',var,'levels:',paste0(levels(factor(alspac.table_ACE[[var]])),sep=','),'\n\n')
  rm(recode)
}

## C. Add in non-exposed for variables with skips: questions where answers depend on previous question
var_follow_up <- adv_description$variable_unique[
  which(!is.na(adv_description$followup_inc_other_question)& 
          !adv_description$followup_inc_other_question=='no')]
cat('\n\n',paste0(Sys.time()),
    'Start changing levels for variables that are skips:
    questions where answers depend on previous question\n')
for(var in var_follow_up){
  ## print(table(alspac.table_ACE[[var]]))
  non_miss <- !is.na(
    alspac.table_ACE[,gsub('_.*$','',adv_description$followup_inc_other_question[
      which(adv_description$variable_unique==var)])])
  alspac.table_ACE[[var]]<-as.character(alspac.table_ACE[[var]])#to be able to add in FALSE values  
  alspac.table_ACE[non_miss&is.na(alspac.table_ACE[[var]]),var]<-FALSE
  if(adv_description$reverse_scale[
    which(adv_description$variable_unique==var)]=='yes'){
    adv_description[which(adv_description$variable_unique==var),'recode_ACE']<-
      paste0(adv_description$recode_ACE[which(adv_description$variable_unique==var)],";FALSE")
    cutoff_pre<-
      lapply(strsplit(adv_description$recode_ACE[which(adv_description$variable_unique==var)],
                      '];'),strsplit,split=';')[[1]]
    new.levels<-
      unlist( lapply(1:length(cutoff_pre), function(i,...){
        v<- abs(rep(i-length(cutoff_pre),length(cutoff_pre[[i]])))
        names(v)<-cutoff_pre[[i]]
        v}))
    if(!all(names(new.levels)%in%levels(factor(alspac.table_ACE[[var]])))){
      new.levels<-new.levels[levels(factor(alspac.table_ACE[[var]]))]}
    new_labels<-rev(gsub(';','_',unlist(strsplit(adv_description$recode_ACE[
      which(adv_description$variable_unique ==var)],'];'))))
    stopifnot(length(new.levels)==length(levels(factor(alspac.table_ACE[[var]]))))
    if(!all(new.levels==levels(factor(alspac.table_ACE[[var]])))){
      alspac.table_ACE[[var]]<-factor(alspac.table_ACE[[var]],levels=names(new.levels))}
  }else{
    if(grepl(']',adv_description[which(adv_description$variable_unique==var),'recode_ACE'])){
      adv_description[which(adv_description$variable_unique==var),'recode_ACE']<-
        paste0("FALSE;",adv_description$recode_ACE[which(adv_description$variable_unique==var)])
    } else {adv_description[which(adv_description$variable_unique==var),'recode_ACE']<-
      paste0("FALSE];",adv_description$recode_ACE[which(adv_description$variable_unique==var)])}
    cutoff_pre<-lapply(strsplit(adv_description$recode_ACE[which(adv_description$variable_unique==var)],
                                '];'),strsplit,split=';')[[1]]
    new.levels<-
      unlist(lapply(1:length(cutoff_pre), function(i,...){
        v<- rep(i,length(cutoff_pre[[i]]))-1
        names(v)<-cutoff_pre[[i]]
        v}))
    if(!all(names(new.levels)%in%levels(factor(alspac.table_ACE[[var]])))){
      new.levels<-new.levels[levels(factor(alspac.table_ACE[[var]]))]}
    new_labels<-gsub(';','_',unlist(strsplit(adv_description$recode_ACE[
      which(adv_description$variable_unique ==var)],'];')))
    stopifnot(length(new.levels)==length(levels(factor(alspac.table_ACE[[var]]))))
    if(!all(new.levels==levels(factor(alspac.table_ACE[[var]])))){
      alspac.table_ACE[[var]]<-factor(alspac.table_ACE[[var]],levels=names(new.levels))}
  }
  #make sure all levels have variables in them
  ##print(table(alspac.table_ACE[[var]]));
  rm(cutoff_pre,new.levels,new_labels,non_miss)
}

## D. Ensure trauma exposure is highest (last) level (if necessary reverse order variables):
# This is only done for reverse coded variables, which are ordered different to other vars
# e.g.
# levels(alspac.table_ACE$t3321) # reverse coded
# levels(alspac.table_ACE$fa3321) # reverse coded
# levels(alspac.table_ACE$fg4120) # not reverse coded
ACE_var_rec <- adv_description$variable_unique[adv_description$reverse_scale%in%'yes']
# only factors, no use numeric
ACE_var_rec <- ACE_var_rec[sapply(alspac.table_ACE[,ACE_var_rec],is.factor)]
alspac.table_ACE[,ACE_var_rec] <-
  lapply(alspac.table_ACE[,ACE_var_rec],function(x) factor(x,levels=rev(levels(x))))
## Note: trauma exposure is now listed as the highest level for all of these 
levels(alspac.table_ACE$f257)
levels(alspac.table_ACE$f258)
levels(alspac.table_ACE$b608)
levels(alspac.table_ACE$pd257)
levels(alspac.table_ACE$fa3321)
levels(alspac.table_ACE$t3321)
## Check for sexual abuse variable which has been coded as 100%
levels(alspac.table_ACE$kd505a)
levels(alspac.table_ACE$kj465)
levels(alspac.table_ACE$kl475)
levels(alspac.table_ACE$kn4005)
levels(alspac.table_ACE$kq365)
levels(alspac.table_ACE$kt5005)

# Check through all variables to see which are coded wrong
list_levels <- sapply(alspac.table_ACE[,ACE_var_rec], levels)
class(list_levels)
df <- plyr::ldply(list_levels, rbind)
dim(df)
#lapply(df, function(x) write.table( data.frame(df), 'test.csv'  , append= T, sep=',' ))
write.csv(df,"order of levels in reverse coded vars.csv", row.names = FALSE)


## E.Check data is either factor or numeric
if(!all(sapply(alspac.table_ACE[,ACE_var],
               class)%in%c('factor','numeric'))){
  cat('\nalspac.table_ACE dataframe contains other type of data (not factor or numeric):',
      sapply(alspac.table_ACE[,ACE_var],
             class)[!sapply(alspac.table_ACE[,ACE_var],
                            class)%in%c('factor','numeric')])
}


##################################################################
# Code block 5: Recode data to binary variables
# A. 	define cutoff (see recode_ACE column in xlsx sheet 2)
# B.	Recode to binary
# C. 	Add non-dichotomised variables (id, auxiliary imputation)
# D.	Save
##################################################################
## A. define cutoff based on info in recode_ACE column of the Excel file with description variables 
lookup_cutoff <- lapply(strsplit(adv_description$recode_ACE,'];'),strsplit,split=';')
names(lookup_cutoff) <- adv_description$variable_unique
create_cutoff<-
  function(x,reverse_scale,...)
    if(reverse_scale=='yes'){
      rev(unlist( lapply(1:length(x), function(i,...){
        v<- abs(rep(i-length(x),length(x[[i]])))
        names(v)<-x[[i]]
        v
      })))} else{
        unlist(lapply(1:length(x), function(i,...){
          v<- rep(i,length(x[[i]]))-1
          names(v)<-x[[i]]
          v}))
      } 
cutoff_levels <-
  mapply(create_cutoff,lookup_cutoff,reverse_scale = adv_description$reverse_scale)
names(cutoff_levels) <- adv_description$variable_unique
new_labels <- cutoff_levels
new_levels <- cutoff_levels
class(new_labels)
# check new levels are correct
new_labels$f257
new_levels$f257
new_levels$p1063
new_levels$t5402

## B.recode dataset to binary: 
binary_alspacKids_ACE_data <-
  
  data.frame(sapply(adv_description$variable_unique, function(var)
    
  {#cat(var,'  ')#to avoid truncated output in Rstudio: 
    #https://stackoverflow.com/questions/36800475/avoid-string-printed-to-console-getting-truncated-in-rstudio
    if(!all(names(new_levels[[var]]) %in% levels(factor(alspac.table_ACE[[var]])))) {
      
      new_levels[[var]] <- new_levels[[var]][levels(factor(alspac.table_ACE[[var]]))]
      
      new_labels[[var]] <-
        gsub(';','_', unlist(strsplit(adv_description$recode_ACE[
          which(adv_description$variable_unique ==var)],'];')))
      new_labels[[var]]<-
        new_labels[[var]][grepl(paste0(names(new_levels[[var]]),collapse='|'),new_labels[[var]])]
    } else{
      new_labels[[var]]<-
        gsub(';','_',unlist(strsplit(adv_description$recode_ACE[
          which(adv_description$variable_unique ==var)],'];')))}
    alspac.table_ACE[[var]]<-factor(new_levels[[var]][
      factor(alspac.table_ACE[[var]])],labels=new_labels[[var]])
    #convert to logical
    alspac.table_ACE[[var]]<-as.numeric(alspac.table_ACE[[var]])==2 # Codes 1=true, 2=false
  }
  ))


## C. add non-dichotomised variables
# alspac do file data 
binary_alspacKids_ACE_data[,alspac_do_file] <- 
  alspac.table_ACE[,alspac_do_file]
# Error in `[.data.frame`(alspac.table_ACE, , alspac_do_file) : undefined columns selected
# Check which variables are not included
alspac.table_ACE$aln #is not there
alspac_do_file <- c('cidB3219', 'qlet','mz001','mz010','mz010a','mz013','mz014','mz028b',
                    'a006','a525','b032','b650','b663','b665','b667',
                    'c645a','c755','c765',paste0('c80',0:4),'bestgest',
                    'kz011b','kz021','kz030','mult',
                    'in_core','in_alsp','in_phase2','in_phase3','tripquad')
binary_alspacKids_ACE_data[,alspac_do_file] <- 
  alspac.table_ACE[,alspac_do_file]

# this includes IDs
head(binary_alspacKids_ACE_data[,c('aln', 'qlet')])
head(binary_alspacKids_ACE_data[,c('cidB3219', 'qlet')]) #works without 'aln' but I think "cidB3219" is the ID var in my dataset


#survived 1st year (kz011b)
table(binary_alspacKids_ACE_data$kz011b)
#as well as whether the child was part of a twin (mz010a)
table(binary_alspacKids_ACE_data$mz010a)

# for imputation also add the original variable for the pregnancy adversities and ses
table(paste0(impute_ACE,'_org')%in%names(binary_alspacKids_ACE_data))
# should be FALSE for all 115 variables
binary_alspacKids_ACE_data[,paste0(impute_ACE,'_org')]<-
  alspac.table_ACE[,impute_ACE]
table(paste0(impute_ACE,'_org')%in%names(binary_alspacKids_ACE_data))
# should be TRUE for all 115 variables
# NOTE: to distinguish the non-binary version have added _org to the name
# So for variable 'c804', the original version is
table(binary_alspacKids_ACE_data$b608_org)
# dichotomised:
table(binary_alspacKids_ACE_data$b608)

##D. save
getwd()
save(binary_alspacKids_ACE_data,file='binary_alspacKids_ACE_data.RData')

################################################################
##################### End of script ############################
################################################################