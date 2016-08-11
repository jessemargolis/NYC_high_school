#######################################
##  STEP 1: READ DATA & ORGANIZE ##
#######################################

library(readr)
data_2002<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2002-03_Scrambled.csv')
data_2003<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2003-04_Scrambled.csv')
data_2003_2<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2003-04_Scrambled.csv')
data_2004<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2004-05_Scrambled.csv')
data_2005<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2005-06_Scrambled.csv')
data_2006<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2006-07_Scrambled.csv')
data_2007<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2007-08_Scrambled.csv')
data_2008<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2008-09_Scrambled.csv')
data_2009<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2009-10_Scrambled.csv')
data_2010<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2010-11_Scrambled.csv')
data_2011<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2011-12_Scrambled.csv')
data_2012<-read.table('Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2012-13_Scram.txt', sep = '', header = T, na.string = '')
data_2013<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2013-14_Scram.csv')
data_2013_2<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2013-14_Scram.csv')

data_2014<-read.csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Raw/biog1031y2014-15_Scrambled.csv')

#Blank or missing values
#Convert all blank value to NA
data_2002[data_2002==""] <- NA
data_2003[data_2003==""] <- NA
data_2004[data_2004==""] <- NA
data_2005[data_2005==""] <- NA
data_2006[data_2006==""] <- NA
data_2007[data_2007==""] <- NA
data_2008[data_2008==""] <- NA
data_2008[data_2008=="."] <- NA
data_2009[data_2009==""] <- NA
data_2010[data_2010==""] <- NA
data_2011[data_2011==""] <- NA
data_2012[data_2012==""] <- NA
data_2013[data_2013==""] <- NA
data_2014[data_2014==""] <- NA
#Summarise blank or missing values
'Creating a table showing the number of missing values for each of the variables?'
sapply(data_2002, function(x) sum(is.na(x)))
sapply(data_2003, function(x) sum(is.na(x)))
sapply(data_2004, function(x) sum(is.na(x)))
sapply(data_2005, function(x) sum(is.na(x)))
sapply(data_2006, function(x) sum(is.na(x)))
sapply(data_2007, function(x) sum(is.na(x)))
sapply(data_2008, function(x) sum(is.na(x)))
sapply(data_2009, function(x) sum(is.na(x)))
sapply(data_2010, function(x) sum(is.na(x)))
sapply(data_2011, function(x) sum(is.na(x)))
sapply(data_2012, function(x) sum(is.na(x)))
sapply(data_2013, function(x) sum(is.na(x)))
sapply(data_2014, function(x) sum(is.na(x)))

missing_values1<-data.frame(sapply(data_2002, function(x) sum(is.na(x))),sapply(data_2003, function(x) sum(is.na(x))),sapply(data_2004, function(x) sum(is.na(x))),sapply(data_2005, function(x) sum(is.na(x))),sapply(data_2006, function(x) sum(is.na(x))),sapply(data_2007, function(x) sum(is.na(x))),sapply(data_2008, function(x) sum(is.na(x))),sapply(data_2010, function(x) sum(is.na(x))), sapply(data_2011,function(x) sum(is.na(x))))
missing_values2<-data.frame(sapply(data_2013, function(x) sum(is.na(x))),sapply(data_2014, function(x) sum(is.na(x))))              
missing_values3<-data.frame(sapply(data_2012, function(x) sum(is.na(x))))
missing_values4<-data.frame(sapply(data_2009, function(x) sum(is.na(x))))                           

write.csv(missing_values1, file = '/Users/elmerleezy/Desktop/02-11.csv')
write.csv(missing_values2, file = "/Users/elmerleezy/Desktop/13-14.csv")
write.csv(missing_values3, file = "/Users/elmerleezy/Desktop/12.csv")
write.csv(missing_values4, file = "/Users/elmerleezy/Desktop/09.csv")

#Delete Blank Values 
'data_2013 <- data_2013[!(data_2013$student_id_scram == ""), ]
data_2013 <- data_2013[!(data_2013$dbn == ""), ]
data_2013 <- data_2013[!(data_2013$birth_mo_yr == ""), ]
data_2013 <- data_2013[!(data_2013$sex == ""), ]'

#Find duplicate values for student id scram
data_2002$student_id_scram[duplicated(data_2002$student_id_scram)]
data_2003$student_id_scram[duplicated(data_2003$student_id_scram)]
data_2004$student_id_scram[duplicated(data_2004$student_id_scram)]
data_2005$student_id_scram[duplicated(data_2005$student_id_scram)]
data_2006$student_id_scram[duplicated(data_2006$student_id_scram)]
data_2007$student_id_scram[duplicated(data_2007$student_id_scram)]
data_2008$student_id_scram[duplicated(data_2008$student_id_scram)]
data_2009$student_id_scram[duplicated(data_2009$student_id_scram)]
data_2010$student_id_scram[duplicated(data_2010$student_id_scram)]
data_2011$student_id_scram[duplicated(data_2011$student_id_scram)]
data_2012$student_id_scram[duplicated(data_2012$student_id_scram)]
data_2013$student_id_scram[duplicated(data_2013$student_id_scram)]
data_2014$student_id_scram[duplicated(data_2014$student_id_scram)]
duplicate_studentid <- data.frame(data_2013$student_id_scram[duplicated(data_2013$student_id_scram)])
'Find two student ids that are duplicated:
data_2002: 641152411
data_2003: 804402954 184752914
data_2013: 01032503 444022674'
#Remove Duplicates
data_2002 = data_2002[-771030,]
data_2003 = data_2003[-617515,]
data_2003 = data_2003[-834337,]
data_2013 = data_2013[-442789,]
data_2013 = data_2013[-490454,]
#Remove duplicates (not to be used)
unique(!data_2013$student_id_scram)
data_2013_dupremoved<-data.frame(data_2013[!duplicated(data_2013$student_id_scram),])
#Another way (not to be used)
n_occur <- data.frame(table(data_2013$student_id_scram))
n_occur[n_occur$Freq > 1,]
data_2013[data_2013$student_id_scram %in% n_occur$Var1[n_occur$Freq > 1],]


#Unify "birth" variable
library(stringr)
data_2011$birth_mo<- str_pad(data_2011$birth_mo, 2, pad = "0")
data_2011$birth_mm_yyyy = paste(data_2011$birth_mo, data_2011$birth_yr,sep = '-')
data_2011$birth_mo<-NULL
data_2011$birth_yr<-NULL

data_2010$birth_mo<- str_pad(data_2010$birth_mo, 2, pad = "0")
data_2010$birth_mm_yyyy = paste(data_2010$birth_mo, data_2010$birth_yr,sep = '-')
data_2010$birth_mo<-NULL
data_2010$birth_yr<-NULL

data_2009$birth_mo<- str_pad(data_2009$birth_mo, 2, pad = "0")
data_2009$birth_mm_yyyy = paste(data_2009$birth_mo, data_2009$birth_yr,sep = '-')
data_2009$birth_mo<-NULL
data_2009$birth_yr<-NULL

data_2008$birth_mo<- str_pad(data_2008$birth_mo, 2, pad = "0")
data_2008$birth_mm_yyyy = paste(data_2008$birth_mo, data_2008$birth_yr,sep = '-')
data_2008$birth_mo<-NULL
data_2008$birth_yr<-NULL

data_2007$birth_mo<- str_pad(data_2007$birth_mo, 2, pad = "0")
data_2007$birth_mm_yyyy = paste(data_2007$birth_mo, data_2007$birth_yr,sep = '-')
data_2007$birth_mo<-NULL
data_2007$birth_yr<-NULL

data_2006$birth_mo<- str_pad(data_2006$birth_mo, 2, pad = "0")
data_2006$birth_mm_yyyy = paste(data_2006$birth_mo, data_2006$birth_yr,sep = '-')
data_2006$birth_mo<-NULL
data_2006$birth_yr<-NULL

data_2005$birth_mo<- str_pad(data_2005$birth_mo, 2, pad = "0")
data_2005$birth_mm_yyyy = paste(data_2005$birth_mo, data_2005$birth_yr,sep = '-')
data_2005$birth_mo<-NULL
data_2005$birth_yr<-NULL

data_2004$birth_mo<- str_pad(data_2004$birth_mo, 2, pad = "0")
data_2004$birth_mm_yyyy = paste(data_2004$birth_mo, data_2004$birth_yr,sep = '-')
data_2004$birth_mo<-NULL
data_2004$birth_yr<-NULL

data_2003$birth_mo<- str_pad(data_2003$birth_mo, 2, pad = "0")
data_2003$birth_mm_yyyy = paste(data_2003$birth_mo, data_2003$birth_yr,sep = '-')
data_2003$birth_mo<-NULL
data_2003$birth_yr<-NULL

data_2002$birth_mo<- str_pad(data_2002$birth_mo, 2, pad = "0")
data_2002$birth_mm_yyyy = paste(data_2002$birth_mo, data_2002$birth_yr,sep = '-')
data_2002$birth_mo<-NULL
data_2002$birth_yr<-NULL

#Unify Ethnicity Variable

eth14 <- data.frame(table(data_2014$ethnicity)) 
names(eth14) <- c('ethnicity','freq_2014')

eth13 <- data.frame(table(data_2013$ethnicity)) 
names(eth13) <- c('ethnicity','freq_2013')

eth12 <- data.frame(table(data_2012$ethnicity)) 
names(eth12) <- c('ethnicity','freq_2012')

eth11 <- data.frame(table(data_2011$ethnicity)) 
names(eth11) <- c('ethnicity','freq_2011')

eth10 <- data.frame(table(data_2010$ethnicity)) 
names(eth10) <- c('ethnicity','freq_2010')

eth09 <- data.frame(ethnicity = NA, freq_2009 = NA) 

eth08 <- data.frame(table(data_2008$ethnicity)) 
names(eth08) <- c('ethnicity','freq_2008')

eth07 <- data.frame(table(data_2007$ethnicity)) 
names(eth07) <- c('ethnicity','freq_2007')

eth06 <- data.frame(table(data_2006$ethnicity)) 
names(eth06) <- c('ethnicity','freq_2006')

eth05 <- data.frame(table(data_2005$ethnicity)) 
names(eth05) <- c('ethnicity','freq_2005')

eth04 <- data.frame(table(data_2004$ethnicity)) 
names(eth04) <- c('ethnicity','freq_2004')

eth03 <- data.frame(table(data_2003$ethnicity)) 
names(eth03) <- c('ethnicity','freq_2003')

eth02 <- data.frame(table(data_2002$ethnicity)) 
names(eth02) <- c('ethnicity','freq_2002')

eth_count<-Reduce(function(x, y) merge(x, y, by="ethnicity", all=TRUE), list(eth02,eth03,eth04,eth05,eth06,eth07,eth08,eth09,eth10,eth11,eth12,eth13,eth14))
write.csv(eth_count,file = '/Users/elmerleezy/Desktop/eth_count.csv')

#Unify variable names
data_2002 <- data_2002[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2003 <- data_2003[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2004 <- data_2004[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2005 <- data_2005[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2006 <- data_2006[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2007 <- data_2007[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2008 <- data_2008[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2009 <- data_2009[c(24,1,26,2,6,25,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
data_2010 <- data_2010[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2011 <- data_2011[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data_2012 <- data_2012[c(1,2,3,4,9,5,6,7,8,11,10,12)]
data_2013 <- data_2013[c(1,2,3,4,9,5,6,7,8,11,10,12)]

names(data_2002) <- c('student_id_scram', "dbn_2002","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2002","grade_code_2002","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2002","bn_2002")
names(data_2003) <- c('student_id_scram', "dbn_2003","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2003","grade_code_2003","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2003","bn_2003")
names(data_2004) <- c('student_id_scram', "dbn_2004","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2004","grade_code_2004","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2004","bn_2004")
names(data_2005) <- c('student_id_scram', "dbn_2005","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2005","grade_code_2005","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2005","bn_2005")
names(data_2006) <- c('student_id_scram', "dbn_2006","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2006","grade_code_2006","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2006","bn_2006")
names(data_2007) <- c('student_id_scram', "dbn_2007","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2007","grade_code_2007","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2007","bn_2007")
names(data_2008) <- c('student_id_scram', "dbn_2008","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2008","grade_code_2008","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2008","bn_2008")
names(data_2009) <- c('student_id_scram', "dbn_2009","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2009","grade_code_2009","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss','lepflg',"d_2009","bn_2009")
names(data_2010) <- c('student_id_scram', "dbn_2010","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2010","grade_code_2010","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2010","bn_2010")
names(data_2011) <- c('student_id_scram', "dbn_2011","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2011","grade_code_2011","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2011","bn_2011")
names(data_2012) <- c('student_id_scram', "dbn_2012","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_code_2012","grade_level_2012","official_class","d_2012","bn_2012")
names(data_2013) <- c('student_id_scram', "dbn_2013","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2013","grade_code_2013","official_class","d_2013","bn_2013")
names(data_2014) <- c('student_id_scram', "dbn_2014","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2014","grade_code_2014","official_class","d_2014","bn_2014")

#Add 0s to student id of year 2013
library(stringr)
data_2014$student_id_scram<- str_pad(data_2014$student_id_scram, 9, pad = "0")
data_2013$student_id_scram<- str_pad(data_2013$student_id_scram, 9, pad = "0")
data_2012$student_id_scram<- str_pad(data_2012$student_id_scram, 9, pad = "0")
data_2011$student_id_scram<- str_pad(data_2011$student_id_scram, 9, pad = "0")
data_2010$student_id_scram<- str_pad(data_2010$student_id_scram, 9, pad = "0")
data_2009$student_id_scram<- str_pad(data_2009$student_id_scram, 9, pad = "0")
data_2008$student_id_scram<- str_pad(data_2008$student_id_scram, 9, pad = "0")
data_2007$student_id_scram<- str_pad(data_2007$student_id_scram, 9, pad = "0")
data_2006$student_id_scram<- str_pad(data_2006$student_id_scram, 9, pad = "0")
data_2005$student_id_scram<- str_pad(data_2005$student_id_scram, 9, pad = "0")
data_2004$student_id_scram<- str_pad(data_2004$student_id_scram, 9, pad = "0")
data_2003$student_id_scram<- str_pad(data_2003$student_id_scram, 9, pad = "0")
data_2002$student_id_scram<- str_pad(data_2002$student_id_scram, 9, pad = "0")

#Substring Dbn to Borough and School
data_2002$dbn1to2 <- substr(data_2002$dbn,1,2)
data_2002$dbn3to6 <- substr(data_2002$dbn,3,6)

data_2003$dbn1to2 <- substr(data_2003$dbn,1,2)
data_2003$dbn3to6 <- substr(data_2003$dbn,3,6)

data_2004$dbn1to2 <- substr(data_2004$dbn,1,2)
data_2004$dbn3to6 <- substr(data_2004$dbn,3,6)

data_2005$dbn1to2 <- substr(data_2005$dbn,1,2)
data_2005$dbn3to6 <- substr(data_2005$dbn,3,6)

data_2006$dbn1to2 <- substr(data_2006$dbn,1,2)
data_2006$dbn3to6 <- substr(data_2006$dbn,3,6)

data_2007$dbn1to2 <- substr(data_2007$dbn,1,2)
data_2007$dbn3to6 <- substr(data_2007$dbn,3,6)

data_2008$dbn1to2 <- substr(data_2008$dbn,1,2)
data_2008$dbn3to6 <- substr(data_2008$dbn,3,6)

data_2009$dbn1to2 <- substr(data_2009$dbn,1,2)
data_2009$dbn3to6 <- substr(data_2009$dbn,3,6)

data_2010$dbn1to2 <- substr(data_2010$dbn,1,2)
data_2010$dbn3to6 <- substr(data_2010$dbn,3,6)

data_2011$dbn1to2 <- substr(data_2011$dbn,1,2)
data_2011$dbn3to6 <- substr(data_2011$dbn,3,6)

data_2012$dbn1to2 <- substr(data_2012$dbn,1,2)
data_2012$dbn3to6 <- substr(data_2012$dbn,3,6)

data_2013$dbn1to2 <- substr(data_2013$dbn,1,2)
data_2013$dbn3to6 <- substr(data_2013$dbn,3,6)

data_2014$dbn1to2 <- substr(data_2014$dbn,1,2)
data_2014$dbn3to6 <- substr(data_2014$dbn,3,6)


#Recode Grade level
library(plyr)
#2013_2014
Merge_2013_2014$grade_level_2013<- mapvalues(Merge_2013_2014$grade_level_2013, from = c("0K", "PK"), to = c(.5, 0))
Merge_2013_2014$grade_level_2014<- mapvalues(Merge_2013_2014$grade_level_2014, from = c("0K", "PK"), to = c(.5, 0))

Merge_2013_2014$grade_level_2013 <- as.numeric(Merge_2013_2014$grade_level_2013)
Merge_2013_2014$grade_level_2013 <- mapvalues(Merge_2013_2014$grade_level_2013, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

Merge_2013_2014$grade_level_2014 <- as.numeric(Merge_2013_2014$grade_level_2014)
Merge_2013_2014$grade_level_2014 <- mapvalues(Merge_2013_2014$grade_level_2014, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

data_2014$grade_level_2014 <- as.numeric(data_2014$grade_level_2014)
data_2014$grade_level_2014 <- mapvalues(data_2014$grade_level_2014, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

data_2013$grade_level_2013 <- as.numeric(data_2013$grade_level_2013)
data_2013$grade_level_2013 <- mapvalues(data_2013$grade_level_2013, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

data_2012$grade_level_2012_2 <- as.numeric(data_2012$grade_level_2012)
data_2012$grade_level_2012_2 <- mapvalues(data_2012$grade_level_2012_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
data_2012$grade_level_2012 <- data_2012$grade_level_2012_2
data_2012$grade_level_2012_2 <- NULL

data_2011$grade_level_2011_2 <- as.numeric(data_2011$grade_level_2011)
data_2011$grade_level_2011_2 <- mapvalues(data_2011$grade_level_2011_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1))
#??????
data_2011$grade_level_2011 <- mapvalues(data_2011$grade_level_2011, from = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1))

data_2011$grade_level_2011 <- data_2011$grade_level_2011_2
data_2011$grade_level_2011_2 <- NULL

data_2010$grade_level_2010_2 <- as.numeric(data_2010$grade_level_2010)
data_2010$grade_level_2010_2 <- mapvalues(data_2010$grade_level_2010_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), to = c(NA,1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1))
#??????
data_2010$grade_level_2010 <- mapvalues(data_2010$grade_level_2010, from = c(NA,1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1), to = c(NA,1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1))

data_2010$grade_level_2010 <- data_2010$grade_level_2010_2
data_2010$grade_level_2010_2 <- NULL

data_2009$grade_level_2009_2 <- as.numeric(data_2009$grade_level_2009)
data_2009$grade_level_2009_2<- mapvalues(data_2009$grade_level_2009_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
data_2009$grade_level_2009 <- data_2009$grade_level_2009_2
data_2009$grade_level_2009_2 <- NULL

data_2008$grade_level_2008_2 <- as.numeric(data_2008$grade_level_2008)
data_2008$grade_level_2008_2 <- mapvalues(data_2008$grade_level_2008_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
data_2008$grade_level_2008 <- data_2008$grade_level_2008_2
data_2008$grade_level_2008_2 <- NULL


data_2007$grade_level_2007_2 <- as.numeric(data_2007$grade_level_2007)
data_2007$grade_level_2007_2 <- mapvalues(data_2007$grade_level_2007_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
data_2007$grade_level_2007 <- data_2007$grade_level_2007_2
data_2007$grade_level_2007_2 <- NULL


data_2006$grade_level_2006_2 <- as.numeric(data_2006$grade_level_2006)
data_2006$grade_level_2006_2 <- mapvalues(data_2006$grade_level_2006_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
data_2006$grade_level_2006 <- data_2006$grade_level_2006_2 
data_2006$grade_level_2006_2 <- NULL

data_2005$grade_level_2005_2 <- as.numeric(data_2005$grade_level_2005)
data_2005$grade_level_2005 <- mapvalues(data_2005$grade_level_2005_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

data_2004$grade_level_2004_2 <- as.numeric(data_2004$grade_level_2004)
data_2004$grade_level_2004 <- mapvalues(data_2004$grade_level_2004_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

data_2003$grade_level_2003_2 <- as.numeric(data_2003$grade_level_2003)
data_2003$grade_level_2003_2 <- mapvalues(data_2003$grade_level_2003_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1))
#??????
data_2003$grade_level_2003 <- mapvalues(data_2003$grade_level_2003, from = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1))

data_2003$grade_level_2003 <- data_2003$grade_level_2003_2
data_2003$grade_level_2003_2 <- NULL

data_2002$grade_level_2002_2 <- as.numeric(data_2002$grade_level_2002)
data_2002$grade_level_2002_2 <- mapvalues(data_2002$grade_level_2002_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1))
#??????
data_2002$grade_level_2002 <- mapvalues(data_2002$grade_level_2002, from = c(1,2,3,4,5,6,7,8,9,0,10,11,12,0.5,-1), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1))

data_2002$grade_level_2002 <- data_2002$grade_level_2002_2
data_2002$grade_level_2002_2 <- NULL

##### STEP 2: DETECT OUTLIERS
#Detect student dbn out of a range
df<-data_2002$dbn1to2
df<-as.integer(df)
out_dbn_2002<-data.frame(data_2002[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2003$dbn1to2
df<-as.integer(df)
out_dbn_2003<-data.frame(data_2003[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2004$dbn1to2
df<-as.integer(df)
out_dbn_2004<-data.frame(data_2004[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2005$dbn1to2
df<-as.integer(df)
out_dbn_2005<-data.frame(data_2005[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2006$dbn1to2
df<-as.integer(df)
out_dbn_2006<-data.frame(data_2006[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2007$dbn1to2
df<-as.integer(df)
out_dbn_2007<-data.frame(data_2007[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2008$dbn1to2
df<-as.integer(df)
out_dbn_2008<-data.frame(data_2008[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2009$dbn1to2
df<-as.integer(df)
out_dbn_2009<-data.frame(data_2009[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2010$dbn1to2
df<-as.integer(df)
out_dbn_2010<-data.frame(data_2010[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2011$dbn1to2
df<-as.integer(df)
out_dbn_2011<-data.frame(data_2011[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2012$dbn1to2
df<-as.integer(df)
out_dbn_2012<-data.frame(data_2012[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2013$dbn1to2
df<-as.integer(df)
out_dbn_2013<-data.frame(data_2013[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2014$dbn1to2
df<-as.integer(df)
out_dbn_2014<-data.frame(data_2014[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79)| (df == 84)),])

#Detect student lta_flag not blank
out_lta_flg_2002<- data_2002[!is.na(data_2002$lta_flg), ]
out_lta_flg_2003<- data_2003[!is.na(data_2003$lta_flg), ]
out_lta_flg_2004<- data_2004[!is.na(data_2004$lta_flg), ]
out_lta_flg_2005<- data_2005[!is.na(data_2005$lta_flg), ]
out_lta_flg_2006<- data_2006[!is.na(data_2006$lta_flg), ]
out_lta_flg_2007<- data_2007[!is.na(data_2007$lta_flg), ]
out_lta_flg_2008<- data_2008[!is.na(data_2008$lta_flg), ]
out_lta_flg_2009<- data_2009[!is.na(data_2009$lta_flg), ]
out_lta_flg_2010<- data_2010[!is.na(data_2010$lta_flg), ]
out_lta_flg_2011<- data_2011[!is.na(data_2011$lta_flg), ]

#Merge 2013 and 2014 Dataset
Merge_2013_2014 <- merge(data_2013, data_2014, by="student_id_scram", all = TRUE)
Merge_2012_2013 <- merge(data_2012, data_2013, by="student_id_scram", all = TRUE)
Merge_2011_2012 <- merge(data_2011, data_2012, by="student_id_scram", all = TRUE)
Merge_2010_2011 <- merge(data_2010, data_2011, by="student_id_scram", all = TRUE)
Merge_2009_2010 <- merge(data_2009, data_2010, by="student_id_scram", all = TRUE)
Merge_2008_2009 <- merge(data_2008, data_2009, by="student_id_scram", all = TRUE)
Merge_2007_2008 <- merge(data_2007, data_2008, by="student_id_scram", all = TRUE)
Merge_2006_2007 <- merge(data_2006, data_2007, by="student_id_scram", all = TRUE)
Merge_2005_2006 <- merge(data_2005, data_2006, by="student_id_scram", all = TRUE)
Merge_2004_2005 <- merge(data_2004, data_2005, by="student_id_scram", all = TRUE)
Merge_2003_2004 <- merge(data_2003, data_2004, by="student_id_scram", all = TRUE)
Merge_2002_2003 <- merge(data_2002, data_2003, by="student_id_scram", all = TRUE)



