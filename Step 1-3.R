#######################################
##  STEP 1: READ DATA & ORGANIZE ##
#######################################

### Import Datasets

library(readr)
data_2002<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2002-03_Biog1031_Scrambled with LTA.csv')
data_2003<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2003-04_Biog1031_Scrambled with LTA.csv')
data_2004<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2004-05_Biog1031_Scrambled with LTA.csv')
data_2005<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2005-06_Biog1031_Scrambled with LTA.csv')
data_2006<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2006-07_Biog1031_Scrambled with LTA.csv')
data_2007<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2007-08_Biog1031_Scrambled with LTA.csv')
data_2008<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2008-09_Biog1031_Scrambled with LTA.csv')
data_2009<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2009-10_Biog1031_Scrambled with LTA.csv')
data_2010<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2010-11_Biog1031_Scrambled with LTA.csv')
data_2011<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2011-12_Biog1031_Scrambled with LTA.csv')
data_2012<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2012-13_Biog1031_Scrambled with LTA.csv')
data_2013<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2013-14_Biog1031_Scrambled with LTA.csv')
data_2014<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2014-15_Biog1031_Scrambled with LTA.csv')
data_2015<-read_csv('/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA/2015-16_Biog1031_Scrambled with LTA.csv')


### Blank/Missing/Duplicate Values

  # Convert all blank value to NA
data_2002[data_2002==""] <- NA
data_2003[data_2003==""] <- NA
data_2004[data_2004==""] <- NA
data_2005[data_2005==""] <- NA
data_2006[data_2006==""] <- NA
data_2007[data_2007==""] <- NA
data_2008[data_2008==""] <- NA
data_2008[data_2008==""] <- NA
data_2009[data_2009==""] <- NA
data_2010[data_2010==""] <- NA
data_2011[data_2011==""] <- NA
data_2012[data_2012==""] <- NA
data_2013[data_2013==""] <- NA
data_2014[data_2014==""] <- NA
data_2015[data_2015==""] <- NA
  # Summarise blank or missing values
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
sapply(data_2015, function(x) sum(is.na(x)))
	# Creating a table summarizing missing values
count_missing<-data.frame(sapply(data_2002, function(x) sum(is.na(x))),sapply(data_2003, function(x) sum(is.na(x))),
                            sapply(data_2004, function(x) sum(is.na(x))),sapply(data_2005, function(x) sum(is.na(x))),
                            sapply(data_2006, function(x) sum(is.na(x))),sapply(data_2007, function(x) sum(is.na(x))),
                            sapply(data_2008, function(x) sum(is.na(x))),sapply(data_2009, function(x) sum(is.na(x))), 
                            sapply(data_2010, function(x) sum(is.na(x))),sapply(data_2011, function(x) sum(is.na(x))),
                            sapply(data_2012, function(x) sum(is.na(x))),sapply(data_2013, function(x) sum(is.na(x))),
                            sapply(data_2014, function(x) sum(is.na(x))), sapply(data_2015, function(x) sum(is.na(x))))  
count_obs <- data.frame(
	nrow(data_2002),
	nrow(data_2003),
	nrow(data_2004),
	nrow(data_2005),
	nrow(data_2006),
	nrow(data_2007),
	nrow(data_2008),
	nrow(data_2009),
	nrow(data_2010),
	nrow(data_2011),
	nrow(data_2012),
	nrow(data_2013),
	nrow(data_2014),
	nrow(data_2015))                                                 

names(count_missing) <- c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
names(count_obs) <- c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
count_missing<-rbind(count_obs,count_missing)
write.csv(count_missing, file = '/Users/elmerleezy/Desktop/count_missing.csv')

	# Delete Blank Values 
'data_2013 <- data_2013[!(data_2013$student_id_scram == ""), ]
data_2013 <- data_2013[!(data_2013$dbn == ""), ]
data_2013 <- data_2013[!(data_2013$birth_mo_yr == ""), ]
data_2013 <- data_2013[!(data_2013$sex == ""), ]'

	# Find duplicate values for student id scram
		# result: there is no duplicates

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
data_2015$student_id_scram[duplicated(data_2015$student_id_scram)]

			# duplicate_studentid <- data.frame(data_2013$student_id_scram[duplicated(data_2013$student_id_scram)])
			# 'Find two student ids that are duplicated:
			# data_2002: 641152411
			# data_2003: 804402954 184752914
			# data_2013: 01032503 444022674'
			# 	# Remove Duplicates
			# data_2002 = data_2002[-771030,]
			# data_2003 = data_2003[-617515,]
			# data_2003 = data_2003[-834337,]
			# data_2013 = data_2013[-442789,]
			# data_2013 = data_2013[-490454,]
			# 	# Remove duplicates (not to be used)
			# unique(!data_2013$student_id_scram)
			# data_2013_dupremoved<-data.frame(data_2013[!duplicated(data_2013$student_id_scram),])
			# 	# Another way (not to be used)
			# n_occur <- data.frame(table(data_2013$student_id_scram))
			# n_occur[n_occur$Freq > 1,]
			# data_2013[data_2013$student_id_scram %in% n_occur$Var1[n_occur$Freq > 1],]


### Unify Variables


	# Unify "birth" variable
		# result: unified
			# library(stringr)
			# data_2011$birth_mo<- str_pad(data_2011$birth_mo, 2, pad = "0")
			# data_2011$birth_mm_yyyy = paste(data_2011$birth_mo, data_2011$birth_yr,sep = '-')
			# data_2011$birth_mo<-NULL
			# data_2011$birth_yr<-NULL

			# data_2010$birth_mo<- str_pad(data_2010$birth_mo, 2, pad = "0")
			# data_2010$birth_mm_yyyy = paste(data_2010$birth_mo, data_2010$birth_yr,sep = '-')
			# data_2010$birth_mo<-NULL
			# data_2010$birth_yr<-NULL

			# data_2009$birth_mo<- str_pad(data_2009$birth_mo, 2, pad = "0")
			# data_2009$birth_mm_yyyy = paste(data_2009$birth_mo, data_2009$birth_yr,sep = '-')
			# data_2009$birth_mo<-NULL
			# data_2009$birth_yr<-NULL

			# data_2008$birth_mo<- str_pad(data_2008$birth_mo, 2, pad = "0")
			# data_2008$birth_mm_yyyy = paste(data_2008$birth_mo, data_2008$birth_yr,sep = '-')
			# data_2008$birth_mo<-NULL
			# data_2008$birth_yr<-NULL

			# data_2007$birth_mo<- str_pad(data_2007$birth_mo, 2, pad = "0")
			# data_2007$birth_mm_yyyy = paste(data_2007$birth_mo, data_2007$birth_yr,sep = '-')
			# data_2007$birth_mo<-NULL
			# data_2007$birth_yr<-NULL

			# data_2006$birth_mo<- str_pad(data_2006$birth_mo, 2, pad = "0")
			# data_2006$birth_mm_yyyy = paste(data_2006$birth_mo, data_2006$birth_yr,sep = '-')
			# data_2006$birth_mo<-NULL
			# data_2006$birth_yr<-NULL

			# data_2005$birth_mo<- str_pad(data_2005$birth_mo, 2, pad = "0")
			# data_2005$birth_mm_yyyy = paste(data_2005$birth_mo, data_2005$birth_yr,sep = '-')
			# data_2005$birth_mo<-NULL
			# data_2005$birth_yr<-NULL

			# data_2004$birth_mo<- str_pad(data_2004$birth_mo, 2, pad = "0")
			# data_2004$birth_mm_yyyy = paste(data_2004$birth_mo, data_2004$birth_yr,sep = '-')
			# data_2004$birth_mo<-NULL
			# data_2004$birth_yr<-NULL

			# data_2003$birth_mo<- str_pad(data_2003$birth_mo, 2, pad = "0")
			# data_2003$birth_mm_yyyy = paste(data_2003$birth_mo, data_2003$birth_yr,sep = '-')
			# data_2003$birth_mo<-NULL
			# data_2003$birth_yr<-NULL

			# data_2002$birth_mo<- str_pad(data_2002$birth_mo, 2, pad = "0")
			# data_2002$birth_mm_yyyy = paste(data_2002$birth_mo, data_2002$birth_yr,sep = '-')
			# data_2002$birth_mo<-NULL
			# data_2002$birth_yr<-NULL

	# Add 0s to student id 
		# result: unified
			# library(stringr)
			# data_2014$student_id_scram<- str_pad(data_2014$student_id_scram, 9, pad = "0")
			# data_2013$student_id_scram<- str_pad(data_2013$student_id_scram, 9, pad = "0")
			# data_2012$student_id_scram<- str_pad(data_2012$student_id_scram, 9, pad = "0")
			# data_2011$student_id_scram<- str_pad(data_2011$student_id_scram, 9, pad = "0")
			# data_2010$student_id_scram<- str_pad(data_2010$student_id_scram, 9, pad = "0")
			# data_2009$student_id_scram<- str_pad(data_2009$student_id_scram, 9, pad = "0")
			# data_2008$student_id_scram<- str_pad(data_2008$student_id_scram, 9, pad = "0")
			# data_2007$student_id_scram<- str_pad(data_2007$student_id_scram, 9, pad = "0")
			# data_2006$student_id_scram<- str_pad(data_2006$student_id_scram, 9, pad = "0")
			# data_2005$student_id_scram<- str_pad(data_2005$student_id_scram, 9, pad = "0")
			# data_2004$student_id_scram<- str_pad(data_2004$student_id_scram, 9, pad = "0")
			# data_2003$student_id_scram<- str_pad(data_2003$student_id_scram, 9, pad = "0")
			# data_2002$student_id_scram<- str_pad(data_2002$student_id_scram, 9, pad = "0")

	# Unify Ethnicity Variable
eth15 <- data.frame(table(data_2015$ethnicity)) 
eth14 <- data.frame(table(data_2014$ethnicity)) 
eth13 <- data.frame(table(data_2013$ethnicity)) 
eth12 <- data.frame(table(data_2012$ethnicity)) 
eth11 <- data.frame(table(data_2011$ethnicity)) 
eth10 <- data.frame(table(data_2010$ethnicity)) 
eth09 <- data.frame(table(data_2009$ethnicity)) 
eth08 <- data.frame(table(data_2008$ethnicity)) 
eth07 <- data.frame(table(data_2007$ethnicity)) 
eth06 <- data.frame(table(data_2006$ethnicity)) 
eth05 <- data.frame(table(data_2005$ethnicity)) 
eth04 <- data.frame(table(data_2004$ethnicity)) 
eth03 <- data.frame(table(data_2003$ethnicity)) 
eth02 <- data.frame(table(data_2002$ethnicity)) 

names(eth15) <- c('ethnicity','freq_2015')
names(eth14) <- c('ethnicity','freq_2014')
names(eth13) <- c('ethnicity','freq_2013')
names(eth12) <- c('ethnicity','freq_2012')
names(eth11) <- c('ethnicity','freq_2011')
names(eth10) <- c('ethnicity','freq_2010')
names(eth09) <- c('ethnicity','freq_2009')
names(eth08) <- c('ethnicity','freq_2008')
names(eth07) <- c('ethnicity','freq_2007')
names(eth06) <- c('ethnicity','freq_2006')
names(eth05) <- c('ethnicity','freq_2005')
names(eth04) <- c('ethnicity','freq_2004')
names(eth03) <- c('ethnicity','freq_2003')
names(eth02) <- c('ethnicity','freq_2002')

count_eth<-Reduce(function(x, y) merge(x, y, by="ethnicity", all=TRUE), list(eth02,eth03,eth04,eth05,eth06,eth07,eth08,eth09,eth10,eth11,eth12,eth13,eth14))
write.csv(count_eth,file = '/Users/elmerleezy/Desktop/count_eth.csv')

	# Unify variable sequences
total_var <- data.frame(names(data_2002), names(data_2003), names(data_2004), names(data_2005), names(data_2006), names(data_2007), names(data_2008), names(data_2009), names(data_2010), names(data_2011), names(data_2012), names(data_2013), names(data_2014), names(data_2015)) 
		# result: variables are unified

			# data_2002 <- data_2002[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2003 <- data_2003[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2004 <- data_2004[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2005 <- data_2005[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2006 <- data_2006[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2007 <- data_2007[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2008 <- data_2008[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2009 <- data_2009[c(24,1,26,2,6,25,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
			# data_2010 <- data_2010[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2011 <- data_2011[c(23,1,25,2,6,24,3,4,5,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
			# data_2012 <- data_2012[c(1,2,3,4,9,5,6,7,8,11,10,12)]
			# data_2013 <- data_2013[c(1,2,3,4,9,5,6,7,8,11,10,12)]
			# data_2014 <- data_2014[c(1,2,3,4,9,5,6,7,8,11,10,12)]
			# data_2015 <- data_2015[c(1,2,3,4,9,5,6,7,8,11,10,12)]

	# Substring Dbn to Borough and School
data_2002$d_2002 <- substr(data_2002$dbn,1,2)
data_2003$d_2003 <- substr(data_2003$dbn,1,2)
data_2004$d_2004 <- substr(data_2004$dbn,1,2)
data_2005$d_2005 <- substr(data_2005$dbn,1,2)
data_2006$d_2006 <- substr(data_2006$dbn,1,2)
data_2007$d_2007 <- substr(data_2007$dbn,1,2)
data_2008$d_2008 <- substr(data_2008$dbn,1,2)
data_2009$d_2009 <- substr(data_2009$dbn,1,2)
data_2010$d_2010 <- substr(data_2010$dbn,1,2)
data_2011$d_2011 <- substr(data_2011$dbn,1,2)
data_2012$d_2012 <- substr(data_2012$dbn,1,2)
data_2013$d_2013 <- substr(data_2013$dbn,1,2)
data_2014$d_2014 <- substr(data_2014$dbn,1,2)
data_2015$d_2015 <- substr(data_2015$dbn,1,2)

data_2002$bn_2002 <- substr(data_2002$dbn,3,6)
data_2003$bn_2003 <- substr(data_2003$dbn,3,6)
data_2004$bn_2004 <- substr(data_2004$dbn,3,6)
data_2005$bn_2005 <- substr(data_2005$dbn,3,6)
data_2006$bn_2006 <- substr(data_2006$dbn,3,6)
data_2007$bn_2007 <- substr(data_2007$dbn,3,6)
data_2008$bn_2008 <- substr(data_2008$dbn,3,6)
data_2009$bn_2009 <- substr(data_2009$dbn,3,6)
data_2010$bn_2010 <- substr(data_2010$dbn,3,6)
data_2011$bn_2011 <- substr(data_2011$dbn,3,6)
data_2012$bn_2012 <- substr(data_2012$dbn,3,6)
data_2013$bn_2013 <- substr(data_2013$dbn,3,6)
data_2014$bn_2014 <- substr(data_2014$dbn,3,6)
data_2015$bn_2015 <- substr(data_2015$dbn,3,6)

	# Unify variable names
		# result: unified
			# names(data_2002) <- c('student_id_scram', "dbn_2002","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2002","grade_code_2002","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2002","bn_2002")
			# names(data_2003) <- c('student_id_scram', "dbn_2003","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2003","grade_code_2003","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2003","bn_2003")
			# names(data_2004) <- c('student_id_scram', "dbn_2004","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2004","grade_code_2004","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2004","bn_2004")
			# names(data_2005) <- c('student_id_scram', "dbn_2005","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2005","grade_code_2005","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2005","bn_2005")
			# names(data_2006) <- c('student_id_scram', "dbn_2006","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2006","grade_code_2006","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2006","bn_2006")
			# names(data_2007) <- c('student_id_scram', "dbn_2007","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2007","grade_code_2007","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2007","bn_2007")
			# names(data_2008) <- c('student_id_scram', "dbn_2008","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2008","grade_code_2008","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2008","bn_2008")
			# names(data_2009) <- c('student_id_scram', "dbn_2009","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2009","grade_code_2009","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss','lepflg',"d_2009","bn_2009")
			# names(data_2010) <- c('student_id_scram', "dbn_2010","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2010","grade_code_2010","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2010","bn_2010")
			# names(data_2011) <- c('student_id_scram', "dbn_2011","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2011","grade_code_2011","official_class",'lta_flg','create_dte','spec_ed_code','univ_pk_grade','school_admin_dbn','school_level','funding_cde','room_num','from_school_dbn','acd_flg','cons_teach','ctt_part_flg','periods_per_wk_setss',"d_2011","bn_2011")
			# names(data_2012) <- c('student_id_scram', "dbn_2012","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_code_2012","grade_level_2012","official_class","d_2012","bn_2012")
			# names(data_2013) <- c('student_id_scram', "dbn_2013","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2013","grade_code_2013","official_class","d_2013","bn_2013")
			# names(data_2014) <- c('student_id_scram', "dbn_2014","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2014","grade_code_2014","official_class","d_2014","bn_2014")
			# names(data_2015) <- c('student_id_scram', "dbn_2015","birth_mm_yyyy","sex","ethnicity","lunch","admission_date","home_lang","pob_code","grade_level_2015","grade_code_2015","official_class","d_2015","bn_2015")


colnames(data_2002)[10] <- "grade_level_2002"
colnames(data_2003)[10] <- "grade_level_2003"
colnames(data_2004)[10] <- "grade_level_2004"
colnames(data_2005)[10] <- "grade_level_2005"
colnames(data_2006)[10] <- "grade_level_2006"
colnames(data_2007)[10] <- "grade_level_2007"
colnames(data_2008)[10] <- "grade_level_2008"
colnames(data_2009)[10] <- "grade_level_2009"
colnames(data_2010)[10] <- "grade_level_2010"
colnames(data_2011)[10] <- "grade_level_2011"
colnames(data_2012)[10] <- "grade_level_2012"
colnames(data_2013)[10] <- "grade_level_2013"
colnames(data_2014)[10] <- "grade_level_2014"
colnames(data_2015)[10] <- "grade_level_2015"

	# Recode Grade level
		# Summarize grade distribution
library(plyr)

grade_level_2015_t <- data.frame(table(data_2015$grade_level_2015))
grade_level_2014_t <- data.frame(table(data_2014$grade_level_2014))
grade_level_2013_t <- data.frame(table(data_2013$grade_level_2013))
grade_level_2012_t <- data.frame(table(data_2012$grade_level_2012))
grade_level_2011_t <- data.frame(table(data_2011$grade_level_2011))
grade_level_2010_t <- data.frame(table(data_2010$grade_level_2010))
grade_level_2009_t <- data.frame(table(data_2009$grade_level_2009))
grade_level_2008_t <- data.frame(table(data_2008$grade_level_2008))
grade_level_2007_t <- data.frame(table(data_2007$grade_level_2007))
grade_level_2006_t <- data.frame(table(data_2006$grade_level_2006))
grade_level_2005_t <- data.frame(table(data_2005$grade_level_2005))
grade_level_2004_t <- data.frame(table(data_2004$grade_level_2004))
grade_level_2003_t <- data.frame(table(data_2003$grade_level_2003))
grade_level_2002_t <- data.frame(table(data_2002$grade_level_2002))

names(grade_level_2015_t) <- c('grade','freq_2015')
names(grade_level_2014_t) <- c('grade','freq_2014')
names(grade_level_2013_t) <- c('grade','freq_2013')
names(grade_level_2012_t) <- c('grade','freq_2012')
names(grade_level_2011_t) <- c('grade','freq_2011')
names(grade_level_2010_t) <- c('grade','freq_2010')
names(grade_level_2009_t) <- c('grade','freq_2009')
names(grade_level_2008_t) <- c('grade','freq_2008')
names(grade_level_2007_t) <- c('grade','freq_2007')
names(grade_level_2006_t) <- c('grade','freq_2006')
names(grade_level_2005_t) <- c('grade','freq_2005')
names(grade_level_2004_t) <- c('grade','freq_2004')
names(grade_level_2003_t) <- c('grade','freq_2003')
names(grade_level_2002_t) <- c('grade','freq_2002')

count_grade<-Reduce(function(x, y) merge(x, y, by="grade", all=TRUE), 
	list(
		grade_level_2015_t,
		grade_level_2014_t,
		grade_level_2013_t,
		grade_level_2012_t,
		grade_level_2011_t,
		grade_level_2010_t,
		grade_level_2009_t,
		grade_level_2008_t,
		grade_level_2007_t,
		grade_level_2006_t,
		grade_level_2005_t,
		grade_level_2004_t,
		grade_level_2003_t,
		grade_level_2002_t
		))
write.csv(count_grade,file = '/Users/elmerleezy/Desktop/count_grade.csv')
		# result: AD - 2011,2015; IN - 2002,2003,2010; 

		# Delete AD and IN
		data_2002 <- data_2003[!(data_2002$grade_level_2002 == "IN"), ]
		data_2003 <- data_2003[!(data_2003$grade_level_2003 == "IN"), ]
		data_2010 <- data_2010[!(data_2010$grade_level_2010 == "IN"), ]
		data_2011 <- data_2011[!(data_2011$grade_level_2011 == "AD"), ]
		data_2015 <- data_2015[!(data_2015$grade_level_2015 == "AD"), ]

		# Recode 0K and PK
data_2002$grade_level_2002[which(data_2002$grade_level_2002 == '0K' )] <- 0
data_2003$grade_level_2003[which(data_2003$grade_level_2003 == '0K' )] <- 0
data_2004$grade_level_2004[which(data_2004$grade_level_2004 == '0K' )] <- 0
data_2005$grade_level_2005[which(data_2005$grade_level_2005 == '0K' )] <- 0
data_2006$grade_level_2006[which(data_2006$grade_level_2006 == '0K' )] <- 0
data_2007$grade_level_2007[which(data_2007$grade_level_2007 == '0K' )] <- 0
data_2008$grade_level_2008[which(data_2008$grade_level_2008 == '0K' )] <- 0
data_2009$grade_level_2009[which(data_2009$grade_level_2009 == '0K' )] <- 0
data_2010$grade_level_2010[which(data_2010$grade_level_2010 == '0K' )] <- 0
data_2011$grade_level_2011[which(data_2011$grade_level_2011 == '0K' )] <- 0
data_2012$grade_level_2012[which(data_2012$grade_level_2012 == '0K' )] <- 0
data_2013$grade_level_2013[which(data_2013$grade_level_2013 == '0K' )] <- 0
data_2014$grade_level_2014[which(data_2014$grade_level_2014 == '0K' )] <- 0
data_2015$grade_level_2015[which(data_2015$grade_level_2015 == '0K' )] <- 0

data_2002$grade_level_2002[which(data_2002$grade_level_2002 == 'PK' )] <- -1
data_2003$grade_level_2003[which(data_2003$grade_level_2003 == 'PK' )] <- -1
data_2004$grade_level_2004[which(data_2004$grade_level_2004 == 'PK' )] <- -1
data_2005$grade_level_2005[which(data_2005$grade_level_2005 == 'PK' )] <- -1
data_2006$grade_level_2006[which(data_2006$grade_level_2006 == 'PK' )] <- -1
data_2007$grade_level_2007[which(data_2007$grade_level_2007 == 'PK' )] <- -1
data_2008$grade_level_2008[which(data_2008$grade_level_2008 == 'PK' )] <- -1
data_2009$grade_level_2009[which(data_2009$grade_level_2009 == 'PK' )] <- -1
data_2010$grade_level_2010[which(data_2010$grade_level_2010 == 'PK' )] <- -1
data_2011$grade_level_2011[which(data_2011$grade_level_2011 == 'PK' )] <- -1
data_2012$grade_level_2012[which(data_2012$grade_level_2012 == 'PK' )] <- -1
data_2013$grade_level_2013[which(data_2013$grade_level_2013 == 'PK' )] <- -1
data_2014$grade_level_2014[which(data_2014$grade_level_2014 == 'PK' )] <- -1
data_2015$grade_level_2015[which(data_2015$grade_level_2015 == 'PK' )] <- -1

data_2015$grade_level_2015_2 <- NULL
data_2014$grade_level_2014_2 <- NULL
data_2013$grade_level_2013_2 <- NULL
data_2012$grade_level_2012_2 <- NULL
data_2011$grade_level_2011_2 <- NULL
data_2010$grade_level_2010_2 <- NULL
data_2009$grade_level_2009_2 <- NULL
data_2008$grade_level_2008_2 <- NULL
data_2007$grade_level_2007_2 <- NULL
data_2006$grade_level_2006_2 <- NULL
data_2005$grade_level_2005_2 <- NULL
data_2004$grade_level_2004_2 <- NULL
data_2003$grade_level_2003_2 <- NULL
data_2002$grade_level_2002_2 <- NULL

data_2015$grade_level_2015 <- as.numeric(data_2015$grade_level_2015)
data_2014$grade_level_2014 <- as.numeric(data_2014$grade_level_2014)
data_2013$grade_level_2013 <- as.numeric(data_2013$grade_level_2013)
data_2012$grade_level_2012 <- as.numeric(data_2012$grade_level_2012)
data_2011$grade_level_2011 <- as.numeric(data_2011$grade_level_2011)
data_2010$grade_level_2010 <- as.numeric(data_2010$grade_level_2010)
data_2009$grade_level_2009 <- as.numeric(data_2009$grade_level_2009)
data_2008$grade_level_2008 <- as.numeric(data_2008$grade_level_2008)
data_2007$grade_level_2007 <- as.numeric(data_2007$grade_level_2007)
data_2006$grade_level_2006 <- as.numeric(data_2006$grade_level_2006)
data_2005$grade_level_2005 <- as.numeric(data_2005$grade_level_2005)
data_2004$grade_level_2004 <- as.numeric(data_2004$grade_level_2004)
data_2003$grade_level_2003 <- as.numeric(data_2003$grade_level_2003)
data_2002$grade_level_2002 <- as.numeric(data_2002$grade_level_2002)


		# previous:
			# data_2015$grade_level_2015 <- mapvalues(data_2015$grade_level_2015, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1)) # grade_level_2015 has AD

			# data_2014$grade_level_2014 <- mapvalues(data_2014$grade_level_2014, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

			# data_2013$grade_level_2013 <- mapvalues(data_2013$grade_level_2013, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))

			# data_2012$grade_level_2012_2 <- mapvalues(data_2012$grade_level_2012_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2012$grade_level_2012 <- data_2012$grade_level_2012_2
			# data_2012$grade_level_2012_2 <- NULL

			# data_2011$grade_level_2011_2 <- mapvalues(data_2011$grade_level_2011_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1)) # grade_level_2011 has AD
			# data_2011$grade_level_2011 <- data_2011$grade_level_2011_2
			# data_2011$grade_level_2011_2 <- NULL

			# data_2010$grade_level_2010_2 <- mapvalues(data_2010$grade_level_2010_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), to = c(NA,1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1)) # grade_level_2010 has NA and IN
			# data_2010$grade_level_2010 <- data_2010$grade_level_2010_2
			# data_2010$grade_level_2010_2 <- NULL

			# data_2009$grade_level_2009_2<- mapvalues(data_2009$grade_level_2009_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2009$grade_level_2009 <- data_2009$grade_level_2009_2
			# data_2009$grade_level_2009_2 <- NULL

			# data_2008$grade_level_2008_2 <- mapvalues(data_2008$grade_level_2008_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2008$grade_level_2008 <- data_2008$grade_level_2008_2
			# data_2008$grade_level_2008_2 <- NULL


			# data_2007$grade_level_2007_2 <- mapvalues(data_2007$grade_level_2007_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2007$grade_level_2007 <- data_2007$grade_level_2007_2
			# data_2007$grade_level_2007_2 <- NULL


			# data_2006$grade_level_2006_2 <- mapvalues(data_2006$grade_level_2006_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2006$grade_level_2006 <- data_2006$grade_level_2006_2 
			# data_2006$grade_level_2006_2 <- NULL

			# data_2005$grade_level_2005 <- mapvalues(data_2005$grade_level_2005_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2005$grade_level_2005_2 <- NULL


			# data_2004$grade_level_2004 <- mapvalues(data_2004$grade_level_2004_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-1))
			# data_2004$grade_level_2004_2 <- NULL

			# data_2003$grade_level_2003_2 <- mapvalues(data_2003$grade_level_2003_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1))  # grade_level 2003 contains IN
			# data_2003$grade_level_2003 <- data_2003$grade_level_2003_2
			# data_2003$grade_level_2003_2 <- NULL

			# data_2002$grade_level_2002_2 <- mapvalues(data_2002$grade_level_2002_2, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to = c(1,2,3,4,5,6,7,8,9,0,10,11,12,-2,-1)) # grade_level 2002 contains IN
			# data_2002$grade_level_2002 <- data_2002$grade_level_2002_2
			# data_2002$grade_level_2002_2 <- NULL




#######################################
##  STEP 2: DETECT OUTLIERS ##
#######################################

# Detect student dbn out of a range
df<-data_2015$d_2015
df<-as.integer(df)
out_dbn_2015<-data.frame(data_2015[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79)| (df == 84)),])

df<-data_2014$d_2014
df<-as.integer(df)
out_dbn_2014<-data.frame(data_2014[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79)| (df == 84)),])

df<-data_2013$d_2013
df<-as.integer(df)
out_dbn_2013<-data.frame(data_2013[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2012$d_2012
df<-as.integer(df)
out_dbn_2012<-data.frame(data_2012[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2011$d_2011
df<-as.integer(df)
out_dbn_2011<-data.frame(data_2011[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2010$d_2010
df<-as.integer(df)
out_dbn_2010<-data.frame(data_2010[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2009$d_2009
df<-as.integer(df)
out_dbn_2009<-data.frame(data_2009[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2008$d_2008
df<-as.integer(df)
out_dbn_2008<-data.frame(data_2008[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2007$d_2007
df<-as.integer(df)
out_dbn_2007<-data.frame(data_2007[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2006$d_2006
df<-as.integer(df)
out_dbn_2006<-data.frame(data_2006[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2005$d_2005
df<-as.integer(df)
out_dbn_2005<-data.frame(data_2005[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2004$d_2004
df<-as.integer(df)
out_dbn_2004<-data.frame(data_2004[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2003$d_2003
df<-as.integer(df)
out_dbn_2003<-data.frame(data_2003[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])

df<-data_2002$d_2002
df<-as.integer(df)
out_dbn_2002<-data.frame(data_2002[!((df >= 1 & df <= 32) | (df == 75) | ( df== 79) | (df == 84)),])
		# result: 2002-2005, with b out of range
		# create a count table of b out of range
		out_b_2002 <- data.frame(table(out_dbn_2002$d_2002))
		out_b_2003 <- data.frame(table(out_dbn_2003$d_2003))
		out_b_2004 <- data.frame(table(out_dbn_2004$d_2004))
		out_b_2005 <- data.frame(table(out_dbn_2005$d_2005))

		names(out_b_2002) <- c('b','freq_2002')
		names(out_b_2003) <- c('b','freq_2003')
		names(out_b_2004) <- c('b','freq_2004')
		names(out_b_2005) <- c('b','freq_2005')

		count_b<-Reduce(function(x, y) merge(x, y, by="b", all=TRUE), list(out_b_2002, out_b_2003, out_b_2004, out_b_2005 )) 
		write.csv(count_b,file = '/Users/elmerleezy/Desktop/count_b.csv')


# Detect student lta_flag not blank
	# not to be used
			# out_lta_flg_2002<- data_2002[!is.na(data_2002$lta_flg), ]
			# out_lta_flg_2003<- data_2003[!is.na(data_2003$lta_flg), ]
			# out_lta_flg_2004<- data_2004[!is.na(data_2004$lta_flg), ]
			# out_lta_flg_2005<- data_2005[!is.na(data_2005$lta_flg), ]
			# out_lta_flg_2006<- data_2006[!is.na(data_2006$lta_flg), ]
			# out_lta_flg_2007<- data_2007[!is.na(data_2007$lta_flg), ]
			# out_lta_flg_2008<- data_2008[!is.na(data_2008$lta_flg), ]
			# out_lta_flg_2009<- data_2009[!is.na(data_2009$lta_flg), ]
			# out_lta_flg_2010<- data_2010[!is.na(data_2010$lta_flg), ]
			# out_lta_flg_2011<- data_2011[!is.na(data_2011$lta_flg), ]

# Merge 2013 and 2014 Dataset
Merge_2014_2015 <- merge(data_2014, data_2015, by="student_id_scram", all = TRUE)
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



