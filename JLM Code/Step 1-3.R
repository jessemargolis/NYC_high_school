##################################################
# Startup
##################################################

# remove objects from current session
rm(list=ls())

# create varibles with filepaths
dir <- "C:/Users/Jesse/Dropbox/Docs/1. Research/Student Retention"

# set working directory for import
setwd(file.path(dir, "Data/Raw"))

# Elmer - setwd("/Users/elmerleezy/Dropbox/NYC High School Re-enrollment/Data/Original/2016 08 15 - Audited Register with LTA")

# set up useful functions 
`%S%` <- function(x, y) {
  paste0(x, y)
}

`%notin%` <- Negate(`%in%`)

# include libraries

package_list <- c("readr", "tidyr", "dplyr","pryr","plyr", "knitr", "stringr", "ggplot2","tidyverse")
new_packages <- package_list[package_list %notin% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(readr)
library(tidyr)
library(dplyr)
library(pryr)
library(plyr)
library(knitr)
library(stringr)
library(ggplot2)
library(tidyverse)


#######################################
##  STEP 1: READ DATA & ORGANIZE ##
#######################################

### Import Datasets
data_2002<-read_csv('2002-03_Biog1031_Scrambled with LTA.csv')
data_2003<-read_csv('2003-04_Biog1031_Scrambled with LTA.csv')
data_2004<-read_csv('2004-05_Biog1031_Scrambled with LTA.csv')
data_2005<-read_csv('2005-06_Biog1031_Scrambled with LTA.csv')
data_2006<-read_csv('2006-07_Biog1031_Scrambled with LTA.csv')
data_2007<-read_csv('2007-08_Biog1031_Scrambled with LTA.csv')
data_2008<-read_csv('2008-09_Biog1031_Scrambled with LTA.csv')
data_2009<-read_csv('2009-10_Biog1031_Scrambled with LTA.csv')
data_2010<-read_csv('2010-11_Biog1031_Scrambled with LTA.csv')
data_2011<-read_csv('2011-12_Biog1031_Scrambled with LTA.csv')
data_2012<-read_csv('2012-13_Biog1031_Scrambled with LTA.csv')
data_2013<-read_csv('2013-14_Biog1031_Scrambled with LTA.csv')
data_2014<-read_csv('2014-15_Biog1031_Scrambled with LTA.csv')
data_2015<-read_csv('2015-16_Biog1031_Scrambled with LTA.csv')

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

# Create a table with counts by ethnicity

eth02 <- data.frame(table(data_2002$ethnicity)) 
eth03 <- data.frame(table(data_2003$ethnicity)) 
eth04 <- data.frame(table(data_2004$ethnicity)) 
eth05 <- data.frame(table(data_2005$ethnicity)) 
eth06 <- data.frame(table(data_2006$ethnicity)) 
eth07 <- data.frame(table(data_2007$ethnicity)) 
eth08 <- data.frame(table(data_2008$ethnicity)) 
eth09 <- data.frame(table(data_2009$ethnicity)) 
eth10 <- data.frame(table(data_2010$ethnicity)) 
eth11 <- data.frame(table(data_2011$ethnicity)) 
eth12 <- data.frame(table(data_2012$ethnicity)) 
eth13 <- data.frame(table(data_2013$ethnicity)) 
eth14 <- data.frame(table(data_2014$ethnicity)) 
eth15 <- data.frame(table(data_2015$ethnicity)) 

names(eth02) <- c('ethnicity','freq_2002')
names(eth03) <- c('ethnicity','freq_2003')
names(eth04) <- c('ethnicity','freq_2004')
names(eth05) <- c('ethnicity','freq_2005')
names(eth06) <- c('ethnicity','freq_2006')
names(eth07) <- c('ethnicity','freq_2007')
names(eth08) <- c('ethnicity','freq_2008')
names(eth09) <- c('ethnicity','freq_2009')
names(eth10) <- c('ethnicity','freq_2010')
names(eth11) <- c('ethnicity','freq_2011')
names(eth12) <- c('ethnicity','freq_2012')
names(eth13) <- c('ethnicity','freq_2013')
names(eth14) <- c('ethnicity','freq_2014')
names(eth15) <- c('ethnicity','freq_2015')

count_eth<-Reduce(function(x, y) merge(x, y, by="ethnicity", all=TRUE), list(eth02,eth03,eth04,eth05,eth06,eth07,eth08,eth09,eth10,eth11,eth12,eth13,eth14,eth15))
rm(eth02, eth03, eth04, eth05, eth06, eth07, eth08, eth09, eth10, eth11, eth12, eth13, eth14, eth15)

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

# Rename Grade level
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

# Summarize grade distribution
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
		grade_level_2002_t,
		grade_level_2003_t,
		grade_level_2004_t,
		grade_level_2005_t,
		grade_level_2006_t,
		grade_level_2007_t,
		grade_level_2008_t,
		grade_level_2009_t,
		grade_level_2010_t,
		grade_level_2011_t,
		grade_level_2012_t,
		grade_level_2013_t,
		grade_level_2014_t,
		grade_level_2015_t
		))

rm(grade_level_2002_t, grade_level_2003_t, grade_level_2004_t, grade_level_2005_t, grade_level_2006_t, grade_level_2007_t, grade_level_2008_t, grade_level_2009_t, grade_level_2010_t, grade_level_2011_t, grade_level_2012_t, grade_level_2013_t, grade_level_2014_t, grade_level_2015_t)

# Delete AD and IN
data_2002 <- data_2002[!(data_2002$grade_level_2002 == "IN"), ]
data_2003 <- data_2003[!(data_2003$grade_level_2003 == "IN"), ]
data_2010 <- data_2010[!(data_2010$grade_level_2010 == "IN"), ]
data_2011 <- dplyr::filter(data_2011,!grade_level_2011 == "AD")
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

# convert to numeric
data_2002$grade_level_2002 <- as.numeric(data_2002$grade_level_2002)
data_2003$grade_level_2003 <- as.numeric(data_2003$grade_level_2003)
data_2004$grade_level_2004 <- as.numeric(data_2004$grade_level_2004)
data_2005$grade_level_2005 <- as.numeric(data_2005$grade_level_2005)
data_2006$grade_level_2006 <- as.numeric(data_2006$grade_level_2006)
data_2007$grade_level_2007 <- as.numeric(data_2007$grade_level_2007)
data_2008$grade_level_2008 <- as.numeric(data_2008$grade_level_2008)
data_2009$grade_level_2009 <- as.numeric(data_2009$grade_level_2009)
data_2010$grade_level_2010 <- as.numeric(data_2010$grade_level_2010)
data_2011$grade_level_2011 <- as.numeric(data_2011$grade_level_2011)
data_2012$grade_level_2012 <- as.numeric(data_2012$grade_level_2012)
data_2013$grade_level_2013 <- as.numeric(data_2013$grade_level_2013)
data_2014$grade_level_2014 <- as.numeric(data_2014$grade_level_2014)
data_2015$grade_level_2015 <- as.numeric(data_2015$grade_level_2015)


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
		
# result: 2002-2005, with d out of range
# JLM: ALSO 6 ROWS IN 2011 WITH ALL NA.  WHAT ARE THEY?
# - ELmer: This happens when removing the gradelevel AD. I used another way and now this problem is fixed.

# create a count table of d out of range
out_d_2002 <- data.frame(table(out_dbn_2002$d_2002))
out_d_2003 <- data.frame(table(out_dbn_2003$d_2003))
out_d_2004 <- data.frame(table(out_dbn_2004$d_2004))
out_d_2005 <- data.frame(table(out_dbn_2005$d_2005))

names(out_d_2002) <- c('d','freq_2002')
names(out_d_2003) <- c('d','freq_2003')
names(out_d_2004) <- c('d','freq_2004')
names(out_d_2005) <- c('d','freq_2005')

count_d<-Reduce(function(x, y) merge(x, y, by="d", all=TRUE), list(out_d_2002, out_d_2003, out_d_2004, out_d_2005 )) 
rm(out_dbn_2002, out_dbn_2003, out_dbn_2004, out_dbn_2005, out_dbn_2006, out_dbn_2007, out_dbn_2008, out_dbn_2009, out_dbn_2010, out_dbn_2011, out_dbn_2012, out_dbn_2013, out_dbn_2014, out_dbn_2015 )

#######################################
##  MERGE ALL DATASETS ##
#######################################

# Save original file
raw_data_2002 <- data_2002
raw_data_2003 <- data_2003
raw_data_2004 <- data_2004
raw_data_2005 <- data_2005
raw_data_2006 <- data_2006
raw_data_2007 <- data_2007
raw_data_2008 <- data_2008
raw_data_2009 <- data_2009
raw_data_2010 <- data_2010
raw_data_2011 <- data_2011
raw_data_2012 <- data_2012
raw_data_2013 <- data_2013
raw_data_2014 <- data_2014
raw_data_2015 <- data_2015

data_2002 <- dplyr::left_join(data_2002, data_2003[,c("student_id_scram", "grade_level_2003","d_2003", "bn_2003")], by="student_id_scram")
data_2003 <- dplyr::left_join(data_2003, data_2004[,c("student_id_scram", "grade_level_2004","d_2004", "bn_2004")], by="student_id_scram")
data_2004 <- dplyr::left_join(data_2004, data_2005[,c("student_id_scram", "grade_level_2005","d_2005", "bn_2005")], by="student_id_scram")
data_2005 <- dplyr::left_join(data_2005, data_2006[,c("student_id_scram", "grade_level_2006","d_2006", "bn_2006")], by="student_id_scram")
data_2006 <- dplyr::left_join(data_2006, data_2007[,c("student_id_scram", "grade_level_2007","d_2007", "bn_2007")], by="student_id_scram")
data_2007 <- dplyr::left_join(data_2007, data_2008[,c("student_id_scram", "grade_level_2008","d_2008", "bn_2008")], by="student_id_scram")
data_2008 <- dplyr::left_join(data_2008, data_2009[,c("student_id_scram", "grade_level_2009","d_2009", "bn_2009")], by="student_id_scram")
data_2009 <- dplyr::left_join(data_2009, data_2010[,c("student_id_scram", "grade_level_2010","d_2010", "bn_2010")], by="student_id_scram")
data_2010 <- dplyr::left_join(data_2010, data_2011[,c("student_id_scram", "grade_level_2011","d_2011", "bn_2011")], by="student_id_scram")
data_2011 <- dplyr::left_join(data_2011, data_2012[,c("student_id_scram", "grade_level_2012","d_2012", "bn_2012")], by="student_id_scram")
data_2012 <- dplyr::left_join(data_2012, data_2013[,c("student_id_scram", "grade_level_2013","d_2013", "bn_2013")], by="student_id_scram")
data_2013 <- dplyr::left_join(data_2013, data_2014[,c("student_id_scram", "grade_level_2014","d_2014", "bn_2014")], by="student_id_scram")
data_2014 <- dplyr::left_join(data_2014, data_2015[,c("student_id_scram", "grade_level_2015","d_2015", "bn_2015")], by="student_id_scram")

#######################################
##  EXPORT DATA ##
#######################################

# Set file path for exporting CSV files
setwd("/Users/elmerleezy/Google Drive/Job/RA/Data Copy/Raw/R")

# Export data
write.csv(count_missing, file = 'count_missing.csv')
write.csv(count_eth, file = 'count_eth.csv')
write.csv(count_grade,file = 'count_grade.csv')
write.csv(count_d,file = 'count_d.csv')

# Set file path for exporting R files
setwd(file.path(dir, "Data/R"))

# save data files with merged results from subsequent year
save(data_2002, file = "data_2002.RData")
save(data_2003, file = "data_2003.RData")
save(data_2004, file = "data_2004.RData")
save(data_2005, file = "data_2005.RData")
save(data_2006, file = "data_2006.RData")
save(data_2007, file = "data_2007.RData")
save(data_2008, file = "data_2008.RData")
save(data_2009, file = "data_2009.RData")
save(data_2010, file = "data_2010.RData")
save(data_2011, file = "data_2011.RData")
save(data_2012, file = "data_2012.RData")
save(data_2013, file = "data_2013.RData")
save(data_2014, file = "data_2014.RData")
save(data_2015, file = "data_2015.RData")

save(raw_data_2002, file = "raw_data_2002.RData")
save(raw_data_2003, file = "raw_data_2003.RData")
save(raw_data_2004, file = "raw_data_2004.RData")
save(raw_data_2005, file = "raw_data_2005.RData")
save(raw_data_2006, file = "raw_data_2006.RData")
save(raw_data_2007, file = "raw_data_2007.RData")
save(raw_data_2008, file = "raw_data_2008.RData")
save(raw_data_2009, file = "raw_data_2009.RData")
save(raw_data_2010, file = "raw_data_2010.RData")
save(raw_data_2011, file = "raw_data_2011.RData")
save(raw_data_2012, file = "raw_data_2012.RData")
save(raw_data_2013, file = "raw_data_2013.RData")
save(raw_data_2014, file = "raw_data_2014.RData")
save(raw_data_2015, file = "raw_data_2015.RData")

#######################################
## EXPLORATION OF LOOPS ##
#######################################

# create a list with the years
years <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 
           2009, 2010, 2011, 2012, 2013, 2014, 2015)

# loop through list to save data (THIS WORKS)
for (i in 1:length(years)) {
  save(list = (paste0("data_", years[i])), file = paste0("data_", years[i], ".RData"))
}

# try the same with a simpler loop (WHY DOES THIS NOT WORK?)
for (year in c(2002, 2003)) {
  save(paste0("data_", year), file = paste0("data_", year, ".RData"))
}

# Try something similar with lapply (WHY DOES THIS NOT WORK?)
lapply(years, function(x) {
  save(paste0("data_", years), file = paste0("data_", years, ".RData"))
})




