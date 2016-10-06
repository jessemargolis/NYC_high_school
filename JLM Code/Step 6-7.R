
##################################################
# Startup
##################################################

# remove objects from current session
rm(list=ls())

# increase memory limit
# memory.limit(size = 10000)

# create varibles with filepaths
dir <- "C:/Users/Jesse/Dropbox/Docs/1. Research/Student Retention"

# include libraries
library(pryr)

##################################################
# Load Data
##################################################

# set working directory for importing CSV files
setwd(file.path(dir, "Data/Raw"))

# two sources of school names 
nyc_schools<-read.csv('NYC_schools.csv') 
nyc_schools2<-read.csv('NYC_schools2.csv') #we should used a version of this that includes year for sorting/clarity

# set working directory for importing R files
setwd(file.path(dir, "Data/R2"))

# load R files
load("data_2002.RData")
load("data_2003.RData")
load("data_2004.RData")
load("data_2005.RData")
load("data_2006.RData")
load("data_2007.RData")
load("data_2008.RData")
load("data_2009.RData")
load("data_2010.RData")
load("data_2011.RData")
load("data_2012.RData")
load("data_2013.RData")
load("data_2014.RData")
load("data_2015.RData")

###############################################################################
## District wide retention rate by grade
###############################################################################

#Aggregate and calculate rate
dist_wide_retention_2015 <- data.frame(aggregate(cbind(data_2014$retained_in_district, data_2014$include_in_dist_calc) ~  grade_level_2014, data_2014,sum))
dist_wide_retention_2014 <- data.frame(aggregate(cbind(data_2013$retained_in_district, data_2013$include_in_dist_calc) ~  grade_level_2013, data_2013,sum))
dist_wide_retention_2013 <- data.frame(aggregate(cbind(data_2012$retained_in_district, data_2012$include_in_dist_calc) ~  grade_level_2012, data_2012,sum))
dist_wide_retention_2012 <- data.frame(aggregate(cbind(data_2011$retained_in_district, data_2011$include_in_dist_calc) ~  grade_level_2011, data_2011,sum))
dist_wide_retention_2011 <- data.frame(aggregate(cbind(data_2010$retained_in_district, data_2010$include_in_dist_calc) ~  grade_level_2010, data_2010,sum))
dist_wide_retention_2010 <- data.frame(aggregate(cbind(data_2009$retained_in_district, data_2009$include_in_dist_calc) ~  grade_level_2009, data_2009,sum))
dist_wide_retention_2009 <- data.frame(aggregate(cbind(data_2008$retained_in_district, data_2008$include_in_dist_calc) ~  grade_level_2008, data_2008,sum))
dist_wide_retention_2008 <- data.frame(aggregate(cbind(data_2007$retained_in_district, data_2007$include_in_dist_calc) ~  grade_level_2007, data_2007,sum))
dist_wide_retention_2007 <- data.frame(aggregate(cbind(data_2006$retained_in_district, data_2006$include_in_dist_calc) ~  grade_level_2006, data_2006,sum))
dist_wide_retention_2006 <- data.frame(aggregate(cbind(data_2005$retained_in_district, data_2005$include_in_dist_calc) ~  grade_level_2005, data_2005,sum))
dist_wide_retention_2005 <- data.frame(aggregate(cbind(data_2004$retained_in_district, data_2004$include_in_dist_calc) ~  grade_level_2004, data_2004,sum))
dist_wide_retention_2004 <- data.frame(aggregate(cbind(data_2003$retained_in_district, data_2003$include_in_dist_calc) ~  grade_level_2003, data_2003,sum))
dist_wide_retention_2003 <- data.frame(aggregate(cbind(data_2002$retained_in_district, data_2002$include_in_dist_calc) ~  grade_level_2002, data_2002,sum))

dist_wide_retention_2015$rate <- (dist_wide_retention_2015$V1)/(dist_wide_retention_2015$V2)
dist_wide_retention_2014$rate <- (dist_wide_retention_2014$V1)/(dist_wide_retention_2014$V2)
dist_wide_retention_2013$rate <- (dist_wide_retention_2013$V1)/(dist_wide_retention_2013$V2)
dist_wide_retention_2012$rate <- (dist_wide_retention_2012$V1)/(dist_wide_retention_2012$V2)
dist_wide_retention_2011$rate <- (dist_wide_retention_2011$V1)/(dist_wide_retention_2011$V2)
dist_wide_retention_2010$rate <- (dist_wide_retention_2010$V1)/(dist_wide_retention_2010$V2)
dist_wide_retention_2009$rate <- (dist_wide_retention_2009$V1)/(dist_wide_retention_2009$V2)
dist_wide_retention_2008$rate <- (dist_wide_retention_2008$V1)/(dist_wide_retention_2008$V2)
dist_wide_retention_2007$rate <- (dist_wide_retention_2007$V1)/(dist_wide_retention_2007$V2)
dist_wide_retention_2006$rate <- (dist_wide_retention_2006$V1)/(dist_wide_retention_2006$V2)
dist_wide_retention_2005$rate <- (dist_wide_retention_2005$V1)/(dist_wide_retention_2005$V2)
dist_wide_retention_2004$rate <- (dist_wide_retention_2004$V1)/(dist_wide_retention_2004$V2)
dist_wide_retention_2003$rate <- (dist_wide_retention_2003$V1)/(dist_wide_retention_2003$V2)

# reorder
dist_wide_retention_2015 <- dist_wide_retention_2015[c(1,3,2,4)]
dist_wide_retention_2014 <- dist_wide_retention_2014[c(1,3,2,4)]
dist_wide_retention_2013 <- dist_wide_retention_2013[c(1,3,2,4)]
dist_wide_retention_2012 <- dist_wide_retention_2012[c(1,3,2,4)]
dist_wide_retention_2011 <- dist_wide_retention_2011[c(1,3,2,4)]
dist_wide_retention_2010 <- dist_wide_retention_2010[c(1,3,2,4)]
dist_wide_retention_2009 <- dist_wide_retention_2009[c(1,3,2,4)]
dist_wide_retention_2008 <- dist_wide_retention_2008[c(1,3,2,4)]
dist_wide_retention_2007 <- dist_wide_retention_2007[c(1,3,2,4)]
dist_wide_retention_2006 <- dist_wide_retention_2006[c(1,3,2,4)]
dist_wide_retention_2005 <- dist_wide_retention_2005[c(1,3,2,4)]
dist_wide_retention_2004 <- dist_wide_retention_2004[c(1,3,2,4)]
dist_wide_retention_2003 <- dist_wide_retention_2003[c(1,3,2,4)]

# rename
names(dist_wide_retention_2003) <- c('grade_level','include_2003','retained_2003','rate_2003')
names(dist_wide_retention_2004) <- c('grade_level','include_2004','retained_2004','rate_2004')
names(dist_wide_retention_2005) <- c('grade_level','include_2005','retained_2005','rate_2005')
names(dist_wide_retention_2006) <- c('grade_level','include_2006','retained_2006','rate_2006')
names(dist_wide_retention_2007) <- c('grade_level','include_2007','retained_2007','rate_2007')
names(dist_wide_retention_2008) <- c('grade_level','include_2008','retained_2008','rate_2008')
names(dist_wide_retention_2009) <- c('grade_level','include_2009','retained_2009','rate_2009')
names(dist_wide_retention_2010) <- c('grade_level','include_2010','retained_2010','rate_2010')
names(dist_wide_retention_2011) <- c('grade_level','include_2011','retained_2011','rate_2011')
names(dist_wide_retention_2012) <- c('grade_level','include_2012','retained_2012','rate_2012')
names(dist_wide_retention_2013) <- c('grade_level','include_2013','retained_2013','rate_2013')
names(dist_wide_retention_2014) <- c('grade_level','include_2014','retained_2014','rate_2014')
names(dist_wide_retention_2015) <- c('grade_level','include_2015','retained_2015','rate_2015')
  
dist_wide_retention_grade<-Reduce(function(x, y) merge(x, y, by=c('grade_level'), all=TRUE), list(dist_wide_retention_2003,dist_wide_retention_2004,dist_wide_retention_2005,dist_wide_retention_2006,dist_wide_retention_2007,
                                                                                             dist_wide_retention_2008,dist_wide_retention_2009,dist_wide_retention_2010,dist_wide_retention_2011,dist_wide_retention_2012,
                                                                                                dist_wide_retention_2013,dist_wide_retention_2014,dist_wide_retention_2015))
# reorder columns to group include, retained, and retention rate
dist_wide_retention_grade <- 
  dist_wide_retention_grade[c(1,
                              2,5, 8,11,14,17,20,23,26,29,32,35,38,
                              3,6, 9,12,15,18,21,24,27,30,33,36,39,
                              4,7,10,13,16,19,22,25,28,31,34,37,40)] 

###############################################################################
# Calculate overall retention rate for the district (including PK)
###############################################################################

# add column of ones to use for aggregating (so everything collapses to one row)
dist_wide_retention_grade$agg_col <- rep(1, nrow(dist_wide_retention_grade))

# aggregate all grades to the district level
dist_wide_retention <- aggregate(dist_wide_retention_grade[c(2:27)], 
                                 by = list(dist_wide_retention_grade$agg_col), FUN = sum, na.rm=TRUE)

# remove aggregation column
dist_wide_retention$Group.1 <- NULL

# calculate retention rate
dist_wide_retention$rate_2003 <- dist_wide_retention$retained_2003 / dist_wide_retention$include_2003
dist_wide_retention$rate_2004 <- dist_wide_retention$retained_2004 / dist_wide_retention$include_2004
dist_wide_retention$rate_2005 <- dist_wide_retention$retained_2005 / dist_wide_retention$include_2005
dist_wide_retention$rate_2006 <- dist_wide_retention$retained_2006 / dist_wide_retention$include_2006
dist_wide_retention$rate_2007 <- dist_wide_retention$retained_2007 / dist_wide_retention$include_2007
dist_wide_retention$rate_2008 <- dist_wide_retention$retained_2008 / dist_wide_retention$include_2008
dist_wide_retention$rate_2009 <- dist_wide_retention$retained_2009 / dist_wide_retention$include_2009
dist_wide_retention$rate_2010 <- dist_wide_retention$retained_2010 / dist_wide_retention$include_2010
dist_wide_retention$rate_2011 <- dist_wide_retention$retained_2011 / dist_wide_retention$include_2011
dist_wide_retention$rate_2012 <- dist_wide_retention$retained_2012 / dist_wide_retention$include_2012
dist_wide_retention$rate_2013 <- dist_wide_retention$retained_2013 / dist_wide_retention$include_2013
dist_wide_retention$rate_2014 <- dist_wide_retention$retained_2014 / dist_wide_retention$include_2014
dist_wide_retention$rate_2015 <- dist_wide_retention$retained_2015 / dist_wide_retention$include_2015

###############################################################################
# Calculate overall retention rate for the district (excluding PK)
###############################################################################

# create a version of data frame with no pk (grade_level != -1)
dist_wide_retention_grade_nopk <- dist_wide_retention_grade[dist_wide_retention_grade$grade_level != -1, ]

# aggregate all grades to the district level
dist_wide_retention_nopk <- aggregate(dist_wide_retention_grade_nopk[c(2:27)], 
                                      by = list(dist_wide_retention_grade_nopk$agg_col), FUN = sum, na.rm=TRUE)

# remove aggregation column
dist_wide_retention_nopkGroup.1 <- NULL

# calculate retention rate
dist_wide_retention_nopk$rate_2003 <- dist_wide_retention_nopk$retained_2003 / dist_wide_retention_nopk$include_2003
dist_wide_retention_nopk$rate_2004 <- dist_wide_retention_nopk$retained_2004 / dist_wide_retention_nopk$include_2004
dist_wide_retention_nopk$rate_2005 <- dist_wide_retention_nopk$retained_2005 / dist_wide_retention_nopk$include_2005
dist_wide_retention_nopk$rate_2006 <- dist_wide_retention_nopk$retained_2006 / dist_wide_retention_nopk$include_2006
dist_wide_retention_nopk$rate_2007 <- dist_wide_retention_nopk$retained_2007 / dist_wide_retention_nopk$include_2007
dist_wide_retention_nopk$rate_2008 <- dist_wide_retention_nopk$retained_2008 / dist_wide_retention_nopk$include_2008
dist_wide_retention_nopk$rate_2009 <- dist_wide_retention_nopk$retained_2009 / dist_wide_retention_nopk$include_2009
dist_wide_retention_nopk$rate_2010 <- dist_wide_retention_nopk$retained_2010 / dist_wide_retention_nopk$include_2010
dist_wide_retention_nopk$rate_2011 <- dist_wide_retention_nopk$retained_2011 / dist_wide_retention_nopk$include_2011
dist_wide_retention_nopk$rate_2012 <- dist_wide_retention_nopk$retained_2012 / dist_wide_retention_nopk$include_2012
dist_wide_retention_nopk$rate_2013 <- dist_wide_retention_nopk$retained_2013 / dist_wide_retention_nopk$include_2013
dist_wide_retention_nopk$rate_2014 <- dist_wide_retention_nopk$retained_2014 / dist_wide_retention_nopk$include_2014
dist_wide_retention_nopk$rate_2015 <- dist_wide_retention_nopk$retained_2015 / dist_wide_retention_nopk$include_2015

###############################################################################
## School wide retention rate by grade
###############################################################################

# Aggregate and calculate rate
sch_wide_retention_2015 <- data.frame(aggregate(cbind(data_2014$retained_in_school, data_2014$include_in_school_calc) ~ bn_2014 + grade_level_2014, data_2014,sum))
sch_wide_retention_2014 <- data.frame(aggregate(cbind(data_2013$retained_in_school, data_2013$include_in_school_calc) ~ bn_2013 + grade_level_2013, data_2013,sum))
sch_wide_retention_2013 <- data.frame(aggregate(cbind(data_2012$retained_in_school, data_2012$include_in_school_calc) ~ bn_2012 + grade_level_2012, data_2012,sum))
sch_wide_retention_2012 <- data.frame(aggregate(cbind(data_2011$retained_in_school, data_2011$include_in_school_calc) ~ bn_2011 + grade_level_2011, data_2011,sum))
sch_wide_retention_2011 <- data.frame(aggregate(cbind(data_2010$retained_in_school, data_2010$include_in_school_calc) ~ bn_2010 + grade_level_2010, data_2010,sum))
sch_wide_retention_2010 <- data.frame(aggregate(cbind(data_2009$retained_in_school, data_2009$include_in_school_calc) ~ bn_2009 + grade_level_2009, data_2009,sum))
sch_wide_retention_2009 <- data.frame(aggregate(cbind(data_2008$retained_in_school, data_2008$include_in_school_calc) ~ bn_2008 + grade_level_2008, data_2008,sum))
sch_wide_retention_2008 <- data.frame(aggregate(cbind(data_2007$retained_in_school, data_2007$include_in_school_calc) ~ bn_2007 + grade_level_2007, data_2007,sum))
sch_wide_retention_2007 <- data.frame(aggregate(cbind(data_2006$retained_in_school, data_2006$include_in_school_calc) ~ bn_2006 + grade_level_2006, data_2006,sum))
sch_wide_retention_2006 <- data.frame(aggregate(cbind(data_2005$retained_in_school, data_2005$include_in_school_calc) ~ bn_2005 + grade_level_2005, data_2005,sum))
sch_wide_retention_2005 <- data.frame(aggregate(cbind(data_2004$retained_in_school, data_2004$include_in_school_calc) ~ bn_2004 + grade_level_2004, data_2004,sum))
sch_wide_retention_2004 <- data.frame(aggregate(cbind(data_2003$retained_in_school, data_2003$include_in_school_calc) ~ bn_2003 + grade_level_2003, data_2003,sum))
sch_wide_retention_2003 <- data.frame(aggregate(cbind(data_2002$retained_in_school, data_2002$include_in_school_calc) ~ bn_2002 + grade_level_2002, data_2002,sum))

sch_wide_retention_2015$rate <- (sch_wide_retention_2015$V1)/(sch_wide_retention_2015$V2)
sch_wide_retention_2014$rate <- (sch_wide_retention_2014$V1)/(sch_wide_retention_2014$V2)
sch_wide_retention_2013$rate <- (sch_wide_retention_2013$V1)/(sch_wide_retention_2013$V2)
sch_wide_retention_2012$rate <- (sch_wide_retention_2012$V1)/(sch_wide_retention_2012$V2)
sch_wide_retention_2011$rate <- (sch_wide_retention_2011$V1)/(sch_wide_retention_2011$V2)
sch_wide_retention_2010$rate <- (sch_wide_retention_2010$V1)/(sch_wide_retention_2010$V2)
sch_wide_retention_2009$rate <- (sch_wide_retention_2009$V1)/(sch_wide_retention_2009$V2)
sch_wide_retention_2008$rate <- (sch_wide_retention_2008$V1)/(sch_wide_retention_2008$V2)
sch_wide_retention_2007$rate <- (sch_wide_retention_2007$V1)/(sch_wide_retention_2007$V2)
sch_wide_retention_2006$rate <- (sch_wide_retention_2006$V1)/(sch_wide_retention_2006$V2)
sch_wide_retention_2005$rate <- (sch_wide_retention_2005$V1)/(sch_wide_retention_2005$V2)
sch_wide_retention_2004$rate <- (sch_wide_retention_2004$V1)/(sch_wide_retention_2004$V2)
sch_wide_retention_2003$rate <- (sch_wide_retention_2003$V1)/(sch_wide_retention_2003$V2)

# Reorder and re-name
sch_wide_retention_2015 <- sch_wide_retention_2015[c(1,2,4,3,5)]
sch_wide_retention_2014 <- sch_wide_retention_2014[c(1,2,4,3,5)]
sch_wide_retention_2013 <- sch_wide_retention_2013[c(1,2,4,3,5)]
sch_wide_retention_2012 <- sch_wide_retention_2012[c(1,2,4,3,5)]
sch_wide_retention_2011 <- sch_wide_retention_2011[c(1,2,4,3,5)]
sch_wide_retention_2010 <- sch_wide_retention_2010[c(1,2,4,3,5)]
sch_wide_retention_2009 <- sch_wide_retention_2009[c(1,2,4,3,5)]
sch_wide_retention_2008 <- sch_wide_retention_2008[c(1,2,4,3,5)]
sch_wide_retention_2007 <- sch_wide_retention_2007[c(1,2,4,3,5)]
sch_wide_retention_2006 <- sch_wide_retention_2006[c(1,2,4,3,5)]
sch_wide_retention_2005 <- sch_wide_retention_2005[c(1,2,4,3,5)]
sch_wide_retention_2004 <- sch_wide_retention_2004[c(1,2,4,3,5)]
sch_wide_retention_2003 <- sch_wide_retention_2003[c(1,2,4,3,5)]

names(sch_wide_retention_2003) <- c('bn','grade_level','include_2003','retained_2003','rate_2003')
names(sch_wide_retention_2004) <- c('bn','grade_level','include_2004','retained_2004','rate_2004')
names(sch_wide_retention_2005) <- c('bn','grade_level','include_2005','retained_2005','rate_2005')
names(sch_wide_retention_2006) <- c('bn','grade_level','include_2006','retained_2006','rate_2006')
names(sch_wide_retention_2007) <- c('bn','grade_level','include_2007','retained_2007','rate_2007')
names(sch_wide_retention_2008) <- c('bn','grade_level','include_2008','retained_2008','rate_2008')
names(sch_wide_retention_2009) <- c('bn','grade_level','include_2009','retained_2009','rate_2009')
names(sch_wide_retention_2010) <- c('bn','grade_level','include_2010','retained_2010','rate_2010')
names(sch_wide_retention_2011) <- c('bn','grade_level','include_2011','retained_2011','rate_2011')
names(sch_wide_retention_2012) <- c('bn','grade_level','include_2012','retained_2012','rate_2012')
names(sch_wide_retention_2013) <- c('bn','grade_level','include_2013','retained_2013','rate_2013')
names(sch_wide_retention_2014) <- c('bn','grade_level','include_2014','retained_2014','rate_2014')
names(sch_wide_retention_2015) <- c('bn','grade_level','include_2015','retained_2015','rate_2015')

# Create a summary table
sch_wide_retention_grade <- Reduce(function(x, y) merge(x, y, by=c('bn','grade_level'), all=TRUE), list(sch_wide_retention_2003,sch_wide_retention_2004,sch_wide_retention_2005,sch_wide_retention_2006,sch_wide_retention_2007,
                                                                                   sch_wide_retention_2008,sch_wide_retention_2009,sch_wide_retention_2010,sch_wide_retention_2011,sch_wide_retention_2012,
                                                                                   sch_wide_retention_2013,sch_wide_retention_2014,sch_wide_retention_2015))

# Match school names (first use the newer file, then the older one)
sch_wide_retention_grade$school_name1 <- as.character(nyc_schools[match(sch_wide_retention_grade$bn, nyc_schools$bn),'school_name'])
sch_wide_retention_grade$school_name2 <- as.character(nyc_schools2[match(sch_wide_retention_grade$bn, nyc_schools2$bn), 'school_name'])
sch_wide_retention_grade$school_name <- ifelse(!is.na(sch_wide_retention_grade$school_name1), 
                                              sch_wide_retention_grade$school_name1,
                                              sch_wide_retention_grade$school_name2)

# remove intermediate school name variables
sch_wide_retention_grade$school_name1 <- NULL
sch_wide_retention_grade$school_name2 <- NULL

# move the names to the left
sch_wide_retention_grade <- sch_wide_retention_grade[c(1,42,2:41)]

# Reorder columns to group include, retained, and retention rate
sch_wide_retention_grade <- 
  sch_wide_retention_grade[c(1,2,3,
                           4,7,10,13,16,19,22,25,28,31,34,37,40,
                           5,8,11,14,17,20,23,26,29,32,35,38,41,
                           6,9,12,15,18,21,24,27,30,33,36,39,42)] 

###############################################################################
# Calculate overall retention rate by school (including PK)
###############################################################################

# aggregate all grades to the school level
sch_wide_retention <- aggregate(sch_wide_retention_grade[c(4:29)], by = list(bn = sch_wide_retention_grade$bn), FUN = sum, na.rm=TRUE)

# calculate retention rate
sch_wide_retention$rate_2003 <- sch_wide_retention$retained_2003 / sch_wide_retention$include_2003
sch_wide_retention$rate_2004 <- sch_wide_retention$retained_2004 / sch_wide_retention$include_2004
sch_wide_retention$rate_2005 <- sch_wide_retention$retained_2005 / sch_wide_retention$include_2005
sch_wide_retention$rate_2006 <- sch_wide_retention$retained_2006 / sch_wide_retention$include_2006
sch_wide_retention$rate_2007 <- sch_wide_retention$retained_2007 / sch_wide_retention$include_2007
sch_wide_retention$rate_2008 <- sch_wide_retention$retained_2008 / sch_wide_retention$include_2008
sch_wide_retention$rate_2009 <- sch_wide_retention$retained_2009 / sch_wide_retention$include_2009
sch_wide_retention$rate_2010 <- sch_wide_retention$retained_2010 / sch_wide_retention$include_2010
sch_wide_retention$rate_2011 <- sch_wide_retention$retained_2011 / sch_wide_retention$include_2011
sch_wide_retention$rate_2012 <- sch_wide_retention$retained_2012 / sch_wide_retention$include_2012
sch_wide_retention$rate_2013 <- sch_wide_retention$retained_2013 / sch_wide_retention$include_2013
sch_wide_retention$rate_2014 <- sch_wide_retention$retained_2014 / sch_wide_retention$include_2014
sch_wide_retention$rate_2015 <- sch_wide_retention$retained_2015 / sch_wide_retention$include_2015

# Match school names (first use the newer file, then the older one)
sch_wide_retention$school_name1 <- as.character(nyc_schools[match(sch_wide_retention$bn, nyc_schools$bn),'school_name'])
sch_wide_retention$school_name2 <- as.character(nyc_schools2[match(sch_wide_retention$bn, nyc_schools2$bn), 'school_name'])
sch_wide_retention$school_name <- ifelse(!is.na(sch_wide_retention$school_name1), 
                                               sch_wide_retention$school_name1,
                                               sch_wide_retention$school_name2)

# remove intermediate school name variables
sch_wide_retention$school_name1 <- NULL
sch_wide_retention$school_name2 <- NULL

# move the names to the left
sch_wide_retention <- sch_wide_retention[c(1,41,2:40)]

###############################################################################
# Calculate overall retention rate by school (excluding PK)
###############################################################################

# create a version of data frame with no pk (grade_level != -1)
sch_wide_retention_grade_nopk <- sch_wide_retention_grade[sch_wide_retention_grade$grade_level != -1, ]

# aggregate all grades to the school level
sch_wide_retention_nopk <- aggregate(sch_wide_retention_grade_nopk[c(4:29)], 
                                     by = list(bn = sch_wide_retention_grade_nopk$bn), FUN = sum, na.rm=TRUE)

# calculate retention rate
sch_wide_retention_nopk$rate_2003 <- sch_wide_retention_nopk$retained_2003 / sch_wide_retention_nopk$include_2003
sch_wide_retention_nopk$rate_2004 <- sch_wide_retention_nopk$retained_2004 / sch_wide_retention_nopk$include_2004
sch_wide_retention_nopk$rate_2005 <- sch_wide_retention_nopk$retained_2005 / sch_wide_retention_nopk$include_2005
sch_wide_retention_nopk$rate_2006 <- sch_wide_retention_nopk$retained_2006 / sch_wide_retention_nopk$include_2006
sch_wide_retention_nopk$rate_2007 <- sch_wide_retention_nopk$retained_2007 / sch_wide_retention_nopk$include_2007
sch_wide_retention_nopk$rate_2008 <- sch_wide_retention_nopk$retained_2008 / sch_wide_retention_nopk$include_2008
sch_wide_retention_nopk$rate_2009 <- sch_wide_retention_nopk$retained_2009 / sch_wide_retention_nopk$include_2009
sch_wide_retention_nopk$rate_2010 <- sch_wide_retention_nopk$retained_2010 / sch_wide_retention_nopk$include_2010
sch_wide_retention_nopk$rate_2011 <- sch_wide_retention_nopk$retained_2011 / sch_wide_retention_nopk$include_2011
sch_wide_retention_nopk$rate_2012 <- sch_wide_retention_nopk$retained_2012 / sch_wide_retention_nopk$include_2012
sch_wide_retention_nopk$rate_2013 <- sch_wide_retention_nopk$retained_2013 / sch_wide_retention_nopk$include_2013
sch_wide_retention_nopk$rate_2014 <- sch_wide_retention_nopk$retained_2014 / sch_wide_retention_nopk$include_2014
sch_wide_retention_nopk$rate_2015 <- sch_wide_retention_nopk$retained_2015 / sch_wide_retention_nopk$include_2015

# Match school names (first use the newer file, then the older one)
sch_wide_retention_nopk$school_name1 <- as.character(nyc_schools[match(sch_wide_retention_nopk$bn, nyc_schools$bn),'school_name'])
sch_wide_retention_nopk$school_name2 <- as.character(nyc_schools2[match(sch_wide_retention_nopk$bn, nyc_schools2$bn), 'school_name'])
sch_wide_retention_nopk$school_name <- ifelse(!is.na(sch_wide_retention_nopk$school_name1), 
                                         sch_wide_retention_nopk$school_name1,
                                         sch_wide_retention_nopk$school_name2)

# remove intermediate school name variables
sch_wide_retention_nopk$school_name1 <- NULL
sch_wide_retention_nopk$school_name2 <- NULL

# move the names to the left
sch_wide_retention_nopk <- sch_wide_retention_nopk[c(1,41,2:40)]


#######################################
##  EXPORT DATA ##
#######################################

# Set file path for exporting CSV files
setwd(file.path(dir, "Data/Output"))

# Export CSV files
write.csv(dist_wide_retention, file = 'dist_wide_retention.csv')
write.csv(dist_wide_retention_nopk, file = 'dist_wide_retention_nopk.csv')
write.csv(dist_wide_retention_grade, file = 'dist_wide_retention_grade.csv')
write.csv(sch_wide_retention, file = 'sch_wide_retention.csv')
write.csv(sch_wide_retention_nopk, file = 'sch_wide_retention_nopk.csv')
write.csv(sch_wide_retention_grade, file = 'sch_wide_retention_grade.csv', row.names=FALSE)









