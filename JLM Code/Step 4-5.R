
##################################################
# Startup
##################################################

# remove objects from current session
rm(list=ls())

# create varibles with filepaths
dir <- "C:/Users/Jesse/Dropbox/Docs/1. Research/Student Retention"

# set working directory for import
setwd(file.path(dir, "Data/R")) 
setwd("/Users/elmerleezy/Google Drive/Job/RA/Data Copy/Raw/R")

# include libraries
library(pryr)
library(plyr)
library(purrr)
library(dplyr)

##################################################
# Load Data
##################################################

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

#---------------------------------------------------------------------
# This part below needs more consideration, since it is supposed to 
# process unmerged datasets
#---------------------------------------------------------------------

######################################################################
## Count frequency of each grade level to each school and
## Identify eligible school/grade combinations for re-enrollment
######################################################################

### Count frequency of each grade level to each school

datasets <- list(data_2002, data_2003, data_2004, data_2005, data_2006, data_2007, data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015)
  #give value of the year
y <- 2002 
for(df in datasets) {
  grade_level<- as.name("grade_level_" %S% y) #indicate variable name
  grade_count <- df %>%
    group_by_(.dots = "bn_" %S% y) %>% 
    summarise(
      grade0=sum(bquote(.(grade_level))==0),
      grade1=sum(bquote(.(grade_level))==1),
      grade2=sum(bquote(.(grade_level))==2),
      grade3=sum(bquote(.(grade_level))==3),
      grade4=sum(bquote(.(grade_level))==4),
      grade5=sum(bquote(.(grade_level))==5),
      grade6=sum(bquote(.(grade_level))==6),
      grade7=sum(bquote(.(grade_level))==7),
      grade8=sum(bquote(.(grade_level))==8),
      grade9=sum(bquote(.(grade_level))==9),
      grade10=sum(bquote(.(grade_level))==10),
      grade11=sum(bquote(.(grade_level))==11),
      grade12=sum(bquote(.(grade_level))==12))
  # convert first column to row names
  grade_count <- data.frame(grade_count)
  grade_count <- grade_count[rowSums(is.na(grade_count)) == 0,] #delete missing values
  grade_count <- data.frame(grade_count[,-1], row.names=grade_count[,1])
  assign("grade_count_"%S% y,grade_count)
  y <- y+1
}

### Count grade range

gradecount.list <- list(grade_count_2002, grade_count_2003, grade_count_2004, grade_count_2005, grade_count_2006, grade_count_2007, grade_count_2008, grade_count_2009, grade_count_2010, grade_count_2011, grade_count_2012, grade_count_2013, grade_count_2014, grade_count_2015 )
rm(grade_count_2002, grade_count_2003, grade_count_2004, grade_count_2005, grade_count_2006, grade_count_2007, grade_count_2008, grade_count_2009, grade_count_2010, grade_count_2011, grade_count_2012, grade_count_2013, grade_count_2014, grade_count_2015 )

y <- 2002
for (df in gradecount.list) {
  # count grade freq >10
  grade_range <- data.frame(cbind(apply(df,1 ,function(x) which(x>10))))
  # convert first column names to row names
  names(grade_range) <- c('grade_range_'%S% y)
  # minus 2 to get the right grades
  var1 <- "grade_range_" %S% y
  grade_range[[var1]] <- lapply(grade_range[,1],function(x) x-2)
  # minus 1 to determine continuing grade in the previous year
  var2 <- "continuous_" %S% y
  grade_range[[var2]] <- lapply(grade_range[,1],function(x) x-1) 
  # insert first column as row names
  grade_range[['bn']] <- rownames(grade_range)
  # re-order grade_range columns 
  grade_range <- grade_range[c(3,1,2)]
  assign("grade_range_"%S% y,grade_range)
  y <- y+1
}


######################################################################
## Flag students as eligible or not for inclusion in calculation
######################################################################

### Create Dummy for include_in_dist_calc & include_in_school_calc

# include_in_dist_calc 
data_2002$include_in_dist_calc <- ifelse(data_2002$grade_level_2002 < 12,1,0)  
data_2003$include_in_dist_calc <- ifelse(data_2003$grade_level_2003 < 12,1,0)  
data_2004$include_in_dist_calc <- ifelse(data_2004$grade_level_2004 < 12,1,0)  
data_2005$include_in_dist_calc <- ifelse(data_2005$grade_level_2005 < 12,1,0)  
data_2006$include_in_dist_calc <- ifelse(data_2006$grade_level_2006 < 12,1,0)  
data_2007$include_in_dist_calc <- ifelse(data_2007$grade_level_2007 < 12,1,0)
data_2008$include_in_dist_calc <- ifelse(data_2008$grade_level_2008 < 12,1,0)
data_2009$include_in_dist_calc <- ifelse(data_2009$grade_level_2009 < 12,1,0)
data_2010$include_in_dist_calc <- ifelse(data_2010$grade_level_2010 < 12,1,0)
data_2011$include_in_dist_calc <- ifelse(data_2011$grade_level_2011 < 12,1,0)
data_2012$include_in_dist_calc <- ifelse(data_2012$grade_level_2012 < 12,1,0)
data_2013$include_in_dist_calc <- ifelse(data_2013$grade_level_2013 < 12,1,0)
data_2014$include_in_dist_calc <- ifelse(data_2014$grade_level_2014 < 12,1,0)

# include_in_school_calc
# first indicate continuing grades
data_2014$continuous_2014 <- grade_range_2015[match(data_2014$bn_2014, grade_range_2015$bn),'continuous_2014']
data_2013$continuous_2013 <- grade_range_2014[match(data_2013$bn_2013, grade_range_2014$bn),'continuous_2013']
data_2012$continuous_2012 <- grade_range_2013[match(data_2012$bn_2012, grade_range_2013$bn),'continuous_2012']
data_2011$continuous_2011 <- grade_range_2012[match(data_2011$bn_2011, grade_range_2012$bn),'continuous_2011']
data_2010$continuous_2010 <- grade_range_2011[match(data_2010$bn_2010, grade_range_2011$bn),'continuous_2010']
data_2009$continuous_2009 <- grade_range_2010[match(data_2009$bn_2009, grade_range_2010$bn),'continuous_2009']
data_2008$continuous_2008 <- grade_range_2009[match(data_2008$bn_2008, grade_range_2009$bn),'continuous_2008']
data_2007$continuous_2007 <- grade_range_2008[match(data_2007$bn_2007, grade_range_2008$bn),'continuous_2007']
data_2006$continuous_2006 <- grade_range_2007[match(data_2006$bn_2006, grade_range_2007$bn),'continuous_2006']
data_2005$continuous_2005 <- grade_range_2006[match(data_2005$bn_2005, grade_range_2006$bn),'continuous_2005']
data_2004$continuous_2004 <- grade_range_2005[match(data_2004$bn_2004, grade_range_2005$bn),'continuous_2004']
data_2003$continuous_2003 <- grade_range_2004[match(data_2003$bn_2003, grade_range_2004$bn),'continuous_2003']
data_2002$continuous_2002 <- grade_range_2003[match(data_2002$bn_2002, grade_range_2003$bn),'continuous_2002']

# then use if condition
data_2014$include_in_school_calc <- ifelse(map2_lgl(data_2014$grade_level_2014,data_2014$continuous_2014,is.element),1,ifelse(map_lgl(data_2014$continuous_2014,is.null),NA,0))
data_2013$include_in_school_calc <- ifelse(map2_lgl(data_2013$grade_level_2013,data_2013$continuous_2013,is.element),1,ifelse(map_lgl(data_2013$continuous_2013,is.null),NA,0))
data_2012$include_in_school_calc <- ifelse(map2_lgl(data_2012$grade_level_2012,data_2012$continuous_2012,is.element),1,ifelse(map_lgl(data_2012$continuous_2012,is.null),NA,0))
data_2011$include_in_school_calc <- ifelse(map2_lgl(data_2011$grade_level_2011,data_2011$continuous_2011,is.element),1,ifelse(map_lgl(data_2011$continuous_2011,is.null),NA,0))
data_2010$include_in_school_calc <- ifelse(map2_lgl(data_2010$grade_level_2010,data_2010$continuous_2010,is.element),1,ifelse(map_lgl(data_2010$continuous_2010,is.null),NA,0))
data_2009$include_in_school_calc <- ifelse(map2_lgl(data_2009$grade_level_2009,data_2009$continuous_2009,is.element),1,ifelse(map_lgl(data_2009$continuous_2009,is.null),NA,0))
data_2008$include_in_school_calc <- ifelse(map2_lgl(data_2008$grade_level_2008,data_2008$continuous_2008,is.element),1,ifelse(map_lgl(data_2008$continuous_2008,is.null),NA,0))
data_2007$include_in_school_calc <- ifelse(map2_lgl(data_2007$grade_level_2007,data_2007$continuous_2007,is.element),1,ifelse(map_lgl(data_2007$continuous_2007,is.null),NA,0))
data_2006$include_in_school_calc <- ifelse(map2_lgl(data_2006$grade_level_2006,data_2006$continuous_2006,is.element),1,ifelse(map_lgl(data_2006$continuous_2006,is.null),NA,0))
data_2005$include_in_school_calc <- ifelse(map2_lgl(data_2005$grade_level_2005,data_2005$continuous_2005,is.element),1,ifelse(map_lgl(data_2005$continuous_2005,is.null),NA,0))
data_2004$include_in_school_calc <- ifelse(map2_lgl(data_2004$grade_level_2004,data_2004$continuous_2004,is.element),1,ifelse(map_lgl(data_2004$continuous_2004,is.null),NA,0))
data_2003$include_in_school_calc <- ifelse(map2_lgl(data_2003$grade_level_2003,data_2003$continuous_2003,is.element),1,ifelse(map_lgl(data_2003$continuous_2003,is.null),NA,0))
data_2002$include_in_school_calc <- ifelse(map2_lgl(data_2002$grade_level_2002,data_2002$continuous_2002,is.element),1,ifelse(map_lgl(data_2002$continuous_2002,is.null),NA,0))

#########################################
##  CATEGORIZE STUDENTS ##
#########################################
### Create dummies for retained_in_school, retained_in_district

  # retained_in_district
data_2014$retained_in_district <- ifelse(data_2014$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2014$bn_2015),1,0))
data_2013$retained_in_district <- ifelse(data_2013$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2013$bn_2014),1,0))
data_2012$retained_in_district <- ifelse(data_2012$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2012$bn_2013),1,0))
data_2011$retained_in_district <- ifelse(data_2011$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2011$bn_2012),1,0))
data_2010$retained_in_district <- ifelse(data_2010$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2010$bn_2011),1,0))
data_2009$retained_in_district <- ifelse(data_2009$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2009$bn_2010),1,0))
data_2008$retained_in_district <- ifelse(data_2008$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2008$bn_2009),1,0))
data_2007$retained_in_district <- ifelse(data_2007$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2007$bn_2008),1,0))
data_2006$retained_in_district <- ifelse(data_2006$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2006$bn_2007),1,0))
data_2005$retained_in_district <- ifelse(data_2005$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2005$bn_2006),1,0))
data_2004$retained_in_district <- ifelse(data_2004$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2004$bn_2005),1,0))
data_2003$retained_in_district <- ifelse(data_2003$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2003$bn_2004),1,0))
data_2002$retained_in_district <- ifelse(data_2002$include_in_dist_calc == 0,NA,ifelse(!is.na(data_2002$bn_2003),1,0))

# retained_in_school
data_2014$retained_in_school <- ifelse(data_2014$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2014$bn_2015),0,
                                                    ifelse(data_2014$bn_2014 == data_2014$bn_2015,1,0)))

data_2013$retained_in_school <- ifelse(data_2013$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2013$bn_2014),0,
                                                    ifelse(data_2013$bn_2013 == data_2013$bn_2014,1,0)))

data_2012$retained_in_school <- ifelse(data_2012$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2012$bn_2013),0,
                                                    ifelse(data_2012$bn_2012 == data_2012$bn_2013,1,0)))

data_2011$retained_in_school <- ifelse(data_2011$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2011$bn_2012),0,
                                                    ifelse(data_2011$bn_2011 == data_2011$bn_2012,1,0)))

data_2010$retained_in_school <- ifelse(data_2010$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2010$bn_2011),0,
                                                    ifelse(data_2010$bn_2010 == data_2010$bn_2011,1,0)))

data_2009$retained_in_school <- ifelse(data_2009$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2009$bn_2010),0,
                                                    ifelse(data_2009$bn_2009 == data_2009$bn_2010,1,0)))

data_2008$retained_in_school <- ifelse(data_2008$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2008$bn_2009),0,
                                                    ifelse(data_2008$bn_2008 == data_2008$bn_2009,1,0)))

data_2007$retained_in_school <- ifelse(data_2007$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2007$bn_2008),0,
                                                    ifelse(data_2007$bn_2007 == data_2007$bn_2008,1,0)))

data_2006$retained_in_school <- ifelse(data_2006$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2006$bn_2007),0,
                                                    ifelse(data_2006$bn_2006 == data_2006$bn_2007,1,0)))

data_2005$retained_in_school <- ifelse(data_2005$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2005$bn_2006),0,
                                                    ifelse(data_2005$bn_2005 == data_2005$bn_2006,1,0)))

data_2004$retained_in_school <- ifelse(data_2004$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2004$bn_2005),0,
                                                    ifelse(data_2004$bn_2004 == data_2004$bn_2005,1,0)))

data_2003$retained_in_school <- ifelse(data_2003$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2003$bn_2004),0,
                                                    ifelse(data_2003$bn_2003 == data_2003$bn_2004,1,0)))

data_2002$retained_in_school <- ifelse(data_2002$include_in_school_calc == 0,NA,
                                             ifelse(is.na(data_2002$bn_2003),0,
                                                    ifelse(data_2002$bn_2002 == data_2002$bn_2003,1,0)))

###################################################
##Remove unnecessary variables to reduce size
## focus initially on removing list of continuing grades
## becuase that causes size to expand when reloaded
###################################################

data_2002 <- data_2002[, !(colnames(data_2002) %in% c("continuous_2002"))]
data_2003 <- data_2003[, !(colnames(data_2003) %in% c("continuous_2003"))]
data_2004 <- data_2004[, !(colnames(data_2004) %in% c("continuous_2004"))]
data_2005 <- data_2005[, !(colnames(data_2005) %in% c("continuous_2005"))]
data_2006 <- data_2006[, !(colnames(data_2006) %in% c("continuous_2006"))]
data_2007 <- data_2007[, !(colnames(data_2007) %in% c("continuous_2007"))]
data_2008 <- data_2008[, !(colnames(data_2008) %in% c("continuous_2008"))]
data_2009 <- data_2009[, !(colnames(data_2009) %in% c("continuous_2009"))]
data_2010 <- data_2010[, !(colnames(data_2010) %in% c("continuous_2010"))]
data_2011 <- data_2011[, !(colnames(data_2011) %in% c("continuous_2011"))]
data_2012 <- data_2012[, !(colnames(data_2012) %in% c("continuous_2012"))]
data_2013 <- data_2013[, !(colnames(data_2013) %in% c("continuous_2013"))]
data_2014 <- data_2014[, !(colnames(data_2014) %in% c("continuous_2014"))]


#######################################
##  EXPORT DATA ##
#######################################

# Set file path for exporting CSV files
setwd(file.path(dir, "Data/Output"))

# Export CSV files
write.csv(grade_range_all_char, file = 'grade_range_all.csv')

# Set file path for exporting R files
setwd(file.path(dir, "Data/R2"))

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
