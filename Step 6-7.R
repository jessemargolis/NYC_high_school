###### STEP 6 & 7: CALCULATE RETENTION RATE & MAKE A SUMMARIZE TABLE
#District wide retention rate:
dist_wide_retention_2014 <- (sum( Merge_2013_2014$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2013_2014$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2013 <- (sum( Merge_2012_2013$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2012_2013$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2012 <- (sum( Merge_2011_2012$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2011_2012$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2011 <- (sum( Merge_2010_2011$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2010_2011$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2010 <- (sum( Merge_2009_2010$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2009_2010$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2009 <- (sum( Merge_2008_2009$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2008_2009$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2008 <- (sum( Merge_2007_2008$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2007_2008$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2007 <- (sum( Merge_2006_2007$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2006_2007$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2006 <- (sum( Merge_2005_2006$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2005_2006$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2005 <- (sum( Merge_2004_2005$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2004_2005$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2004 <- (sum( Merge_2003_2004$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2003_2004$include_in_dist_calc == 1, na.rm = TRUE))
dist_wide_retention_2003 <- (sum( Merge_2002_2003$retained_in_district == 1, na.rm = TRUE))/(sum(Merge_2002_2003$include_in_dist_calc == 1, na.rm = TRUE))

dist_wide_retention_all <- data.frame(t(dist_wide_retention_2003,dist_wide_retention_2004,dist_wide_retention_2005,dist_wide_retention_2006,dist_wide_retention_2007,
                                        dist_wide_retention_2008,dist_wide_retention_2009,dist_wide_retention_2010,dist_wide_retention_2011,dist_wide_retention_2012,
                                        dist_wide_retention_2013,dist_wide_retention_2014))
dist_wide_retention_all <- data.frame(t(dist_wide_retention_all))
write.csv(dist_wide_retention_all, file = '/Users/elmerleezy/Desktop/dist_wide_retention_all.csv')

### School wide retention rate by school:
# Aggregate and calculate rate
sch_wide_retention_2014 <- data.frame(aggregate(cbind(Merge_2013_2014$retained_in_school, Merge_2013_2014$include_in_school_calc) ~ bn_2013 + grade_level_2013, Merge_2013_2014,sum))
sch_wide_retention_2013 <- data.frame(aggregate(cbind(Merge_2012_2013$retained_in_school, Merge_2012_2013$include_in_school_calc) ~ bn_2012 + grade_level_2012, Merge_2012_2013,sum))
sch_wide_retention_2012 <- data.frame(aggregate(cbind(Merge_2011_2012$retained_in_school, Merge_2011_2012$include_in_school_calc) ~ bn_2011 + grade_level_2011, Merge_2011_2012,sum))
sch_wide_retention_2011 <- data.frame(aggregate(cbind(Merge_2010_2011$retained_in_school, Merge_2010_2011$include_in_school_calc) ~ bn_2010 + grade_level_2010, Merge_2010_2011,sum))
sch_wide_retention_2010 <- data.frame(aggregate(cbind(Merge_2009_2010$retained_in_school, Merge_2009_2010$include_in_school_calc) ~ bn_2009 + grade_level_2009, Merge_2009_2010,sum))
sch_wide_retention_2009 <- data.frame(aggregate(cbind(Merge_2008_2009$retained_in_school, Merge_2008_2009$include_in_school_calc) ~ bn_2008 + grade_level_2008, Merge_2008_2009,sum))
sch_wide_retention_2008 <- data.frame(aggregate(cbind(Merge_2007_2008$retained_in_school, Merge_2007_2008$include_in_school_calc) ~ bn_2007 + grade_level_2007, Merge_2007_2008,sum))
sch_wide_retention_2007 <- data.frame(aggregate(cbind(Merge_2006_2007$retained_in_school, Merge_2006_2007$include_in_school_calc) ~ bn_2006 + grade_level_2006, Merge_2006_2007,sum))
sch_wide_retention_2006 <- data.frame(aggregate(cbind(Merge_2005_2006$retained_in_school, Merge_2005_2006$include_in_school_calc) ~ bn_2005 + grade_level_2005, Merge_2005_2006,sum))
sch_wide_retention_2005 <- data.frame(aggregate(cbind(Merge_2004_2005$retained_in_school, Merge_2004_2005$include_in_school_calc) ~ bn_2004 + grade_level_2004, Merge_2004_2005,sum))
sch_wide_retention_2004 <- data.frame(aggregate(cbind(Merge_2003_2004$retained_in_school, Merge_2003_2004$include_in_school_calc) ~ bn_2003 + grade_level_2003, Merge_2003_2004,sum))
sch_wide_retention_2003 <- data.frame(aggregate(cbind(Merge_2002_2003$retained_in_school, Merge_2002_2003$include_in_school_calc) ~ bn_2002 + grade_level_2002, Merge_2002_2003,sum))

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

######################################
##     CREATE THE SUMMARY TABLE  ##
######################################

### Merge Scholl Retention Rate to make a different school retention rate table

sch_wide_retention_all<-Reduce(function(x, y) merge(x, y, by=c('bn','grade_level'), all=TRUE), list(sch_wide_retention_2003,sch_wide_retention_2004,sch_wide_retention_2005,sch_wide_retention_2006,sch_wide_retention_2007,
                                                                                   sch_wide_retention_2008,sch_wide_retention_2009,sch_wide_retention_2010,sch_wide_retention_2011,sch_wide_retention_2012,
                                                                                   sch_wide_retention_2013,sch_wide_retention_2014))
# Match school names
  # two sources of school names 
NYC_schools<-read.csv('/Users/elmerleezy/Desktop/NYC_schools.csv') 
NYC_schools2<-read.csv('/Users/elmerleezy/Desktop/NYC_schools2.csv')
  # first match the newsest one, then the older one
sch_wide_retention_all$school_name <- NYC_schools[match(sch_wide_retention_all$bn,NYC_schools$bn),'school_name']
ifelse(is.na(sch_wide_retention_all$school_name), sch_wide_retention_all$school_name <- NYC_schools2[match(sch_wide_retention_all$bn,NYC_schools2$bn),'school_name'], NULL)
  # then move the names to the front
sch_wide_retention_all <- sch_wide_retention_all[c(1,39,2:38)]

# Export: 
write.csv(sch_wide_retention_all, file = '/Users/elmerleezy/Desktop/sch_wide_retention_all.csv')











# ###### Make a rention rate table of different grades, acroos different years
# require(plyr)
# grade_retain_2014 <- data.frame(ddply(Merge_2013_2014,.(bn_2013),summarise,
#                                      gradePK= sum(grade_level_2013==-1 & retained_in_school ==1)/sum((grade_level_2014==-1 & retained_in_school ==1)|(grade_level_2014==-1 & retained_in_school ==0)),
#                                      grade0= sum(grade_level_2013==0 & retained_in_school ==1)/sum((grade_level_2014==0 & retained_in_school ==1)|(grade_level_2014==0 & retained_in_school ==0)),
#                                      grade1= sum(grade_level_2013==1 & retained_in_school ==1)/sum((grade_level_2014==1 & retained_in_school ==1)|(grade_level_2014==1 & retained_in_school ==0)),
#                                      grade2= sum(grade_level_2013==2 & retained_in_school ==1)/sum((grade_level_2014==2 & retained_in_school ==1)|(grade_level_2014==2 & retained_in_school ==0)),
#                                      grade3= sum(grade_level_2013==3 & retained_in_school ==1)/sum((grade_level_2014==3 & retained_in_school ==1)|(grade_level_2014==3 & retained_in_school ==0)),
#                                      grade4= sum(grade_level_2013==4 & retained_in_school ==1)/sum((grade_level_2014==4 & retained_in_school ==1)|(grade_level_2014==4 & retained_in_school ==0)),
#                                      grade5= sum(grade_level_2013==5 & retained_in_school ==1)/sum((grade_level_2014==5 & retained_in_school ==1)|(grade_level_2014==5 & retained_in_school ==0)),
#                                      grade6= sum(grade_level_2013==6 & retained_in_school ==1)/sum((grade_level_2014==6 & retained_in_school ==1)|(grade_level_2014==6 & retained_in_school ==0)),
#                                      grade7= sum(grade_level_2013==7 & retained_in_school ==1)/sum((grade_level_2014==7 & retained_in_school ==1)|(grade_level_2014==7 & retained_in_school ==0)),
#                                      grade8= sum(grade_level_2013==8 & retained_in_school ==1)/sum((grade_level_2014==8 & retained_in_school ==1)|(grade_level_2014==8 & retained_in_school ==0)),
#                                      grade9= sum(grade_level_2013==9 & retained_in_school ==1)/sum((grade_level_2014==9 & retained_in_school ==1)|(grade_level_2014==9 & retained_in_school ==0)),
#                                      grade10= sum(grade_level_2013==10 & retained_in_school ==1)/sum((grade_level_2014==10 & retained_in_school ==1)|(grade_level_2014==10 & retained_in_school ==0)),
#                                      grade11= sum(grade_level_2013==11 & retained_in_school ==1)/sum((grade_level_2014==11 & retained_in_school ==1)|(grade_level_2014==11 & retained_in_school ==0)),
#                                      grade12= sum(grade_level_2013==12 & retained_in_school ==1)/sum((grade_level_2014==12 & retained_in_school ==1)|(grade_level_2014==12 & retained_in_school ==0))))
# 
# #test
# 
# test <- ddply(Merge_2013_2014,.(bn_2013),summarise,
#       gradePK= sum(Merge_2013_2014[which(Merge_2013_2014['grade_level_2013']==-1 & Merge_2013_2014['retained_in_school'] ==1),'include_in_school']))
# 
# sum(Merge_2013_2014['grade_level_2013']==3 & Merge_2013_2014['retained_in_school'] ==1)
# 
# sum(ifelse(Merge_2013_2014$grade_level_2013==-1 & Merge_2013_2014$retained_in_school ==1, 1, 0))
# 
# nrow(c[a==2 & b==2, ])
# nrow(Merge_2013_2014[grade_level_2013==11 & retained_in_school ==1, ])
# 
# class(Merge_2013_2014$grade_level_2013)


