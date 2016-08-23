########################################
## DECIDE ELIGIBILITY ##
## grade_count, include_dis/sch ##
########################################

# Create Max for Grade Level 2013
# '*************not to be used*****************'
# grade_max_2013 <- data.frame(aggregate(cbind(Merge_2013_2014$grade_level_2013) ~ dbn3to6_2013, Merge_2013_2014, max))
# Merge_2013_2014$grademax_2013 <- grade_max_2013[match(Merge_2013_2014$dbn3to6_2013,grade_max_2013$dbn3to6_2013),'V1']
# '*************not to be used*****************'

### Count frequency of each grade level to each school
require(plyr)
  # Delete NAs in gradelevel (if with NA, the whole school return NA)
data_2003 <- data_2003[!is.na(data_2003$grade_level_2003), ]
data_2004 <- data_2004[!is.na(data_2004$grade_level_2004), ]
data_2005 <- data_2005[!is.na(data_2005$grade_level_2005), ]
data_2006 <- data_2006[!is.na(data_2006$grade_level_2006), ]
data_2011 <- data_2011[!is.na(data_2011$grade_level_2011), ]

  # 2015
  grade_count_2015 <- data.frame(ddply(data_2015,.(bn_2015),summarise,
                                       gradePK=sum(grade_level_2015==-1),
                                       grade0=sum(grade_level_2015==0),
                                       grade1=sum(grade_level_2015==1),
                                       grade2=sum(grade_level_2015==2),
                                       grade3=sum(grade_level_2015==3),
                                       grade4=sum(grade_level_2015==4),
                                       grade5=sum(grade_level_2015==5),
                                       grade6=sum(grade_level_2015==6),
                                       grade7=sum(grade_level_2015==7),
                                       grade8=sum(grade_level_2015==8),
                                       grade9=sum(grade_level_2015==9),
                                       grade10=sum(grade_level_2015==10),
                                       grade11=sum(grade_level_2015==11),
                                       grade12=sum(grade_level_2015==12)))
  # 2014
  grade_count_2014 <- data.frame(ddply(data_2014,.(bn_2014),summarise,
                                       gradePK=sum(grade_level_2014==-1),
                                       grade0=sum(grade_level_2014==0),
                                       grade1=sum(grade_level_2014==1),
                                       grade2=sum(grade_level_2014==2),
                                       grade3=sum(grade_level_2014==3),
                                       grade4=sum(grade_level_2014==4),
                                       grade5=sum(grade_level_2014==5),
                                       grade6=sum(grade_level_2014==6),
                                       grade7=sum(grade_level_2014==7),
                                       grade8=sum(grade_level_2014==8),
                                       grade9=sum(grade_level_2014==9),
                                       grade10=sum(grade_level_2014==10),
                                       grade11=sum(grade_level_2014==11),
                                       grade12=sum(grade_level_2014==12)))
  # 2013
  grade_count_2013 <- data.frame(ddply(data_2013,.(bn_2013),summarise,
                                       gradePK=sum(grade_level_2013==-1),
                                       grade0=sum(grade_level_2013==0),
                                       grade1=sum(grade_level_2013==1),
                                       grade2=sum(grade_level_2013==2),
                                       grade3=sum(grade_level_2013==3),
                                       grade4=sum(grade_level_2013==4),
                                       grade5=sum(grade_level_2013==5),
                                       grade6=sum(grade_level_2013==6),
                                       grade7=sum(grade_level_2013==7),
                                       grade8=sum(grade_level_2013==8),
                                       grade9=sum(grade_level_2013==9),
                                       grade10=sum(grade_level_2013==10),
                                       grade11=sum(grade_level_2013==11),
                                       grade12=sum(grade_level_2013==12)))
  # 2012
  grade_count_2012 <- data.frame(ddply(data_2012,.(bn_2012),summarise,
                                       gradePK=sum(grade_level_2012==-1),
                                       grade0=sum(grade_level_2012==0),
                                       grade1=sum(grade_level_2012==1),
                                       grade2=sum(grade_level_2012==2),
                                       grade3=sum(grade_level_2012==3),
                                       grade4=sum(grade_level_2012==4),
                                       grade5=sum(grade_level_2012==5),
                                       grade6=sum(grade_level_2012==6),
                                       grade7=sum(grade_level_2012==7),
                                       grade8=sum(grade_level_2012==8),
                                       grade9=sum(grade_level_2012==9),
                                       grade10=sum(grade_level_2012==10),
                                       grade11=sum(grade_level_2012==11),
                                       grade12=sum(grade_level_2012==12)))
  # 2011
  grade_count_2011 <- data.frame(ddply(data_2011,.(bn_2011),summarise,
                                       gradePK=sum(grade_level_2011==-1),
                                       grade0=sum(grade_level_2011==0),
                                       grade1=sum(grade_level_2011==1),
                                       grade2=sum(grade_level_2011==2),
                                       grade3=sum(grade_level_2011==3),
                                       grade4=sum(grade_level_2011==4),
                                       grade5=sum(grade_level_2011==5),
                                       grade6=sum(grade_level_2011==6),
                                       grade7=sum(grade_level_2011==7),
                                       grade8=sum(grade_level_2011==8),
                                       grade9=sum(grade_level_2011==9),
                                       grade10=sum(grade_level_2011==10),
                                       grade11=sum(grade_level_2011==11),
                                       grade12=sum(grade_level_2011==12)))
  # 2010
  grade_count_2010 <- data.frame(ddply(data_2010,.(bn_2010),summarise,
                                       gradePK=sum(grade_level_2010==-1),
                                       grade0=sum(grade_level_2010==0),
                                       grade1=sum(grade_level_2010==1),
                                       grade2=sum(grade_level_2010==2),
                                       grade3=sum(grade_level_2010==3),
                                       grade4=sum(grade_level_2010==4),
                                       grade5=sum(grade_level_2010==5),
                                       grade6=sum(grade_level_2010==6),
                                       grade7=sum(grade_level_2010==7),
                                       grade8=sum(grade_level_2010==8),
                                       grade9=sum(grade_level_2010==9),
                                       grade10=sum(grade_level_2010==10),
                                       grade11=sum(grade_level_2010==11),
                                       grade12=sum(grade_level_2010==12)))
  # 2009
  grade_count_2009 <- data.frame(ddply(data_2009,.(bn_2009),summarise,
                                       gradePK=sum(grade_level_2009==-1),
                                       grade0=sum(grade_level_2009==0),
                                       grade1=sum(grade_level_2009==1),
                                       grade2=sum(grade_level_2009==2),
                                       grade3=sum(grade_level_2009==3),
                                       grade4=sum(grade_level_2009==4),
                                       grade5=sum(grade_level_2009==5),
                                       grade6=sum(grade_level_2009==6),
                                       grade7=sum(grade_level_2009==7),
                                       grade8=sum(grade_level_2009==8),
                                       grade9=sum(grade_level_2009==9),
                                       grade10=sum(grade_level_2009==10),
                                       grade11=sum(grade_level_2009==11),
                                       grade12=sum(grade_level_2009==12)))
  # 2008
  grade_count_2008 <- data.frame(ddply(data_2008,.(bn_2008),summarise,
                                       gradePK=sum(grade_level_2008==-1),
                                       grade0=sum(grade_level_2008==0),
                                       grade1=sum(grade_level_2008==1),
                                       grade2=sum(grade_level_2008==2),
                                       grade3=sum(grade_level_2008==3),
                                       grade4=sum(grade_level_2008==4),
                                       grade5=sum(grade_level_2008==5),
                                       grade6=sum(grade_level_2008==6),
                                       grade7=sum(grade_level_2008==7),
                                       grade8=sum(grade_level_2008==8),
                                       grade9=sum(grade_level_2008==9),
                                       grade10=sum(grade_level_2008==10),
                                       grade11=sum(grade_level_2008==11),
                                       grade12=sum(grade_level_2008==12)))
  # 2007
  grade_count_2007 <- data.frame(ddply(data_2007,.(bn_2007),summarise,
                                       gradePK=sum(grade_level_2007==-1),
                                       grade0=sum(grade_level_2007==0),
                                       grade1=sum(grade_level_2007==1),
                                       grade2=sum(grade_level_2007==2),
                                       grade3=sum(grade_level_2007==3),
                                       grade4=sum(grade_level_2007==4),
                                       grade5=sum(grade_level_2007==5),
                                       grade6=sum(grade_level_2007==6),
                                       grade7=sum(grade_level_2007==7),
                                       grade8=sum(grade_level_2007==8),
                                       grade9=sum(grade_level_2007==9),
                                       grade10=sum(grade_level_2007==10),
                                       grade11=sum(grade_level_2007==11),
                                       grade12=sum(grade_level_2007==12)))
  # 2006
  grade_count_2006 <- data.frame(ddply(data_2006,.(bn_2006),summarise,
                                       gradePK=sum(grade_level_2006==-1),
                                       grade0=sum(grade_level_2006==0),
                                       grade1=sum(grade_level_2006==1),
                                       grade2=sum(grade_level_2006==2),
                                       grade3=sum(grade_level_2006==3),
                                       grade4=sum(grade_level_2006==4),
                                       grade5=sum(grade_level_2006==5),
                                       grade6=sum(grade_level_2006==6),
                                       grade7=sum(grade_level_2006==7),
                                       grade8=sum(grade_level_2006==8),
                                       grade9=sum(grade_level_2006==9),
                                       grade10=sum(grade_level_2006==10),
                                       grade11=sum(grade_level_2006==11),
                                       grade12=sum(grade_level_2006==12)))
  # 2005
  grade_count_2005 <- data.frame(ddply(data_2005,.(bn_2005),summarise,
                                       gradePK=sum(grade_level_2005==-1),
                                       grade0=sum(grade_level_2005==0),
                                       grade1=sum(grade_level_2005==1),
                                       grade2=sum(grade_level_2005==2),
                                       grade3=sum(grade_level_2005==3),
                                       grade4=sum(grade_level_2005==4),
                                       grade5=sum(grade_level_2005==5),
                                       grade6=sum(grade_level_2005==6),
                                       grade7=sum(grade_level_2005==7),
                                       grade8=sum(grade_level_2005==8),
                                       grade9=sum(grade_level_2005==9),
                                       grade10=sum(grade_level_2005==10),
                                       grade11=sum(grade_level_2005==11),
                                       grade12=sum(grade_level_2005==12)))
  # 2004
  grade_count_2004 <- data.frame(ddply(data_2004,.(bn_2004),summarise,
                                       gradePK=sum(grade_level_2004==-1),
                                       grade0=sum(grade_level_2004==0),
                                       grade1=sum(grade_level_2004==1),
                                       grade2=sum(grade_level_2004==2),
                                       grade3=sum(grade_level_2004==3),
                                       grade4=sum(grade_level_2004==4),
                                       grade5=sum(grade_level_2004==5),
                                       grade6=sum(grade_level_2004==6),
                                       grade7=sum(grade_level_2004==7),
                                       grade8=sum(grade_level_2004==8),
                                       grade9=sum(grade_level_2004==9),
                                       grade10=sum(grade_level_2004==10),
                                       grade11=sum(grade_level_2004==11),
                                       grade12=sum(grade_level_2004==12)))
  # 2003
  grade_count_2003 <- data.frame(ddply(data_2003,.(bn_2003),summarise,
                                       gradePK=sum(grade_level_2003==-1),
                                       grade0=sum(grade_level_2003==0),
                                       grade1=sum(grade_level_2003==1),
                                       grade2=sum(grade_level_2003==2),
                                       grade3=sum(grade_level_2003==3),
                                       grade4=sum(grade_level_2003==4),
                                       grade5=sum(grade_level_2003==5),
                                       grade6=sum(grade_level_2003==6),
                                       grade7=sum(grade_level_2003==7),
                                       grade8=sum(grade_level_2003==8),
                                       grade9=sum(grade_level_2003==9),
                                       grade10=sum(grade_level_2003==10),
                                       grade11=sum(grade_level_2003==11),
                                       grade12=sum(grade_level_2003==12)))
  # 2002
  grade_count_2002 <- data.frame(ddply(data_2002,.(bn_2002),summarise,

                                       gradePK=sum(grade_level_2002==-1),
                                       grade0=sum(grade_level_2002==0),
                                       grade1=sum(grade_level_2002==1),
                                       grade2=sum(grade_level_2002==2),
                                       grade3=sum(grade_level_2002==3),
                                       grade4=sum(grade_level_2002==4),
                                       grade5=sum(grade_level_2002==5),
                                       grade6=sum(grade_level_2002==6),
                                       grade7=sum(grade_level_2002==7),
                                       grade8=sum(grade_level_2002==8),
                                       grade9=sum(grade_level_2002==9),
                                       grade10=sum(grade_level_2002==10),
                                       grade11=sum(grade_level_2002==11),
                                       grade12=sum(grade_level_2002==12)))
  
  # convert first column to row names
grade_count_2015 <- data.frame(grade_count_2015[,-1], row.names=grade_count_2015[,1])
grade_count_2014 <- data.frame(grade_count_2014[,-1], row.names=grade_count_2014[,1])
grade_count_2013 <- data.frame(grade_count_2013[,-1], row.names=grade_count_2013[,1])
grade_count_2012 <- data.frame(grade_count_2012[,-1], row.names=grade_count_2012[,1])
grade_count_2011 <- data.frame(grade_count_2011[,-1], row.names=grade_count_2011[,1])
grade_count_2010 <- data.frame(grade_count_2010[,-1], row.names=grade_count_2010[,1])
grade_count_2009 <- data.frame(grade_count_2009[,-1], row.names=grade_count_2009[,1])
grade_count_2008 <- data.frame(grade_count_2008[,-1], row.names=grade_count_2008[,1])
grade_count_2007 <- data.frame(grade_count_2007[,-1], row.names=grade_count_2007[,1])
grade_count_2006 <- data.frame(grade_count_2006[,-1], row.names=grade_count_2006[,1])
grade_count_2005 <- data.frame(grade_count_2005[,-1], row.names=grade_count_2005[,1])
grade_count_2004 <- data.frame(grade_count_2004[,-1], row.names=grade_count_2004[,1])
grade_count_2003 <- data.frame(grade_count_2003[,-1], row.names=grade_count_2003[,1])
grade_count_2002 <- data.frame(grade_count_2002[,-1], row.names=grade_count_2002[,1])
  
# #### Terminal Grade table
# library(plyr)
#   #Add prefix to each dataset
# rownames(grade_count_2002) <- paste(rownames(grade_count_2002),2002, sep = "_")
# rownames(grade_count_2003) <- paste(rownames(grade_count_2003),2003, sep = "_")
# rownames(grade_count_2004) <- paste(rownames(grade_count_2004),2004, sep = "_")
# rownames(grade_count_2005) <- paste(rownames(grade_count_2005),2005, sep = "_")
# rownames(grade_count_2006) <- paste(rownames(grade_count_2006),2006, sep = "_")
# rownames(grade_count_2007) <- paste(rownames(grade_count_2007),2007, sep = "_")
# rownames(grade_count_2008) <- paste(rownames(grade_count_2008),2008, sep = "_")
# rownames(grade_count_2009) <- paste(rownames(grade_count_2009),2009, sep = "_")
# rownames(grade_count_2010) <- paste(rownames(grade_count_2010),2010, sep = "_")
# rownames(grade_count_2011) <- paste(rownames(grade_count_2011),2011, sep = "_")
# rownames(grade_count_2012) <- paste(rownames(grade_count_2012),2012, sep = "_")
# rownames(grade_count_2013) <- paste(rownames(grade_count_2013),2013, sep = "_")
# rownames(grade_count_2014) <- paste(rownames(grade_count_2014),2014, sep = "_")
#   #Bind Data List
# RBIND <- function(datalist) {
#   require(plyr)
#   temp <- rbind.fill(datalist)
#   rownames(temp) <- unlist(lapply(datalist, row.names))
#   temp
# }'requirement: rowname must be different'
#   
# grade_count_all <- RBIND(list(grade_count_2002,grade_count_2003,grade_count_2004,grade_count_2005,grade_count_2006,grade_count_2007,
#                               grade_count_2008,grade_count_2009,grade_count_2010,grade_count_2011,grade_count_2012,grade_count_2013,grade_count_2014))  
# 
# grade_count_all <- grade_count_all[order(rownames(grade_count_all)),]
# write.csv(grade_count_all, file = '/Users/elmerleezy/Desktop/grade_count_all.csv')


# # print
# write.csv(grade_count_2002, file = '/Users/elmerleezy/Desktop/grade_count_2002.csv')
# write.csv(grade_count_2003, file = '/Users/elmerleezy/Desktop/grade_count_2003.csv')
# write.csv(grade_count_2004, file = '/Users/elmerleezy/Desktop/grade_count_2004.csv')
# write.csv(grade_count_2005, file = '/Users/elmerleezy/Desktop/grade_count_2005.csv')
# write.csv(grade_count_2006, file = '/Users/elmerleezy/Desktop/grade_count_2006.csv')
# write.csv(grade_count_2007, file = '/Users/elmerleezy/Desktop/grade_count_2007.csv')
# write.csv(grade_count_2008, file = '/Users/elmerleezy/Desktop/grade_count_2008.csv')
# write.csv(grade_count_2009, file = '/Users/elmerleezy/Desktop/grade_count_2009.csv')
# write.csv(grade_count_2010, file = '/Users/elmerleezy/Desktop/grade_count_2010.csv')
# write.csv(grade_count_2011, file = '/Users/elmerleezy/Desktop/grade_count_2011.csv')
# write.csv(grade_count_2012, file = '/Users/elmerleezy/Desktop/grade_count_2012.csv')
# write.csv(grade_count_2013, file = '/Users/elmerleezy/Desktop/grade_count_2013.csv')
# write.csv(grade_count_2014, file = '/Users/elmerleezy/Desktop/grade_count_2014.csv')


### Decide grade range by count grade freq > 10
  # count grade freq >10
grade_range_2015 <- data.frame(cbind(apply(grade_count_2015,1 ,function(x) which(x>10))))
grade_range_2014 <- data.frame(cbind(apply(grade_count_2014,1 ,function(x) which(x>10))))
grade_range_2013 <- data.frame(cbind(apply(grade_count_2013,1 ,function(x) which(x>10))))
grade_range_2012 <- data.frame(cbind(apply(grade_count_2012,1 ,function(x) which(x>10))))
grade_range_2011 <- data.frame(cbind(apply(grade_count_2011,1 ,function(x) which(x>10))))
grade_range_2010 <- data.frame(cbind(apply(grade_count_2010,1 ,function(x) which(x>10))))
grade_range_2009 <- data.frame(cbind(apply(grade_count_2009,1 ,function(x) which(x>10))))
grade_range_2008 <- data.frame(cbind(apply(grade_count_2008,1 ,function(x) which(x>10))))
grade_range_2007 <- data.frame(cbind(apply(grade_count_2007,1 ,function(x) which(x>10))))
grade_range_2006 <- data.frame(cbind(apply(grade_count_2006,1 ,function(x) which(x>10))))
grade_range_2005 <- data.frame(cbind(apply(grade_count_2005,1 ,function(x) which(x>10))))
grade_range_2004 <- data.frame(cbind(apply(grade_count_2004,1 ,function(x) which(x>10))))
grade_range_2003 <- data.frame(cbind(apply(grade_count_2003,1 ,function(x) which(x>10))))
grade_range_2002 <- data.frame(cbind(apply(grade_count_2002,1 ,function(x) which(x>10))))
  # convert first column names to row names
names(grade_range_2015) <- c('grade_range_2015')
names(grade_range_2014) <- c('grade_range_2014')
names(grade_range_2013) <- c('grade_range_2013')
names(grade_range_2012) <- c('grade_range_2012')
names(grade_range_2011) <- c('grade_range_2011')
names(grade_range_2010) <- c('grade_range_2010')
names(grade_range_2009) <- c('grade_range_2009')
names(grade_range_2008) <- c('grade_range_2008')
names(grade_range_2007) <- c('grade_range_2007')
names(grade_range_2006) <- c('grade_range_2006')
names(grade_range_2005) <- c('grade_range_2005')
names(grade_range_2004) <- c('grade_range_2004')
names(grade_range_2003) <- c('grade_range_2003')
names(grade_range_2002) <- c('grade_range_2002')
  # minus 2 to get the right grades
grade_range_2015$grade_range_2015<- lapply(grade_range_2015$grade_range_2015,function(x) x-2) 
grade_range_2014$grade_range_2014<- lapply(grade_range_2014$grade_range_2014,function(x) x-2) 
grade_range_2013$grade_range_2013<- lapply(grade_range_2013$grade_range_2013,function(x) x-2) 
grade_range_2012$grade_range_2012<- lapply(grade_range_2012$grade_range_2012,function(x) x-2) 
grade_range_2011$grade_range_2011<- lapply(grade_range_2011$grade_range_2011,function(x) x-2) 
grade_range_2010$grade_range_2010<- lapply(grade_range_2010$grade_range_2010,function(x) x-2) 
grade_range_2009$grade_range_2009<- lapply(grade_range_2009$grade_range_2009,function(x) x-2) 
grade_range_2008$grade_range_2008<- lapply(grade_range_2008$grade_range_2008,function(x) x-2) 
grade_range_2007$grade_range_2007<- lapply(grade_range_2007$grade_range_2007,function(x) x-2) 
grade_range_2006$grade_range_2006<- lapply(grade_range_2006$grade_range_2006,function(x) x-2) 
grade_range_2005$grade_range_2005<- lapply(grade_range_2005$grade_range_2005,function(x) x-2) 
grade_range_2004$grade_range_2004<- lapply(grade_range_2004$grade_range_2004,function(x) x-2) 
grade_range_2003$grade_range_2003<- lapply(grade_range_2003$grade_range_2003,function(x) x-2) 
grade_range_2002$grade_range_2002<- lapply(grade_range_2002$grade_range_2002,function(x) x-2) 

### Determine continuous grades for the previous year
  # minus 1 to determine continuous grade in the previous year
grade_range_2015$continuous_2014<- lapply(grade_range_2015$grade_range_2015,function(x) x-1) 
grade_range_2014$continuous_2013<- lapply(grade_range_2014$grade_range_2014,function(x) x-1) 
grade_range_2013$continuous_2012<- lapply(grade_range_2013$grade_range_2013,function(x) x-1) 
grade_range_2012$continuous_2011<- lapply(grade_range_2012$grade_range_2012,function(x) x-1) 
grade_range_2011$continuous_2010<- lapply(grade_range_2011$grade_range_2011,function(x) x-1) 
grade_range_2010$continuous_2009<- lapply(grade_range_2010$grade_range_2010,function(x) x-1) 
grade_range_2009$continuous_2008<- lapply(grade_range_2009$grade_range_2009,function(x) x-1) 
grade_range_2008$continuous_2007<- lapply(grade_range_2008$grade_range_2008,function(x) x-1) 
grade_range_2007$continuous_2006<- lapply(grade_range_2007$grade_range_2007,function(x) x-1) 
grade_range_2006$continuous_2005<- lapply(grade_range_2006$grade_range_2006,function(x) x-1) 
grade_range_2005$continuous_2004<- lapply(grade_range_2005$grade_range_2005,function(x) x-1) 
grade_range_2004$continuous_2003<- lapply(grade_range_2004$grade_range_2004,function(x) x-1) 
grade_range_2003$continuous_2002<- lapply(grade_range_2003$grade_range_2003,function(x) x-1) 
grade_range_2002$continuous_2001<- lapply(grade_range_2002$grade_range_2002,function(x) x-1) 
  # insert first column as row names
grade_range_2002$bn <- rownames(grade_range_2002)
grade_range_2003$bn <- rownames(grade_range_2003)
grade_range_2004$bn <- rownames(grade_range_2004)
grade_range_2005$bn <- rownames(grade_range_2005)
grade_range_2006$bn <- rownames(grade_range_2006)
grade_range_2007$bn <- rownames(grade_range_2007)
grade_range_2008$bn <- rownames(grade_range_2008)
grade_range_2009$bn <- rownames(grade_range_2009)
grade_range_2010$bn <- rownames(grade_range_2010)
grade_range_2011$bn <- rownames(grade_range_2011)
grade_range_2012$bn <- rownames(grade_range_2012)
grade_range_2013$bn <- rownames(grade_range_2013)
grade_range_2014$bn <- rownames(grade_range_2014)
grade_range_2015$bn <- rownames(grade_range_2015)

  # re-order grade_range columns 
grade_range_reorder <- function(grade_range){
  grade_range <- grade_range[c(3,1,2)]
  grade_range
}
grade_range_2002 <- grade_range_reorder(grade_range_2002)
grade_range_2003 <- grade_range_reorder(grade_range_2003)
grade_range_2004 <- grade_range_reorder(grade_range_2004)
grade_range_2005 <- grade_range_reorder(grade_range_2005)
grade_range_2006 <- grade_range_reorder(grade_range_2006)
grade_range_2007 <- grade_range_reorder(grade_range_2007)
grade_range_2008 <- grade_range_reorder(grade_range_2008)
grade_range_2009 <- grade_range_reorder(grade_range_2009)
grade_range_2010 <- grade_range_reorder(grade_range_2010)
grade_range_2011 <- grade_range_reorder(grade_range_2011)
grade_range_2012 <- grade_range_reorder(grade_range_2012)
grade_range_2013 <- grade_range_reorder(grade_range_2013)
grade_range_2014 <- grade_range_reorder(grade_range_2014)
grade_range_2015 <- grade_range_reorder(grade_range_2015)

### Create grade_range_all & print
grade_range_all<-Reduce(function(x, y) merge(x, y, by='bn', all=TRUE), list(grade_range_2014,grade_range_2013,grade_range_2012,grade_range_2011,grade_range_2010,grade_range_2009,grade_range_2008,grade_range_2007,grade_range_2006,grade_range_2005,grade_range_2004,grade_range_2003,grade_range_2002)) 
grade_range_all_char <- data.frame(lapply(grade_range_all, as.character), stringsAsFactors=FALSE)
write.csv(grade_range_all_char, file = '/Users/elmerleezy/Desktop/grade_range_all.csv')

# '*************************  test  *****************************'
# matrix_2 <- data.frame(matrix(apply(matrix,1 ,function(x) which(x==1))))
# matrix_2_list<-apply(matrix,1 ,function(x) which(x==1))
# matrix_3 <- lapply(matrix_2_list, function(x) x[which.max(abs(x))])
# matrix_2$tg <- matrix_3
# 
# matrix_4 <- data.frame(lapply(matrix_2, as.character), stringsAsFactors=FALSE)
# write.csv(matrix_4, file = '/Users/elmerleezy/Desktop/matrix_4.csv')
# '*************************test end *****************************'


### Create Dummy for include_in_dist_calc & include_in_school_calc
# include_in_dist_calc 
Merge_2014_2015$include_in_dist_calc <- ifelse(Merge_2014_2015$grade_level_2014 < 12,1,0)  
Merge_2013_2014$include_in_dist_calc <- ifelse(Merge_2013_2014$grade_level_2013 < 12,1,0)  
Merge_2012_2013$include_in_dist_calc <- ifelse(Merge_2012_2013$grade_level_2012 < 12,1,0)  
Merge_2011_2012$include_in_dist_calc <- ifelse(Merge_2011_2012$grade_level_2011 < 12,1,0)  
Merge_2010_2011$include_in_dist_calc <- ifelse(Merge_2010_2011$grade_level_2010 < 12,1,0)  
Merge_2009_2010$include_in_dist_calc <- ifelse(Merge_2009_2010$grade_level_2009 < 12,1,0)
Merge_2008_2009$include_in_dist_calc <- ifelse(Merge_2008_2009$grade_level_2008 < 12,1,0)
Merge_2007_2008$include_in_dist_calc <- ifelse(Merge_2007_2008$grade_level_2007 < 12,1,0)
Merge_2006_2007$include_in_dist_calc <- ifelse(Merge_2006_2007$grade_level_2006 < 12,1,0)
Merge_2005_2006$include_in_dist_calc <- ifelse(Merge_2005_2006$grade_level_2005 < 12,1,0)
Merge_2004_2005$include_in_dist_calc <- ifelse(Merge_2004_2005$grade_level_2004 < 12,1,0)
Merge_2003_2004$include_in_dist_calc <- ifelse(Merge_2003_2004$grade_level_2003 < 12,1,0)
Merge_2002_2003$include_in_dist_calc <- ifelse(Merge_2002_2003$grade_level_2002 < 12,1,0)
# include_in_school_calc
  # first indicate continuous grades
Merge_2014_2015$continuous_2014 <- grade_range_2015[match(Merge_2014_2015$bn_2014, grade_range_2015$bn),'continuous_2014']
Merge_2013_2014$continuous_2013 <- grade_range_2014[match(Merge_2013_2014$bn_2013, grade_range_2014$bn),'continuous_2013']
Merge_2012_2013$continuous_2012 <- grade_range_2013[match(Merge_2012_2013$bn_2012, grade_range_2013$bn),'continuous_2012']
Merge_2011_2012$continuous_2011 <- grade_range_2012[match(Merge_2011_2012$bn_2011, grade_range_2012$bn),'continuous_2011']
Merge_2010_2011$continuous_2010 <- grade_range_2011[match(Merge_2010_2011$bn_2010, grade_range_2011$bn),'continuous_2010']
Merge_2009_2010$continuous_2009 <- grade_range_2010[match(Merge_2009_2010$bn_2009, grade_range_2010$bn),'continuous_2009']
Merge_2008_2009$continuous_2008 <- grade_range_2009[match(Merge_2008_2009$bn_2008, grade_range_2009$bn),'continuous_2008']
Merge_2007_2008$continuous_2007 <- grade_range_2008[match(Merge_2007_2008$bn_2007, grade_range_2008$bn),'continuous_2007']
Merge_2006_2007$continuous_2006 <- grade_range_2007[match(Merge_2006_2007$bn_2006, grade_range_2007$bn),'continuous_2006']
Merge_2005_2006$continuous_2005 <- grade_range_2006[match(Merge_2005_2006$bn_2005, grade_range_2006$bn),'continuous_2005']
Merge_2004_2005$continuous_2004 <- grade_range_2005[match(Merge_2004_2005$bn_2004, grade_range_2005$bn),'continuous_2004']
Merge_2003_2004$continuous_2003 <- grade_range_2004[match(Merge_2003_2004$bn_2003, grade_range_2004$bn),'continuous_2003']
Merge_2002_2003$continuous_2002 <- grade_range_2003[match(Merge_2002_2003$bn_2002, grade_range_2003$bn),'continuous_2002']
  # then use if condition
library(purrr)
Merge_2014_2015$include_in_school_calc <- ifelse(map2_lgl(Merge_2014_2015$grade_level_2014,Merge_2014_2015$continuous_2014,is.element),1,ifelse(map_lgl(Merge_2014_2015$continuous_2014,is.null),NA,0))
Merge_2013_2014$include_in_school_calc <- ifelse(map2_lgl(Merge_2013_2014$grade_level_2013,Merge_2013_2014$continuous_2013,is.element),1,ifelse(map_lgl(Merge_2013_2014$continuous_2013,is.null),NA,0))
Merge_2012_2013$include_in_school_calc <- ifelse(map2_lgl(Merge_2012_2013$grade_level_2012,Merge_2012_2013$continuous_2012,is.element),1,ifelse(map_lgl(Merge_2012_2013$continuous_2012,is.null),NA,0))
Merge_2011_2012$include_in_school_calc <- ifelse(map2_lgl(Merge_2011_2012$grade_level_2011,Merge_2011_2012$continuous_2011,is.element),1,ifelse(map_lgl(Merge_2011_2012$continuous_2011,is.null),NA,0))
Merge_2010_2011$include_in_school_calc <- ifelse(map2_lgl(Merge_2010_2011$grade_level_2010,Merge_2010_2011$continuous_2010,is.element),1,ifelse(map_lgl(Merge_2010_2011$continuous_2010,is.null),NA,0))
Merge_2009_2010$include_in_school_calc <- ifelse(map2_lgl(Merge_2009_2010$grade_level_2009,Merge_2009_2010$continuous_2009,is.element),1,ifelse(map_lgl(Merge_2009_2010$continuous_2009,is.null),NA,0))
Merge_2008_2009$include_in_school_calc <- ifelse(map2_lgl(Merge_2008_2009$grade_level_2008,Merge_2008_2009$continuous_2008,is.element),1,ifelse(map_lgl(Merge_2008_2009$continuous_2008,is.null),NA,0))
Merge_2007_2008$include_in_school_calc <- ifelse(map2_lgl(Merge_2007_2008$grade_level_2007,Merge_2007_2008$continuous_2007,is.element),1,ifelse(map_lgl(Merge_2007_2008$continuous_2007,is.null),NA,0))
Merge_2006_2007$include_in_school_calc <- ifelse(map2_lgl(Merge_2006_2007$grade_level_2006,Merge_2006_2007$continuous_2006,is.element),1,ifelse(map_lgl(Merge_2006_2007$continuous_2006,is.null),NA,0))
Merge_2005_2006$include_in_school_calc <- ifelse(map2_lgl(Merge_2005_2006$grade_level_2005,Merge_2005_2006$continuous_2005,is.element),1,ifelse(map_lgl(Merge_2005_2006$continuous_2005,is.null),NA,0))
Merge_2004_2005$include_in_school_calc <- ifelse(map2_lgl(Merge_2004_2005$grade_level_2004,Merge_2004_2005$continuous_2004,is.element),1,ifelse(map_lgl(Merge_2004_2005$continuous_2004,is.null),NA,0))
Merge_2003_2004$include_in_school_calc <- ifelse(map2_lgl(Merge_2003_2004$grade_level_2003,Merge_2003_2004$continuous_2003,is.element),1,ifelse(map_lgl(Merge_2003_2004$continuous_2003,is.null),NA,0))
Merge_2002_2003$include_in_school_calc <- ifelse(map2_lgl(Merge_2002_2003$grade_level_2002,Merge_2002_2003$continuous_2002,is.element),1,ifelse(map_lgl(Merge_2002_2003$continuous_2002,is.null),NA,0))

#########################################
##  CATEGORIZE STUDENTS ##
#########################################
### Create dummies for retained_in_school, retained_in_district

  # retained_in_district
Merge_2014_2015$retained_in_district <- ifelse(Merge_2014_2015$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2014_2015$bn_2015),1,0))
Merge_2013_2014$retained_in_district <- ifelse(Merge_2013_2014$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2013_2014$bn_2014),1,0))
Merge_2012_2013$retained_in_district <- ifelse(Merge_2012_2013$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2012_2013$bn_2013),1,0))
Merge_2011_2012$retained_in_district <- ifelse(Merge_2011_2012$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2011_2012$bn_2012),1,0))
Merge_2010_2011$retained_in_district <- ifelse(Merge_2010_2011$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2010_2011$bn_2011),1,0))
Merge_2009_2010$retained_in_district <- ifelse(Merge_2009_2010$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2009_2010$bn_2010),1,0))
Merge_2008_2009$retained_in_district <- ifelse(Merge_2008_2009$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2008_2009$bn_2009),1,0))
Merge_2007_2008$retained_in_district <- ifelse(Merge_2007_2008$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2007_2008$bn_2008),1,0))
Merge_2006_2007$retained_in_district <- ifelse(Merge_2006_2007$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2006_2007$bn_2007),1,0))
Merge_2005_2006$retained_in_district <- ifelse(Merge_2005_2006$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2005_2006$bn_2006),1,0))
Merge_2004_2005$retained_in_district <- ifelse(Merge_2004_2005$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2004_2005$bn_2005),1,0))
Merge_2003_2004$retained_in_district <- ifelse(Merge_2003_2004$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2003_2004$bn_2004),1,0))
Merge_2002_2003$retained_in_district <- ifelse(Merge_2002_2003$include_in_dist_calc == 0,NA,ifelse(!is.na(Merge_2002_2003$bn_2003),1,0))
  # retained_in_school
Merge_2014_2015$retained_in_school <- ifelse(Merge_2014_2015$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2014_2015$bn_2015),0,
                                                    ifelse(Merge_2014_2015$bn_2014 == Merge_2014_2015$bn_2015,1,0)))

Merge_2013_2014$retained_in_school <- ifelse(Merge_2013_2014$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2013_2014$bn_2014),0,
                                                    ifelse(Merge_2013_2014$bn_2013 == Merge_2013_2014$bn_2014,1,0)))

Merge_2012_2013$retained_in_school <- ifelse(Merge_2012_2013$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2012_2013$bn_2013),0,
                                                    ifelse(Merge_2012_2013$bn_2012 == Merge_2012_2013$bn_2013,1,0)))

Merge_2011_2012$retained_in_school <- ifelse(Merge_2011_2012$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2011_2012$bn_2012),0,
                                                    ifelse(Merge_2011_2012$bn_2011 == Merge_2011_2012$bn_2012,1,0)))

Merge_2010_2011$retained_in_school <- ifelse(Merge_2010_2011$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2010_2011$bn_2011),0,
                                                    ifelse(Merge_2010_2011$bn_2010 == Merge_2010_2011$bn_2011,1,0)))

Merge_2009_2010$retained_in_school <- ifelse(Merge_2009_2010$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2009_2010$bn_2010),0,
                                                    ifelse(Merge_2009_2010$bn_2009 == Merge_2009_2010$bn_2010,1,0)))

Merge_2008_2009$retained_in_school <- ifelse(Merge_2008_2009$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2008_2009$bn_2009),0,
                                                    ifelse(Merge_2008_2009$bn_2008 == Merge_2008_2009$bn_2009,1,0)))

Merge_2007_2008$retained_in_school <- ifelse(Merge_2007_2008$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2007_2008$bn_2008),0,
                                                    ifelse(Merge_2007_2008$bn_2007 == Merge_2007_2008$bn_2008,1,0)))

Merge_2006_2007$retained_in_school <- ifelse(Merge_2006_2007$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2006_2007$bn_2007),0,
                                                    ifelse(Merge_2006_2007$bn_2006 == Merge_2006_2007$bn_2007,1,0)))

Merge_2005_2006$retained_in_school <- ifelse(Merge_2005_2006$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2005_2006$bn_2006),0,
                                                    ifelse(Merge_2005_2006$bn_2005 == Merge_2005_2006$bn_2006,1,0)))

Merge_2004_2005$retained_in_school <- ifelse(Merge_2004_2005$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2004_2005$bn_2005),0,
                                                    ifelse(Merge_2004_2005$bn_2004 == Merge_2004_2005$bn_2005,1,0)))

Merge_2003_2004$retained_in_school <- ifelse(Merge_2003_2004$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2003_2004$bn_2004),0,
                                                    ifelse(Merge_2003_2004$bn_2003 == Merge_2003_2004$bn_2004,1,0)))

Merge_2002_2003$retained_in_school <- ifelse(Merge_2002_2003$include_in_school_calc == 0,NA,
                                             ifelse(is.na(Merge_2002_2003$bn_2003),0,
                                                    ifelse(Merge_2002_2003$bn_2002 == Merge_2002_2003$bn_2003,1,0)))
