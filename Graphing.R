########################################
########################################
##                                    ##
##                                    ##
##         Create Graphs              ##
##                                    ##
##                                    ##
########################################
########################################


########################################
## District Retention ##
########################################

	# District retention by year
ggplot(data=dist_wide_retention_all, aes(x=year, y=retention_rate, group=1)) + 
    geom_line(colour="red", linetype="dashed", size=0.1) + 
    geom_point(colour="red", size=2, shape=21, fill="white")+
    expand_limits(y=0.85) +
    xlab("Year") + ylab("District Retention Rate") +
    ggtitle("District Retention")
    ggplotly()

    # District retention by grade(2015)
library(RColorBrewer)
    	# convert grade level to char
dist_wide_retention_2015$grade_level <- c('PK','0K','1','2','3','4','5','6','7','8','9','10','11') 
    	# lock the positions
positions <- c('PK','0K','1','2','3','4','5','6','7','8','9','10','11') 
    	# create bar graph
ggplot(data=dist_wide_retention_2015, aes(x=grade_level, y=rate_2015)) +
	geom_bar( stat="identity",fill='steelblue') + #set color, with black line outside
	scale_x_discrete(limits = positions)+
	  xlab("Grade") + ylab("District Retention Rate") +
	  ggtitle("District Retention by Grade (2015)")

   # District retention, by grade - Table 2
   # Prepare: 
       	# convert grade level to char
		dist_wide_retention_all_grade$grade_level <- c('PK','0K','1','2','3','4','5','6','7','8','9','10','11') 
    	# tranpose the table
		dist_all <- dist_wide_retention_all_grade[,c('grade_level', 'rate_2003', 'rate_2004', 'rate_2005', 'rate_2006', 'rate_2007', 'rate_2008', 'rate_2009', 'rate_2010', 'rate_2011', 'rate_2012', 'rate_2013', 'rate_2014', 'rate_2015')]
		dist_all_t <- data.frame(t(dist_all[,2:ncol(dist_all)]))
		colnames(dist_all_t) <- dist_all[,1]
		dist_all_t$year <- c('2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015')

		dist_all_long <- melt(dist_all_t , id="year", variable.name = 'grade')  # convert to long format
	#achieve
    ggplot(subset(dist_all_long), aes(x=value, fill=grade)) +
    geom_histogram(binwidth=0.05) +
    facet_grid(grade ~ .) +
    ggtitle("District Retention Rate, by Year & Grade") 
    ggplotly()
    
   # District retention by grade - Table 3
    ggplot(data=dist_all_long,
	aes(x=grade, y=value,fill= grade)) + # for white bar, use "color = Grade"
	geom_boxplot()+
    ggtitle("District Retention Rate, by Grade") 
    ggplotly()
  
   # District retention, by grade, by year
ggplot(data=dist_all_long,
	aes(x=year, y=value, group = grade, color = grade)) +
	geom_line() +
  geom_point(alpha=.3) + 
   # geom_smooth(alpha=.2, size=1) + # this creates a smooth curve with a range
	xlab("Year") + ylab("District Retention Rate by Grade") +
	ggtitle("District Retention Rate by Year & Grade") 
  ggplotly()


########################################
## Scholl Wide Retention ##
########################################

### Distribution of retention, 2015
rate_2015.mean <- mean(sch_wide_retention_2015$rate_2015)

ggplot(sch_wide_retention_2015, aes(x=rate_2015)) +
    geom_density(color="steelblue2")+
    geom_vline(data=sch_wide_retention_2015, aes(xintercept=rate_2015.mean),
               linetype="dashed", size=.5, color="brown2") + 
  	xlab("Retention Rate") + ylab("Density") +
  	ggtitle("Distribution of Retention(2015)") 

### Distribution of retention, by grade
sch_wide_retention_2015$grade_level_char <- as.character(sch_wide_retention_2015$grade_level)

ggplot(sch_wide_retention_2015, aes(x=rate_2015, color=grade_level_char)) +
    geom_density()+
	xlab("Retention Rate") + ylab("Density") +
	ggtitle("Distribution of Retention, by Grade (2015)") +
	scale_color_discrete(name="Grade") # legend titile # if use fill in ggplot, here is fill_discrete

### Change in retention, 2015 - Example School
	# Recalculate the retention by school 
sch_retention_2015 <- data.frame(aggregate(cbind(Merge_2014_2015$retained_in_school, Merge_2014_2015$include_in_school_calc) ~ bn_2014, Merge_2014_2015,sum))
sch_retention_2014 <- data.frame(aggregate(cbind(Merge_2013_2014$retained_in_school, Merge_2013_2014$include_in_school_calc) ~ bn_2013, Merge_2013_2014,sum))
sch_retention_2013 <- data.frame(aggregate(cbind(Merge_2012_2013$retained_in_school, Merge_2012_2013$include_in_school_calc) ~ bn_2012, Merge_2012_2013,sum))
sch_retention_2012 <- data.frame(aggregate(cbind(Merge_2011_2012$retained_in_school, Merge_2011_2012$include_in_school_calc) ~ bn_2011, Merge_2011_2012,sum))
sch_retention_2011 <- data.frame(aggregate(cbind(Merge_2010_2011$retained_in_school, Merge_2010_2011$include_in_school_calc) ~ bn_2010, Merge_2010_2011,sum))
sch_retention_2010 <- data.frame(aggregate(cbind(Merge_2009_2010$retained_in_school, Merge_2009_2010$include_in_school_calc) ~ bn_2009, Merge_2009_2010,sum))
sch_retention_2009 <- data.frame(aggregate(cbind(Merge_2008_2009$retained_in_school, Merge_2008_2009$include_in_school_calc) ~ bn_2008, Merge_2008_2009,sum))
sch_retention_2008 <- data.frame(aggregate(cbind(Merge_2007_2008$retained_in_school, Merge_2007_2008$include_in_school_calc) ~ bn_2007, Merge_2007_2008,sum))
sch_retention_2007 <- data.frame(aggregate(cbind(Merge_2006_2007$retained_in_school, Merge_2006_2007$include_in_school_calc) ~ bn_2006, Merge_2006_2007,sum))
sch_retention_2006 <- data.frame(aggregate(cbind(Merge_2005_2006$retained_in_school, Merge_2005_2006$include_in_school_calc) ~ bn_2005, Merge_2005_2006,sum))
sch_retention_2005 <- data.frame(aggregate(cbind(Merge_2004_2005$retained_in_school, Merge_2004_2005$include_in_school_calc) ~ bn_2004, Merge_2004_2005,sum))
sch_retention_2004 <- data.frame(aggregate(cbind(Merge_2003_2004$retained_in_school, Merge_2003_2004$include_in_school_calc) ~ bn_2003, Merge_2003_2004,sum))
sch_retention_2003 <- data.frame(aggregate(cbind(Merge_2002_2003$retained_in_school, Merge_2002_2003$include_in_school_calc) ~ bn_2002, Merge_2002_2003,sum))

sch_retention_2015$rate <- (sch_retention_2015$V1)/(sch_retention_2015$V2)
sch_retention_2014$rate <- (sch_retention_2014$V1)/(sch_retention_2014$V2)
sch_retention_2013$rate <- (sch_retention_2013$V1)/(sch_retention_2013$V2)
sch_retention_2012$rate <- (sch_retention_2012$V1)/(sch_retention_2012$V2)
sch_retention_2011$rate <- (sch_retention_2011$V1)/(sch_retention_2011$V2)
sch_retention_2010$rate <- (sch_retention_2010$V1)/(sch_retention_2010$V2)
sch_retention_2009$rate <- (sch_retention_2009$V1)/(sch_retention_2009$V2)
sch_retention_2008$rate <- (sch_retention_2008$V1)/(sch_retention_2008$V2)
sch_retention_2007$rate <- (sch_retention_2007$V1)/(sch_retention_2007$V2)
sch_retention_2006$rate <- (sch_retention_2006$V1)/(sch_retention_2006$V2)
sch_retention_2005$rate <- (sch_retention_2005$V1)/(sch_retention_2005$V2)
sch_retention_2004$rate <- (sch_retention_2004$V1)/(sch_retention_2004$V2)
sch_retention_2003$rate <- (sch_retention_2003$V1)/(sch_retention_2003$V2)

  # Reorder and re-name
sch_retention_2015 <- sch_retention_2015[c(1,4)]
sch_retention_2014 <- sch_retention_2014[c(1,4)]
sch_retention_2013 <- sch_retention_2013[c(1,4)]
sch_retention_2012 <- sch_retention_2012[c(1,4)]
sch_retention_2011 <- sch_retention_2011[c(1,4)]
sch_retention_2010 <- sch_retention_2010[c(1,4)]
sch_retention_2009 <- sch_retention_2009[c(1,4)]
sch_retention_2008 <- sch_retention_2008[c(1,4)]
sch_retention_2007 <- sch_retention_2007[c(1,4)]
sch_retention_2006 <- sch_retention_2006[c(1,4)]
sch_retention_2005 <- sch_retention_2005[c(1,4)]
sch_retention_2004 <- sch_retention_2004[c(1,4)]
sch_retention_2003 <- sch_retention_2003[c(1,4)]

names(sch_retention_2003) <- c('bn','rate_2003')
names(sch_retention_2004) <- c('bn','rate_2004')
names(sch_retention_2005) <- c('bn','rate_2005')
names(sch_retention_2006) <- c('bn','rate_2006')
names(sch_retention_2007) <- c('bn','rate_2007')
names(sch_retention_2008) <- c('bn','rate_2008')
names(sch_retention_2009) <- c('bn','rate_2009')
names(sch_retention_2010) <- c('bn','rate_2010')
names(sch_retention_2011) <- c('bn','rate_2011')
names(sch_retention_2012) <- c('bn','rate_2012')
names(sch_retention_2013) <- c('bn','rate_2013')
names(sch_retention_2014) <- c('bn','rate_2014')
names(sch_retention_2015) <- c('bn','rate_2015')

# Create a summary table
sch_retention_all<-Reduce(function(x, y) merge(x, y, by=c('bn'), all=TRUE), list(sch_retention_2003, sch_retention_2004, sch_retention_2005, sch_retention_2006, sch_retention_2007, sch_retention_2008, sch_retention_2009, sch_retention_2010, sch_retention_2011, sch_retention_2012, sch_retention_2013, sch_retention_2014, sch_retention_2015 ))

sch_retention_change <- data.frame(sch_retention_all$bn)
colnames(sch_retention_change)[1] <- 'bn'
sch_retention_change$change_2004 <- sch_retention_all$rate_2004 - sch_retention_all$rate_2003 
sch_retention_change$change_2005 <- sch_retention_all$rate_2005 - sch_retention_all$rate_2004 
sch_retention_change$change_2006 <- sch_retention_all$rate_2006 - sch_retention_all$rate_2005 
sch_retention_change$change_2007 <- sch_retention_all$rate_2007 - sch_retention_all$rate_2006 
sch_retention_change$change_2008 <- sch_retention_all$rate_2008 - sch_retention_all$rate_2007 
sch_retention_change$change_2009 <- sch_retention_all$rate_2009 - sch_retention_all$rate_2008 
sch_retention_change$change_2010 <- sch_retention_all$rate_2010 - sch_retention_all$rate_2009 
sch_retention_change$change_2011 <- sch_retention_all$rate_2011 - sch_retention_all$rate_2010 
sch_retention_change$change_2012 <- sch_retention_all$rate_2012 - sch_retention_all$rate_2011 
sch_retention_change$change_2013 <- sch_retention_all$rate_2013 - sch_retention_all$rate_2012 
sch_retention_change$change_2014 <- sch_retention_all$rate_2014 - sch_retention_all$rate_2013 
sch_retention_change$change_2015 <- sch_retention_all$rate_2015 - sch_retention_all$rate_2014 

sch_retention_change_t <- data.frame(t(sch_retention_change[,2:ncol(sch_retention_change)]))
colnames(sch_retention_change_t) <- sch_retention_change[,1]
sch_retention_change_t$year <- c('2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015')
sch_retention_change_t <- sch_retention_change_t[c(2061,1:2060)]

ggplot(data=sch_retention_change_t, aes(x=year, y=K001, group =1)) + 
    geom_line(colour="tomato1", linetype="dashed", size=0.1) + 
    geom_point(alpha=.3)+
    #expand_limits(y=0.85) +
    xlab("Year") + ylab("Retention Rate") +
    ggtitle("School Retention Rate - K001")

sch_retention_change$change_net <- rowSums(sch_retention_change[2:13]) 

ggplot(data=sch_retention_change,aes(x=bn,y=change_net, fill = bn)) +
    geom_bar(data = subset(sch_retention_change, change_net > 0),stat='identity',color="steelblue2") +
    geom_bar(data = subset(sch_retention_change, change_net < 0),stat='identity',color="tomato1") +
    scale_x_discrete(drop=FALSE)

    ggplot(data=dane,aes(x=x,y=y,fill=g)) + 
    geom_bar(data = subset(dane, g=='A'),stat='identity') +
    geom_bar(data = subset(dane, g=='A'),stat='identity') +
    scale_x_discrete(drop=FALSE)












