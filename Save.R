# method 1
count_b.list <- split(count_b, seq(nrow(count_b)))
data_2002.list <- split(data_2002, seq(nrow(data_2002)))

# method 2 
sch_retention_all.list <- as.list(as.data.frame(t(sch_retention_all)))
sch_retention_change.list <- as.list(as.data.frame(t(sch_retention_change)))
sch_retention_change_t.list <- as.list(as.data.frame(t(sch_retention_change_t)))

dist_all.list <- as.list(as.data.frame(t(dist_all)))
dist_all_long.list <- as.list(as.data.frame(t(dist_all_long)))
dist_all_t.list <- as.list(as.data.frame(t(dist_all_t)))


# reture to dataframe
data_2002.back <- do.call(rbind.data.frame, data_2002.list)
sch_retention_all.back <- do.call(rbind.data.frame, sch_retention_all.list)