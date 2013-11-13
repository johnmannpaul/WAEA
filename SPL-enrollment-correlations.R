#correlations of elementary and middle school SPLs with Enrollments

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"SPL"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"SPL"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"ENROLLMENT"],
                  method=c("spearman")))


with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"SPL_ADJUSTED"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"SPL_ADJUSTED"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),"ENROLLMENT"],
                  method=c("spearman")))

nrow(with(schools, schools[SCHOOL_YEAR==current.school.year & !is.na(SPL),]))

#correlations of high school SPLs with Enrollments

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"SPL_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"SPL_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"ENROLLMENT"],
                  method=c("spearman")))


with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"SPL_ADJUSTED_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"SPL_ADJUSTED_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),"ENROLLMENT"],
                  method=c("spearman")))

nrow(with(schools, schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS),]))


#correlations of all school SPL with enrollment
schools.SPL.HS <- with(schools, schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS), 
                                        c("SCHOOL_ID", "SPL_HS", "SPL_ADJUSTED_HS", "ENROLLMENT")])
names(schools.SPL.HS) <- c("SCHOOL_ID", "SPL", "SPL_ADJUSTED", "ENROLLMENT")

schools.SPL.all <- with(schools, rbind(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL), c("SCHOOL_ID", "SPL", "SPL_ADJUSTED", "ENROLLMENT")],
                     schools.SPL.HS))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                  method=c("pearson")))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                  method=c("spearman")))


with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL_ADJUSTED"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("pearson")))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL_ADJUSTED"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("spearman")))

nrow(schools.SPL.all)



#correlations of non-alternative high school SPLs with Enrollments

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"SPL_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"SPL_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"ENROLLMENT"],
                  method=c("spearman")))


with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"SPL_ADJUSTED_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"ENROLLMENT"],
                  method=c("pearson")))

with(schools, cor(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"SPL_ADJUSTED_HS"], 
                  schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',"ENROLLMENT"],
                  method=c("spearman")))

nrow(with(schools, schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS) & ALTERNATIVE_SCHOOL=='F',]))




#correlations of all non-alternative school SPL with enrollment
schools.SPL.HS <- with(schools, schools[SCHOOL_YEAR==current.school.year & !is.na(SPL_HS)  & ALTERNATIVE_SCHOOL=='F', 
                                        c("SCHOOL_ID", "SPL_HS", "SPL_ADJUSTED_HS", "ENROLLMENT")])
names(schools.SPL.HS) <- c("SCHOOL_ID", "SPL", "SPL_ADJUSTED", "ENROLLMENT")

schools.SPL.all <- with(schools, rbind(schools[SCHOOL_YEAR==current.school.year & !is.na(SPL), c("SCHOOL_ID", "SPL", "SPL_ADJUSTED", "ENROLLMENT")],
                                       schools.SPL.HS))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("pearson")))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("spearman")))


with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL_ADJUSTED"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("pearson")))

with(schools.SPL.all, cor(schools.SPL.all[!is.na(SPL),"SPL_ADJUSTED"], 
                          schools.SPL.all[!is.na(SPL),"ENROLLMENT"],
                          method=c("spearman")))

nrow(schools.SPL.all)
