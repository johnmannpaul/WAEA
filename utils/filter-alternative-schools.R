with(schools.multiyear.subgroup, 
     schools.multiyear.subgroup[DISTRICT_ID %in% c('1809000','1301000', '1501000'),]$DISTRICT_NAME)



names(grads_nongrads_corrected_full) <- unlist(lapply(names(grads_nongrads_corrected_full), 
                                                      function (x) 
                                                        toupper(do.call(
                                                          paste, as.list(c(strsplit(x,"[.]")[[1]],sep="_"))))))


do.call(paste, as.list(c(unlist(strsplit("School.Id","[.]")),sep="_")))


schools[!is.na(schools$ALTERNATIVE_SCHOOL) & 
          (schools$ALTERNATIVE_SCHOOL == 'T' &  !is.na(schools$SPL_HS)) & 
          schools$SCHOOL_YEAR==current.school.year,]

table(schools$ALTERNATIVE_SCHOOL, useNA="ifany")
#schools minus the alternative high schools (the few other alternative non-high schools are included)
schools.no.alt <- schools[!is.na(schools$ALTERNATIVE_SCHOOL) & 
                            (schools$ALTERNATIVE_SCHOOL == 'F'  |  is.na(schools$SPL_HS)),]
nrow(schools[schools$SCHOOL_YEAR==current.school.year,])
nrow(schools.no.alt[schools.no.alt$SCHOOL_YEAR==current.school.year,])

schools <- schools.no.alt
