# #sourced from data/2013-08-26-2011-12-returning nongrads.xlsx
# #save(returning_nongrads, file="data/returning_nongrads.Rdata")
# load(file="data/returning_nongrads.Rdata")
# returning_nongrads$SCHOOL_YEAR <- '2012-13'
# 
# 
# returning_nongrads$INDEX <- rep('Returning', nrow(returning_nongrads)) 
# head(returning_nongrads)
# #sourced from data/2013-08-26-2011-12-grads and nongrads.xlsx
# #save(grads_nongrads, file="data/grads_nongrads.Rdata")
# load(file="data/grads_nongrads.Rdata")
# grads_nongrads$SCHOOL_YEAR <- '2012-13'
# 
# 
# grads_nongrads$INDEX <- apply(grads_nongrads[,c("GRAD_RATE_TYPE_CODE", "GRAD_STATUS")],
#                               c(1),
#                               function (student) {
#                                 type.code = student[["GRAD_RATE_TYPE_CODE"]]
#                                 status = student[["GRAD_STATUS"]]
#                                 ifelse(status == 'Non-Graduate', 'Non-Graduate', type.code)
#                               })
# 
# head(grads_nongrads)
# 
# grads.df <- rbind(grads_nongrads[,c("SCHOOL_YEAR", "SCHOOL_ID", "INDEX")],
#                   returning_nongrads[,c("SCHOOL_YEAR", "SCHOOL_ID", "INDEX")])


##correction 10/10/2013
##based on data/2013-10-10-grad-index-students-all-corrected.csv
#save(grads_nongrads_corrected, file="data/grads_nongrads_corrected.Rdata")
load(file="data/grads_nongrads_corrected.Rdata")
grads_nongrads_corrected$INDEX <- apply(grads_nongrads_corrected[,c("GRAD_TYPE", "VALUE")],
                                        c(1),
                                        function (student) {
                                          grad.type = student[["GRAD_TYPE"]]
                                          value = student[["VALUE"]]
                                          ifelse(value == '100', grad.type, 
                                                 ifelse(value == '50',
                                                        'Returning',
                                                        'Non-Graduate'))
                                        })
table(grads_nongrads_corrected[grads_nongrads_corrected$SCHOOL_ID %in% c('0706055', '0706056'),]$SCHOOL_ID)
grads_nongrads_corrected$SCHOOL_ID <- ifelse(grads_nongrads_corrected$SCHOOL_ID == '0706055', '0706056', grads_nongrads_corrected$SCHOOL_ID)
table(grads_nongrads_corrected[grads_nongrads_corrected$SCHOOL_ID %in% c('0706055', '0706056'),]$SCHOOL_ID)


#replace grads.df with corrected value
grads.df <- grads_nongrads_corrected[,c("SCHOOL_YEAR", "SCHOOL_ID", "INDEX")]




grads.average.index <- calc.indexed.grads(schools, grads.df)


with(grads.average.index, grads.average.index[SCHOOL_ID==state.school.id,])
head(grads.average.index)

schools <- calc.school.grad.index(schools)