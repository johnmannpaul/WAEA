#prologue
source("constants.R")
source("function-defs.R")
source("participation-fun.R")
source("services.R")
source("initialize-schools.R")
source("mean-z-score-fun.R")
source("calc-small-schools-fun.R")
source("participation-fun.R")
source("compute-indicator.R")

#start non-high schools
schools <- schools[schools$SCHOOL_YEAR==current.school.year,]  #limit to current school year

#source("g38-lookback-2013-14.R")  #run when data changes
source("achievement.R")
source("growth.R")

#source("equity-z-scores.R") #run when data changes
source("equity.R")
source("SPL-nonHS.R")

##start high schools
#source("act-lookback-2013-14.R")  #run when data changes
source("achievement-act.R")
source("high-school-equity.R")
source("readiness-ref.R")
source("grade-nine-credits.R")
source("hathaway-eligibility.R")
source("grad-rate-ref.R")
source("additional-readiness-types.R")
source("SPL-HS.R")
