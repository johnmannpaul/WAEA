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
#source("initialize-paws.R")
# source("small-schools.R")
source("achievement.R")
source("growth.R")
source("equity-z-scores.R")
# source("participation.R")
source("SPL-nonHS.R")
##start high schools
#schools <- schools[schools$SCHOOL_YEAR==current.school.year,]  #limit to current school year
source("achievement-act.R")
source("high-school-equity.R")
source("readiness-ref.R")
source("grade-nine-credits.R")
source("hathaway-eligibility.R")
source("grad-rate-ref.R")
source("additional-readiness-types.R")

# source("init-HS-achievement.R")

# source("readiness-ref.R")
# source("grad-rate-ref.R")
# source("total-readiness.R")

# source("SPL-HS.R")
##finish
#source("schools-fin.R")