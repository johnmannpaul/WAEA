#prologue
source("constants.R")

# min.N.achievement <-  6
# min.N.growth <- 6
# min.N.subgroup <- 6

min.N.achievement <-  10
min.N.growth <- 10
min.N.subgroup <- 10

min.N.achievement.multiyear <-  10
min.N.growth.multiyear <- 10
min.N.subgroup.multiyear <- 10

source("function-defs.R")
source("services.R")
source("initialize-schools.R")
#start non-high schools
source("initialize-paws.R")

source("small-achievement.R")
source("achievement-bands.R")

source("small-growth.R")
source("growth.R")

source("equity-multiyear.R")

source("participation.R")
source("SPL-nonHS.R")
##finish
state.school <- with(schools, schools[SCHOOL_ID==state.school.id,])
schools <- with(schools, schools[SCHOOL_ID != state.school.id,])
