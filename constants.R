library("reshape")
options(stringsAsFactors=FALSE)
current.school.year <- '2013-14'
prior.school.year <- '2012-13'
precision <- 1
#tested readiness, grade nine credits, and hathaway eligibility subindicators will all 
#be rounded to the nearest whole number
precision.readiness <- 0
precision.add.readiness <- 0
z.score.precision <- 3
#
#non HS minimum N values
min.N.achievement <-  10
min.N.growth <- 10
min.N.subgroup <- 10

min.N.achievement.multiyear <-  10
min.N.growth.multiyear <- 10
min.N.subgroup.multiyear <- 10

#HS minimum N values
min.N.achievement.hs <- 10
min.N.tested.readiness <- 10
min.N.grad <- 10
min.N.readiness.hs <- 10
min.N.equity.hs <- 10

min.N.achievement.hs.multiyear <- 10
min.N.grade.nine.credits <- 10
min.N.hath.eligibility <- 10

nonHS.types = c(1,3,4,5)
HS.types = c(2,4,5)
paired.types = c(6,7)

state.school.id = '7700000'

type.lookup <- list(`P-5`=1, 
                    `P-6`=1, 
                    `K-4`=1,
                    `K-5`=1, 
                    `K-6`=1,
                    `3-4`=1,
                    `3-5`=1,
                    `3-6`=1,
                    `4-5`=1,
                    `4-6`=1,
                    `5-5`=1,
                    `5-6`=1,
                    `7-8`=1,
                    `7-9`=1,
                    `9-12`=2,
                    `10-12`=2,
                    `P-8`=3,
                    `K-8`=3,
                    `K-9`=3,
                    `5-8`=3,
                    `6-8`=3,
                    `6-9`=3,
                    `P-12`=4,
                    `K-12`=4,
                    `6-12`=4,
                    `7-12`=5,
                    `8-12`=5,
                    `K-3`=6,
                    `2-3`=6,
                    `K-1`=7,
                    `K-2`=7,
                    `P-3`=7)

band.lookup <- list(`03`=1,
                    `04`=1,
                    `05`=1,
                    `06`=1,
                    `07`=2,
                    `08`=2,
                    `11`=3)



#PODER Academy (1101040) is of type 6, but current not paired with any school
school.pairing.lookup <- list(`2010-11` = list(`0501002` = '0501010',
                                               `0701007` = '0701009',
                                               `0701008` = '0701009',
                                               `0706001` = '0706002',
                                               `0725001` = '0725007',
                                               `0725005` = '0725007',
                                               `0725003` = '0725007',
                                               `0801007` = '0801006',
                                               `1001006` = '1001002',
                                               `1101021` = '1101013',
                                               `1101010` = '1101022',
                                               `1202001` = '1202005',
                                               `1202003` = '1202004',
                                               `1601003` = '1601005',
                                               `2001010` = '2001009',
                                               `2104001` = '2104002',
                                               `2301003` = '2301001'),
                              `2011-12` = list(`0501002` = '0501010',
                                               `0701007` = '0701009',
                                               `0701008` = '0701009',
                                               `0706001` = '0706002',
                                               `0725001` = '0725007',
                                               `0725005` = '0725007',
                                               `0725003` = '0725007',
                                               `0801007` = '0801006',
                                               `1001006` = '1001002',
                                               `1101021` = '1101013',
                                               `1101010` = '1101022',
                                               `1202001` = '1202005',
                                               `1202003` = '1202004',
                                               `1601003` = '1601005',
                                               `2001010` = '2001009',
                                               `2104001` = '2104002',
                                               `2301003` = '2301001'),
                              `2012-13` = list(`0501002` = '0501010',
                                               `0701007` = '0701009',
                                               `0701008` = '0701009',
                                               `0706001` = '0706002',
                                               `0725001` = '0725007',
                                               `0725005` = '0725007',
                                               `0725003` = '0725007',
                                               `0801007` = '0801006',
                                               `1001006` = '1001002',
                                               `1101021` = '1101013',
                                               `1101010` = '1101022',
                                               `1202001` = '1202005',
                                               `1202003` = '1202004',
                                               `1601003` = '1601005',
                                               `2001010` = '2001009',
                                               `2104001` = '2104002',
                                               `2301003` = '2301001'),
                              `2013-14` = list(`0501002` = '0501010',
                                               `0501013` = '0501010',
                                               `0701007` = '0701009',
                                               `0701008` = '0701009',
                                               `0706001` = '0706002',
                                               `0725001` = '0725007',
                                               `0725005` = '0725007',
                                               `0725003` = '0725007',
                                               `0801007` = '0801006',
                                               `1001006` = '1001002',
                                               `1101021` = '1101013',
                                               `1101010` = '1101022',
                                               `1202001` = '1202005',
                                               `1202003` = '1202004',
                                               `1601003` = '1601005',
                                               `2001010` = '2001009',
                                               `2104001` = '2104002',
                                               `2301003` = '2301001')
)

participation.level.lookup <- c(90, 95)

participation.labels <- c("Not Met", "Docked", "Met")
indicator.labels <- c("Below Targets", "Meeting Targets", "Exceeding Targets")
SPL.labels <- c("Not Meeting Expectations",
                "Partially Meeting Expectations",
                "Meeting Expectations",
                "Exceeding Expectations")

#reflect the 2012-13 PJP matrix decisions
SPL.lookup <- list( nonHS = list( `3` = cast(read.csv(file="const/AGE.csv"), ACHIEVEMENT~GROWTH~EQUITY),
                                  `2` = as.matrix(cast(read.csv(file="const/AG.csv"), ACHIEVEMENT~GROWTH))),
                    HS = list( Readiness = as.matrix(cast(read.csv(file="const/Readiness-GradRate.csv"), ADD_READINESS~GRAD_RATE)),
                               Achievement = as.matrix(cast(read.csv(file="const/Achievement-Equity.csv"), ACHIEVEMENT~EQUITY)),
                               Overall = as.matrix(cast(read.csv(file="const/Achievement-Readiness.csv"), READINESS~ACHIEVEMENT))))



#PJP determines
g38.achievement.cuts.lookup.year <- list(`2013-14` = c(53, 70))
g38.achievement.cuts <- g38.achievement.cuts.lookup.year[[current.school.year]]

#PJP determines
g38.growth.cuts.lookup.year <- list(`2011-12` = c(47,55),                                                        
                                 `2012-13` = c(45, 60), #2012-13 PJP cuts
                                 `2013-14` = c(45, 60))  
g38.growth.cuts <- g38.growth.cuts.lookup.year[[current.school.year]]

#PJP determines
g38.equity.cuts.lookup.year <- list(`2011-12` = c(49,58),                                                        
                                 `2012-13` = c(40, 55), #2012-13 PJP cuts
                                 `2013-14` = c(80, 85)) 
g38.equity.cuts<- g38.equity.cuts.lookup.year[[current.school.year]]

#PJP determines
hs.achievement.cuts.lookup.year <- list(`2013-14` = c(32, 45))
hs.achievement.cuts <- hs.achievement.cuts.lookup.year[[current.school.year]]
#hs.achievement.cuts <- c(30, 40)

#act readiness
calc.index <- function (domain.runs, range) {
  
  result <- do.call(c, lapply(1:length(range), function(i) rep(range[i], length(domain.runs[[i]]))))
  names(result) <- do.call(c, domain.runs)
  result
}

readiness.standard.test.types <- c('ACT','PLAN','EXPLORE')

#PJP determines
tested.readiness <- c(level1.points = 20,
                      level2.points = 50,
                      level3.points = 80,
                      level4.points = 100)
# tested.readiness <- c(level1.points = 0,
#                       level2.points = 40,
#                       level3.points = 80,
#                       level4.points = 100)


explore.index.runs <- list(1:14, 15:17, 18:20, 21:25)
#explore.index.range <- c(0, 40, 80, 100)
explore.index.range <- c(tested.readiness["level1.points"], 
                         tested.readiness["level2.points"],
                         tested.readiness["level3.points"], 
                         tested.readiness["level4.points"])  #2012-13 PJP index
#explore_index <- c(rep(0,14), rep(40, 3), rep(80, 3), rep(100,5))
#names(explore_index) <- c(1:14, 15:17, 18:20, 21:25)
explore_index <- calc.index(explore.index.runs, explore.index.range)


plan.index.runs <- list(1:15, 16:18, 19:21, 22:32)
#plan.index.range <- c(0,40,80,100)
plan.index.range <- c(tested.readiness["level1.points"],
                      tested.readiness["level2.points"],
                      tested.readiness["level3.points"],
                      tested.readiness["level4.points"])  #2012-13 PJP index
plan_index <- calc.index(plan.index.runs, plan.index.range)
#names(plan_index) <- c(1:15, 16:18, 19:21, 22:32)

act.index.runs <- list(1:16, 17:20, 21:24, 25:36)
##act.index.range <- c(0, 40, 80, 100)
act.index.range <- c(tested.readiness["level1.points"], 
                     tested.readiness["level2.points"], 
                     tested.readiness["level3.points"], 
                     tested.readiness["level4.points"]) #2012-13 PJP index

act_index  <- calc.index(act.index.runs, act.index.range)


# act_index <- c(rep(0,16), rep(40, 4), rep(80, 4), rep(100,12))
# names(act_index) <- c(1:16, 17:20, 21:24, 25:36)


#same index is used except we add an interpolation point between .5 and .75 for .66
tested.readiness.alt <- tested.readiness


#The only possible percentages are 0, .25, .33, .5,  .66, .75, and 1
#     1         2        2.5       3         4
#..., .25) [.25, .5) [.5, .66) [.66, .8) [.8,....
alt.index.intervals <- c(.25, .5, .66, .8)
alt.index.runs = as.list(0:4)

alt.index.range = c(tested.readiness.alt[["level1.points"]],
                    tested.readiness.alt[["level2.points"]], 
                    (tested.readiness.alt[["level2.points"]] + tested.readiness.alt[["level3.points"]])/2,
                    tested.readiness.alt[["level3.points"]],
                    tested.readiness.alt[["level4.points"]]) 

alt_index <- calc.index(alt.index.runs, alt.index.range)
# alt_index <- c(0,40, 80, 100)
# names(alt_index) <- 0:3

readiness.indeces <- list(ALT=alt_index,
                          ACT=act_index,
                          PLAN=plan_index,
                          EXPLORE=explore_index)


##grad rate index
grad.index.runs = list('Non-Graduate', 'Returning', '6YR', '5YR', '4YR')
#grad.index.range = c(0, 50, 75, 75, 100)
grad.index.range = c(0, 50, 100, 100, 100) #2012-13 PJP index
grad.index <- calc.index(grad.index.runs, grad.index.range)
grad.rate.precision <- 1


grad.rate.labels <- c(extended="CAT_EXTENDED_2013", 
                      `4yr`="GRAD_RATE_4_YR.2012.13", 
                      `4yr.prior`="GRAD_RATE_4_YR_2012",
                      extended.N="COHORT_EXTENDED_N.2012.13",
                      `4yr.N`="COHORT_4_YR_N.2012.13",
                      `4yr.N.prior`="COHORT_4_YR_N.2011.12")

grad.rate.cats <- c(`4yr`="CAT_4_YR_2013", extended="CAT_EXTENDED_2013", improve="IMPROVE_CAT_2013")

#PJP determines
hs.grad.rate.cuts.lookup.year <- list(`2013-14` = c(80, 90))  
hs.grad.rate.cuts <- hs.grad.rate.cuts.lookup.year[[current.school.year]]
#hs.grad.rate.cuts <- c(78, 92)
  
##PJP determines
hathaway.eligibility.index <- c(40, 70, 80, 90, 100)  
#hathaway.eligibility.index <- c(0, 30, 50, 80, 100)  


hathcat.labels = c("CAT_1",
                   "CAT_2",
                   "CAT_3", 
                   "CAT_4",
                   "CAT_5",
                   "Undefined")

##PJP determines
additional.readiness.weights <- c(tested=.30,
                                  grade.nine=.30,
                                  hathaway=.40)
# additional.readiness.weights <- c(tested=.45,
#                                   grade.nine=.15,
#                                   hathaway=.40)


additional.readiness.types <- c("all", "tested only","tested and gd9 only", "tested and Hath only")


#PJP determines
type.1.additional.readiness.cuts.lookup.year <- list(`2013-14` = c(70, 80))  #2012-13 PJP cuts, corrected
type.1.additional.readiness.cuts <- type.1.additional.readiness.cuts.lookup.year[[current.school.year]]
#type.1.additional.readiness.cuts <- c(56, 68)

#not set by PJP
subgroup.hs.math.cut <- 17  
subgroup.hs.reading.cut <- 16

subgroup.labels.hs <- c("SUBGROUP_MATH_HS", "SUBGROUP_READING_HS", "SUBGROUP_CONSOLIDATED_HS")

#PJP determines
hs.equity.cuts.lookup.year <- list(`2013-14` = c(120, 127))
hs.equity.cuts <- hs.equity.cuts.lookup.year[[current.school.year]]
#hs.equity.cuts <- c(123, 128)
  
hs.equity.precision  <- 3


g38.indicator.labels <- c(achievement="G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT",
                             growth="G38_GROWTH_MGP",
                             equity="G38_EQUITY_MEAN")

g38.target.level.labels <- c(achievement="G38_ACHIEVEMENT_ALL_TARGET_LEVEL",
                             growth="G38_GROWTH_TARGET_LEVEL",
                             equity="G38_EQUITY_TARGET_LEVEL")

g38.participation.labels <- c(achievement="G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE",
                              equity="G38_EQUITY_PARTICIPATION_RATE")

g38.SPL.labels <- c(unmodified="G38_SPL",
                    accountability="G38_SPL_ACCOUNTABILITY")


hs.grad.rates <- c(`4yr`="GRAD_RATE_4_YR.2012.13",
                         extended="GRAD_RATE_EXTENDED",
                         improvement.target="IMPROVEMENT_TARGET")

hs.indicator.labels <- c(achievement="HS_ACHIEVEMENT_PERCENT_PROFICIENT",
                         equity="HS_EQUITY_MEAN",
                         readiness="HS_ADD_READINESS_SCORE_TYPE1",
                         hs.grad.rates)


hs.target.level.labels <- c(achievement="HS_ACHIEVEMENT_TARGET_LEVEL",
                            equity="HS_EQUITY_TARGET_LEVEL",
                            readiness="HS_ADD_READINESS_CAT",                            
                            grad.rate="IMPROVE_CAT_2013")

hs.achievement.target.level.labels <- hs.target.level.labels[1:2]
hs.readiness.target.level.labels <- hs.target.level.labels[3:4]

hs.overall.target.level.labels <- c(readiness="HS_OVERALL_READINESS",
                                    achievement="HS_OVERALL_ACHIEVEMENT")

hs.participation.labels <- c(achievement="HS_ACHIEVEMENT_PARTICIPATION_RATE",
                             tested.readiness="HS_TESTED_READINESS_PARTICIPATION_RATE",
                             equity="HS_EQUITY_PARTICIPATION_RATE"
)

hs.SPL.labels <- c(unmodified="HS_SPL",
                    accountability="HS_SPL_ACCOUNTABILITY")



#original.labels <- c("SCHOOL_ID","SCHOOL_YEAR","DISTRICT_ID","DISTRICT_NAME","NAME","SHORT_NAME","CATEGORY","LOW_GRADE","HIGH_GRADE","GRADES_SERVED","WAEA_SCHOOL_TYPE","PAIRED_SCHOOL_ID","PAIRED_SCHOOL_NAME","YEAR","TITLE_1_SCHOOL","ALTERNATIVE_SCHOOL","ENROLLMENT","GRADE_BAND_COMPOSITION","G38_ACHIEVEMENT_ALL_SMALL_SCHOOL","G38_ACHIEVEMENT_ALL_YEARS_BACK","G38_ACHIEVEMENT_ALL_N_TESTS","G38_ACHIEVEMENT_ALL_N_PROFICIENT_TESTS","G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT","G38_ACHIEVEMENT_ALL_N","G38_ACHIEVEMENT_ALL_TESTS_ACTUAL_COUNT","G38_ACHIEVEMENT_ALL_TESTS_EXPECTED_COUNT","G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE","G38_ACHIEVEMENT_ALL_TARGET_LEVEL","G38_GROWTH_SMALL_SCHOOL","G38_GROWTH_YEARS_BACK","G38_GROWTH_N_TESTS","G38_GROWTH_MGP","G38_GROWTH_N","G38_GROWTH_TESTS_ACTUAL_COUNT","G38_GROWTH_TESTS_EXPECTED_COUNT","G38_GROWTH_PARTICIPATION_RATE","G38_GROWTH_TARGET_LEVEL","G38_EQUITY_SMALL_SCHOOL","G38_EQUITY_YEARS_BACK","G38_EQUITY_N_TESTS","G38_EQUITY_MEAN","G38_EQUITY_N","G38_EQUITY_TESTS_ACTUAL_COUNT","G38_EQUITY_TESTS_EXPECTED_COUNT","G38_EQUITY_PARTICIPATION_RATE","G38_EQUITY_TARGET_LEVEL","G38_INDICATORS_N","G38_SPL","G38_SPL_ACCOUNTABILITY","HS_ACHIEVEMENT_SMALL_SCHOOL","HS_ACHIEVEMENT_YEARS_BACK","HS_ACHIEVEMENT_N_TESTS","HS_ACHIEVEMENT_N_PROFICIENT_TESTS","HS_ACHIEVEMENT_PERCENT_PROFICIENT","HS_ACHIEVEMENT_N","HS_ACHIEVEMENT_TESTS_ACTUAL_COUNT","HS_ACHIEVEMENT_TESTS_EXPECTED_COUNT","HS_ACHIEVEMENT_PARTICIPATION_RATE","HS_ACHIEVEMENT_TARGET_LEVEL","HS_EQUITY_SMALL_SCHOOL","HS_EQUITY_YEARS_BACK","HS_EQUITY_N_SCORES","HS_EQUITY_MEAN","HS_EQUITY_N","HS_EQUITY_TESTS_ACTUAL_COUNT","HS_EQUITY_TESTS_EXPECTED_COUNT","HS_EQUITY_PARTICIPATION_RATE","HS_EQUITY_TARGET_LEVEL","HS_TESTED_READINESS_SMALL_SCHOOL","HS_TESTED_READINESS_YEARS_BACK","HS_TESTED_READINESS_N_sCORES","HS_TESTED_READINESS_MEAN","HS_TESTED_READINESS_N","HS_TESTED_READINESS_TESTS_ACTUAL_COUNT","HS_TESTED_READINESS_TESTS_EXPECTED_COUNT","HS_TESTED_READINESS_PARTICIPATION_RATE","GRADE_NINE_CREDITS_N","GRADE_NINE_CREDITS_MET_N","SMALL_SCHOOL_GRADE_NINE_CREDIT","PERCENT_GD_9_CREDIT_MET","HATH_INDEX_SCORE_N","HATH_INDEX_SCORE_MEAN","SMALL_SCHOOL_HATH_ELIGIBILITY","HATH_CAT_N","HATH_CAT_MEAN","LOOK_BACK_YRS_4_YR","EXTENDED_LOOK_BACK_YRS","GRAD_RATE_4_YR.2012.13","GRAD_RATE_EXTENDED","COHORT_4_YR_N.2012.13","COHORT_EXTENDED_N.2012.13","CAT_4_YR_2013","CAT_EXTENDED_2013","IMPROVE_TARGET_FOR_MEETS","IMPROVE_TARGET_FOR_EXCEED","IMPROVE_CAT_2013","HS_ADD_READINESS_TYPE","HS_ADD_READINESS_TYPE_LABEL","HS_ADD_READINESS_SCORE_TYPE1","HS_ADD_READINESS_N_TYPE1","HS_ADD_READINESS_CAT_TYPE1","HS_ADD_READINESS_SCORE_TYPE2","HS_ADD_READINESS_CAT_TYPE2","HS_ADD_READINESS_N_TYPE2","HS_ADD_READINESS_CUT1_TYPE2","HS_ADD_READINESS_CUT2_TYPE2","HS_ADD_READINESS_SCORE_TYPE3","HS_ADD_READINESS_CAT_TYPE3","HS_ADD_READINESS_N_TYPE3","HS_ADD_READINESS_CUT1_TYPE3","HS_ADD_READINESS_CUT2_TYPE3","HS_ADD_READINESS_SCORE_TYPE4","HS_ADD_READINESS_CAT_TYPE4","HS_ADD_READINESS_N_TYPE4","HS_ADD_READINESS_CUT1_TYPE4","HS_ADD_READINESS_CUT2_TYPE4","HS_ADD_READINESS_CAT","HS_ACHIEVEMENT_INDICATORS_N","HS_READINESS_INDICATORS_N","HS_INDICATORS_N","HS_OVERALL_READINESS","HS_OVERALL_ACHIEVEMENT","HS_SPL","HS_PARTICIPATION_RATE_CAT","HS_SPL_ACCOUNTABILITY","ALL_SPL_ACCOUNTABILITY")
original.labels <- c("SCHOOL_ID","SCHOOL_YEAR","DISTRICT_ID","DISTRICT_NAME","NAME","SHORT_NAME","CATEGORY","LOW_GRADE","HIGH_GRADE","GRADES_SERVED","WAEA_SCHOOL_TYPE","PAIRED_SCHOOL_ID","PAIRED_SCHOOL_NAME","YEAR","TITLE_1_SCHOOL","ALTERNATIVE_SCHOOL","ENROLLMENT","GRADE_BAND_COMPOSITION","G38_ACHIEVEMENT_ALL_SMALL_SCHOOL","G38_ACHIEVEMENT_ALL_YEARS_BACK","G38_ACHIEVEMENT_ALL_N_TESTS","G38_ACHIEVEMENT_ALL_N_PROFICIENT_TESTS","G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT","G38_ACHIEVEMENT_ALL_N","G38_ACHIEVEMENT_ALL_TESTS_ACTUAL_COUNT","G38_ACHIEVEMENT_ALL_TESTS_EXPECTED_COUNT","G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE","G38_ACHIEVEMENT_ALL_TARGET_LEVEL","G38_GROWTH_SMALL_SCHOOL","G38_GROWTH_YEARS_BACK","G38_GROWTH_N_TESTS","G38_GROWTH_MGP","G38_GROWTH_N","G38_GROWTH_TESTS_ACTUAL_COUNT","G38_GROWTH_TESTS_EXPECTED_COUNT","G38_GROWTH_PARTICIPATION_RATE","G38_GROWTH_TARGET_LEVEL","G38_EQUITY_SMALL_SCHOOL","G38_EQUITY_YEARS_BACK","G38_EQUITY_N_TESTS","G38_EQUITY_MEAN","G38_EQUITY_N","G38_EQUITY_TESTS_ACTUAL_COUNT","G38_EQUITY_TESTS_EXPECTED_COUNT","G38_EQUITY_PARTICIPATION_RATE","G38_EQUITY_TARGET_LEVEL","G38_INDICATORS_N","G38_SPL","G38_SPL_ACCOUNTABILITY","HS_ACHIEVEMENT_SMALL_SCHOOL","HS_ACHIEVEMENT_YEARS_BACK","HS_ACHIEVEMENT_N_TESTS","HS_ACHIEVEMENT_N_PROFICIENT_TESTS","HS_ACHIEVEMENT_PERCENT_PROFICIENT","HS_ACHIEVEMENT_N","HS_ACHIEVEMENT_TESTS_ACTUAL_COUNT","HS_ACHIEVEMENT_TESTS_EXPECTED_COUNT","HS_ACHIEVEMENT_PARTICIPATION_RATE","HS_ACHIEVEMENT_TARGET_LEVEL","HS_EQUITY_SMALL_SCHOOL","HS_EQUITY_YEARS_BACK","HS_EQUITY_N_SCORES","HS_EQUITY_MEAN","HS_EQUITY_N","HS_EQUITY_TESTS_ACTUAL_COUNT","HS_EQUITY_TESTS_EXPECTED_COUNT","HS_EQUITY_PARTICIPATION_RATE","HS_EQUITY_TARGET_LEVEL","HS_TESTED_READINESS_SMALL_SCHOOL","HS_TESTED_READINESS_YEARS_BACK","HS_TESTED_READINESS_N_sCORES","HS_TESTED_READINESS_MEAN","HS_TESTED_READINESS_N","HS_TESTED_READINESS_TESTS_ACTUAL_COUNT","HS_TESTED_READINESS_TESTS_EXPECTED_COUNT","HS_TESTED_READINESS_PARTICIPATION_RATE","GRADE_NINE_CREDITS_N","GRADE_NINE_CREDITS_MET_N","SMALL_SCHOOL_GRADE_NINE_CREDIT","PERCENT_GD_9_CREDIT_MET","HATH_INDEX_SCORE_N","HATH_INDEX_SCORE_MEAN","SMALL_SCHOOL_HATH_ELIGIBILITY","HATH_CAT_N","HATH_CAT_MEAN","LOOK_BACK_YRS_4_YR","EXTENDED_LOOK_BACK_YRS","GRAD_RATE_4_YR_2012","GRAD_RATE_4_YR.2012.13","GRAD_RATE_EXTENDED","COHORT_4_YR_N.2011.12","COHORT_4_YR_N.2012.13","COHORT_EXTENDED_N.2012.13","CAT_4_YR_2013","CAT_EXTENDED_2013","IMPROVE_TARGET_FOR_MEETS","IMPROVE_TARGET_FOR_EXCEED","IMPROVEMENT_TARGET","IMPROVE_CAT_2013","HS_ADD_READINESS_TYPE","HS_ADD_READINESS_TYPE_LABEL","HS_ADD_READINESS_SCORE_TYPE1","HS_ADD_READINESS_N_TYPE1","HS_ADD_READINESS_CAT_TYPE1","HS_ADD_READINESS_SCORE_TYPE2","HS_ADD_READINESS_N_TYPE2","HS_ADD_READINESS_CAT_TYPE2","HS_ADD_READINESS_CUT1_TYPE2","HS_ADD_READINESS_CUT2_TYPE2","HS_ADD_READINESS_SCORE_TYPE3","HS_ADD_READINESS_N_TYPE3","HS_ADD_READINESS_CAT_TYPE3","HS_ADD_READINESS_CUT1_TYPE3","HS_ADD_READINESS_CUT2_TYPE3","HS_ADD_READINESS_SCORE_TYPE4","HS_ADD_READINESS_N_TYPE4","HS_ADD_READINESS_CAT_TYPE4","HS_ADD_READINESS_CUT1_TYPE4","HS_ADD_READINESS_CUT2_TYPE4","HS_ADD_READINESS_SCORE","HS_ADD_READINESS_CAT","HS_ACHIEVEMENT_INDICATORS_N","HS_READINESS_INDICATORS_N","HS_INDICATORS_N","HS_OVERALL_READINESS","HS_OVERALL_ACHIEVEMENT","HS_SPL","HS_PARTICIPATION_RATE_CAT","HS_SPL_ACCOUNTABILITY","ALL_SPL","ALL_SPL_ACCOUNTABILITY")

SPL.types <- c(g38.SPL.labels, hs.SPL.labels, "ALL_SPL", "ALL_SPL_ACCOUNTABILITY")
