library("reshape")
current.school.year <- '2012-13'
precision <- 1

#non HS minimum N values
min.N.achievement <-  6
min.N.growth <- 6
min.N.subgroup <- 15

min.N.achievement.multiyear <-  10
min.N.growth.multiyear <- 10
min.N.subgroup.multiyear <- 15

#HS minimum N values
min.N.achievement.hs <- 6
min.N.tested.readiness <- 6
min.N.grad <- 6
min.N.readiness.hs <- 6
min.N.equity.hs <- 15

min.N.achievement.hs.multiyear <- 10

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


# school.pairing.lookup <- list(`2010-11` = list(`0701008`='0701009',
#                                                `0706001`='0706002',
#                                                `1001006`='1001002',
#                                                `1101010`='1101022',
#                                                `1202001`='1202005',
#                                                `1202003`='1202004'),
#                               `2011-12` = list(`0701008`='0701009',
#                                                `0706001`='0706002',
#                                                `1001006`='1001002',
#                                                `1101010`='1101022',
#                                                `1202001`='1202005',
#                                                `1202003`='1202004'),
#                               `2012-13` = list(`0701008`='0701009',
#                                                `0706001`='0706002',
#                                                `1001006`='1001002',
#                                                `1101010`='1101022',
#                                                `1202001`='1202005',
#                                                `1202003`='1202004'
#                               ))

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
                                               `2301003` = '2301001')
)

participation.labels <- c("Not Met", "Docked", "Met")
indicator.labels <- c("Not Meeting Targets", "Meeting Targets", "Exceeding Targets")
SPL.labels <- c("Not Meeting Expectations",
                "Partially Meeting Expectations",
                "Meeting Expectations",
                "Exceeding Expectations")

#reflect the 2012-13 PJP matrix decisions
SPL.lookup <- list( nonHS = list( `3` = cast(read.csv(file="data/AGE.csv"), ACHIEVEMENT~GROWTH~EQUITY),
                                  `2` = as.matrix(cast(read.csv(file="data/AG.csv"), ACHIEVEMENT~GROWTH))),
                    HS = list( `3` = cast(read.csv(file="data/ARE.csv"), ACHIEVEMENT~READINESS~EQUITY),
                               `2` = as.matrix(cast(read.csv(file="data/AR.csv"), ACHIEVEMENT~READINESS))))


#achievement

# lapply(seq(1,3), 
#        function (band) {
#          quantile(achievement[achievement$SCHOOL_YEAR == current.school.year & achievement$SCHOOL_ID != state.school.id &
#                                 achievement$ACCOUNTABILITY_N_BAND > min.N.achievement &
#                                 achievement$GRADE_BAND == band,]$PERCENT_PROFICIENT, 
#                   probs=c(.35,.65))
#          
#        })



#default values based on the above percentiles
achievement.level.lookup.year <- list(`2011-12` = list(c(79,85), 
                                                       c(68,76)),
                                      #                                       `2012-13` = list(c(76, 82),
                                      #                                                        c(67,74))
                                      `2012-13` = list(c(75, 86), #2012-13 PJP cuts
                                                       c(68,80)))

achievement.level.lookup <- achievement.level.lookup.year[[current.school.year]]

achievement.labels <- c(cut.1="ACHIEVEMENT_CUT_1", cut.2="ACHIEVEMENT_CUT_2", "PERCENT_PROFICIENT", N="N_ACHIEVEMENT", "ACHIEVEMENT_TARGET_LEVEL")
achievement.grade.band.labels <- c("PERCENT_PROFICIENT_BAND_1", "PERCENT_PROFICIENT_BAND_2", "N_ACHIEVEMENT_BAND_1", "N_ACHIEVEMENT_BAND_2")



#growth
# quantile(growth[growth$SCHOOL_YEAR == current.school.year & growth$SCHOOL_ID != state.school.id &
#                   growth$GROWTH_ACCOUNTABILITY_N > 5,]$MGP, 
#          probs=c(.35,.65))


#default values based on the above percentiles

# growth.level.lookup.year <- list(`2011-12` = c(47,55),                                                        
#                                  `2012-13` = c(47, 55))

growth.level.lookup.year <- list(`2011-12` = c(47,55),                                                        
                                 `2012-13` = c(45, 60))  #2012-13 PJP cuts

growth.level.lookup <- growth.level.lookup.year[[current.school.year]]

growth.labels <- c(cut.1="GROWTH_CUT_1", cut.2="GROWTH_CUT_2", "PERCENT_PROFICIENT_PRIOR", "MGP", N="N_GROWTH", "GROWTH_TARGET_LEVEL")


#equity
# quantile(equity[equity$SCHOOL_YEAR == current.school.year & 
#                   equity$N_SUBGROUP > 14 & equity$SCHOOL_ID != state.school.id,]$PERCENT_MEETING_AGP, 
#          probs=c(.35,.65))


# equity.level.lookup.year <- list(`2011-12` = c(49,58),                                                        
#                                  `2012-13` = c(37, 45))

equity.level.lookup.year <- list(`2011-12` = c(49,58),                                                        
                                 `2012-13` = c(40, 55)) #2012-13 PJP cuts

equity.level.lookup <- equity.level.lookup.year[[current.school.year]]

equity.labels <- c(cut.1="EQUITY_CUT_1", cut.2="EQUITY_CUT_2", "PERCENT_MEETING_AGP", N="N_SUBGROUP", "EQUITY_TARGET_LEVEL")


#act achievement
# quantile(act.achievement[act.achievement$SCHOOL_YEAR == current.school.year & 
#                            act.achievement$N_ACHIEVEMENT > 5 &
#                            act.achievement$PARTICIPATION_RATE_ACHIEVEMENT >= 90 & 
#                            act.achievement$SCHOOL_ID != state.school.id,]$PERCENT_PROFICIENT, 
#          probs=c(.35,.65))

# quantile(act.achievement[act.achievement$SCHOOL_YEAR == current.school.year & 
#                            act.achievement$N_ACHIEVEMENT > 5 &
#                            act.achievement$PARTICIPATION_RATE_ACHIEVEMENT >= 90 & 
#                            act.achievement$SCHOOL_ID != state.school.id,]$PERCENT_PROFICIENT, 
#          probs=seq(.20,.90,.01))

##based on above percentiles
##act.achievement.level.lookup.year <- list(`2012-13` = c(70, 77))
#act.achievement.level.lookup.year <- list(`2012-13` = c(70, 83)) #2012-13 PJP cuts
act.achievement.level.lookup.year <- list(`2012-13` = c(63, 78)) #equipercentile linkage of 2012-13 PJP cuts once ACT cuts were corrected  

act.achievement.level.lookup <- act.achievement.level.lookup.year[[current.school.year]]


act.achievement.labels <- c(cut.1="ACHIEVEMENT_CUT_1_HS", cut.2="ACHIEVEMENT_CUT_2_HS", "PERCENT_PROFICIENT_HS", N="N_ACHIEVEMENT_HS", "PARTICIPATION_RATE_ACHIEVEMENT_HS", "ACHIEVEMENT_TARGET_LEVEL_HS")

#act readiness
calc.index <- function (domain.runs, range) {
  
  result <- do.call(c, lapply(1:length(range), function(i) rep(range[i], length(domain.runs[[i]]))))
  names(result) <- do.call(c, domain.runs)
  result
}

explore.index.runs <- list(1:14, 15:17, 18:20, 21:25)
#explore.index.range <- c(0, 40, 80, 100)
explore.index.range <- c(20, 50, 80, 100)  #2012-13 PJP index
#explore_index <- c(rep(0,14), rep(40, 3), rep(80, 3), rep(100,5))
#names(explore_index) <- c(1:14, 15:17, 18:20, 21:25)
explore_index <- calc.index(explore.index.runs, explore.index.range)

plan.index.runs <- list(1:15, 16:18, 19:21, 22:32)
#plan.index.range <- c(0,40,80,100)
plan.index.range <- c(20,50,80,100)  #2012-13 PJP index
plan_index <- calc.index(plan.index.runs, plan.index.range)
#names(plan_index) <- c(1:15, 16:18, 19:21, 22:32)

act.index.runs <- list(1:16, 17:20, 21:24, 25:36)
##act.index.range <- c(0, 40, 80, 100)
act.index.range <- c(20, 50, 80, 100) #2012-13 PJP index
act_index  <- calc.index(act.index.runs, act.index.range)
# act_index <- c(rep(0,16), rep(40, 4), rep(80, 4), rep(100,12))
# names(act_index) <- c(1:16, 17:20, 21:24, 25:36)

alt.index.runs = as.list(0:3)
#alt.index.range = c(0,40, 80, 100)
alt.index.range = c(20,50, 80, 100) #2012-13 PJP index
alt_index <- calc.index(alt.index.runs, alt.index.range)
# alt_index <- c(0,40, 80, 100)
# names(alt_index) <- 0:3


tested.readiness.labels <- c( 
  "TESTED_READINESS", 
  N="N_TESTED_READINESS", 
  "PARTICIPATION_RATE_TESTED_READINESS")


##grad rate index
grad.index.runs = list('Non-Graduate', 'Returning', '6YR', '5YR', '4YR')
#grad.index.range = c(0, 50, 75, 75, 100)
grad.index.range = c(0, 50, 100, 100, 100) #2012-13 PJP index
grad.index <- calc.index(grad.index.runs, grad.index.range)
# grad.index <- c(0, 50, 75, 75, 100)
# names(grad.index) <- c('Non-Graduate', 'Returning', '5YR', '6YR', '4YR')

grad.index.labels <- c(Indicator="SCHOOL_GRADUATION_INDEX", 
                       N="N_GRADUATION")


#hs readiness weights
# hs.readiness.weights = c(TESTED_READINESS=0.44,
#                          SCHOOL_GRADUATION_INDEX=0.56)

#PJP 2013 changed to this
hs.readiness.weights = c(TESTED_READINESS=0.4,
                         SCHOOL_GRADUATION_INDEX=0.6)



# quantile(with(schools, schools[!is.na(TOTAL_READINESS_HS) & (N_TESTED_READINESS > 5 & N_GRADUATION > 5),]$TOTAL_READINESS_HS),
#          probs=c(.35,.65))

#based on the above quantiles
#total.readiness.level.lookup.year <- list(`2012-13` = c(63,72))
#total.readiness.level.lookup.year <- list(`2012-13` = c(68,80))  #2012-13 PJP cuts
total.readiness.level.lookup.year <- list(`2012-13` = c(71,81))  #2012-13 PJP cuts, corrected
total.readiness.level.lookup <- total.readiness.level.lookup.year[[current.school.year]]
total.readiness.labels <- c(cut.1="READINESS_CUT_1", cut.2="READINESS_CUT_2", "READINESS_TARGET_LEVEL")

##hs equity
#quantile(hs.equity.df$EQUITY_VALUE, c(.1,.5))

# <= first cut gets 3 (exceeds), >= first cut and <= second cut gets 2, > second cut gets 1


#quantile(hs.equity.df[hs.equity.df$N_ACHIEVEMENT > 14 & hs.equity.df$SCHOOL_ID!=state.school.id,"IMPROVEMENT_VALUE"], c(.33,.66))
#quantile(hs.equity.df[hs.equity.df$N_ACHIEVEMENT > 14 & hs.equity.df$SCHOOL_ID!=state.school.id,"PERCENT_NONPROFICIENT"], c(.33,.66))  
#based on the above quantiles
hs.equity.level.lookup.year <- list(`2012-13` = list(#IMPROVEMENT_VALUE = c(-3.120,2.764),
  #IMPROVEMENT_VALUE = c(-12.085,-7.15),
  IMPROVEMENT_VALUE = c(-3.2,3.4), #once 2013 act cuts were corrected
  #PERCENT_NONPROFICIENT=c(13.43, 28.596)
  #PERCENT_NONPROFICIENT=c(13.63, 18.86)
  PERCENT_NONPROFICIENT=c(21.70, 29.86)  #This is not used anymore in the current model
))

#decimal place to which the percent non proficient is rounded in the current and prior year
hs.equity.precision  <- 3
#hs.equity.precision  <- 1
hs.equity.level.lookup <- hs.equity.level.lookup.year[[current.school.year]]


hs.equity.labels <- c(N="N_EQUITY_HS", "N_ACHIEVEMENT_HS_PRIOR", "PERCENT_NONPROFICIENT", "PERCENT_NONPROFICIENT_PRIOR", "PERCENT_NONPROFICIENT_CUT_LOW_REVERSED", 
                      "PERCENT_NONPROFICIENT_CUT_HIGH_REVERSED", "IMPROVEMENT_SCORE", cut.2="IMPROVEMENT_CUT_LOW_REVERSED",
                      cut.1="IMPROVEMENT_CUT_HIGH_REVERSED", "PERCENT_NONPROFICIENT_CATEGORY", 
                      "IMPROVEMENT_CATEGORY", "EQUITY_TARGET_LEVEL_HS")

hs.equity.improve.labels <- c("Low", "Middle", "High")
hs.equity.np.labels <- c("Minus", "Neutral", "Plus")

# hs.equity.labels <- c("N_ACHIEVEMENT_PRIOR", HS_EQUITY_LOW", 
#                       "HS_EQUITY_HIGH", 
#                       "HS_EQUITY", 
#                       "IMPROVEMENT_NONPROFICIENT",
#                       "PERCENT_NONPROFICIENT",
#                       "PERCENT_NONPROFICIENT_PRIOR",
#                       "N_HS_EQUITY",                      
#                       "N_HS_EQUITY_PRIOR",
#                       "PARTICIPATION_RATE_HS_EQUITY", 
#                       "HS_EQUITY_TARGET_LEVEL")

#based on the above quantiles

##small schools
small.school.labels <- c("SMALL_SCHOOL", "YEARS_BACK")
small.school.hs.labels <- c("SMALL_SCHOOL_HS", "YEARS_BACK_HS")

small.school.labels.achievement <- c("SMALL_SCHOOL_ACHIEVEMENT", "YEARS_BACK_ACHIEVEMENT")
small.school.labels.growth <- c("SMALL_SCHOOL_GROWTH", "YEARS_BACK_GROWTH")
small.school.labels.equity <- c("SMALL_SCHOOL_EQUITY", "YEARS_BACK_EQUITY")
