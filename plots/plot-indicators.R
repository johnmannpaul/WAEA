#SOME FUNCTIONS WILL NEED TO BE RE-WRITTEN BECAUSE THEY ARE CURRENTLY DESIGNED TO ONLY HANDLE ONE OF EACH ALTERNATIVE VALUE. THIS IS
#DUE TO THE FACT THAT THERE IS ONLY ONE ALTERNATIVE SCHOOL FOR SOME INDICATORS AT THIS TIME.

#ALL FUNCTIONS HAVE BEEN WRITTEN ON A SEPARATE DOCUMENT TITLES "FUNCTIONS"

# LOAD DATA

grad=read.csv(file.choose(),sep=',',header=T)
grade9=read.csv(file.choose(),sep=',',header=T)
hath=read.csv(file.choose(),sep=',',header=T)
test=read.csv(file.choose(),sep=',',header=T)
add=read.csv(file.choose(),sep=',',header=T)
equity=read.csv(file.choose(),sep=',',header=T)
achieve=read.csv(file.choose(),sep=',',header=T)
elem_achieve=read.csv(file.choose(),sep=',',header=T)
elem_equity=read.csv(file.choose(),sep=',',header=T)
elem_growth=read.csv(file.choose(),sep=',',header=T)
part=read.csv(file.choose(),sep=',',header=T)


# GRADUATION RATES
#_______________________________________________________________________________________________________________________________________

# 4-YEAR GRADUATION  (Use both Curves, Round 2 places since percentages are in decimal form)


grad_nonalt=grad[grad$ALTERNATIVE_SCHOOL=="FALSE",]
grad_alt=grad[grad$ALTERNATIVE_SCHOOL=="TRUE",]

Grad4_hist(T,T)

dist(grad$GRAD_RATE_4_YR.2012.13,0)
dist(grad_nonalt$GRAD_RATE_4_YR.2012.13,0)


# EXTENDED GRADUATION (Use both Curves, Round 2 places since percentages are in decimal form)

Gradext_hist(T,T)

dist(grad$GRAD_RATE_EXTENDED,0)
dist(grad_nonalt$GRAD_RATE_EXTENDED,0)




# GRADE 9 CREDIT
#----------------------------------------------------------------------------------------------------------------------------------
# GRADE 9 (No Inputs, Round to 0 places, *It will only graph 1 of each alternative value*)
# LOAD DATA, REMOVE SMALL SCHOOLS, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST


grade9_nonalt=grade9[grade9$ALTERNATIVE_SCHOOL=="FALSE",]
grade9_alt=grade9[grade9$ALTERNATIVE_SCHOOL=="TRUE",]

Credit9_hist(F)

dist(grade9$PERCENT_GD_9_CREDIT_MET,0)
dist(grade9_nonalt$PERCENT_GD_9_CREDIT_MET,0)




# HATHAWAY
#------------------------------------------------------------------------------------------------------------------------------------
# HATHAWAY MEAN CATEGORY (USE BOTH CURVES, ROUND TO 1 PLACE)
# LOAD DATA, REMOVE SMALL SCHOOLS, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST


hath_nonalt=hath[hath$ALTERNATIVE_SCHOOL=="FALSE",]
hath_alt=hath[hath$ALTERNATIVE_SCHOOL=="TRUE",]

Hath_hist(T,T)

dist(hath$HATH_CAT_MEAN,1)
dist(hath_nonalt$HATH_CAT_MEAN,1)

# HATHAWAY INDEX SCORE (USE BOTH CURVES, ROUND TO 1 PLACE)
# LOAD DATA, REMOVE SMALL SCHOOLS, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST

Hath_index_hist(T,T)

dist(hath$HATH_INDEX_SCORE_MEAN,1)
dist(hath_nonalt$HATH_INDEX_SCORE_MEAN,1)




# TESTED READINESS
#--------------------------------------------------------------------------------------------------------------------------------------
# TESTED READINESS (USE BOTH CURVES, ROUND=0)
# LOAD DATA, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST

test_nonalt=test[test$ALTERNATIVE_SCHOOL=="FALSE",]
test_alt=test[test$ALTERNATIVE_SCHOOL=="TRUE",]

Test_hist(T,T)

dist(test$HS_TESTED_READINESS_MEAN,0)
dist(test_nonalt$HS_TESTED_READINESS_MEAN,0)



# ADDITIONAL READINESS
#--------------------------------------------------------------------------------------------------------------------------------------
# ADDITIONAL READINESS (ONLY 1 CURVE, NO NEED TO ROUND)
# LOAD DATA, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST

add_nonalt=add[add$ALTERNATIVE_SCHOOL=="FALSE",]
add_alt=add[add$ALTERNATIVE_SCHOOL=="TRUE",]

Add_Read_hist(T)

dist(add$HS_ADD_READINESS_SCORE_TYPE1,0)
dist(add_nonalt$HS_ADD_READINESS_SCORE_TYPE1,0)



# HIGH SCHOOL EQUITY
#---------------------------------------------------------------------------------------------------------------------------------------
# HIGH SCHOOL EQUITY (CURVE ON, ROUND TO 0 PLACES)
# LOAD DATA, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST


equity_nonalt=equity[equity$ALTERNATIVE_SCHOOL=="FALSE",]
equity_alt=equity[equity$ALTERNATIVE_SCHOOL=="TRUE",]

Equity_hist(T)

dist(equity$HS_EQUITY_MEAN,0)
dist(equity_nonalt$HS_EQUITY_MEAN,0)



# HIGH SCHOOL ACHIEVEMENT
#---------------------------------------------------------------------------------------------------------------------------------------
# HIGH SCHOOL ACHIEVEMENT  (USE BOTH CURVES, ROUND TO 0 PLACES)
# LOAD DATA, SEPARATE ALTERNATIVE SCHOOLS OUT, GRAPH, FREQ DIST


achieve_nonalt=achieve[achieve$ALTERNATIVE_SCHOOL=="FALSE",]
achieve_alt=achieve[achieve$ALTERNATIVE_SCHOOL=="TRUE",]

Achieve_hist(T,T)

dist(achieve$HS_ACHIEVEMENT_PERCENT_PROFICIENT,0)
dist(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT,0)



# ELEMENTARY SCHOOL INDICATORS
#---------------------------------------------------------------------------------------------------------------------------------------
# ELEMENTARY ACHIEVEMENT (CURVE=T, ROUND=0)
# LOAD DATA, DIVIDE BY SCHOOL TYPE, GRAPH, FREQ. DIST.


e_elem=elem_achieve[elem_achieve$GRADE_BAND_COMPOSITION =="< 7 only",]
e_mixed=elem_achieve[elem_achieve$GRADE_BAND_COMPOSITION=="mixed",]
e_junior=elem_achieve[elem_achieve$GRADE_BAND_COMPOSITION=="> 6 only",]

AchieveE_hist(T)

dist(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,0)


# ELEMENTARY EQUITY (CURVE = T, ROUND = 0)
# LOAD DATA, GRAPH, FREQ. DIST. 


EquityE_hist(T)

dist(elem_equity$G38_EQUITY_MEAN,0)


# ELEMENTARY GROWTH (CURVE=T, ROUND=0)
# LOAD DATA, GRAPH, FREQ. DIST. 


Growth_hist(T)

dist(elem_growth$G38_GROWTH_MGP,0)



# HIGH SCHOOL PARTICIPATION
#---------------------------------------------------------------------------------------------------------------------------------------
# DATA IS ALREADY LOADED, ALT SCHOOLS ARE ALREADY SEPARATED OUT EXCEPT FOR LOW PARTICIPATION
# GRAPH AND FREQ. DIST.

# ACHIEVEMENT PARTICIPATION RATE (CURVE=(F,T), ROUND TO 0 PLACES)

AchieveP_hist(F,T)

dist(achieve$HS_ACHIEVEMENT_PARTICIPATION_RATE,0)
dist(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE,0)


# EQUITY PARTICIPATION RATE (CURVE=F, ROUND=0)

EquityP_hist(F)

dist(equity$HS_EQUITY_PARTICIPATION_RATE,0)
dist(equity_nonalt$HS_EQUITY_PARTICIPATION_RATE,0)


# TESTED READINESS PARTICIPATION RATE (CURVE=(F,T), ROUND=0)

TestP_hist(F,T)

dist(test$HS_TESTED_READINESS_PARTICIPATION_RATE,0)
dist(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE,0)

# LOAD DATA AND SEPARATE OUT ALT SCHOOLS
# OVERALL PARTICIPATION RATE (CURVE=F,T, ROUND=0)


part_nonalt=part[part$ALTERNATIVE.SCHOOL=="FALSE",] 
part_alt=part[part$ALTERNATIVE.SCHOOL=="TRUE",] 

LowP_hist(F,T)

dist(part$LOWEST.RATE,0)
dist(part_nonalt$LOWEST.RATE,0)
