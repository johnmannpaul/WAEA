
drop table acct.GradeLevelAchievement;

CREATE TABLE acct.GradeLevelAchievement (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Grade varchar(3) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT acct_GradeLevelStats_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Grade, Statistic)
)
GO





CREATE NONCLUSTERED INDEX acct_GradeLevelAchievement_idx1
ON acct.GradeLevelAchievement (SchoolYear, SchoolId, Statistic)

CREATE NONCLUSTERED INDEX acct_GradeLevelAchievement_idx2
ON acct.GradeLevelAchievement ([Order])



drop table acct.GradeLevelGrowth;

CREATE TABLE acct.GradeLevelGrowth(

   [Scope] varchar(50) not null,   
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Grade varchar(3) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT acct_GradeLevelGrowth_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Grade, Statistic)
)
GO




CREATE NONCLUSTERED INDEX acct_GradeLevelGrowth_idx1
ON acct.GradeLevelGrowth (SchoolYear, SchoolId, Statistic)

CREATE NONCLUSTERED INDEX acct_GradeLevelGrowth_idx2
ON acct.GradeLevelGrowth (Statistic, [Order])



DROP TABLE acct.SchoolIndictorsNonHS
DROP TABLE acct.SchoolIndictorsHS
Drop Table acct.School

Create table acct.School (
SchoolYear varchar(7) not null, 
DistrictId varchar(7) not null, 
DistrictName varchar(250) not null,
SchoolId varchar(7) not null, 
Name varchar(250) not null,
ShortName varchar(250) null, 
LowGrade varchar(2) not null,
HighGrade varchar(2) not null, 
GradesServed varchar(20) not null, 
WAEASchoolType tinyint not null,
PairedSchoolId varchar(7) null,
PairedSchoolName varchar(250),
SPLAccountability varchar(100),
CONSTRAINT acct_School_PK Primary Key (SchoolYear, SchoolId)
)

CREATE Unique Nonclustered Index acct_School_Type_Idx 
ON acct.School (SchoolYear, SchoolId, WAEASchoolType)



CREATE TABLE acct.SchoolIndictorsNonHS
(

SchoolYear varchar(7) not null,
SchoolId varchar(7) not null,
SmallSchool varchar(1) not null,
YearsBack tinyint null,
YearsBackSubgroup tinyint null,
AchievementLowCut decimal(3,1) null,
AchievmentHighCut decimal(3,1) null,
PercentProficient decimal (4,1) null,
NAchievement int null,
AchievementTargetLevel varchar(100) null,
GrowthLowCut decimal(3,1) null,
GrowthHighCut decimal(3,1) null,
MGP decimal(3,1) null,
NGrowth int null,
GrowthTargetLevel varchar(100) null,
EquityLowCut decimal(3,1) null,
EquityHighCut decimal(3,1) null,
PercentMeetingAGP decimal(4,1) null,
NSubgroup int null,
EquityTargetLevel varchar(100) null,
ParticipationRate decimal(4,1) null,
ParticipationRateLevel varchar(100) null,
NIndicators tinyint not null,
SPL varchar(100),
SPLAdjusted varchar(100),
CONSTRAINT acct_SchoolIndictorsNonHS_PK PRIMARY KEY (SchoolYear, SchoolId),
CONSTRAINT acct_SchoolIndictorsNonHS_FK FOREIGN KEY (SchoolYear, SchoolId) REFERENCES acct.School(SchoolYear, SchoolId)
)




CREATE TABLE acct.SchoolIndictorsHS
(

SchoolYear varchar(7) not null,
SchoolId varchar(7) not null,
SmallSchool varchar(1) not null,
YearsBack tinyint null,
AchievementLowCut decimal(3,1) null,
AchievmentHighCut decimal(3,1) null,
PercentProficient decimal (4,1) null,
NAchievement int null,
AchievementTargetLevel varchar(100) null,
ReadinessLowCut decimal(3,1) null,
ReadinessHighCut decimal(3,1) null,
ReadinessIndexValue decimal(3,1) null,
NTestedReadiness int null,
NGraduation int null,
NReadiness int null,
ReadinessTargetLevel varchar(100) null,
EquityLowCutReversed decimal(3,1) null,
EquityHighCutReversed decimal(3,1) null,
ChangePercentNonProficient decimal(4,1) null,
NSubgroup int null,
EquityTargetLevel varchar(100) null,
ParticipationRateAchievement decimal(4,1) null,
ParticipationRateTestedReadiness decimal(4,1) null,
ParticipationRate decimal(4,1) null,
ParticipationRateLevel varchar(100) null,
NIndicators tinyint not null,
SPL varchar(100),
SPLAdjusted varchar(100),
CONSTRAINT acct_SchoolIndictorsHS_PK PRIMARY KEY (SchoolYear, SchoolId),
CONSTRAINT acct_SchoolIndictorsHS_FK FOREIGN KEY (SchoolYear, SchoolId) REFERENCES acct.School(SchoolYear, SchoolId)
)




DROP TABLE acct.GradeLevelAchievementHS
GO 

CREATE TABLE acct.GradeLevelAchievementHS (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Grade varchar(3) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   ParticipationRate decimal(4,1) not null
   CONSTRAINT acct_GradeLevelStatsHS_PK PRIMARY KEY ([Scope],SchoolYear, SchoolId, Grade, Statistic)
)
GO



DROP TABLE acct.GradeLevelEquityHS
GO 

CREATE TABLE acct.GradeLevelEquityHS (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Grade varchar(3) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   [Order] bigint not null,
   N int null,
   CONSTRAINT acct_GradeLevelEquityHS_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Grade, Statistic)
)
GO



DROP TABLE acct.ReadinessHS
GO 

CREATE TABLE acct.ReadinessHS (

   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   TestedReadinessIndex decimal(4,1) not null,
   NTestedReadiness int not null,
   GraduationIndex  decimal(4,1) not null,
   NGraduation int not null,
   TestedReadinessWeight decimal(3,2) not null,
   GraduationWeight decimal(3,2) not null,
   TestedReadinessIndexWeighted decimal(4,1) not null,
   GraduationIndexWeighted  decimal(4,1) not null,
   [Order] bigint not null,   
   CONSTRAINT acct_ReadinessHS_PK PRIMARY KEY (SchoolYear, SchoolId)
)
GO




DROP TABLE acct.GradeLevelACTSuite
GO 

CREATE TABLE acct.GradeLevelACTSuite (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Explore decimal(10,1) null,
   [Plan] decimal(10,1) null,
   ACT decimal(10,1) null,
   [Paws Alternate] decimal(10,1) null,
   [Order] bigint not null,
   N int null,
   CONSTRAINT acct_GradeLevelACTSuite_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Statistic)
)
GO



drop view acct.SchoolListRated
GO

CREATE VIEW acct.SchoolListRated AS
select s.*, 
nhs.SPLAdjusted as [Elementary/Middle SPL], 
isnull(nhs.NIndicators,0) as [Number of Elementary/Middle Indicators], 
hs.SPLAdjusted as [High School SPL], 
isnull(hs.NIndicators,0) as [Number of High School Indicators]
from
acct.School s left join 
acct.SchoolIndictorsNonHS nhs on s.SchoolYear = nhs.SchoolYear and s.SchoolId = nhs.SchoolId left join
acct.SchoolIndictorsHS hs on s.SchoolYear = hs.SchoolYear and s.SchoolId = hs.SchoolId
--ORDER BY s.SchoolYear, s.DistrictName, s.Name

