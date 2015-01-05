

drop table acct.G38GradeLevelStats;
GO

CREATE TABLE acct.G38GradeLevelStats (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Grade varchar(3) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   Writing decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT acct_G38GradeLevelStats_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Grade, Statistic)
)
GO



CREATE NONCLUSTERED INDEX acct_G38GradeLevelStats_idx1
ON acct.G38GradeLevelStats (SchoolYear, SchoolId, Statistic)
GO

CREATE NONCLUSTERED INDEX acct_G38GradeLevelStats_idx2
ON acct.G38GradeLevelStats ([Order])
GO

drop table acct.HSSchoolIndicators
GO

DROP TABLE acct.G38SchoolIndicators
GO

Drop Table acct.SPLSchool
GO


Create table acct.SPLSchool (
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
CONSTRAINT acct_SPLSchool_PK Primary Key (SchoolYear, SchoolId)
)

GO

CREATE Unique Nonclustered Index acct_SPLSchool_Type_Idx 
ON acct.SPLSchool (SchoolYear, SchoolId)

GO

CREATE TABLE acct.G38SchoolIndicators
(

SchoolYear varchar(7) not null,
SchoolId varchar(7) not null,
WAEASmallSchool varchar(1) not null,
SmallSchoolAchievement varchar(1) not null,
YearsBackAchievement tinyint null,
AchievementLowCut tinyint null,
AchievmentHighCut tinyint null,
AchievementTests int null,
AchievementTestsProficient int null,
PercentProficient tinyint null,
NAchievement int null,
AchievementTestsActual int null,
AchievementTestsExpected int null,
AchievementParticipationRate decimal(4,1) null,
AchievementRequiredTestedLevel1 int not null,
AchievementRequiredTestedLevel2 int not null,
AchievmentParticipationCat int not null,
AchievementParticipationMetLevel1 varchar(10) not null,
AchievementParticipationMetLevel2 varchar(10) not null,
AchievementTargetLevel varchar(100) null,
SmallSchoolGrowth varchar(1) not null,
YearsBackGrowth tinyint null,
GrowthLowCut tinyint null,
GrowthHighCut tinyint null,
MGP decimal(3,1) null,
NGrowth int null,
GrowthTargetLevel varchar(100) null,
SmallSchoolEquity varchar(1) not null,
YearsBackEquity tinyint null,
EquityLowCut tinyint null,
EquityHighCut tinyint null,
EquityScore tinyint null,
NSubgroup int null,
EquityTestsActual int null,
EquityTestsExpected int null,
EqityParticipationRate decimal(4,1) null,
EquityRequiredTestedLevel1 int not null,
EquityRequiredTestedLevel2 int not null,
EquityParticipationCat int not null,
EquityParticipationMetLevel1 varchar(10) not null,
EquityParticipationMetLevel2 varchar(10) not null,
EquityTargetLevel varchar(100) null,
NIndicators tinyint not null,
ParticipationRate decimal(4,1) null,
ParticipationRateLevel varchar(100) null,
ParticipationCat varchar(100) null,
SPL varchar(100),
SPLAccountability varchar(100),
SmallSchoolScore tinyint null,
SmallSchoolDecision varchar(100) null,
CONSTRAINT acct_G38SchoolIndictors_PK PRIMARY KEY (SchoolYear, SchoolId),
CONSTRAINT acct_G38SchoolIndictors_FK FOREIGN KEY (SchoolYear, SchoolId) REFERENCES acct.SPLSchool(SchoolYear, SchoolId)
)

GO


CREATE TABLE acct.HSSchoolIndicators
(

SchoolYear varchar(7) not null,
SchoolId varchar(7) not null,
WAEASmallSchool varchar(1) not null,
SmallSchoolAchievement varchar(1) not null,
YearsBackAchievement tinyint null,
AchievementLowCut tinyint null,
AchievmentHighCut tinyint null,
AchievementTests int null,
AchievementTestsProficient int null,
PercentProficient tinyint null,
NAchievement int not null,
AchievementTestsActual int not null,
AchievementTestsExpected int not null,
AchievementParticipationRate decimal(4,1) null,
AchievementRequiredTestedLevel1 int not null,
AchievementRequiredTestedLevel2 int not null,
AchievmentParticipationCat int not null,
AchievementParticipationMetLevel1 varchar(10) not null,
AchievementParticipationMetLevel2 varchar(10) not null,
AchievementTargetLevel varchar(100) null,


SmallSchoolEquity varchar(1) not null,
YearsBackEquity tinyint null,
EquityLowCut tinyint null,
EquityHighCut tinyint null,
EquityScore tinyint null,
NSubgroup int not null,
EquityTestsActual int not null,
EquityTestsExpected int not null,
EqityParticipationRate decimal(4,1) null,
EquityRequiredTestedLevel1 int not null,
EquityRequiredTestedLevel2 int not null,
EquityParticipationCat int not null,
EquityParticipationMetLevel1 varchar(10) not null,
EquityParticipationMetLevel2 varchar(10) not null,
EquityTargetLevel varchar(100) null,

GradRateLowCut tinyint null,
GradRateHighCut tinyint null,
GradRate4yearPrior decimal(4,1) null,
NGradRate4yearPrior int null,
GradRate4year decimal(4,1) null,
NGradRate4year int null,
GradRateExtended decimal(4,1) null,
NGradRateExtended int null,
GradRateImprove decimal(4,1) null,
NGradRate int null,
GradRateTargetLevel varchar(100) null,

SmallSchoolGrade9Credits varchar(1) not null,
NGrade9Credits int null,
NGrade9CreditsMet int null,
Grade9CreditsPercentMet tinyint null,
GradCreditsRequired decimal(4,1) null,
Grade9CreditsRequired decimal(5,3) null,
Grade9CreditsPercentMetWeight tinyint null,
Grade9CreditsPercentMetWeighted decimal(4,1) null,
Grade9CreditsPercentMetSubReport tinyint null,

SmallSchoolHathEligibile varchar(1) not null,
NHathEligibile int null,
HathEligibileMeanScore tinyint null,
HathEligibileMeanScoreWeight tinyint null,
HathEligibileMeanScoreWeighted decimal(4,1) null,


SmallSchoolTestedReadiness varchar(1) not null,
YearsBackTestedReadiness tinyint null,
TestedReadinessMeanScore tinyint null,
NTestedReadiness int not null,
TestedReadinessTestsActual int not null,
TestedReadinessTestsExpected int not null,
TestedReadinessParticipationRate decimal(4,1) null,
TestedReadinessRequiredTestedLevel1 int not null,
TestedReadinessRequiredTestedLevel2 int not null,
TestedReadinessParticipationCat int not null,
TestedReadinessParticipationMetLevel1 varchar(10) not null,
TestedReadinessParticipationMetLevel2 varchar(10) not null,
TestedReadinessMeanScoreWeight tinyint null,
TestedReadinessMeanScoreWeighted decimal(4,1) null,

AddReadinessType varchar(20) not null,
AddReadinessLowCut tinyint null,
AddReadinessHighCut tinyint null,
NAddReadiness int null,
AddReadinessScore tinyint null,
AddReadinessTargetLevel varchar(100) null,

NAchievementIndictors tinyint not null,
NReadinessIndictors tinyint not null,
NIndicators tinyint not null,
OverallReadinessTargetLevel varchar(100) null,
AcademicPerformanceTargetLevel varchar(100) null,
ParticipationRate decimal(4,1) null,
ParticipationRateLevel varchar(100) null,
ParticipationCat varchar(100) null,

SPL varchar(100),
SPLAccountability varchar(100),
SmallSchoolScore tinyint null,
SmallSchoolDecision varchar(100) null,
CONSTRAINT acct_HSSchoolIndictors_PK PRIMARY KEY (SchoolYear, SchoolId),
CONSTRAINT acct_HSSchoolIndictors_FK FOREIGN KEY (SchoolYear, SchoolId) REFERENCES acct.SPLSchool(SchoolYear, SchoolId)
)
GO


drop table acct.HSStatsBySubject;
GO

CREATE TABLE acct.HSStatsBySubject (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   Science decimal(10,1) null,
   Writing decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT acct_HSStatsBySubject_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Statistic)
)
GO



CREATE NONCLUSTERED INDEX acct_HSStatsBySubject_idx1
ON acct.HSStatsBySubject (SchoolYear, SchoolId, Statistic)
GO

CREATE NONCLUSTERED INDEX acct_HSStatsBySubject_idx2
ON acct.HSStatsBySubject ([Order])
GO


drop table acct.HSStatsByTest;
GO

CREATE TABLE acct.HSStatsByTest (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Explore decimal(10,1) null,
   [Plan] decimal(10,1) null,
   ACT decimal(10,1) null,
   Alt decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT acct_HSStatsByTest_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Statistic)
)
GO



CREATE NONCLUSTERED INDEX acct_HSStatsByTest_idx1
ON acct.HSStatsByTest (SchoolYear, SchoolId, Statistic)
GO

CREATE NONCLUSTERED INDEX acct_HSStatsByTest_idx2
ON acct.HSStatsByTest ([Order])
GO



drop table acct.HSHathaway
GO

CREATE TABLE acct.HSHathaway (

   [Scope] varchar(50) not null,
   SchoolYear varchar(7) not null,
   SchoolId varchar(7) not null,
   Statistic varchar(100) not null,
   [Order] bigint not null,
   [Cat1] tinyint null,
   [Cat2] tinyint null,
   [Cat3] tinyint null,
   [Cat4] tinyint null,
   [Cat5] tinyint null,
   N int not null,
   CONSTRAINT acct_HSHathaway_PK PRIMARY KEY ([Scope], SchoolYear, SchoolId, Statistic)
)
GO



CREATE NONCLUSTERED INDEX acct_HSHathaway_idx1
ON acct.HSHathaway (SchoolYear, SchoolId, Statistic)
GO

CREATE NONCLUSTERED INDEX acct_HSHathaway_idx2
ON acct.HSHathaway ([Order])
GO


drop view acct.G38SPLSummary
GO 

CREATE view acct.G38SPLSummary
as
select 
s.DistrictId, 
s.DistrictName, 
s.SchoolId,
s.Name,
s.GradesServed,
i.SPLAccountability,
i.GrowthTargetLevel,
i.EquityTargetLevel, 
i.AchievementTargetLevel,
i.ParticipationCat,
p.SPLAdjusted SPLAccountabilityPilot,
(case p.GrowthTargetLevel 
when 'Not Meeting Targets' then 'Below Targets'
else p.GrowthTargetLevel end) GrowthTargetLevelPilot,
(case p.EquityTargetLevel
when 'Not Meeting Targets' then 'Below Targets'
else  p.EquityTargetLevel end) EquityTargetLevelPilot, 
(case p.AchievementTargetLevel 
when 'Not Meeting Targets' then 'Below Targets'
else p.AchievementTargetLevel end) AchievementTargetLevelPilot,
p.ParticipationRateLevel ParticipationRateLevelPilot,
(case i.SPLAccountability when 'Exceeding Expectations' then 4
 when 'Meeting Expectations' then 3
 when 'Partially Meeting Expectations' then 2
 when 'Not Meeting Expectations' then 1
 else NULL end) - 
 (case p.SPLAdjusted when 'Exceeding Expectations' then 4
 when 'Meeting Expectations' then 3
 when 'Partially Meeting Expectations' then 2
 when 'Not Meeting Expectations' then 1
 else NULL end) as ChangeInSPL
from 
acct.SPLSchool s join
acct.G38SchoolIndicators i on s.schoolyear=i.schoolyear and s.schoolid=i.schoolid left join
acct.SchoolIndictorsNonHS p on p.schoolyear=dbo.DeltaSchoolYear(s.SchoolYear,-1) and p.schoolid=i.schoolid 

GO

drop view acct.HSSPLSummary
GO

CREATE view acct.HSSPLSummary
as
select 
s.DistrictId, 
s.DistrictName, 
s.SchoolId,
s.Name,
s.GradesServed,
i.SPLAccountability,
i.AchievementTargetLevel,
i.EquityTargetLevel,
i.GradRateTargetLevel,
i.AddReadinessTargetLevel AdditionalReadinessTargetLevel,
i.AcademicPerformanceTargetLevel,
i.OverallReadinessTargetLevel,
i.ParticipationCat,
p.SPLAdjusted SPLAccountabilityPilot,
(case p.AchievementTargetLevel 
when 'Not Meeting Targets' then 'Below Targets'
else p.AchievementTargetLevel end) AchievementTargetLevelPilot,
(case p.ReadinessTargetLevel 
when 'Not Meeting Targets' then 'Below Targets'
else p.ReadinessTargetLevel  end) ReadinessTargetLevelPilot,
(case p.EquityTargetLevel 
when 'Not Meeting Targets' then 'Below Targets'
else p.EquityTargetLevel  end) EquityTargetLevelPilot,
p.ParticipationRateLevel ParticipationRateLevelPilot,
(case i.SPLAccountability when 'Exceeding Expectations' then 4
 when 'Meeting Expectations' then 3
 when 'Partially Meeting Expectations' then 2
 when 'Not Meeting Expectations' then 1
 else NULL end) - 
 (case p.SPLAdjusted when 'Exceeding Expectations' then 4
 when 'Meeting Expectations' then 3
 when 'Partially Meeting Expectations' then 2
 when 'Not Meeting Expectations' then 1
 else NULL end) as ChangeInSPL
from 
acct.SPLSchool s join
acct.HSSchoolIndicators i on s.schoolyear=i.schoolyear and s.schoolid=i.schoolid left join
acct.SchoolIndictorsHS p on p.schoolyear=dbo.DeltaSchoolYear(s.SchoolYear,-1) and p.schoolid=i.schoolid 


GO
