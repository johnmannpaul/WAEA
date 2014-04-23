use report_dm
go

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



--Save the file as comma delimited text, no quotes, the largest value for a statistic is 999999999.9
--You have to copy the file to the database server or use a share.
--It is assumed that the first row of the file has column headings.
BULK
INSERT acct.GradeLevelAchievement
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\grade-level-statistics.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


CREATE NONCLUSTERED INDEX acct_GradeLevelAchievement_idx1
ON acct.GradeLevelAchievement (SchoolYear, SchoolId, Statistic)

CREATE NONCLUSTERED INDEX acct_GradeLevelAchievement_idx2
ON acct.GradeLevelAchievement ([Order])


SELECT * from 
acct.GradeLevelAchievement where
SchoolYear='2012-13' and SchoolId in ('0101001') and Statistic = 'PERCENT_PROFICIENT'
order by [Order]


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


truncate table acct.GradeLevelGrowth
GO

--Save the file as comma delimited text, no quotes, the largest value for a statistic is 999999999.9
--You have to copy the file to the database server or use a share.
--It is assumed that the first row of the file has column headings.
BULK
INSERT acct.GradeLevelGrowth
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\grade-level-statistics-growth.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


CREATE NONCLUSTERED INDEX acct_GradeLevelGrowth_idx1
ON acct.GradeLevelGrowth (SchoolYear, SchoolId, Statistic)

CREATE NONCLUSTERED INDEX acct_GradeLevelGrowth_idx2
ON acct.GradeLevelGrowth (Statistic, [Order])


SELECT * from 
acct.GradeLevelGrowth where
SchoolYear='2012-13' and SchoolId in ('0101001') and Statistic = 'MGP'
order by [Order]



BULK
INSERT acct.GradeLevelGrowth
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\grade-level-statistics-equity.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


SELECT * from 
acct.GradeLevelGrowth where
SchoolYear='2012-13' and SchoolId in ('1101023') and Statistic = 'PERCENT_MET_AGP'
order by [Order]


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


BULK
INSERT acct.School
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\schools.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO

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

BULK
INSERT acct.SchoolIndictorsNonHS
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\school-indicators-nonHS.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO



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


BULK
INSERT acct.SchoolIndictorsHS
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\school-indicators-HS.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO

--delete the indicator rows for the schools for which they do not apply
delete acct.SchoolIndictorsNonHS from 
       acct.SchoolIndictorsNonHS i join acct.School s on i.SchoolId = s.SchoolId and i.SchoolYear = s.SchoolYear and s.WAEASchoolType not in (1,3,4,5,6,7)
       
delete acct.SchoolIndictorsHS from 
       acct.SchoolIndictorsHS i join acct.School s on i.SchoolId = s.SchoolId and i.SchoolYear = s.SchoolYear and s.WAEASchoolType not in (2,4,5)       


--put the unrated schools into the under-review category
update acct.School set SPLAccountability='Under Review' where SPLAccountability is NULL;
update acct.SchoolIndictorsNonHS set SPL='Under Review', SPLAdjusted='Under Review' where SPL is NULL;
update acct.SchoolIndictorsHS set SPL='Under Review', SPLAdjusted='Under Review' where SPL is NULL;

--just fill these in for completeness
update acct.SchoolIndictorsHS set ReadinessHighCut= 81.0,
ReadinessLowCut = 71.0 where ReadinessHighCut is null and 
SchoolYear='2012-13'

update acct.SchoolIndictorsHS set EquityHighCutReversed = 3.4,
EquityLowCutReversed = -3.2 where EquityHighCutReversed is null and 
SchoolYear='2012-13'

update acct.SchoolIndictorsNonHS set GrowthHighCut= 60,
GrowthLowCut = 45 where GrowthHighCut is null and 
SchoolYear='2012-13'

update acct.SchoolIndictorsNonHS set EquityHighCut= 55,
EquityLowCut = 40 where EquityHighCut is null and 
SchoolYear='2012-13'

--only three schools this year with no achievement scores.
--all three are elementaries, so just use the elementary cut scores.
update acct.SchoolIndictorsNonHS set AchievmentHighCut= 86,
AchievementLowCut = 75 where AchievmentHighCut is null and 
SchoolYear='2012-13'


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



--Save the file as comma delimited text, no quotes, the largest value for a statistic is 999999999.9
--You have to copy the file to the database server or use a share.
--It is assumed that the first row of the file has column headings.
BULK
INSERT acct.GradeLevelAchievementHS
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\grade-level-high-school-achievement.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


SELECT * from acct.GradeLevelAchievementHS
where SchoolId in ('1101055','7700000') and Grade='11' and Statistic='PERCENT_PROFICIENT'
order by [Order]

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


BULK
INSERT acct.GradeLevelEquityHS
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\high-school-equity.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO

SELECT * from acct.GradeLevelEquityHS
where SchoolId in ('1101055','7700000')
order by [Order]



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


BULK
INSERT acct.ReadinessHS
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\high-school-readiness.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


SELECT * from acct.ReadinessHS
where SchoolId in ('1101055','7700000')
order by [Order]


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


BULK
INSERT acct.GradeLevelACTSuite
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\high-school-act.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


SELECT * FROM acct.GradeLevelACTSuite
WHERE SchoolId='1301057' and schoolyear='2012-13'
order by Statistic, [Order]

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

select * from acct.SchoolListRated 
ORDER BY SchoolYear, DistrictName, Name