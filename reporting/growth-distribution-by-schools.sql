
drop table growth.GrowthDistributionBySchoolScoped;

CREATE TABLE growth.GrowthDistributionBySchoolScoped(
        DataScope varchar(15) NOT NULL,
	DistrictId varchar(7) NULL,
	DistrictName varchar(255) NULL,
	SchoolId varchar(7) NOT NULL,
	SchoolName varchar(255) NULL,
	SchoolYear varchar(7) NOT NULL,
	GradeEnrolled varchar(7) NOT NULL,
	SubjectCode varchar(31) NOT NULL,
  	Subject varchar(31) NOT NULL,
	NLowGrowth int NULL,
	NTypicalGrowth int NULL,
	NHighGrowth int NULL,
	NGrowthScores int NULL,
	NStudentsGrowth int NULL,
	PLowGrowth decimal(5, 2) NULL,
	PTypicalGrowth decimal(5, 2) NULL,
	PHighGrowth decimal(5, 2) NULL,
	PProficient decimal(5, 2) NULL,
	NProficient int NULL,
	NStudentsAchievement int NULL,
	NTestsAchievement int NULL,
    CONSTRAINT GrowthDistributionBySchoolScoped_UNQ UNIQUE CLUSTERED (
	DataScope ASC,
	SchoolYear ASC,
	DistrictId ASC,
	SchoolId ASC,
	GradeEnrolled ASC,
	SubjectCode ASC)
)


drop table growth.GrowthDistributionBySchool
GO

CREATE VIEW growth.GrowthDistributionBySchool
AS
SELECT
DistrictId,
DistrictName,
SchoolId,
SchoolName,
SchoolYear,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NGrowth, --column is obsolete
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTested, --column is obsolete
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE GradeEnrolled = 'ALL' and DataScope='SCHOOL'
GO

drop view growth.GrowthCountsBySchool
GO
drop view growth.vGrowthDistributionBySchool
GO


CREATE VIEW growth.GrowthDistributionBySchoolGrade
AS
SELECT
DistrictId,
DistrictName,
SchoolId,
SchoolName,
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE DataScope='SCHOOL'
GO


CREATE VIEW growth.GrowthDistributionByDistrict
AS
SELECT
DistrictId,
DistrictName,
SchoolYear,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE GradeEnrolled = 'ALL' and DataScope='DISTRICT'
GO


CREATE VIEW growth.GrowthDistributionByDistrictGrade
AS
SELECT
DistrictId,
DistrictName,
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE DataScope='DISTRICT'
GO


CREATE VIEW growth.GrowthDistributionState
AS
SELECT
SchoolYear,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE GradeEnrolled = 'ALL' and DataScope='STATE'
GO


CREATE VIEW growth.GrowthDistributionByStateGrade
AS
SELECT
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolScoped
WHERE DataScope='STATE'
GO



CREATE TABLE growth.GrowthDistributionBySchoolSubgroups(
        DataScope varchar(15) NOT NULL,
	DistrictId varchar(7) NULL,
	DistrictName varchar(255) NULL,
	SchoolId varchar(7) NULL,
	SchoolName varchar(255) NULL,
	SchoolYear varchar(7) NOT NULL,
	GradeEnrolled varchar(7) NULL,
	SubgroupCode varchar(7) NULL,
	SubgroupDescription varchar(255) NULL,
 	StudentMobility varchar(12) NULL,
	SubjectCode varchar(31) NULL,
  	Subject varchar(31) NULL,
	NLowGrowth int NULL,
	NTypicalGrowth int NULL,
	NHighGrowth int NULL,
	NGrowthScores int NULL,
	NStudentsGrowth int NULL,
	PLowGrowth decimal(5, 2) NULL,
	PTypicalGrowth decimal(5, 2) NULL,
	PHighGrowth decimal(5, 2) NULL,
  MGP decimal(3,1) NULL,
	PProficient decimal(4, 1) NULL,
	NProficient int NULL,
	NStudentsAchievement int NULL,
	NTestsAchievement int NULL,
    CONSTRAINT GrowthDistributionBySchoolSubgroups_UNQ UNIQUE CLUSTERED (
	DataScope ASC,
	SchoolYear ASC,
	DistrictId ASC,
	SchoolId ASC,
	GradeEnrolled ASC,
	SubgroupCode ASC,
	StudentMobility ASC,
	SubjectCode ASC)
)


ALTER VIEW growth.GrowthDistributionBySchool
AS
SELECT
DistrictId,
DistrictName,
SchoolId,
SchoolName,
SchoolYear,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NGrowth, --column is obsolete
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTested, --column is obsolete
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE GradeEnrolled = 'ALL' and DataScope='SCHOOL'
GO


ALTER VIEW growth.GrowthDistributionBySchoolGrade
AS
SELECT
DistrictId,
DistrictName,
SchoolId,
SchoolName,
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE DataScope='SCHOOL'
GO


ALTER VIEW growth.GrowthDistributionByDistrict
AS
SELECT
DistrictId,
DistrictName,
SchoolYear,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE GradeEnrolled = 'ALL' and DataScope='DISTRICT'
GO


ALTER VIEW growth.GrowthDistributionByDistrictGrade
AS
SELECT
DistrictId,
DistrictName,
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE DataScope='DISTRICT'
GO


ALTER VIEW growth.GrowthDistributionState
AS
SELECT
SchoolYear,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE GradeEnrolled = 'ALL' and DataScope='STATE'
GO


ALTER VIEW growth.GrowthDistributionByStateGrade
AS
SELECT
SchoolYear,
GradeEnrolled,
Subject SubjectCode,
SubgroupCode,
SubgroupDescription,
StudentMobility,
NLowGrowth,
NTypicalGrowth,
NHighGrowth,
NGrowthScores NSGPs,
NStudentsGrowth,
PLowGrowth,
PTypicalGrowth,
PHighGrowth,
PProficient,
NProficient,
NTestsAchievement NTests,
NStudentsAchievement
FROM growth.GrowthDistributionBySchoolSubgroups
WHERE DataScope='STATE'
GO


CREATE VIEW growth.SchoolsBubblePlot AS
SELECT
      [SchoolYear]
      ,[SchoolId]
      ,[SchoolName]
      ,DistrictId
      ,DistrictName
      ,[SubgroupCode]
      ,[SubgroupDescription]
      ,[StudentMobility] as FullAcademicYear
      ,[SubjectCode]
      ,[Subject]
      ,MGP
      ,[NGrowthScores]
      ,[NStudentsGrowth]
      ,[PProficient]
      ,[NProficient]
      ,[NStudentsAchievement]
      ,[NTestsAchievement]
FROM      
[growth].[GrowthDistributionBySchoolSubgroups]
WHERE DataScope='SCHOOL' and GradeEnrolled='All'


CREATE VIEW growth.DistrictsBubblePlot AS
SELECT
      [SchoolYear]
      ,DistrictId
      ,DistrictName
      ,[SubgroupCode]
      ,[SubgroupDescription]
      ,[StudentMobility] as FullAcademicYear
      ,[SubjectCode]
      ,[Subject]
      ,MGP
      ,[NGrowthScores]
      ,[NStudentsGrowth]
      ,[PProficient]
      ,[NProficient]
      ,[NStudentsAchievement]
      ,[NTestsAchievement]
FROM      
[growth].[GrowthDistributionBySchoolSubgroups]
WHERE DataScope='DISTRICT' and GradeEnrolled='All' 


