USE report_dm
GO


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
 CONSTRAINT GrowthDistributionBySchoolScoped_PK UNIQUE KEY CLUSTERED (
	DataScope ASC,
	SchoolYear ASC,
	DistrictId ASC,
	SchoolId ASC,
	GradeEnrolled ASC,
	SubjectCode ASC)
) 


