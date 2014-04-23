
create schema growth;


drop table growth.GradeLevelMGP;

CREATE TABLE growth.GradeLevelMGP(

   SchoolYear varchar(7) not null,
   SchoolFullAcademicYear varchar(100) not null,
   Grade varchar(3) not null,
   SchoolId varchar(7) not null,
   Statistic varchar(100) not null,
   [All] decimal(10,1) null,
   Reading decimal(10,1) null,
   Mathematics decimal(10,1) null,
   [Order] bigint not null,
   N int not null,
   CONSTRAINT growth_GradeLevelMGP_PK PRIMARY KEY (SchoolYear, SchoolFullAcademicYear, SchoolId, Grade, Statistic)
)
GO



--Save the file as comma delimited text, no quotes, the largest value for a statistic is 999999999.9
--You have to copy the file to the database server or use a share.
--It is assumed that the first row of the file has column headings.
BULK
INSERT growth.GradeLevelMGP
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\growth.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO



CREATE NONCLUSTERED INDEX growth_GradeLevelMGP_idx1
ON growth.GradeLevelMGP (SchoolYear, SchoolFullAcademicYear, SchoolId, Statistic)

CREATE UNIQUE NONCLUSTERED INDEX growth_GradeLevelMGP_idx2
ON growth.GradeLevelMGP (Statistic, [Order])


SELECT * from 
growth.GradeLevelMGP where
SchoolYear='2012-13' and SchoolId in ('0101001', '7700000') and Statistic = 'MGP'
order by [Order]

select distinct statistic from growth.GradeLevelMGP

/*
BULK
INSERT growth.GradeLevelMGP
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\growth-agp.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO


delete from growth.GradeLevelMGP where STATISTIC in ('PERCENT_MET_AGP', 'N_MET_AGP')
*/

BULK
INSERT growth.GradeLevelMGP
FROM 'D:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\Bulk\growth-met-agp.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '\n',
FIRSTROW=2
)
GO

update growth.GradeLevelMGP 
set SchoolFullAcademicYear = 'Full Academic Year'
Where SchoolFullAcademicYear='T'

update growth.GradeLevelMGP 
set SchoolFullAcademicYear = 'Non Full Academic Year (Mobile)'
Where SchoolFullAcademicYear='F'

update growth.GradeLevelMGP 
set SchoolFullAcademicYear = 'All Students'
Where SchoolFullAcademicYear='ALL'

SELECT * from 
growth.GradeLevelMGP where
SchoolYear='2012-13' and SchoolId in ('1101023', '7700000') and Statistic = 'MGP'
order by [Order]


SELECT * from 
growth.GradeLevelMGP where
SchoolYear='2012-13' and SchoolId in ('1101023', '7700000') and Statistic = 'MAGP'
order by [Order]

SELECT * from 
growth.GradeLevelMGP where
SchoolYear='2012-13' and SchoolId in ('1101023', '7700000') and Statistic = 'PERCENT_MET_AGP'
order by [Order]


