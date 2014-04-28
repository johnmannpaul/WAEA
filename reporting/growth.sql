
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


CREATE NONCLUSTERED INDEX growth_GradeLevelMGP_idx1
ON growth.GradeLevelMGP (SchoolYear, SchoolFullAcademicYear, SchoolId, Statistic)

CREATE UNIQUE NONCLUSTERED INDEX growth_GradeLevelMGP_idx2
ON growth.GradeLevelMGP (Statistic, [Order])


