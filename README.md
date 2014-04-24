WAEA
====

R implementation of accountability model

To reproduce the 2012-13 results you must run "process-with-corrections.R".  This invokes "process-multiyear-subgroup.R", 
which produces results for the elementary and middle schools that take into account a multi-year equity indicator, first,
followed by "process.R" which produces results without the multi-year equity indicator.  The results are then combined 
according to the hold-harmless strategy on the elementary and middle school equity indicator.

Specific steps to reproduce 2012-13 pilot results

1) git clone https://github.com/johnmannpaul/WAEA.git -b 2012-13-Pilot /some/folder/2012-13-Pilot
2) cd /some/folder/2012-13-Pilot
3) mkdir const/private
4) create file db.R defining
   db.dsn <- ...
   assess.user <- ...
   assess.pwd <- ...
   account.user <- ...
   account.pwd <- ...
5) in R source load-from-oracle.R
6) in R source process-with-corrections.R
7) mkdir results
8) in R write.csv(schools, file="results/2012-13-Pilot.csv", na="", row.names=FALSE))

TODO for 2013-14:

* A Participation rate for the subgroup
* All participation rates to include all students not just FAY students.
