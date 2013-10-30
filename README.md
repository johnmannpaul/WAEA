WAEA
====

R implementation of accountability model

To reproduce the 2012-13 results you must run "process-with-corrections.R".  This invokes "process-multiyear-subgroup.R", 
which produces results for the elementary and middle schools that take into account a multi-year equity indicator, first,
followed by "process.R" which produces results without the multi-year equity indicator.  The results are then combined 
according to the hold-harmless strategy on the elementary and middle school equity indicator.

TODO for 2013-14:

* A Participation rate for the subgroup
* All participation rates to include all students not just FAY students.
