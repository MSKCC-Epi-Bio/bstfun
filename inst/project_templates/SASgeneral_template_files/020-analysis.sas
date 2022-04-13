
*****************************************************************************************************************

DESCRIPTION: Analysis

TOC (Use Ctr+F to navigate through code):
- [Code section label]
- Save results

---------------------------------------------------------------------------------------------------------------

NAME:
DATE:
{{Sys.Date()}}: Created

****************************************************************************************************************;

* run prep program;
%include "{{path}}\_prep.sas";

* data library;
%Let path_data = {{ifelse(is.null(path_data), "", path_data)}};
libname data "&path_data.\&date";

* formats library;
options nofmterr;
libname fmt "&path_data";
options fmtsearch = (fmt.formats);


/**************************************************************************************************************
-
**************************************************************************************************************/


/**************************************************************************************************************
-Save results
Save as permanent data sets
**************************************************************************************************************/

* Example: save data used to create KM plot;
data data.kmplotdata;
	set kmplotdata;
run;



