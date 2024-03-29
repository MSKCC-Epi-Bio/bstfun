
*****************************************************************************************************************

DESCRIPTION: Import raw data and create analysis dataset

TOC (Use Ctr+F to navigate through code):
-Import
-Explore raw data
-Create analysis dataset
-Check analysis dataset
-Save analysis dataset

---------------------------------------------------------------------------------------------------------------

NAME:
DATE:
{{Sys.Date()}}: Created project folder

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
-Import
**************************************************************************************************************/

* example code for importing excel file;
proc import datafile = "&path_data.\&date.\insert-file-name.xlsx"
	dbms = xlsx
	out = data.raw
	replace;
	getnames = yes;
	sheet = "Sheet1";
run;



/**************************************************************************************************************
-Explore raw data
**************************************************************************************************************/

proc contents data = data.raw; run;



/**************************************************************************************************************
-Create analysis dataset
**************************************************************************************************************/



/**************************************************************************************************************
-Check analysis dataset
**************************************************************************************************************/



/**************************************************************************************************************
-Save analysis dataset
**************************************************************************************************************/

data data.df_analysis; set df_analysis; run;

