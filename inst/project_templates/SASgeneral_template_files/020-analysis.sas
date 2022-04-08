
*****************************************************************************************************************
                                                                                                     
DESCRIPTION: Analysis

TOC (Use Ctr+F "X)" to navigate through code):
- [Code section label]
- Save results

---------------------------------------------------------------------------------------------------------------
                                      
LANGUAGE: SAS, VERSION 9.4                                  
                                                               
NAME:                               
DATE:
{{Sys.Date()}}: Created                                                                                         
                                                                   
****************************************************************************************************************;

* various settings;
%include "{{path}}\_prep.sas";
%prep();

* data library;
%Let path_data = {{path_data}};
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



