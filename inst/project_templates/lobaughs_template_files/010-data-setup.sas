
*****************************************************************************************************************
                                                                                                     
DESCRIPTION: Import raw data and create analysis dataset

TOC (Use Ctr+F to navigate through code):
-Import
-Explore raw data
-Create analysis dataset
-Check analysis dataset 
-Save analysis dataset
-Export data for use in R

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

* Macro library;
libname gitmacs "C:\Users\lobaughs\GitHub\macros";
options mstored sasmstore = gitmacs;



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

* Import data dictionary to apply labels and formats later;
proc import 
	datafile = "&path_data.\_data-dictionary.xlsx"
	dbms = xlsx
	out = data.dictionary
	replace;
	getnames = yes;
run;

* Run attribute macro to create global macro variable "&attrib" that can be used in the attrib statement
  in a data step to apply labels/formats;
%attribute(dictionary_ds = data.dictionary)


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

* Create list of variable names in derived dataset;
proc sql;
	create table vars_derived as
	select name
	from dictionary.columns where libname = "WORK" and memname = "DERIVED";
quit;

* Create dataset with variable names in both the derived and data dictionary datasets;
proc sql;
	create table vars_both as
	select A.variable, B.name
	from data.dictionary A
			inner join
		 vars_derived B on lowcase(A.variable) = lowcase(B.name);
quit;

proc sql;
	select variable
	into :keep_vars separated by ' '
	from vars_both;
quit;

* Save to a permanent location. Only keep project variables;
data data.df_analysis; set derived; attrib &attrib; keep mrn &keep_vars; run;


/**************************************************************************************************************
-Export data for use in R
**************************************************************************************************************/

proc sql;
	select variable || "=" || strip(variable) || "_old"
	into :rename_old separated by " "
	from data.dictionary where type_1cont_2cat = 2 and not missing(fmt) and fmt ne "$missing." and
		 find(lowcase(variable),"delta") = 0;
 
	select strip(variable) || "=put(" || strip(variable) || "_old," || strip(fmt) || ")" || %str(";")
	into :label_values separated by " "
	from data.dictionary where type_1cont_2cat = 2 and not missing(fmt) and fmt ne "$missing." and
		 find(lowcase(variable),"delta") = 0;;
quit;

data data.drvd_forR;
	set data.df_analysis(rename = (&rename_old));
	&label_values;
run;

proc export
	data = data.df_analysis_forR
	outfile = "&h./secure_data/&date./df_analysis_val_labels.xlsx"
	dbms = xlsx
	replace;
run;

