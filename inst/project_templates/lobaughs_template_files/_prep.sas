
*****************************************************************************************************************
                                                                                                     
DESCRIPTION: Run at beginning of each SAS program to set options, load data_date.txt, create useful
global macro variables, etc.

---------------------------------------------------------------------------------------------------------------                               
                                                                                           
DATE: 
{{Sys.Date()}}: Created project folder                                                                                      
                                                                   
****************************************************************************************************************;

* clear all libraries;
libname _all_ clear;

* Prevent SAS from printing any titles until you request them;
OPTIONS NODATE NONUMBER;
TITLE1; TITLE2;
ods noproctitle;

* load most recent version of each macro and template in the MSK Epi/Biostats repo (https://github.com/MSKCC-Epi-Bio/msk_SAS_macros);
FILENAME mskm URL "https://raw.githubusercontent.com/MSKCC-Epi-Bio/create_msk_SAS_project/main/utility.sas";
%INCLUDE mskm;

* SL macro library;
libname gitmacs "C:\Users\lobaughs\GitHub\macros";
options mstored sasmstore = gitmacs;

* import data_date.txt;
proc import datafile = "{{path}}\data_date.txt"
	out = tmp.data_date
	dbms = dlm
	replace;
	getnames = NO;
run;
* assign data folder date to a macro var called date;
proc sql noprint; select strip(put(var1, yymmdd10.)) into :date trimmed from tmp.data_date; quit;
%put &=date;

* escape character;
ods escapechar = "^";

* Todays date;
%let today_yymmdd = %sysfunc(today(), yymmdd7.);
%let today = %sysfunc(today(), worddate.);
%put &=today_yymmdd &=today;

* Define styles for text in output;
%Let style16 = fontsize = 16pt fontweight = bold fontfamily = Arial;
%Let style14 = fontsize = 14pt fontweight = bold fontfamily = Arial;
%Let style12 = fontsize = 12pt fontweight = bold fontfamily = Arial;
%Let style11 = fontsize = 11pt fontfamily = Arial;

