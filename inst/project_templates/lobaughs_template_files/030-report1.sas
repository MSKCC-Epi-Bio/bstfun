
*****************************************************************************************************************

DESCRIPTION: Generate Report 1 (export to RTF)

TOC (Use Ctr+F to navigate through code):
-Report

---------------------------------------------------------------------------------------------------------------

NAME: Stephanie Lobaugh
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
-Report
This code can be run as is to see what the RTF file looks like
**************************************************************************************************************/

***** Begin RTF file;

* Text styles (e.g. &style16) are defined in "{{path}}\_prep.sas";

ods rtf file = "{{path}}\030_report1_&today_yymmdd..rtf"
	style = styles.msk
	bodytitle
	startpage = no
    wordstyle =
"\s1 Heading 1
 \s2 Heading 2
 \s3 Heading 3";

proc odstext;
	p "{Title}"
    / style = [&style16];
	p "{}" / style = [fontsize = 11pt];

	p "{Stephanie Lobaugh, MS \par &&today.}"
    / style = [&style11 fontstyle = italic];
	p "{}" / style = [fontsize = 11pt];

	p "{Output generated using the following dataset received &date.: [insert raw data file name]}"
    / style = [&style11 fontstyle = italic];
	p "{}" / style = [fontsize = 11pt];

	p "{Please note that some contents of this report are intended for internal use only and do
 not need to be included in the abstract/manuscript (i.e. some descriptions/tables/figures are included to aid
 interpretation or as quality assurance checks for the project team).}" 
    / style = [&style11 fontstyle = italic];
	p "{}" / style = [fontsize = 11pt];
run;

*** Overview;
proc odstext;
	p "{\pard\s1\b\ul Overview \par}" / style = {&style16};
	p "{Study description}"
    / style = [&style11];
	p "{Methods}"
    / style = [&style11];
	p "{Results}"
    / style = [&style11];
		p "{Heading2}"
	    / style = [&style11 leftmargin = 0.35in];
			p "{Heading3}"
		    / style = [&style11 leftmargin = 0.7in];
	p "{Heading1}"
    / style = [&style11];
	p "{}" / style = [fontsize = 11pt];

	p "{To view the navigation pane for this document, please do the following in Word: Under the 'View' tab, check
 the 'Navigation Pane' box in the 'Show' section.}"
	/ style = [fontsize = 11pt fontweight = bold];
	p "{}" / style = [fontsize = 11pt];
run;

*** Study description;
proc odstext;
	p "{\pard\s1\b\ul Study description \par}" / style = {&style16};


	p "{In this report...}"
	/ style = [&style11];
	p "{}" / style = [fontsize = 11pt];

	p "{The analysis objectives included: (1) , (2) , and (3).}"
	/ style = [&style11];
	p "{}" / style = [fontsize = 11pt];
run;

*** Methods;
proc odstext;
	p "{\pard\s1\b\ul Methods \par}" / style = {&style16};

	p "{Analysis objective (1)}"
	/ style = [&style11 fontweight = bold];
	p "{}" / style = [fontsize = 11pt];

	p "{INSERT METHODS FOR ANALYSIS OBJECTIVE 1.}"
	/ style = [&style11 leftmargin = 0.35in];
	p "{}" / style = [fontsize = 11pt];

	p "{Analysis objective (2)}"
	/ style = [&style11 fontweight = bold];
	p "{}" / style = [fontsize = 11pt];

	p "{INSERT METHODS FOR ANALYSIS OBJECTIVE 2.}"
	/ style = [&style11 leftmargin = 0.35in];
	p "{}" / style = [fontsize = 11pt];

	p "{Analysis objective (3)}"
	/ style = [&style11 fontweight = bold];
	p "{}" / style = [fontsize = 11pt];

	p "{METHODS FOR ANALYSIS OBJECTIVE 3.}"
	/ style = [&style11 leftmargin = 0.35in];
	p "{}" / style = [fontsize = 11pt];

	p "{All statistical computations were performed, and all output was generated using SAS Software Version INSERT-VERSION
 (The SAS Institute, Cary, NC).}"
    / style = [&style11];
	p "{}" / style = [fontsize = 11pt];
run;

*** Results;
proc odstext;
	p "{\pard\s1\b\ul Results \par}" / style = {&style16};
run;

* Example heading2;
proc odstext;
	p "{\pard\s2\b\ul Heading2 A \par}" / style = {&style14};
run;

* Example heading3;
proc odstext;
	p "{\pard\s3\b\ul Heading3 A \par}" / style = {&style12};
run;

* Example heading1;
proc odstext;
	p "{\pard\s1\b\ul Heading1 \par}" / style = {&style16};
run;

ods rtf close;
***** end RTF file;

