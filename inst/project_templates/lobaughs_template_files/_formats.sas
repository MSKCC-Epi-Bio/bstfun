
*****************************************************************************************************************

DESCRIPTION: Create project specific SAS formats catalog

---------------------------------------------------------------------------------------------------------------

NAME: Stephanie Lobaugh
DATE:
{{Sys.Date()}}: Created project folder

****************************************************************************************************************;

* formats library;
%Let path_data = {{ifelse(is.null(path_data), "", path_data)}};
libname fmt "&path_data";

* escape character;
ods escapechar = "^";

* Use if using a data dictionary to define the "$varname." format used for variable labeling in tables;
proc sql;
	select "'" || strip(lowcase(variable)) || "'='" || strip(label) || "'"
		into: varname
		separated by " "
	from data.dictionary;
quit;

proc format library = fmt.formats;

	* example variable labels format;
	value $varlabels &varname
					 'sample_size' = 'Sample size'
					 'var' = 'Nice variable label'
	;

	* example yes/no format for a numeric 0/1 var;
	value yn 0 = "No     "
			 1 = "Yes"
             . = "Missing"
	;

	* example format with escapechar for numeric 0/1 var;
	value gte20f 0 = "<20                 "
	             1 = "^{unicode '2265'x}20" /* the escape char + unicode code will create a greater than or equal to symbol */
				 . = "Missing"
	;

	* example rounding format for presenting p-values to apply to a numeric p-value var;
	value pval
		low-<0.001 = "<0.001"
		0.001-<0.01 = [5.3]
	    0.01-<0.04 = [5.2]
		0.04-0.05 = [5.3]
		0.05<-<0.99 = [5.2]
		0.99-high = ">0.99"
		. = " "
	;
run;


