/* 
This code does confounding assessments for a multivariable logistic regression model
when doing model building by backwards elimination
The dataset used for the demonstration can be downloaded from 
http://www.ats.ucla.edu/stat/sas/seminars/sas_logistic/logistic1.htm
for the purpose of demonstrating the SAS macro
Author: Jason Bentley
Date:   June 2014
*/

* library with dataset and macro file, where path must be specified by the user;
libname test "path";

* load the dataset, create a binary outcome and have a look;
data hsb2;
	set test.hsb2;
	hiwrite = write >=52;
run;
proc contents data=hsb2; run;
proc tabulate data=hsb2 noseps missing;
	class ses female schtyp prog race hiwrite;
	table ses female schtyp prog race, hiwrite*(n="n"*f=comma9. colpctn="Column %"*f=8.2);
run;

* fit the main adjusted model;
proc logistic data=hsb2;
	class ses(ref="2") prog(ref="3") race(ref="4") / param=ref;
	model hiwrite(event="1") = ses female schtyp prog race;
run;

* use include to create macro function from sas file, where path must be specified by the user;
%include "path\Logistic_Reg_Step.sas";

/* use the macro to compare crude and adjusted and do confounding assesment for first step of
backwards elimination model building */
%let cs_vars = female schtyp;
%let gp_vars = ses prog race;
%let ref_cat = ses(ref='2') prog(ref='3') race(ref='4');

%Logistic_Reg_Step(data_in=hsb2,outcome=hiwrite,cntsvar=&cs_vars,catgvar=&gp_vars,refcats=&ref_cat,est_out=Model_1);

*save output, where path must be specified by the user;
ods html file = 'path\Summary.html';

* resulting summary table has a lot of information;
ods select Variables;
title "Listing of information in the summary table for a single backwards step";
proc contents data=Model_1; run;
run;
ods select default;

* Table with % confounding assessment;
proc format;
	value backcolor 
	. = 'light grey'
	0 -< 5 = 'white'
	5 -< 10 = 'light orange'
	10 - high = 'light red';
	value forecolor 
	. = 'light grey'
	other = 'black';
run;

title "Table 1: Comparison of crude and adjusted ORs and per cent confounding assessment";
proc print data=Model_1 noobs label;
	var Variable ClassVal0 cOR_Estimate cOR_95pct aOR_Estimate aOR_95pct Towards_Null;
	var pct_drop_: / style(data) = [background=backcolor. foreground=forecolor. font_weight=bold];
	format pct_drop_: f8.1 cOR_Estimate aOR_Estimate f8.2; 
run;

* Table with adjusted ORs under first backwards elimination step;
title "Table 2: Adjusted ORs for main model and one backwards step and LR significance tests";
proc print data=Model_1 noobs label;
	var Variable ClassVal0 aOR_Estimate aOR_95pct Towards_Null LRT_DF LRT_P;
	var aOR_drop_: ;
	format cOR_Estimate aOR_Estimate aOR_drop_: f8.2; 
run;

* Table with some info on the regression coefficients;
title "Table 3: Comparison of regression coefficients";
proc print data=Model_1 noobs label;
	var Variable ClassVal0 Coeffcrude estimate WaldChiSq ProbChiSq coeff_drop_:;
	format Coeffcrude estimate coeff_drop_: f8.2; 
run;

ods html close;
