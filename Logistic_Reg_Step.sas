/* start macro for standard logistic regression summary */
%macro Logistic_Reg_Step(data_in=,outcome=,cntsvar=,catgvar=,refcats=,est_out=);

/* main adjusted results */
proc logistic data = &data_in;
	class &refcats / param=reference;
	model &outcome(event='1') = &catgvar &cntsvar / rsq lackfit;
	ods output ParameterEstimates = &est_out;
	format &outcome ;
run;

/* use proc genmod just to capture LRT for the above logistic regression */
proc genmod data = &data_in DESCENDING;
	class &refcats / param=reference;
	model &outcome = &catgvar &cntsvar / dist=bin type3;
	ods output Type3 = LRT_stats(drop=Method rename=(Source=Variable DF=LRT_DF ChiSq=LRT_S ProbChiSq=LRT_P));
run;

data &est_out(drop=aOR_lower aOR_upper StdErr); /* create a single field containing confidence intervals as well */
	length ClassVal0 $50;
	length cOR_95pct $12;
	set &est_out;
	if Variable = "Intercept" then delete; /* remove intercept row */
	aOR_Estimate = exp(estimate);
	order = _n_;
	cOR_Estimate = .; /* need this to act as storage */
	Coeffcrude = .; /* need this to act as storage */
	cOR_95pct = "";
	if ClassVal0 = " " then ClassVal0 = "NA"; /* to manage the cnts/ordinal variables that have no associated class values */
	aOR_lower = exp(estimate-1.96*StdErr);
	aOR_upper = exp(estimate+1.96*StdErr);
	aOR_95pct = put(aOR_lower,4.2)||"-"||put(aOR_upper,4.2);
run;

/* crude results requires a loop over all variables */
%local count catg_n all_vars var_one ref_one i drop_one ref_less;
%let all_vars = &catgvar &cntsvar;
%let count = %sysfunc(countw(&all_vars));

/* used to control switch from categorical to cnts/ordinal variables */
%let catg_n = %sysfunc(countw(&catgvar));

%do i=1 %to &count; 					 	/* initiate macro loop over variable list */
%let var_one = %scan(&all_vars,&i,%str( )); /* create a macro variable containing a single variable name at a time */
%let ref_one = %scan(&refcats,&i,%str( ));  /* create a macro variable containing a single variable name at a time */

/* for crude really only want the OR and 95% CI - note that leaving variables in the class list
which are not present in the model statement does not cause the procedure to return an error and fail */
%if &catg_n < &i %then %do;
	proc logistic data = &data_in;	
		model &outcome(event='1') = &var_one;
		ods output ParameterEstimates = crude_&i;
	run;

	data crude_&i(keep=Variable ClassVal0 crude Coeffcrude0 cOR_95pct0);
		length ClassVal0 $50;
		set crude_&i;
		if Variable = "Intercept" then delete; /* remove intercept row */
		crude = exp(estimate);
		Coeffcrude0 = estimate;
		ClassVal0 = "NA"; /* to manage the cnts/ordinal variables that have no associated class values */
		cOR_lower = exp(estimate-1.96*StdErr);
		cOR_upper = exp(estimate+1.96*StdErr);
		cOR_95pct0 = put(cOR_lower,4.2)||"-"||put(cOR_upper,4.2);
	run;
%end;
%if &catg_n >= &i %then %do;
	proc logistic data = &data_in;	
		class &ref_one / param=reference;
		model &outcome(event='1') = &var_one;
		ods output ParameterEstimates = crude_&i;
	run;

	data crude_&i(keep=Variable ClassVal0 crude Coeffcrude0 cOR_95pct0);
		length ClassVal0 $50;
		set crude_&i;
		if Variable = "Intercept" then delete; /* remove intercept row */
		crude = exp(estimate);
		Coeffcrude0 = estimate;
		cOR_lower = exp(estimate-1.96*StdErr);
		cOR_upper = exp(estimate+1.96*StdErr);
		cOR_95pct0 = put(cOR_lower,4.2)||"-"||put(cOR_upper,4.2);
	run;
%end;

/* snap on crude OR statistics - issue: will not add to crude OR column once it is part of &est_out 
When using PROC SQL to combine data sets, all variable attributes (LENGTH, FORMAT, INFORMAT, LABEL) 
and data values for common non-WHERE variables are determined and kept from the first/left data set. */
proc sql;
	create table &est_out as select *,
		case (&est_out..Variable = crude_&i..Variable)
		when 1 then 1 else 0
		end as matchvar
	from &est_out left join crude_&i on &est_out..Variable = crude_&i..Variable and &est_out..ClassVal0 = crude_&i..ClassVal0
  	order by &est_out..order;
quit;

/* easiest to do reconciliation in a data step post-SQL */
data &est_out(drop=crude matchvar cOR_95pct0 Coeffcrude0);
	set &est_out;
	if matchvar = 1 then do;
		cOR_Estimate = crude;
		cOR_95pct = cOR_95pct0;
		Coeffcrude = Coeffcrude0;
	end;
run;

/* finally also need to do a drop one by one to look at confounding */
data _null_;
	call symput('drop_one',tranwrd("&all_vars","&var_one",""));
	if &catg_n >= &i then do;
		call symput('ref_less',tranwrd("&refcats","&ref_one",""));
	end;
	if &catg_n < &i then do;
		call symput('ref_less',"&refcats"); /* when not dropping a cat var */
	end;
run;

/* drop-one adjusted results */
proc logistic data = &data_in;
	class &ref_less / param=reference;
	model &outcome(event='1') = &drop_one / rsq lackfit;
	ods output ParameterEstimates = dp_one_&i(keep=Variable CLassVal0 Estimate);
run;

/* can probably merge on at end in one massive datastep */
data dp_one_&i (drop=Estimate);
	length ClassVal0 $50;
	set dp_one_&i;
	if Variable = "Intercept" then delete; /* remove intercept row */
	coeff_drop_&i = Estimate;
	if ClassVal0 = " " then ClassVal0 = "NA"; /* to manage the cnts/ordinal variables that have no associated class values */
run;

/* sort for merging later */
proc sort data=dp_one_&i; by Variable ClassVal0; run;

%end;

/* create som other print outs of useful info - adjustment away from the null and then also 10% for confounding 
(imagine a DAG for this) */
data &est_out;
	set &est_out;
	Towards_Null = "Yes";
	if abs(estimate) > abs(log(cOR_Estimate)) then Towards_Null = "No";
run;

proc sort data=&est_out;      by Variable; run;
proc sort data=LRT_stats;     by Variable; run;

data &est_out;
	merge &est_out(in=a) LRT_stats (in=b); 
	by Variable; 
	if a;
run;

proc sort data=&est_out; by Variable ClassVal0; run;

/* sort out duplicate P-values as they are variable level before merge */
data &est_out;
	set &est_out;
	by Variable;
	if not first.Variable then do;
		LRT_P = .;
		LRT_DF = .;
	end;
run;

data &est_out;
	merge &est_out(in=a) dp_one_: ; /* worked but dangerous and ordering in the dataset is off */
	by Variable ClassVal0; 
	if a;
run;


proc sort data=&est_out; by order; run;

/* re-sort to original ordering */
proc sort data=&est_out(drop=_ESTTYPE_); by order; run;

/* calculate per cent change in coefficients (could also do ORs if required) */
data &est_out (drop=a order);
	set &est_out;
	array est_drop[*] coeff_drop_1-coeff_drop_&count ;
	array pct_drop[*] pct_drop_1-pct_drop_&count ;
	array aOR_drop[*] aOR_drop_1-aOR_drop_&count ;
	do a=1 to dim(est_drop);
		pct_drop[a] = (abs(exp(est_drop[a])-aOR_Estimate)/abs(aOR_Estimate))*100; /* change in adjusted OR */
		aOR_drop[a] = exp(est_drop[a]); /* adjusted OR */
	end;

	/* do some looped labelling - macro at start of SAS file */
	label %do_label(var_n=coeff_drop_,n=&count,text=aCoeff_);
	label %do_label(var_n=pct_drop_,n=&count,text=aOR_pct_);
	label %do_label(var_n=aOR_drop_,n=&count,text=aOR_);

	/* bit of clean up for tabulating */
	label Towards_Null="|aOR|<|cOR|";
	label cOR_Estimate="cOR";
	label aOR_Estimate="aOR";
	label aOR_95pct="aOR 95% CI";
	label ClassVal0="Level";
	label LRT_P="Variable-LRT p-value";
	label LRT_DF="Variable-LRT df";
	label LRT_S="Variable-LRT statistic";
	label Variable="Variable";
	label DF="Coefficient-Wald df";
	label ProbChiSq="Coefficient-Wald p-value";
	label WaldChiSq="Coefficient-Wald statistic";
	label Coeffcrude="Crude coefficients";
	label cOR_95pct="cOR 95% CI";
	label estimate="Adjusted coefficients";
run;

/* finally add a number counter matched to Variables */

/* need to clean up work space as well, macro vars and interim datasets
proc datasets lib=work nolist;
	delete dp_one_: crude_: Beta_M Beta_M1 LRT_stats;
quit;
run;
 */

%mend Logistic_Reg_Step;

/* this macro called within the macro */
%macro do_label(var_n=,n=,text=);
	%do i = 1 %to &n; 
 		&var_n&i = "&text&i" 
	%end; 
%mend do_label;
