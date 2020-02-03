/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*This is a small SAS program to perform nonparametric bootstraps for a regression						*/
/*It is not efficient nor general																		*/
/*Inputs: 																								*/
/*	- NumberOfLoops: the number of bootstrap iterations													*/
/*	- Dataset: A SAS dataset containing the response and covariate										*/
/*	- XVariable: The covariate for our regression model (gen. continuous numeric)						*/
/*	- YVariable: The response variable for our regression model (gen. continuous numeric)				*/
/*Outputs:																								*/
/*	- ResultHolder: A SAS dataset with NumberOfLoops rows and two columns, RandomIntercept & RandomSlope*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro regBoot(NumberOfLoops, DataSet, XVariable, YVariable);

/*Number of rows in my dataset*/
 	data _null_;
  	set &DataSet NOBS=size;
  	call symput("NROW",size);
 	stop;
 	run;

/*loop over the number of randomisations required*/
%do i=1 %to &NumberOfLoops;

	/*Sample my data with replacement*/
 	/*suggestion is to generate NRow * NumberOfLoops in a single dataset*/
	/*and then perform regression model split BY NRowsn*/

	proc surveyselect data=&DataSet out=bootData seed=-180029290 method=urs noprint sampsize=&NROW;
	run;

	/*Conduct a regression on this randomised dataset and get parameter estimates*/
	proc reg data=bootData outest=ParameterEstimates  noprint;
	Model &YVariable=&XVariable;
	run;
	quit;

	/*Extract just the columns for slope and intercept for storage*/
	data Temp;
	set ParameterEstimates;
	keep Intercept &XVariable;
	run;

	/*Create a new results dataset if the first iteration, append for following iterations*/
	data ResultHolder;
		%if &i=1 %then %do;
			set Temp;
		%end;
		%else %do;
			set ResultHolder Temp;
		%end;
	run;
%end;
/*Rename the results something nice*/
data ResultHolder;
set ResultHolder;
rename Intercept=RandomIntercept &XVariable=RandomSlope;
run;
%mend;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Updated Macro - Attempt 1 which now utilises proc surveyselect to create one huge data set */
/* adding the 'rep = &NumberOfLoops' allows proc reg data to significantly speed up bootstrapping */
/* Full comments added to final macro below - name 'regBootFour' */
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro regBootTwo(NumberOfLoops, DataSet, XVariable, YVariable);

	/* I would like to use this in some way however appears to slow the macro down when low number of 	*/
	/* NumberOfLoops are passed ( ~ < 100000 ) 															*/

	/* sasfile &Dataset load; */

	proc surveyselect data = &DataSet out = bootData2
		seed = -180029290
		method = urs
		noprint 
		samprate = 100
		rep = &NumberOfLoops;
	run;

	/* sasfile &Dataset close; */

	proc reg data = bootdata2 outest = parameterestimates  noprint;
		by Replicate;
		model &yvariable = &xvariable;
	run;

	data ResultHolder2;
		set ParameterEstimates;
		keep Intercept &XVariable;
		rename Intercept = RandomIntercept &XVariable = RandomSlope;
	run;

	proc means data = ResultHolder2 mean lclm uclm alpha = 0.05;
			var RandomIntercept RandomSlope;
	run;

%mend;


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Updated Macro - Attempt 2 which now utilises SASFile to load given data set into R first */
/* Full comments added to final macro below - name 'regBootFour' */
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
%macro regBootThree(NumberOfLoops, DataSet, XVariable, YVariable);

	/* I would like to use this in some way however appears to slow the macro down when low number of 	*/
	/* NumberOfLoops are passed ( ~ < 100000 ) 															*/

	sasfile &Dataset load; 

	proc surveyselect data = &DataSet out = bootData3
		seed = -180029290
		method = urs
		noprint 
		samprate = 100
		rep = &NumberOfLoops;
	run;

	sasfile &Dataset close;

	proc reg data = bootdata3 outest = parameterestimates  noprint;
		by Replicate;
		model &yvariable = &xvariable;
	run;

	data ResultHolder3;
		set ParameterEstimates;
		keep Intercept &XVariable;
		rename Intercept = RandomIntercept &XVariable = RandomSlope;
	run;

	proc means data = ResultHolder3 mean lclm uclm alpha = 0.05;
			var RandomIntercept RandomSlope;
	run;

%mend;




/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* FINAL MACRO for Group Assignment 2 for MT5763 */
/*Our Team SAS macro - name and comments to be added by the group once Sam has stepped through			*/
/*what the macro actual does																			*/
/*Inputs: 																								*/
/*	- NumberOfLoops: the number of bootstrap iterations													*/
/*	- Dataset: A SAS dataset containing the response and covariate										*/
/*	- XVariable: The covariate for our regression model (gen. continuous numeric)						*/
/*	- YVariable: The response variable for our regression model (gen. continuous numeric)				*/
/*Outputs:																								*/
/*	- RTF file: */
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
%macro regBootFour(NumberOfLoops, DataSet, XVariable, YVariable);

    /* SASFILE statement allows us load the data set into buffers in RAM which hold the file, then reads the data into memory */
    sasfile &Dataset load;

    /* PROC SURVEYSELECT allows us to generate random samples of many kinds from an input dataset*/
    /* Invoke PROC SURVEYSELECT and assign it the input and output data set names*/
    /* Specify a random seed */
    /* Use METHOD= option to specify the type of random sampling: Unrestricted Random Sampling(URS)*/
    /* Employ the SAMPRATE= option to get a sample of the same size as original data set without having to figure out the size of dataset first*/
    /* Set the number of bootstrap samples that we wish generate*/
    proc surveyselect data = &DataSet out = bootData4
		seed = -180029290
		method = urs
		noprint 
		samprate = 100
		rep = &NumberOfLoops;
	run;

    sasfile &Dataset close;
    /* SASFILE CLOSE frees up the RAM buffers when we're done with the file*/
    /* Perform a regression on this randomised dataset and generate parameter estimates*/
    /* Use the variable REPLICATE as the by-variable in our procedure*/
    /* Take the already-written code and insert BY statement to get the necessary bootstrap realizations */
	proc reg data = bootdata4 outest = parameterestimates  noprint;
		by Replicate;
		model &yvariable = &xvariable;
	run;
    /* Assign new results dataset and rename the resultant variables*/
	data ResultHolder4;
		set ParameterEstimates;
		keep Intercept &XVariable;
		rename Intercept = RandomIntercept &XVariable = RandomSlope;
	run;
    /* To simplify the contents of our list file or output window, we bracket the procedure with ODS LISTING statements*/
    /* ODS LISTING CLOSE turns off the ODS destination that has our list output*/
    /* While ODS LISTING turns it back on*/
    /* This approach replaces the old NOPRINT option*/
    /* Set the final output results to an RTF file by using the Output Delivery System*/
    /* Invoke Proc Means procedure to calculate 95% confidence intervals for the mean, and the mean estimate of each parameter*/
    ods listing close;
	ods rtf file = 'C:\Users\Lenovo\Desktop\SAS_project\teamOutput.rtf';
		proc means data = ResultHolder4 mean lclm uclm alpha = 0.025;
			var RandomIntercept RandomSlope;
		run;
	ods rtf close;
	ods listing;

%mend;



/* Let's load data into SAS*/
/* IMPORTANT - new users will need to update file location path below to align with their current machine*/
data work.Test;
	infile 'C:\Users\Samwise\Documents\MT5763\Assignment #02\fitness.csv' dlm = ',' firstobs = 2;
   	input Age Weight Oxygen RunTime RestPulse RunPulse MaxPulse;
run;

options nonotes;

/* Run the macro(s) */
/* and extract times for comparison*/

/* Start timer */
%let _timer_start = %sysfunc(datetime());

/* Original Code Provided */
/*%regBoot(NumberOfLoops=1000, DataSet=work.test, XVariable=Age, YVariable=Weight); */

/* Updated code, without SASFile loading data into RAM */
/*%regboottwo(numberofloops = 1000, dataset = work.test, xvariable = age, yvariable = weight);*/

/* Updated code, including SASFile loading data into RAM */
/*%regbootthree(numberofloops = 1000, dataset = work.test, xvariable = age, yvariable = weight); */

/* Updated code, including SASFile loading data into RAM and outputing to RTF file type */
%regbootfour(numberofloops = 1000, dataset = work.test, xvariable = age, yvariable = weight); 


/* Stop timer */
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;
