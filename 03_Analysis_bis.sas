/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
*****************************************************************************/

libname out 'P:\JoachimD\TC\Results\Results models' ; 
%let out=P:\JoachimD\TC\Results\Results models; 
%inc "P:\JoachimD\TC\Programs\models_bis.sas";
%let err_=%str(E)RROR: ;

%macro runmodels (imp_=, dmeandiff_=, meandiff_=);
proc import file=
"P:\JoachimD\TC\Datasets\mmrm_&imp_.&dmeandiff_..csv" 
out=&imp_.&meandiff_ replace;
run;
%models_bis (inds_=&imp_.&meandiff_, where_= );
proc datasets nolist; 
   delete m:;
quit;

%mend runmodels;

proc printto log="&out\Mean diff 50pct_bis.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mcar_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mar_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mnar_, dmeandiff_=0.5, meandiff_=50);

proc printto log="&out\Mean diff 40pct_bis.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mcar_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mar_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mnar_, dmeandiff_=0.4, meandiff_=40);

proc printto log="&out\Mean diff 30pct_bis.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mcar_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mar_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mnar_, dmeandiff_=0.3, meandiff_=30);

proc printto log="&out\Mean diff 15pct_bis.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mcar_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mar_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mnar_, dmeandiff_=0.15, meandiff_=15);

proc printto log="&out\Mean diff 0pct_bis.log";
run;
%runmodels (imp_=full_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mcar_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mar_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mnar_, dmeandiff_=0, meandiff_=0);




/** Derivation of the Operating Characteristics of the chosen sample size **;
** Read in the OC data **;
proc import file=
"T:\BE-80-1900215_56021927PCR3011\STAT\Analyses\Programs\TC\Datasets\mmrm_OC.csv" 
out=mmrm1_oc (where=(n ne 0)) replace;
run;
data mmrm2_2oc;
   set mmrm1_oc (rename=(y0=base y=aval time=avisitn));
   chg=aval-base;
   avisitnn=avisitn;
run;

%mmrm (_part=_2oc);

proc sort data= out.power_1full out=p1 nodupkey; 
	by endpointn endpoint corr incr meandiff sd n positive count power; 
   where positive=1;
run;
proc transpose data=p1 out=p2;
	by endpointn endpoint corr incr meandiff sd; 
   var power;
   id n;
run;


** Tabulate power in the different scenarios **;




** Derivation of Change in Randomization Ratio for the chosen sample size **;
** Read in the RR data **;
proc import file=
"T:\BE-80-1900215_56021927PCR3011\STAT\Analyses\Programs\TC\Datasets\mmrm_RR.csv" 
out=mmrm1_rr (where=(n ne 0)) replace;
run;
data mmrm2_3rr;
   set mmrm1_rr (rename=(y0=base y=aval time=avisitn));
   chg=aval-base;
   avisitnn=avisitn;
run;

%mmrm (_part=_3rr);

** What is the impact of adding an interim analysis ? **;
*** On the full data ***;
*** On the MCAR data ***;
*** On the MNAR data ***;

*/
