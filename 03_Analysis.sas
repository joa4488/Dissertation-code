/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
*****************************************************************************/

libname out 'P:\JoachimD\TC\Results\Results models' ; 
%let out=P:\JoachimD\TC\Results\Results models; 
%inc "P:\JoachimD\TC\Programs\models.sas";
%let err_=%str(E)RROR: ;

%macro runmodels (imp_=, dmeandiff_=, meandiff_=);
proc import file=
"P:\JoachimD\TC_\sample size\mmrm_&imp_.&dmeandiff_..csv" 
out=&imp_.&meandiff_ replace;
run;
%models (inds_=&imp_.&meandiff_, where_= );
proc datasets nolist; 
   delete m:;
quit;

%mend runmodels;

proc printto log="&out\Mean diff 50pct.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mcar_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mar_, dmeandiff_=0.5, meandiff_=50);
%runmodels (imp_=mnar_, dmeandiff_=0.5, meandiff_=50);

proc printto log="&out\Mean diff 40pct.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mcar_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mar_, dmeandiff_=0.4, meandiff_=40);
%runmodels (imp_=mnar_, dmeandiff_=0.4, meandiff_=40);

proc printto log="&out\Mean diff 30pct.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mcar_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mar_, dmeandiff_=0.3, meandiff_=30);
%runmodels (imp_=mnar_, dmeandiff_=0.3, meandiff_=30);

proc printto log="&out\Mean diff 15pct.log";
run;
%runmodels (imp_=full_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mcar_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mar_, dmeandiff_=0.15, meandiff_=15);
%runmodels (imp_=mnar_, dmeandiff_=0.15, meandiff_=15);

proc printto log="&out\Mean diff 0pct.log";
run;
%runmodels (imp_=full_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mcar_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mar_, dmeandiff_=0, meandiff_=0);
%runmodels (imp_=mnar_, dmeandiff_=0, meandiff_=0);
