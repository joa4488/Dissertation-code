/***************************************************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
# Note: this code generates the example for interpretation in section 2.5 - Not essential for the dissertation
****************************************************************************************************************/

proc import file=
"P:\JoachimD\TC\Datasets\mmrm_full_0.3.csv" 
out=full_30 (where=(sim=1)) replace;
run;

proc sort data=full_30 tagsort;
   by corr n meandiff sd sim trt y0; 
run;
quit;
proc transpose data=full_30 out=m1;
   by corr n meandiff sd sim trt y0;
   var y1 y2 y3 y4 y5; 
   
*   where corr=0.15 and n=50 and sd=4 and sim=1; 
run;
data m2 (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
run;

** MMRM **;
proc sort data=m2 tagsort;
   by corr n meandiff sd sim usubjid avisitn;
run;
data _null_; 
   set m2;
   by corr n  meandiff sd sim usubjid avisitn;
   if not (first.avisitn and last.avisitn) then put "&err something wrong in the data " 
      corr= n=  meandiff= sd= sim= usubjid= avisitn=;
run;
 
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   where n=110 and sd=6 and corr=0.3;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn / solution;
   lsmeans trt*avisitn trt / diff cl;
   repeated avisitn / subject=usubjid type=un r rcorr;*ar(1);
run;

proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   where n=110 and sd=6 and corr=0.3;
   class usubjid trt;
   model chg = base avisitn trt trt*avisitn / solution;
   random intercept avisitn / subject=usubjid group=trt g type=un gcorr v vcorr;
   lsmeans trt/ at avisitn=5 diff; 
run;
