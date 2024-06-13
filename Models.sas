/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
*****************************************************************************/

%macro models (inds_=, where_=, _ltr=,);

proc sort data=&inds_ tagsort;
   by corr n meandiff sd sim trt y0; 
run;

proc transpose data=&inds_ out=m1;
   by corr n meandiff sd sim trt y0;
   var y1 y2 y3 y4 y5; 
   &where_;
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
   if not (first.avisitn and last.avisitn) then put "&err womething wrong in the data " 
      corr= n=  meandiff= sd= sim= usubjid= avisitn=;
run;
 
ods listing close;
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn / solution;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=usubjid type=un;*ar(1);
   random intercept / subject=usubjid;
   ods output lsmeans=mmrm_UN_lsmeans_&inds_ solutionf=mmrm_UN_solf_&inds_ tests3=mmrm_UN_tests3_&inds_ ConvergenceStatus=mmrm_UN_cs_&inds_
      diffs=mmrm_UN_lsdiffs_&inds_ (where=(avisitn in (5 ._)));
run;
ods listing;

data out.mmrm_UN_lsmeans_&inds_; set mmrm_UN_lsmeans_&inds_; run;
data out.mmrm_UN_solf_&inds_; set mmrm_UN_solf_&inds_; run;
data out.mmrm_UN_tests3_&inds_; set mmrm_UN_tests3_&inds_; run;
data out.mmrm_UN_cs_&inds_; set mmrm_UN_cs_&inds_; run;
data out.mmrm_UN_lsdiffs_&inds_; set mmrm_UN_lsdiffs_&inds_; run;
proc datasets nolist; 
   delete mmrm_:;
quit;

ods listing close;
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn / solution;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=usubjid type=ar(1);
   random intercept / subject=usubjid;
   ods output lsmeans=mmrm_AR_lsmeans_&inds_ solutionf=mmrm_AR_solf_&inds_ tests3=mmrm_AR_tests3_&inds_ ConvergenceStatus=mmrm_AR_cs_&inds_
      diffs=mmrm_AR_lsdiffs_&inds_ (where=(avisitn in (5 ._)));
run;
ods listing;

data out.mmrm_AR_lsmeans_&inds_; set mmrm_AR_lsmeans_&inds_; run;
data out.mmrm_AR_solf_&inds_; set mmrm_AR_solf_&inds_; run;
data out.mmrm_AR_tests3_&inds_; set mmrm_AR_tests3_&inds_; run;
data out.mmrm_AR_cs_&inds_; set mmrm_AR_cs_&inds_; run;
data out.mmrm_AR_lsdiffs_&inds_; set mmrm_AR_lsdiffs_&inds_; run;
proc datasets nolist; 
   delete mmrm_:;
quit;

ods listing close;
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   class usubjid trt;
   model chg = base avisitn trt trt*avisitn / solution;
   random intercept avisitn / subject=usubjid group=trt;
   lsmeans trt/ at avisitn=5 diff; 
   ods output lsmeans=slope_lsmeans_&inds_ solutionf=slope_solf_&inds_ tests3=slope_tests3_&inds_ ConvergenceStatus=slope_cs_&inds_
      diffs=slope_lsdiffs_&inds_ (where=(avisitn in (5 ._)));
run;
ods listing;

data out.slope_lsmeans_&inds_; set slope_lsmeans_&inds_; run;
data out.slope_solf_&inds_; set slope_solf_&inds_; run;
data out.slope_tests3_&inds_; set slope_tests3_&inds_; run;
data out.slope_cs_&inds_; set slope_cs_&inds_; run;
data out.slope_lsdiffs_&inds_; set slope_lsdiffs_&inds_; run;
proc datasets nolist; 
   delete slope:;
quit;

proc datasets nolist; 
   delete m1 m2;
quit;

%mend models;
