/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
# Note this code was used to investigate the warning on the Hessian - no output generated for the dissertation
*****************************************************************************/

libname cs 'P:\JoachimD\TC\Results\Results models' access=readonly; 

data m1;
   set cs.mmrm_un_cs_full_0(in=a) cs.mmrm_un_cs_full_15(in=a) cs.mmrm_un_cs_full_30(in=a) cs.mmrm_un_cs_full_40(in=a) cs.mmrm_un_cs_full_50(in=a)
   cs.mmrm_un_cs_mar_0(in=b) cs.mmrm_un_cs_mar_15(in=b) cs.mmrm_un_cs_mar_30(in=b) cs.mmrm_un_cs_mar_40(in=b) cs.mmrm_un_cs_mar_50(in=b)
   cs.mmrm_un_cs_mnar_0(in=c) cs.mmrm_un_cs_mnar_15(in=c) cs.mmrm_un_cs_mnar_30(in=c) cs.mmrm_un_cs_mnar_40(in=c) cs.mmrm_un_cs_mnar_50(in=c)
   cs.mmrm_un_cs_mcar_0(in=d) cs.mmrm_un_cs_mcar_15(in=d) cs.mmrm_un_cs_mcar_30(in=d) cs.mmrm_un_cs_mcar_40(in=d) cs.mmrm_un_cs_mcar_50(in=d);
   if a then dropout='FULL';
   if b then dropout='MAR';
   if c then dropout='MNAR';
   if d then dropout='MCAR';
run;

proc sort data=m1 tagsort;
   by dropout corr n meandiff sd; 
run;
proc freq data=m1;
   table reason/out=dropout;
run;
proc freq data=m1;
   table dropout*reason/out=dropout;
run;
proc freq data=m1;
   table corr*reason/out=corr;
run;
proc freq data=m1;
   table n*reason/out=n;
run;
proc freq data=m1;
   table meandiff*reason/out=meandiff;
run;
proc freq data=m1;
   table sd*reason/out=sd;
run;


* CHECK THE MODEL ;
proc import file=
"P:\JoachimD\TC\Datasets\mmrm_full_0.3.csv" 
out=f5 replace;
run;
proc sort data=f5 tagsort;
   by corr n meandiff sd sim trt y0; 
run;

proc transpose data=f5 out=m1;
   by corr n meandiff sd sim trt y0;
   var y1 y2 y3 y4 y5; 
   where corr=0.15 and n=180 and sd=4;* and sim=10; 
run;
data m2 (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
   sim2=floor(sim/2);
run;

** MMRM **;
 
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=usubjid type=un r rcorr;
   random intercept / subject=usubjid;
   ods output ConvergenceStatus=mmrm_UN_cs;
run;
proc freq data=mmrm_UN_cs;
   table reason; *2/2000=0.1%;
run;

data m2 (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
   sim2=floor(sim/2);
   subjid=usubjid+(sim/2 - floor(sim/2));
run;
proc mixed data=m2 method=reml;
   by corr n meandiff sd sim2;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs2;
run;
proc freq data=mmrm_UN_cs2;
   table reason; 
run;

data m3 (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
   sim3=floor(sim/3);
   subjid=usubjid+(round(sim/3,0.1) - floor(sim/3));
run;
proc mixed data=m3 method=reml;
   by corr n meandiff sd sim3;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs3;
run;
proc freq data=mmrm_UN_cs3;
   table reason; 
run;

data m4 (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
   sim4=floor(sim/4);
   subjid=usubjid+(round(sim/4,0.1) - floor(sim/4));
run;
proc mixed data=m4 method=reml;
   by corr n meandiff sd sim4;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs4;
run;
proc freq data=mmrm_UN_cs4;
   table reason; 
run;


%macro runmmrm (i=);
data m&i (drop=_name_);
   set m1 (rename=(y0=base col1=aval));
   by corr n meandiff sd sim trt base;
   if first.sim then usubjid=1;
   else if first.base then usubjid+1;
   chg=aval-base;
   avisitn=input(substr(_name_,2,1),best.);
   sim&i=floor(sim/&i);
   subjid=usubjid+(round(sim/&i,0.1) - floor(sim/&i));
run;
proc mixed data=m&i method=reml;
   by corr n meandiff sd sim&i;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
  * lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs&i;
run;
proc freq data=mmrm_UN_cs&i;
   table reason; 
run;
%mend runmmrm;
%runmmrm(i=5);
%runmmrm(i=6);
%runmmrm(i=7);
%runmmrm(i=8);


proc mixed data=m4 method=reml;
   by corr n meandiff sd sim;
   where sim=100;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs diffs=diffs;
run;
proc mixed data=m4 method=reml;
   by corr n meandiff sd sim;
   where sim=100;
   class usubjid avisitn trt;
   model chg = base avisitn trt trt*avisitn/s;
   lsmeans trt*avisitn trt / diff;
   repeated avisitn / subject=subjid type=un r rcorr;
*   random intercept / subject=subjid;
   ods output ConvergenceStatus=mmrm_UN_cs diffs=diffs_;
run;
