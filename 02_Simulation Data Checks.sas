/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
*****************************************************************************/

libname out 'P:\JoachimD\TC\Datasets' ; 
%let out=P:\JoachimD\TC\Datasets;

%macro simcheck (meandiff_=); 
%if &meandiff_=0 %then %do;
proc import file=
"P:\JoachimD\TC\Datasets\mmrm_full_0.csv" 
out=full replace;
run;
%end;
%else %do;
proc import file=
"P:\JoachimD\TC\Datasets\mmrm_full_0.&meandiff_..csv" 
out=full replace;
run;
%end;
proc sort data=full;
   by corr n meandiff sd trt sim;
run;
proc means data=full noprint;
   by corr n meandiff sd trt sim;
   var y0 y1 y2 y3 y4 y5;
   output out=summary1 n=n0 n1 n2 n3 n4 n5  
      mean=mean0 mean1 mean2 mean3 mean4 mean5
      std= std0 std1 std2 std3 std4 std5;
run;
proc means data=summary1 noprint;
   by corr n meandiff sd trt;
   var mean0 mean1 mean2 mean3 mean4 mean5 std0 std1 std2 std3 std4 std5;
   output out=Summary2 n=n0 n1 n2 n3 n4 n5  
      mean=mmean0 mmean1 mmean2 mmean3 mmean4 mmean5 mstd0 mstd1 mstd2 mstd3 mstd4 mstd5;
run;
data summary3 (drop=_type_ _freq_ n0 n1 n2 n3 n4 n5); 
   set summary2;
   label mmean0='Mean mean baseline' mmean1='Mean mean T1' mmean2='Mean mean T2' mmean3='Mean mean T3'
    mmean4='Mean mean T4' mmean5='Mean mean T5'
         mstd0='Mean STD baseline' mstd1='Mean STD T1' mstd2='Mean STD T2' mstd3='Mean STD T3'
    mstd4='Mean STD T4' mstd5='Mean STD T5'    ;
   format mmean0 mmean1 mmean2 mmean3 mmean4 mmean5 mstd0 mstd1 mstd2 mstd3 mstd4 mstd5 6.1;
   if abs(sd-round(mstd0)) gt 0.05 or abs(sd-round(mstd1)) gt 0.05 or abs(sd-round(mstd2)) gt 0.05 or abs(sd-round(mstd3)) gt 0.05 
      or abs(sd-round(mstd4)) gt 0.05 or abs(sd-round(mstd5)) gt 0.05
      then put 'ERROR: check the assumptions ' corr= n= meandiff= sd= trt=; 
run;
/*
proc export data=summary3 
   file="P:\JoachimD\TC\Datasets\Mean and SD checks.xls" replace;
run;
*/

proc sort data=summary1 ;
   by n meandiff trt;
proc means data=summary1 noprint;
   by n meandiff trt;
   var mean0 mean1 mean2 mean3 mean4 mean5 std0 std1 std2 std3 std4 std5;
   output out=Summary4 
      mean=mmean0 mmean1 mmean2 mmean3 mmean4 mmean5 mstd0 mstd1 mstd2 mstd3 mstd4 mstd5;
run;
proc transpose data=summary4 out=summary5;
   by n meandiff ;
   var mmean0 mmean1 mmean2 mmean3 mmean4 mmean5;
   id trt;
run;
data summary6; 
   set summary5;
   retain base0 base1;
   by n meandiff ;
   avisitn=input(tranwrd(_name_,'mmean',''),best.);
   if first.meandiff then do;
      base0=_0; base1=_1;
   end;
   else do;
      chg0=_0-base0;
      chg1=_1-base1;
      pchg=(chg0-chg1)/chg0;
   end;
run;
/*
proc export data=summary6 
   file="P:\JoachimD\TC\Datasets\Increase checks.xls" replace;
run;
*/
proc transpose data=summary6(drop=_name_) out=summary7;
   by n meandiff avisitn;
   var _0 _1;
run;

ods pdf file="P:\JoachimD\TC\Results\Simulated data checks\Mean Plots 0.&meandiff_ .pdf";
proc sgplot data=summary7;
   by n;
   title "Mean ADAS scores by Treatment Group (Mean difference = 0.&meandiff_ )";
   series x=avisitn y=col1 / group=_name_ name='grouping';
   keylegend 'grouping' / type=linecolor;
   refline 22 24 26 28 30 / axis=y lineattrs=(thickness=0.3 color=black pattern=dash);
run;
ods pdf close;
%mend simcheck;
%simcheck (meandiff_=0);
%simcheck (meandiff_=15);
%simcheck (meandiff_=3);
%simcheck (meandiff_=4);
%simcheck (meandiff_=5);

/** Spaghetti plot for dissertation **;
data FULL;
   set FULL;
   row=_N_;
run;
proc sort data=FULL;
   by corr n meandiff sd sim trt row;
proc transpose data=FULL out=sp1;
   by corr n meandiff sd sim trt row;
   var y0 y1 y2 y3 y4 y5; 
   where corr=0.15 and n=50 and sd=9 and sim=1; 
run;
data sp2 (drop=_name_);
   set sp1 (rename=(col1=aval));
   by corr n meandiff sd sim trt row;
   if first.sim then usubjid=1;
   else if first.row then usubjid+1;
   avisitn=input(substr(_name_,2,1),best.);
run;
proc sgplot data=sp2;
   title 'Spaghetti plot for a few simulations subjects by Treatment Group (Mean difference = 15%)';
   series x=avisitn y=aval / group=usubjid grouplc=trt name='grouping';
   keylegend 'grouping' / type=linecolor;
   where usubjid in (10 12 13 14 15 36 37 38 39 40);
run;
*/
