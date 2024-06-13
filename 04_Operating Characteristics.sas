/*****************************************************************************
# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024
*****************************************************************************/

%let in=P:\JoachimD\TC\Results\Results models; 
%let out=P:\JoachimD\TC\Results\Power;
libname in "&in" access=readonly; 
libname out "&out"; 

** Join potential endpoints **;

%macro conc (model=,misdata=,bis_=);
data power_&model._&misdata.&bis_;
   length endpoint misdata model $200;
	set in.&model._lsdiffs_&misdata._0&bis_ in.&model._lsdiffs_&misdata._15&bis_ in.&model._lsdiffs_&misdata._30&bis_
       in.&model._lsdiffs_&misdata._40&bis_ in.&model._lsdiffs_&misdata._50&bis_ 
       in.&model._tests3_&misdata._0&bis_ (in=c where=(effect='avisitn*trt'))
       in.&model._tests3_&misdata._15&bis_ (in=c where=(effect='avisitn*trt'))
       in.&model._tests3_&misdata._30&bis_ (in=c where=(effect='avisitn*trt'))
       in.&model._tests3_&misdata._40&bis_ (in=c where=(effect='avisitn*trt'))
       in.&model._tests3_&misdata._50&bis_ (in=c where=(effect='avisitn*trt'))
;
	if avisitn=5 then do;
      endpoint='M18 Treatment Effect';
      endpointn=2;
      * I will need to do this in an estimate statement if this would be based on time continuously - depending in how we include the interaction term; 
      if . lt probt lt 0.05 and estimate gt 0 then positive=1;
      else positive=0;
      if . lt probt lt 0.1 and estimate gt 0 then positive_1=1;
      else positive_1=0;
      if . lt probt lt 0.2 and estimate gt 0 then positive_2=1;
      else positive_2=0;
   end;
	else if effect='trt' then do;
      endpoint='Overall Treatment Effect';
      endpointn=1;
      if . lt probt lt 0.05 and estimate gt 0 then positive=1;
      else positive=0;
   end;
	else if c then do;
      endpoint='Interaction term';
      endpointn=3;
      if . lt probf lt 0.05 then positive=1;
      else if probf gt 0.05 then positive=0;
      if . lt probf lt 0.1 then positive_1=1;
      else positive_1=0;
      if . lt probf lt 0.2 then positive_2=1;
      else positive_2=0;
   end;
   misdata="&misdata";
   model="&model.&bis_";
run;

data cs_&model._&misdata.&bis_;
   length misdata model $200;
	set in.&model._cs_&misdata._0&bis_ in.&model._cs_&misdata._15&bis_ in.&model._cs_&misdata._30&bis_
      in.&model._cs_&misdata._40&bis_ in.&model._cs_&misdata._50&bis_ ;
   misdata="&misdata";
   model="&model.&bis_";
run;

%mend conc;

%conc(model=mmrm_un, misdata=full);
%conc(model=mmrm_un, misdata=mcar);
%conc(model=mmrm_un, misdata=mar);
%conc(model=mmrm_un, misdata=mnar);

%conc(model=mmrm_un, misdata=full, bis_=bis);
%conc(model=mmrm_un, misdata=mcar, bis_=bis);
%conc(model=mmrm_un, misdata=mar, bis_=bis);
%conc(model=mmrm_un, misdata=mnar, bis_=bis);

%conc(model=mmrm_ar, misdata=full);
%conc(model=mmrm_ar, misdata=mcar);
%conc(model=mmrm_ar, misdata=mar);
%conc(model=mmrm_ar, misdata=mnar);

%conc(model=slope, misdata=full);
%conc(model=slope, misdata=mcar);
%conc(model=slope, misdata=mar);
%conc(model=slope, misdata=mnar);

data power0;
   set power_:;
   drop stderr df tvalue numdf dendf fvalue base;
run;
proc datasets nolist; 
   delete power_:;
quit;

** Add Covergence status **; 
data cs;
   set cs_:;
run;
proc datasets nolist; 
   delete cs_:;
quit;

proc sort data=cs ;*nodupkey; 
	by n misdata; 
run;
proc freq data= cs; 
*	by n misdata; 
   table reason/out=cs_possibilities; 
*   where model='mmrm_un';
run;

proc sort data=cs tagsort;*nodupkey; 
	by model misdata corr n meandiff sd sim; 
run;
proc freq data= cs noprint; 
	by model misdata corr n meandiff sd; 
   table reason/out=freq_cs; 
run;
proc sort data=freq_cs;
	by corr n meandiff sd misdata reason; 
run;
proc transpose data=freq_cs out=freq_cs1 (drop=_:);
	by corr n meandiff sd misdata reason; 
   id model;
   var percent; 
run;
proc export data=freq_cs1 outfile="&out\Convergence Status.xls" replace;
run;

** Merge convergence status with Power dataset **;
proc sort data= power0 tagsort; 
	by model misdata corr n meandiff sd sim; 
run;
data out.power_Per_Sim power1 (where=(endpoint='M18 Treatment Effect')); 
	merge power0 cs (drop=pdg pdh); 
	by model misdata corr n meandiff sd sim; 
   if meandiff ne 0 and endpoint='M18 Treatment Effect' then bias=100*(estimate-(meandiff*8))/(meandiff*8);
run;


** Precision: Derive the empirical CI **;
data prec1;
   set out.power_Per_Sim;
   where endpoint='M18 Treatment Effect' and model ne 'mmrm_un' and meandiff gt 0;
   corrc=put(corr,4.2);
   meandiffc=put(meandiff,4.2);
run;

%macro prec (by_=, nr_=);
proc sort data=prec1 tagsort;
	by &by_; 
proc univariate data=prec1 noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_a n=nsim1 nsim2 mean=bias_mean diff_mean std=bias_std diff_std median=bias_median diff_median 
      pctlpts=2.5 5 95 97.5 pctlpre=bias_p diff_p ;
run;
proc means data=prec1 noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_b n=nsim1 nsim2 stderr=bias_se diff_se lclm=lclm_bias lclm_diff uclm=uclm_bias uclm_diff;
run;
data emp_ci&nr_ ; 
   merge emp_ci_a emp_ci_b; 
	by &by_; 
   width = diff_p97_5 - diff_p2_5;
run;
%mend prec;
%prec (by_= model, nr_=1);
%prec (by_= misdata model, nr_=2);
%prec (by_= model n, nr_=3);
%prec (by_= model corr, nr_=4);
%prec (by_= model sd, nr_=5);
%prec (by_= model meandiff, nr_=6);

%prec (by_= misdata, nr_=7);
%prec (by_= misdata n, nr_=8);
%prec (by_= misdata corr, nr_=9);
%prec (by_= misdata sd, nr_=10);
%prec (by_= misdata meandiff, nr_=11);

data emp_ci;
   length level $40 var $200;
   set emp_ci1-emp_ci11;
   prob=put(width,6.2)||" ("||put(diff_p2_5,6.2)||"; "||put(diff_p97_5,6.2)||')';
   prob_bias=put(bias_mean,6.2)||" ("||put(lclm_bias,6.2)||"; "||put(uclm_bias,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and n ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and n ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=emp_ci;
   by varn var leveln level; 
run;
data out.emp_ci;
   length fvar $50;
   set emp_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, width, diff_p2_5, diff_median, diff_p97_5, bias_median, bias_p2_5, bias_p97_5, prob_bias, bias_mean, lclm_bias, uclm_bias, chisq);
   output;
   keep fvar varn var leveln level chisq prob: width diff_p2_5 diff_p97_5 diff_median bias_p2_5 bias_p97_5 bias_median bias_mean lclm_bias uclm_bias;
run;
proc sort data=out.emp_ci;
   by varn var leveln level; 
run;

proc export data=out.emp_ci outfile="&out\Precision.xls" replace;
run;

data ph1;
   set out.power_Per_Sim;
   where endpoint='M18 Treatment Effect' and model not in ('mmrm_ar' 'mmrm_un') and misdata='mnar' and meandiff gt 0;
   corrc=put(corr,4.2);
   meandiffc=put(meandiff,4.2);
run;

%macro prec (by_=, nr_=);
proc sort data=ph1 tagsort;
	by &by_; 
proc univariate data=ph1 noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_a n=nsim1 nsim2 mean=bias_mean diff_mean std=bias_std diff_std median=bias_median diff_median 
      pctlpts=2.5 5 95 97.5 pctlpre=bias_p diff_p ;
run;
proc means data=ph1 noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_b n=nsim1 nsim2 stderr=bias_se diff_se lclm=lclm_bias lclm_diff uclm=uclm_bias uclm_diff;
run;
data emp_ci&nr_ ; 
   merge emp_ci_a emp_ci_b; 
	by &by_; 
   width = diff_p97_5 - diff_p2_5;
run;
%mend prec;
%prec (by_= misdata model corr, nr_=2);

data out.posthoc;
   set emp_ci2;
run;
  
proc export data=out.posthoc outfile="&out\Posthoc.xls" replace;
run;

** Power descriptively  **;
proc sort data=out.power_Per_Sim (where=(endpoint='M18 Treatment Effect' and model ne 'mmrm_un')) out=power1 tagsort; 
	by model misdata endpointn endpoint corr meandiff sd n sim; 
run;
data power1;
   set power1;
   if positive=0 then positive=2;
run;

%macro power (by_=, nr_=);
proc sort data=power1 tagsort;
	by &by_; 
proc freq data= power1 noprint; 
	by &by_; 
   table positive / /*out=power2 (rename=(percent=power))*/ binomial;
   where not index(reason,'WARNING:') and meandiff ne 0;
   output out=power_&nr_ binomial;
run;
%mend power;
%power (by_= model, nr_=1);
%power (by_= misdata model, nr_=2);
%power (by_= model n, nr_=3);
%power (by_= model corr, nr_=4);
%power (by_= model sd, nr_=5);
%power (by_= model meandiff, nr_=6);

%power (by_= misdata, nr_=7);
%power (by_= misdata n, nr_=8);
%power (by_= misdata corr, nr_=9);
%power (by_= misdata sd, nr_=10);
%power (by_= misdata meandiff, nr_=11);

data power_ci;
   length level $40 var $200;
   set power_1 power_2 power_3(rename=(n=ss)) power_4-power_7 power_8(rename=(n=ss)) power_9-power_11;
   bin=100*_bin_;
   xlbin=100*xl_bin;
   xubin=100*xu_bin;

   prob=put(100*_bin_,6.2)||" ("||put(100*xl_bin,6.2)||"; "||put(100*xu_bin,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and ss ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and ss ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=power_ci;
   by varn var leveln level; 
run;
data out.power_ci;
   length fvar $50;
   set power_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, _bin_, xl_bin, xu_bin, bin, xlbin, xubin);
   output;
   keep fvar varn var leveln level prob: bin xlbin xubin;
run;
proc sort data=out.power_ci;
   by varn var leveln level; 
run;

proc export data=out.power_ci outfile="&out\Power.xls" replace;
run;

** Type I descriptively  **;
proc sort data=out.power_Per_Sim (where=(endpoint='M18 Treatment Effect' and model ne 'mmrm_un')) out=power1 tagsort; 
	by model misdata endpointn endpoint corr meandiff sd n sim; 
run;
data power1;
   set power1;
   if positive=0 then positive=2;
run;

%macro power (by_=, nr_=);
proc sort data=power1 tagsort;
	by &by_; 
proc freq data= power1 noprint; 
	by &by_; 
   table positive / /*out=power2 (rename=(percent=power))*/ binomial;
   where not index(reason,'WARNING:') and meandiff = 0;
   output out=t1_&nr_ binomial;
run;
%mend power;
%power (by_= model, nr_=1);
%power (by_= misdata model, nr_=2);
%power (by_= model n, nr_=3);
%power (by_= model corr, nr_=4);
%power (by_= model sd, nr_=5);
%power (by_= model meandiff, nr_=6);

%power (by_= misdata, nr_=7);
%power (by_= misdata n, nr_=8);
%power (by_= misdata corr, nr_=9);
%power (by_= misdata sd, nr_=10);
%power (by_= misdata meandiff, nr_=11);

data t1_ci;
   length level $40 var $200;
   set t1_1 t1_2 t1_3(rename=(n=ss)) t1_4-t1_7 t1_8(rename=(n=ss)) t1_9-t1_11;
   bin=100*_bin_;
   xlbin=100*xl_bin;
   xubin=100*xu_bin;

   prob=put(100*_bin_,6.2)||" ("||put(100*xl_bin,6.2)||"; "||put(100*xu_bin,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and ss ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and ss ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=t1_ci;
   by varn var leveln level; 
run;
data out.t1_ci;
   length fvar $50;
   set t1_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, _bin_, xl_bin, xu_bin, bin, xlbin, xubin);
   output;
   keep fvar varn var leveln level prob: bin xlbin xubin;
run;
proc sort data=out.t1_ci;
   by varn var leveln level; 
run;

proc export data=out.t1_ci outfile="&out\T1.xls" replace;
run;


*** RESULTS FOR SELECTED SCENARIOS ***;

** Precision: Derive the empirical CI **;
data prec1_sel;
   set out.power_Per_Sim;
   where endpoint='M18 Treatment Effect' and model ne 'mmrm_un' and meandiff=0.3 and corr=0.3 and sd=6 and n=90;
   corrc=put(corr,4.2);
   meandiffc=put(meandiff,4.2);
run;
proc sql; select distinct model, misdata, corr, n, sd, meandiff from prec1_sel; quit;

%macro prec (by_=, nr_=);
proc sort data=prec1_sel tagsort;
	by &by_; 
proc univariate data=prec1_sel noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_a n=nsim1 nsim2 mean=bias_mean diff_mean std=bias_std diff_std median=bias_median diff_median 
      pctlpts=2.5 5 95 97.5 pctlpre=bias_p diff_p ;
run;
proc means data=prec1_sel noprint;
	by &by_; 
   var bias estimate;
   output out=emp_ci_b n=nsim1 nsim2 stderr=bias_se diff_se lclm=lclm_bias lclm_diff uclm=uclm_bias uclm_diff;
run;
data emp_ci&nr_ ; 
   merge emp_ci_a emp_ci_b; 
	by &by_; 
   width = diff_p97_5 - diff_p2_5;
run;
%mend prec;
%prec (by_= model, nr_=1);
%prec (by_= misdata model, nr_=2);
/*
%prec (by_= model n, nr_=3);
%prec (by_= model corr, nr_=4);
%prec (by_= model sd, nr_=5);
%prec (by_= model meandiff, nr_=6);
*/
%prec (by_= misdata, nr_=7);
/*%prec (by_= misdata n, nr_=8);
%prec (by_= misdata corr, nr_=9);
%prec (by_= misdata sd, nr_=10);
%prec (by_= misdata meandiff, nr_=11);*/

data emp_ci;
   length level $40 var $200;
   set emp_ci1 emp_ci2 emp_ci7;
   prob=put(width,6.2)||" ("||put(diff_p2_5,6.2)||"; "||put(diff_p97_5,6.2)||')';
   prob_bias=put(bias_mean,6.2)||" ("||put(lclm_bias,6.2)||"; "||put(uclm_bias,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and n ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and n ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=emp_ci;
   by varn var leveln level; 
run;
data out.emp_ci_sel;
   length fvar $50;
   set emp_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, width, diff_p2_5, diff_median, diff_p97_5, bias_median, bias_p2_5, bias_p97_5, prob_bias, bias_mean, lclm_bias, uclm_bias, chisq);
   output;
   keep fvar varn var leveln level chisq prob: width diff_p2_5 diff_p97_5 diff_median bias_p2_5 bias_p97_5 bias_median bias_mean lclm_bias uclm_bias;
run;
proc sort data=out.emp_ci_sel;
   by varn var leveln level; 
run;

** Power descriptively  **;
proc sort data=out.power_Per_Sim (where=(endpoint='M18 Treatment Effect' and model ne 'mmrm_un' and meandiff=0.3 and corr=0.3 and sd=6 and n=90)) out=power1_sel tagsort; 
	by model misdata endpointn endpoint corr meandiff sd n sim; 
run;
data power1_sel;
   set power1_sel;
   if positive=0 then positive=2;
run;

%macro power (by_=, nr_=);
proc sort data=power1_sel tagsort;
	by &by_; 
proc freq data= power1_sel noprint; 
	by &by_; 
   table positive / /*out=power2 (rename=(percent=power))*/ binomial;
   where not index(reason,'WARNING:') and meandiff ne 0;
   output out=power_&nr_ binomial;
run;
%mend power;
%power (by_= model, nr_=1);
%power (by_= misdata model, nr_=2);
/*%power (by_= model n, nr_=3);
%power (by_= model corr, nr_=4);
%power (by_= model sd, nr_=5);
%power (by_= model meandiff, nr_=6);
*/
%power (by_= misdata, nr_=7);
/*%power (by_= misdata n, nr_=8);
%power (by_= misdata corr, nr_=9);
%power (by_= misdata sd, nr_=10);
%power (by_= misdata meandiff, nr_=11);*/

data power_ci;
   length level $40 var $200;
   set power_1 power_2 power_7;
   bin=100*_bin_;
   xlbin=100*xl_bin;
   xubin=100*xu_bin;

   prob=put(100*_bin_,6.2)||" ("||put(100*xl_bin,6.2)||"; "||put(100*xu_bin,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and ss ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and ss ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(ss,best.)); 
      leveln=input(ss,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=power_ci;
   by varn var leveln level; 
run;
data out.power_ci_sel;
   length fvar $50;
   set power_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, _bin_, xl_bin, xu_bin, bin, xlbin, xubin);
   output;
   keep fvar varn var leveln level prob: bin xlbin xubin;
run;
proc sort data=out.power_ci_sel;
   by varn var leveln level; 
run;

*** DERIVATIONS ON ABSULTE BIAS ***;
data prec1_abs;
   set prec1;
   if meandiff ne 0 and endpoint='M18 Treatment Effect' then bias=(estimate/8-meandiff);
run;

%macro prec (by_=, nr_=);
proc sort data=prec1_abs tagsort;
	by &by_; 
proc univariate data=prec1_abs noprint;
	by &by_; 
   var bias ;
   output out=emp_ci_a n=nsim1 mean=bias_mean std=bias_std median=bias_median 
      pctlpts=2.5 5 95 97.5 pctlpre=bias_p ;
run;

proc means data=prec1_abs noprint;
	by &by_; 
   var bias ;
   output out=emp_ci_b n=nsim1  stderr=bias_se  lclm=lclm_bias  uclm=uclm_bias ;
run;
data emp_ci&nr_ ; 
   merge emp_ci_a emp_ci_b; 
	by &by_; 
   width = diff_p97_5 - diff_p2_5;
run;
%mend prec;
%prec (by_= model, nr_=1);
%prec (by_= misdata model, nr_=2);
%prec (by_= model n, nr_=3);
%prec (by_= model corr, nr_=4);
%prec (by_= model sd, nr_=5);
%prec (by_= model meandiff, nr_=6);

%prec (by_= misdata, nr_=7);
%prec (by_= misdata n, nr_=8);
%prec (by_= misdata corr, nr_=9);
%prec (by_= misdata sd, nr_=10);
%prec (by_= misdata meandiff, nr_=11);

data emp_ci;
   length level $40 var $200;
   set emp_ci1-emp_ci11;
   prob=put(width,6.2)||" ("||put(diff_p2_5,6.2)||"; "||put(diff_p97_5,6.2)||')';
   bias_mean=100*bias_mean;
   lclm_bias=100*lclm_bias;
   uclm_bias=100*uclm_bias;
   prob_bias=put(bias_mean,6.2)||" ("||put(lclm_bias,6.2)||"; "||put(uclm_bias,6.2)||')';
   
   if model ne '' then do;
      if model=:'mmrm_un' then leveln=0;
      if model=:'mmrm_ar' then leveln=1000;
      if model=:'slope' then leveln=2000;
   end;
   if misdata ne '' then do;
      if misdata=:'full' then leveln=sum(leveln,0);
      if misdata=:'mcar' then leveln=sum(leveln,10000);
      if misdata=:'mar' then leveln=sum(leveln,20000);
      if misdata=:'mnar' then leveln=sum(leveln,30000);
   end;
   if model ne '' and misdata ne '' then do; varn=7; var='Missingness * Model'; level=strip(misdata)||' * '||strip(model); end; 
   else if model ne '' and n ne . then do; varn=7; var='Model * Sample Size'; level=strip(model)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'mmrm_ar' then leveln=leveln+1000;
      if level=:'slope' then leveln=leveln+2000;
   end; 
   else if model ne '' and corr ne . then do; varn=7; var='Model * Autocorrelation'; level=strip(model)||' * '||put(corr,best.); end; 
   else if model ne '' and sd ne . then do;  varn=7; var='Model * Standard Deviation'; level=strip(model)||' * '||strip(put(sd,best.)); end;
   else if model ne '' and meandiff ne . then do; varn=8; var='Model * Treatment Effect'; level=strip(model)||' * '||strip(put(meandiff,best.)); end; 
   else if model ne '' then do; varn=5; var='Model'; level=upcase(model); end; 
   else if misdata ne '' and n ne . then do; varn=7; var='Missingness * Sample Size'; level=strip(misdata)||' * '||strip(put(n,best.)); 
      leveln=input(n,best.); 
      if level=:'ma' then leveln=leveln+2000;
      if level=:'mc' then leveln=leveln+1000;
      if level=:'mn' then leveln=leveln+3000;
   end; 
   else if misdata ne '' and corr ne . then do; varn=7; var='Missingness * Autocorrelation'; level=strip(misdata)||' * '||put(corr,best.); end; 
   else if misdata ne '' and sd ne . then do; varn=7; var='Missingness * Standard Deviation'; level=strip(misdata)||' * '||strip(put(sd,best.)); end; 
   else if misdata ne '' and meandiff ne . then do; varn=7; var='Missingness * Treatment Effect'; level=strip(misdata)||' * '||strip(put(meandiff,best.)); end; 
   else if misdata ne '' then do; varn=7; var='Missingness'; level=strip(misdata); end; 
   else put "error: add source " ;
   level=compbl(upcase(level)); 
run;

proc sort data=emp_ci;
   by varn var leveln level; 
run;
data out.emp_ci_abs;
   length fvar $50;
   set emp_ci;
   by varn var leveln level; 
   if not first.var then chisq='';
   level=tranwrd(upcase(level),'BIS','');
   fvar="      "||strip(level);
   output;
   if first.var then fvar=var;
   else delete;
   call missing(leveln, level, value, prob, width, diff_p2_5, diff_median, diff_p97_5, bias_median, bias_p2_5, bias_p97_5, prob_bias, bias_mean, lclm_bias, uclm_bias, chisq);
   output;
   keep fvar varn var leveln level chisq prob: width diff_p2_5 diff_p97_5 diff_median bias_p2_5 bias_p97_5 bias_median bias_mean lclm_bias uclm_bias;
run;
proc sort data=out.emp_ci_abs;
   by varn var leveln level; 
run;

