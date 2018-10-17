

* The purpose of this do file is to wirte codes for 10/18/2018 PRC talk 

clear
set mem 1000000
set more off

cd "C:\Users\sychao\Dropbox\16_2018Fall\PRC TA\Data for the Talk"

use all.dta, clear 

*=== Explore data ===*

*=1. Missing value
// summarizes # of missing values
mdesc 
// summarizes missing pattern
misschk hp5 female age partner hedu fulltime employed profs finc_d pt60, gen(miss)

*=2. Examine whether the model violates OLS assumtions 
** Research Question: whether family policy is associated with the gap between parents and nonparents, country-level analysis
** Method: OLS
** Model: happiness gap| family policy index, GDP, TFR, extend family, work hours // according to your theory  

gen data=.
 replace data=1 if wvs==1
 replace data=2 if abs==1
 label define data 1"WVS" 2"ABS", modify
 label val data data 
 
gen group=.
  foreach x of numlist 1(1)10{
    replace group=`x'     if country==`x'&wvs==1
    replace group=`x'+10  if country==`x'&abs==1
   }  

  label define group  1"China(WVS)" 2"Japan(WVS)" 3"Korea(WVS)" 4"Taiwan(WVS)" 5"Hongkong(WVS)" 6"India(WVS)" 7"Indonesia(WVS)" 8"Malaysia(WVS)" 9"Thailand(WVS)" 10"Vietnam(WVS)" ///
                      11"China(ABS)"  12"Japan(ABS)" 13"Korea(ABS)"  14"Taiwan(ABS)" 15"Hongkong(ABS)" 16"India(WVS)"  17"Indonesia(ABS)" 18"Malaysia(ABS)" 19"Thailand(ABS)"  20"Vietnam(ABS)" , modify
  label val group group 

gen parhp5=.
gen nonparhp5=.
gen pardiffhp5=.

forvalues i = 1(1)20 { 
foreach var of varlist hp5 { 
qui su `var' if pt60==1 & group==`i'
replace par`var'=r(mean) if group==`i'
qui su `var' if pt60==0 & group==`i' 
replace nonpar`var'=r(mean) if group==`i' 

replace pardiff`var'=par`var'-nonpar`var' if group==`i' 

}
}

sample 1,by(group) count

*A. normal distribution for your dependet variable: the happiness gap, a countinuous variable  
histogram pardiffhp5
sum pardiffhp5, d // Kurtosis=4.418741; Skewness=.2596614


*B. inflential cases: if you remove the cases from anaysis, the estimates have huge changes. 

**B-1 Scattorplots: before running models 

twoway (scatter pardiffhp5 CPI3, mlabel(group) mlabsize(vsmall) mlabposition(12) ti(Parenthood Happiness Gap)) (lfit pardiffhp5 CPI3), ///
        xtitle(Comprehensive Family Policy Index) ytitle(Predicted Happiness) legend(off)
	   
**B-2 Post estimation	   

***Cook’s D: threshold 4/n=4/20=0.2 or 1

reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr 
predict dfit,dfits
predict d, cooksd
list country data d if abs(d)>.2
drop dfit d

*** DFBETAS:  threshold 2/sqrt(n) or 1
reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr
dfbeta
list country data _dfbeta_1 if abs(_dfbeta_1)>.4472136
drop _dfbeta* 

*C. multicollinearity: the independent varibales have no perfect correlations 
// vif (the variance inflation factor 2.5)
reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr
vif
* There are three situations in which a high VIF is not a problem
* 1. The variables with high VIFs are control variables and the variables of interest do not have high VIFs 
* 2. The high VIFs are caused by the inclusion of powers or products of other variables
* 3. The variables with high VIFs are indicator(dummy) variables that represent a categorical variable with htree or more categories 

*C. heteroskedasticity: the error variance is not constant for all observations  
reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr
rvfplot
estat hettest

*D. omitted variable: endogeneity // rely on theories more than computation 
reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr
ovtest

*E. specification error: the relationship may not a linear // rely on theories more than computation 
reg pardiffhp5 CPI3 GDP_p TFR mc_ext wkhr
scatter pardiffhp5 CPI3 ||lowess pardiffhp5 CPI3
acprplot CPI3, lowess
