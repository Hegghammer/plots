********************************************************************************
*Replication files for "Measuring terrorism" by Thomas Hegghammer and Neil Ketchley
*Check the instructions.txt file in the Github
********************************************************************************

set more off

clear

import delimited "\Nesser_transformations.csv", bindquote(strict) 

encode country, gen(country_ID)
xtset country_ID year

*Nesser mean and dispersion
*All mean = 0.44
*All dispersion = 4.82
sum all, det
nbreg all

*Launched mean = 0.19
*Launched dispersion = 11.11
sum launched, det
nbreg launched 

*Intraclass correlation for dispersed count as per 
*https://arxiv.org/ftp/arxiv/papers/1911/1911.06888.pdf

* Plots ICC = .46 
menbreg all || country_ID:, startvalues(iv)

preserve
* Deviance
scalar define deviance = -2*e(ll)
scalar list deviance
* Intercept
scalar define beta0 = _b[_cons]
scalar list beta0
* Cluster variance
scalar define sigma2u = _b[/var(_cons[country_ID])]
scalar list sigma2u
* Overdispersion parameter
scalar define alpha = exp(_b[/lnalpha])
scalar list alpha
* Marginal expectation
scalar define expectation = exp(beta0 + sigma2u/2)
scalar list expectation
* Marginal variance
scalar define variance = expectation ///
 + expectation^2*(exp(sigma2u)*(1 + alpha) - 1)
scalar list variance
* Marginal variance: Level-2 component
scalar define variance2 = expectation^2*(exp(sigma2u) - 1)
scalar list variance2
* Marginal variance: Level-1 component
scalar define variance1 = expectation + expectation^2*exp(sigma2u)*alpha
scalar list variance1
* Level-2 ICC
scalar define vpc2 = variance2/(variance2 + variance1)
scalar list vpc2
restore 

*launched ICC = .23
menbreg launched || country_ID:, startvalues(iv)

preserve
* Deviance
scalar define deviance = -2*e(ll)
scalar list deviance
* Intercept
scalar define beta0 = _b[_cons]
scalar list beta0
* Cluster variance
scalar define sigma2u = _b[/var(_cons[country_ID])]
scalar list sigma2u
* Overdispersion parameter
scalar define alpha = exp(_b[/lnalpha])
scalar list alpha
* Marginal expectation
scalar define expectation = exp(beta0 + sigma2u/2)
scalar list expectation
* Marginal variance
scalar define variance = expectation ///
 + expectation^2*(exp(sigma2u)*(1 + alpha) - 1)
scalar list variance
* Marginal variance: Level-2 component
scalar define variance2 = expectation^2*(exp(sigma2u) - 1)
scalar list variance2
* Marginal variance: Level-1 component
scalar define variance1 = expectation + expectation^2*exp(sigma2u)*alpha
scalar list variance1
* Level-2 ICC
scalar define vpc2 = variance2/(variance2 + variance1)
scalar list vpc2
restore


*missing values
local predictors = "l_all l_launched asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend l_elections sql_rwattacks"
foreach var of local predictors  {
	replace `var' = "" if `var' == "NA"
	}
	
*Formatting data
gen byte notnumeric = real(l_percentagemuslims)==.	
tab notnumeric 
list l_percentagemuslims if notnumeric==1	
replace l_percentagemuslims = "0.188741135" if l_percentagemuslims == "0,188741135"
replace l_percentagemuslims = "2.830605046" if l_percentagemuslims == "2,830605046"
	
gen byte notnumeric2 = real(l_unemployment)==.	
tab notnumeric2
list l_unemployment if notnumeric2==1	
replace l_unemployment = "2.742000103" if l_unemployment == "2,742000103"
replace l_unemployment = "5.521999836" if l_unemployment == "5,521999836"
		
	
*destring
local predictors = "asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend l_elections sql_rwattacks"
foreach var of local predictors  {
	destring `var', replace
	}
	
***
*Table 1
***
	
*Poisson
poisson all asl_all l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store A

poisson launched asl_launched l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store B

esttab A B using results.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(asl_all "Plots (asinh, t-1)" ///
	ll_population "Population (log, t-1)" ///
	ll_gdpcap "GDP per capita (log, t-1)" ///
	ll_refugees "Refugees (log, t-1)" ///
	ll_rightwingness "Left/right government (log, t-1)" ///
	l_cpichg "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (t-1)" ///
	asl_troops_in_mw "Troops in MENA (asinh, t-1)" ///
	l_securityspend "Security budget (t-1)" ///
	l_elections "Election (t-1)" ///
	sql_rwattacks "Right wing attacks (sqrt, t-1)" ///
	years_from_2001 "Years from 2001" ///
	years_from_2001_sq "Years from 2001 (squared)" ///
	years_from_isis "Years from ISIS caliphate" ///
	years_from_isis_sq "Years from ISIS caliphate (squared)") ///
	mtitle("Plots" "Attacks" "Plots" "Attacks") ///	
	b(%9.3f) t(%9.3f) ///
	replace		
	
*Equality of coefficients
poisson all asl_all l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store A

poisson launched asl_launched l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store B

suest A B, vce(cluster country_ID) level(90)

test [A_all]asl_all=[B_launched]asl_launched
cap test [A_all]ll_population=[B_launched]ll_population
test [A_all]l_percentagemuslims=[B_launched]l_percentagemuslims
test [A_all]ll_refugees=[B_launched]ll_refugees
test [A_all]ll_gdpcap=[B_launched ]ll_gdpcap
test [A_all]l_unemployment=[B_launched ]l_unemployment
test [A_all]l_cpichg=[B_launched ]l_cpichg
test [A_all]l_socialspend=[B_launched ]l_socialspend
test [A_all]ll_rightwingness=[B_launched ]ll_rightwingness
test [A_all]l_elections=[B_launched ]l_elections
test [A_all]l_securityspend=[B_launched ]l_securityspend
test [A_all]asl_troops_in_mw=[B_launched ]asl_troops_in_mw
test [A_all]sql_rwattacks=[B_launched ]sql_rwattacks
test [A_all]years_from_2001=[B_launched ]years_from_2001
test [A_all]years_from_2001_sq=[B_launched ]years_from_2001_sq
test [A_all]years_from_isis=[B_launched ]years_from_isis
test [A_all]years_from_isis_sq=[B_launched ]years_from_isis_sq
	
*Appendix Table 8.1 --- OLS Nesser
*OLS
reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store C

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store D

esttab C D using Appendix_results_nesser.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(asl_all "Plots (asinh, t-1)" ///
	ll_population "Population (log, t-1)" ///
	ll_gdpcap "GDP per capita (log, t-1)" ///
	ll_refugees "Refugees (log, t-1)" ///
	ll_rightwingness "Left/right government (log, t-1)" ///
	l_cpichg "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (t-1)" ///
	asl_troops_in_mw "Troops in MENA (asinh, t-1)" ///
	l_securityspend "Security budget (t-1)" ///
	l_elections "Election (t-1)" ///
	sql_rwattacks "Right wing attacks (sqrt, t-1)" ///
	years_from_2001 "Years from 2001" ///
	years_from_2001_sq "Years from 2001 (squared)" ///
	years_from_isis "Years from ISIS caliphate" ///
	years_from_isis_sq "Years from ISIS caliphate (squared)") ///
	mtitle("Plots" "Attacks" "Plots" "Attacks") ///	
	b(%9.3f) t(%9.3f) ///
	replace		
	
*Equality of coefficients	
reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store A

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store B

suest A B, vce(cluster country_ID) level(90)

test [A_mean ]asl_all=[B_mean]asl_launched
test [A_mean ]ll_population=[B_mean]ll_population
test [A_mean ]l_percentagemuslims=[B_mean]l_percentagemuslims
test [A_mean ]ll_refugees=[B_mean]ll_refugees
test [A_mean ]ll_gdpcap=[B_mean ]ll_gdpcap
test [A_mean ]l_unemployment=[B_mean ]l_unemployment
test [A_mean ]l_cpichg=[B_mean ]l_cpichg
test [A_mean ]l_socialspend=[B_mean ]l_socialspend
test [A_mean ]ll_rightwingness=[B_mean ]ll_rightwingness
test [A_mean ]l_elections=[B_mean ]l_elections
test [A_mean ]l_securityspend=[B_mean ]l_securityspend
test [A_mean ]asl_troops_in_mw=[B_mean ]asl_troops_in_mw
test [A_mean ]sql_rwattacks=[B_mean ]sql_rwattacks
test [A_mean ]years_from_2001=[B_mean ]years_from_2001
test [A_mean ]years_from_2001_sq=[B_mean ]years_from_2001_sq
test [A_mean ]years_from_isis=[B_mean ]years_from_isis
test [A_mean ]years_from_isis_sq=[B_mean ]years_from_isis_sq

*Appendix Table 8.3 --- OLS Crenshaw
clear
import delimited "C:\Users\neilke\Dropbox\Neil and Thomas\Plots\Crenshaw_transformations.csv"

encode country, gen(country_ID)
xtset country_ID year

*missing values
local predictors = "l_all l_launched asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend l_elections sql_rwattacks"
foreach var of local predictors  {
	replace `var' = "" if `var' == "NA"
	}
	
	
*destring
local predictors = "asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend l_elections sql_rwattacks"
foreach var of local predictors  {
	destring `var', replace
	}


reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store E

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store F

esttab E F using Appendix_results_crenshaw.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(asl_all "Plots (asinh, t-1)" ///
	ll_population "Population (log, t-1)" ///
	ll_gdpcap "GDP per capita (log, t-1)" ///
	ll_refugees "Refugees (log, t-1)" ///
	ll_rightwingness "Left/right government (log, t-1)" ///
	l_cpichg "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (t-1)" ///
	asl_troops_in_mw "Troops in MENA (asinh, t-1)" ///
	l_securityspend "Security budget (t-1)" ///
	l_elections "Election (t-1)" ///
	sql_rwattacks "Right wing attacks (sqrt, t-1)" ///
	years_from_2001 "Years from 2001" ///
	years_from_2001_sq "Years from 2001 (squared)" ///
	years_from_isis "Years from ISIS caliphate" ///
	years_from_isis_sq "Years from ISIS caliphate (squared)") ///
	mtitle("Plots" "Attacks" "Plots" "Attacks") ///	
	b(%9.3f) t(%9.3f) ///
	replace			
	
	
*Equality of coefficients	
reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store A

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_elections l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store B

suest A B, vce(cluster country_ID) level(90)

test [A_mean ]asl_all=[B_mean]asl_launched
test [A_mean ]ll_population=[B_mean]ll_population
test [A_mean ]l_percentagemuslims=[B_mean]l_percentagemuslims
test [A_mean ]ll_refugees=[B_mean]ll_refugees
test [A_mean ]ll_gdpcap=[B_mean ]ll_gdpcap
test [A_mean ]l_unemployment=[B_mean ]l_unemployment
test [A_mean ]l_cpichg=[B_mean ]l_cpichg
test [A_mean ]l_socialspend=[B_mean ]l_socialspend
test [A_mean ]ll_rightwingness=[B_mean ]ll_rightwingness
test [A_mean ]l_elections=[B_mean ]l_elections
test [A_mean ]l_securityspend=[B_mean ]l_securityspend
test [A_mean ]asl_troops_in_mw=[B_mean ]asl_troops_in_mw
test [A_mean ]sql_rwattacks=[B_mean ]sql_rwattacks
test [A_mean ]years_from_2001=[B_mean ]years_from_2001
test [A_mean ]years_from_2001_sq=[B_mean ]years_from_2001_sq
test [A_mean ]years_from_isis=[B_mean ]years_from_isis
test [A_mean ]years_from_isis_sq=[B_mean ]years_from_isis_sq
	