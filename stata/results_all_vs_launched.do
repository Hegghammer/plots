********************************************************************************
*Replication files for "Measuring terrorism" by Thomas Hegghammer and Neil Ketchley
*Check the instructions.txt file in the Github
********************************************************************************

cd "C:\Users\neilke\Dropbox\Neil and Thomas\r&r_jcr_aug_2022\analysis"
clear
set more off

import delimited "C:\Users\neilke\Dropbox\Neil and Thomas\r&r_jcr_aug_2022\data\nesser_new.csv"

*xtset data
encode country, gen(country_ID)
xtset country_ID year

*get intraclass correlation for count DVs
*Intraclass correlation for dispersed count as per 
*https://arxiv.org/ftp/arxiv/papers/1911/1911.06888.pdf

* Plots ICC = .50
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

*launched ICC = .28
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

***Create independent variables

*create predictors
gen as_all = asinh(all)
gen as_launched = asinh(launched)
gen sqrt_rwattacks = sqrt(rwattacks)

gen ln_refugees = log(refugees)
gen ln_gdpcap = log(gdpcap)
gen ln_population = log(population)
gen as_troops_in_mw = asinh(troops_in_mw)
gen ln_left_right = log(left_right)

gen pct_change_cpi = (cpi - L.cpi) / L.cpi * 100


*years since 9/11
sort country_ID year
by country_ID: gen yearsfrom2001 = _n if year>2001
replace yearsfrom2001 = 0 if yearsfrom2001==.
replace yearsfrom2001 = yearsfrom2001-8 if yearsfrom2001>0	
gen yearsfrom2001_2 = yearsfrom2001^2	

*years since ISIS caliphate
sort country_ID year
by country_ID: gen yearsfromISIScaliph = _n if year>2014
replace yearsfromISIScaliph = 0 if yearsfromISIScaliph==.
replace yearsfromISIScaliph = yearsfromISIScaliph-21 if yearsfromISIScaliph>0	
gen yearsfromISIScaliph2 = yearsfromISIScaliph^2	
		

*gen lagged predictors
local predictors = "as_troops_in_mw ln_population percentagemuslims ln_gdpcap ln_refugees sqrt_rwattacks as_launched as_all left_right ln_left_right securityspend socialspend unemployment"
foreach var of local predictors  {
	gen l_`var' = l.`var'
	}
	

***
*Table 1
***
	
*Poisson
poisson all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store A

poisson launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store B

*Equality of coefficients
poisson all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store A_e

poisson launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store B_e

suest A_e B_e, vce(cluster country_ID) level(90)

test [A_e_all]l_as_all=[B_e_launched]l_as_launched
test [A_e_all]l_ln_population=[B_e_launched]l_ln_population
test [A_e_all]l_percentagemuslims=[B_e_launched]l_percentagemuslims
test [A_e_all]l_ln_refugees=[B_e_launched]l_ln_refugees
test [A_e_all]l_ln_gdpcap=[B_e_launched ]l_ln_gdpcap
test [A_e_all]l_unemployment=[B_e_launched ]l_unemployment
test [A_e_all]pct_change_cpi=[B_e_launched ]pct_change_cpi
test [A_e_all]l_socialspend=[B_e_launched ]l_socialspend
test [A_e_all]l_ln_left_right=[B_e_launched ]l_ln_left_right
test [A_e_all]l_securityspend=[B_e_launched ]l_securityspend
test [A_e_all]l_as_troops_in_mw=[B_e_launched ]l_as_troops_in_mw
test [A_e_all]l_sqrt_rwattacks=[B_e_launched ]l_sqrt_rwattacks
test [A_e_all]yearsfrom2001=[B_e_launched ]yearsfrom2001
test [A_e_all]yearsfrom2001_2=[B_e_launched ]yearsfrom2001_2
test [A_e_all]yearsfromISIScaliph=[B_e_launched ]yearsfromISIScaliph
test [A_e_all]yearsfromISIScaliph2=[B_e_launched ]yearsfromISIScaliph2

esttab A B using nesser_poisson.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(l_as_all "Lagged DV (asinh, t-1)" ///
	l_ln_population "Population (log, t-1)" ///
	l_ln_gdpcap "GDP per capita (log, t-1)" ///
	l_ln_refugees "Refugees (log, t-1)" ///
	l_ln_left_right "Left/right government (log, t-1)" ///
	pct_change_cpi "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (\%, t-1)" ///
	l_as_troops_in_mw "Troops in Muslim countries (asinh, t-1)" ///
	l_securityspend "Security spending (\%, t-1)" ///
	l_sqrt_rwattacks "Right wing attacks (sqrt, t-1)" ///
	yearsfrom2001 "Years from 9/11" ///
	yearsfrom2001_2 "Years from 9/11 (squared)" ///
	yearsfromISIScaliph "Years from ISIS caliphate" ///
	yearsfromISIScaliph2 "Years from ISIS caliphate (squared)") ///
	mtitle("DV: Plots" "DV: Attacks") ///	
	b(%9.3f) t(%9.3f) ///
	replace		
	

***
*Appendix --- Nesser
***		
	
*negative binomial
nbreg all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store X

nbreg launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store Y

*Equality of coefficients
nbreg all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store A_e

nbreg launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store B_e

suest A_e B_e, vce(cluster country_ID) level(90)

test [A_e_all]l_as_all=[B_e_launched]l_as_launched
test [A_e_all]l_ln_population=[B_e_launched]l_ln_population
test [A_e_all]l_percentagemuslims=[B_e_launched]l_percentagemuslims
test [A_e_all]l_ln_refugees=[B_e_launched]l_ln_refugees
test [A_e_all]l_ln_gdpcap=[B_e_launched ]l_ln_gdpcap
test [A_e_all]l_unemployment=[B_e_launched ]l_unemployment
test [A_e_all]pct_change_cpi=[B_e_launched ]pct_change_cpi
test [A_e_all]l_socialspend=[B_e_launched ]l_socialspend
test [A_e_all]l_ln_left_right=[B_e_launched ]l_ln_left_right
test [A_e_all]l_securityspend=[B_e_launched ]l_securityspend
test [A_e_all]l_as_troops_in_mw=[B_e_launched ]l_as_troops_in_mw
test [A_e_all]l_sqrt_rwattacks=[B_e_launched ]l_sqrt_rwattacks
test [A_e_all]yearsfrom2001=[B_e_launched ]yearsfrom2001
test [A_e_all]yearsfrom2001_2=[B_e_launched ]yearsfrom2001_2
test [A_e_all]yearsfromISIScaliph=[B_e_launched ]yearsfromISIScaliph
test [A_e_all]yearsfromISIScaliph2=[B_e_launched ]yearsfromISIScaliph2

esttab X Y using nesser_nb.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(asl_all "Lagged DV (asinh, t-1)" ///
	l_ln_population "Population (log, t-1)" ///
	l_ln_gdpcap "GDP per capita (log, t-1)" ///
	l_ln_refugees "Refugees (log, t-1)" ///
	l_ln_left_right "Left/right government (log, t-1)" ///
	pct_change_cpi "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (\%, t-1)" ///
	l_as_troops_in_mw "Troops in Muslim countries (asinh, t-1)" ///
	l_securityspend "Security spending (\%, t-1)" ///
	l_sqrt_rwattacks "Right wing attacks (sqrt, t-1)" ///
	yearsfrom2001 "Years from 9/11" ///
	yearsfrom2001_2 "Years from 9/11 (squared)" ///
	yearsfromISIScaliph "Years from ISIS caliphate" ///
	yearsfromISIScaliph2 "Years from ISIS caliphate (squared)") ///
	mtitle("DV: Plots" "DV: Attacks") ///	
	b(%9.3f) t(%9.3f) ///
	replace			
	
*OLS - count DV
reg all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store C

reg launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store D
	
*Equality of coefficients - transformed DV
reg all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store C_e

reg launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store D_e

suest C_e D_e, vce(cluster country_ID) level(90)

test [C_e_mean]l_as_all=[D_e_mean]l_as_launched
test [C_e_mean]l_ln_population=[D_e_mean]l_ln_population
test [C_e_mean]l_percentagemuslims=[D_e_mean]l_percentagemuslims
test [C_e_mean]l_ln_refugees=[D_e_mean]l_ln_refugees
test [C_e_mean]l_ln_gdpcap=[D_e_mean]l_ln_gdpcap
test [C_e_mean]l_unemployment=[D_e_mean]l_unemployment
test [C_e_mean]pct_change_cpi=[D_e_mean]pct_change_cpi
test [C_e_mean]l_socialspend=[D_e_mean]l_socialspend
test [C_e_mean]l_ln_left_right=[D_e_mean]l_ln_left_right
test [C_e_mean]l_securityspend=[D_e_mean]l_securityspend
test [C_e_mean]l_as_troops_in_mw=[D_e_mean]l_as_troops_in_mw
test [C_e_mean]l_sqrt_rwattacks=[D_e_mean]l_sqrt_rwattacks
test [C_e_mean]yearsfrom2001=[D_e_mean]yearsfrom2001
test [C_e_mean]yearsfrom2001_2=[D_e_mean]yearsfrom2001_2
test [C_e_mean]yearsfromISIScaliph=[D_e_mean]yearsfromISIScaliph
test [C_e_mean]yearsfromISIScaliph2=[D_e_mean]yearsfromISIScaliph2

*OLS - transformed DV
reg as_all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store E

reg as_launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID, vce(cluster country_ID) level(90)
est store F
		
*Equality of coefficients - transformed DV
reg as_all l_as_all l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store E_e

reg as_launched l_as_launched l_ln_population l_percentagemuslims l_ln_refugees l_ln_gdpcap l_unemployment pct_change_cpi l_socialspend l_ln_left_right l_securityspend l_as_troops_in_mw l_sqrt_rwattacks yearsfrom2001 yearsfrom2001_2 yearsfromISIScaliph yearsfromISIScaliph2 i.country_ID
est store F_e

suest E_e F_e, vce(cluster country_ID) level(90)

test [E_e_mean]l_as_all=[F_e_mean]l_as_launched
test [E_e_mean]l_ln_population=[F_e_mean]l_ln_population
test [E_e_mean]l_percentagemuslims=[F_e_mean]l_percentagemuslims
test [E_e_mean]l_ln_refugees=[F_e_mean]l_ln_refugees
test [E_e_mean]l_ln_gdpcap=[F_e_mean]l_ln_gdpcap
test [E_e_mean]l_unemployment=[F_e_mean]l_unemployment
test [E_e_mean]pct_change_cpi=[F_e_mean]pct_change_cpi
test [E_e_mean]l_socialspend=[F_e_mean]l_socialspend
test [E_e_mean]l_ln_left_right=[F_e_mean]l_ln_left_right
test [E_e_mean]l_securityspend=[F_e_mean]l_securityspend
test [E_e_mean]l_as_troops_in_mw=[F_e_mean]l_as_troops_in_mw
test [E_e_mean]l_sqrt_rwattacks=[F_e_mean]l_sqrt_rwattacks
test [E_e_mean]yearsfrom2001=[F_e_mean]yearsfrom2001
test [E_e_mean]yearsfrom2001_2=[F_e_mean]yearsfrom2001_2
test [E_e_mean]yearsfromISIScaliph=[F_e_mean]yearsfromISIScaliph
test [E_e_mean]yearsfromISIScaliph2=[F_e_mean]yearsfromISIScaliph2


*Table
esttab X Y C D E F using nesser_nb_OLS.tex, ///
	se obslast ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabel(asl_all "Lagged DV (asinh, t-1)" ///
	l_ln_population "Population (log, t-1)" ///
	l_ln_gdpcap "GDP per capita (log, t-1)" ///
	l_ln_refugees "Refugees (log, t-1)" ///
	l_ln_left_right "Left/right government (log, t-1)" ///
	pct_change_cpi "CPI change (\%, t-1)" ///
	l_percentagemuslims "Muslims (\%, t-1)" ///
	l_unemployment "Unemployment (\%, t-1)" ///
	l_socialspend "Social spending (\%, t-1)" ///
	l_as_troops_in_mw "Troops in Muslim countries (asinh, t-1)" ///
	l_securityspend "Security spending (\%, t-1)" ///
	l_sqrt_rwattacks "Right wing attacks (sqrt, t-1)" ///
	years_from_2001 "Years from 9/11" ///
	years_from_2001_sq "Years from 9/11 (squared)" ///
	years_from_isis "Years from ISIS caliphate" ///
	years_from_isis_sq "Years from ISIS caliphate (squared)") ///
	mtitle("DV: Plots" "DV: Attacks" "DV: Plots" "DV: Attacks" "DV: asinh(Plots)" "DV: asinh(Attacks)") ///	
	b(%9.3f) t(%9.3f) ///
	replace			
	
	
***
*Appendix --- Crenshaw
***
clear
import delimited "C:\Users\neilke\Dropbox\Neil and Thomas\review\replication_files\Crenshaw_transformations.csv"

encode country, gen(country_ID)
xtset country_ID year

*missing values
local predictors = "l_all l_launched asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend sql_rwattacks"
foreach var of local predictors  {
	replace `var' = "" if `var' == "NA"
	}
	
	
*destring
local predictors = "asl_all asl_launched ll_population ll_gdpcap ll_refugees ll_rightwingness l_cpichg l_percentagemuslims l_unemployment l_socialspend asl_troops_in_mw l_securityspend sql_rwattacks"
foreach var of local predictors  {
	destring `var', replace
	}

*Poisson
poisson all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store A

poisson launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store B
	
*Equality of coefficients
poisson all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store A_e

poisson launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store B_e

suest A_e B_e, vce(cluster country_ID) level(90)

test [A_e_all]asl_all=[B_e_launched]asl_launched
test [A_e_all]ll_population=[B_e_launched]ll_population
test [A_e_all]l_percentagemuslims=[B_e_launched]l_percentagemuslims
test [A_e_all]ll_refugees=[B_e_launched]ll_refugees
test [A_e_all]ll_gdpcap=[B_e_launched ]ll_gdpcap
test [A_e_all]l_unemployment=[B_e_launched ]l_unemployment
test [A_e_all]l_cpichg=[B_e_launched ]l_cpichg
test [A_e_all]l_socialspend=[B_e_launched ]l_socialspend
test [A_e_all]ll_rightwingness=[B_e_launched ]ll_rightwingness
test [A_e_all]l_securityspend=[B_e_launched ]l_securityspend
test [A_e_all]asl_troops_in_mw=[B_e_launched ]asl_troops_in_mw
test [A_e_all]sql_rwattacks=[B_e_launched ]sql_rwattacks
test [A_e_all]years_from_2001=[B_e_launched ]years_from_2001
test [A_e_all]years_from_2001_sq=[B_e_launched ]years_from_2001_sq
test [A_e_all]years_from_isis=[B_e_launched ]years_from_isis
test [A_e_all]years_from_isis_sq=[B_e_launched ]years_from_isis_sq
		
*OLS - count DV
reg all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store C

reg launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store D
	
*Equality of coefficients - transformed DV
reg all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store C_e

reg launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store D_e

suest C_e D_e, vce(cluster country_ID) level(90)

test [C_e_mean]asl_all=[D_e_mean]asl_launched
test [C_e_mean]ll_population=[D_e_mean]ll_population
test [C_e_mean]l_percentagemuslims=[D_e_mean]l_percentagemuslims
test [C_e_mean]ll_refugees=[D_e_mean]ll_refugees
test [C_e_mean]ll_gdpcap=[D_e_mean]ll_gdpcap
test [C_e_mean]l_unemployment=[D_e_mean]l_unemployment
test [C_e_mean]l_cpichg=[D_e_mean]l_cpichg
test [C_e_mean]l_socialspend=[D_e_mean]l_socialspend
test [C_e_mean]ll_rightwingness=[D_e_mean]ll_rightwingness
test [C_e_mean]l_securityspend=[D_e_mean]l_securityspend
test [C_e_mean]asl_troops_in_mw=[D_e_mean]asl_troops_in_mw
test [C_e_mean]sql_rwattacks=[D_e_mean]sql_rwattacks
test [C_e_mean]years_from_2001=[D_e_mean]years_from_2001
test [C_e_mean]years_from_2001_sq=[D_e_mean]years_from_2001_sq
test [C_e_mean]years_from_isis=[D_e_mean]years_from_isis
test [C_e_mean]years_from_isis_sq=[D_e_mean]years_from_isis_sq
	
*OLS - transformed DV
reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store E

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID, vce(cluster country_ID) level(90)
est store F
	
*Equality of coefficients - transformed DV
reg as_all asl_all ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store E_e

reg as_launched asl_launched ll_population l_percentagemuslims ll_refugees ll_gdpcap l_unemployment l_cpichg l_socialspend ll_rightwingness l_securityspend asl_troops_in_mw sql_rwattacks years_from_2001 years_from_2001_sq years_from_isis years_from_isis_sq i.country_ID
est store F_e

suest E_e F_e, vce(cluster country_ID) level(90)

test [E_e_mean]asl_all=[F_e_mean]asl_launched
test [E_e_mean]ll_population=[F_e_mean]ll_population
test [E_e_mean]l_percentagemuslims=[F_e_mean]l_percentagemuslims
test [E_e_mean]ll_refugees=[F_e_mean]ll_refugees
test [E_e_mean]ll_gdpcap=[F_e_mean]ll_gdpcap
test [E_e_mean]l_unemployment=[F_e_mean]l_unemployment
test [E_e_mean]l_cpichg=[F_e_mean]l_cpichg
test [E_e_mean]l_socialspend=[F_e_mean]l_socialspend
test [E_e_mean]ll_rightwingness=[F_e_mean]ll_rightwingness
test [E_e_mean]l_securityspend=[F_e_mean]l_securityspend
test [E_e_mean]asl_troops_in_mw=[F_e_mean]asl_troops_in_mw
test [E_e_mean]sql_rwattacks=[F_e_mean]sql_rwattacks
test [E_e_mean]years_from_2001=[F_e_mean]years_from_2001
test [E_e_mean]years_from_2001_sq=[F_e_mean]years_from_2001_sq
test [E_e_mean]years_from_isis=[F_e_mean]years_from_isis
test [E_e_mean]years_from_isis_sq=[F_e_mean]years_from_isis_sq
	
*table	
esttab A B C D E F using crenshaw.tex, ///
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
	asl_troops_in_mw "Troops in Muslim countries (asinh, t-1)" ///
	l_securityspend "Security spending (t-1)" ///
	sql_rwattacks "Right wing attacks (sqrt, t-1)" ///
	years_from_2001 "Years from 9/11" ///
	years_from_2001_sq "Years from 9/11 (squared)" ///
	years_from_isis "Years from ISIS caliphate" ///
	years_from_isis_sq "Years from ISIS caliphate (squared)") ///
	mtitle("Plots" "Attacks" "Plots" "Attacks" "asinh(Plots)" "asinh(Attacks)") ///	
	b(%9.3f) t(%9.3f) ///
	replace		
	
	
	