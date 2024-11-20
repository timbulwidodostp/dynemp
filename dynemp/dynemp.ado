version 11 // set the stata version
set seed 0
set varabbrev off

cap program drop dynemp
program define dynemp
set seed 0

#delimit ;

syntax [if] [in], COUNTRY(string) UNIT(string) ID(varname) EMPloyment(varname numeric) YEAR(varname numeric) BIRTH(varname numeric)
	[	ISIC4(varname numeric)   conf(integer 5) 	BLANK 		EXPress
	OUTPUTDIR(string)
	SECTORCHANGE isic3(varname numeric) newindyear(numlist)
	EXITDEATH(varname numeric) EXITCHANGE(varname numeric)
	NOREG regyear(numlist)
	turnover(varname numeric) LEFTcensoring(varname numeric) yeart(numlist integer >1950 <2014 sort) extraformat(string)
	levels(numlist) regsample(integer 10000000)
	transyears(numlist)] ;

#delimit cr

version 11 // set the stata version
set seed 0
set varabbrev off

*** test that newindyear, sectorchange, isic3, isic4 option combination is consistent

if "`sectorchange'"!="" {
	if "`newindyear'"=="" {
		display as error "Error: newindyear should always be specified with the option sectorchange"
		exit 198 
		}
	}

if ("`sectorchange'"=="" | "`newindyear'"=="") & "`isic3'"!="" & "`isic4'"!="" {
		display as error "Error: sectorchange and newindyear should always be specified when both isic3 and isic4 are included"
		exit 198 
		}
	
	
*****

display _n "	****** Running dynemp.ado version 2.5 `blank' `express' `sectorchange' ****** "
display "", _newline(1)
display " *** Project on employment dynamics - thanks for participating! ***", _newline(1)
display "", _newline(1)  
display "© copyright Chiara Criscuolo, Peter Gal and Carlo Menon, OECD 2013" 
display "For any queries, please write to Dynemp@oecd.org", _newline(1)   
display "", _newline(1)


qui {
	
set more off

timer clear 1
timer on 1

/* DATA INSPECTION BEFORE CLEANING */

***
noisily display as text "", _newline(2)
noisily display as text "Inspecting the data before cleaning...", _newline(2)
noisily describe
noisily bys `year': summ
noisily inspect
noisily display as text "", _newline(2)
noisily display as text "Years included in the sample and number of observations in each year", _newline(2)
noisily tab `year' 
noisily display as text "", _newline(2)

***




/* SETTINGS */

gl inputdata_dir "`c(pwd)'"

gl minyear 1998


/* Years for transition matrix (start of all intervals)  */

gl years_trans	2001	2004	2007 
if  "`transyears'"!="" gl years_trans `transyears'

/* J-list elements */
gl jlist_trans 3 5 7


/* Minimum number of observations in cells. The number of cells containing less units than the set number will be reported on the screen */
gl mincell	`conf'


// Define levels of aggregation, depending on whether they are user-specified or not

if "`levels'"=="" local levels 1 2 3 4 

// check that yeart list is consistent with data
	
if "`yeart'"!="" {
	levelsof `year'
	local yeartlist=subinstr("`yeart'", " " ,  " , "  , 50) // inputed year list
	local yeardata=r(levels)
	local yeardatalist=subinstr("`yeardata'", " " ,  " , "  , 50) // year list from the data
	local maxyeart=max(`yeartlist')
	local maxyeardata=max(`yeardatalist')
	local minyeart=min(`yeartlist')
	local minyeardata=min(`yeardatalist')
	
	if `maxyeart'>`maxyeardata' | `minyeart'<`minyeardata'  {
		noisily di as err "Error: the years listed in the yeart() option refer to a longer interval than that available in the data"
		noisily di as err "Please check again the option or the data"
		exit 198 
		}
	}

// create year lists if option is empty 
	
if "`yeart'"=="" {
	levelsof `year'
	local yeart=r(levels)
	// Create year list with comma, to be used later on...
	local yeardatalist=subinstr("`yeart'", " " ,  " , "  , 50)
	}


	
noi di "Initial checks on data consistency..."
count if `birth'<1000 | (`birth'>2013 & `birth'!=.)  
if r(N)>0 {
	noisily di as err "The year of birth variable contains implausible values - please check your data"
	exit 198 
	}

	if "`extraformat'"!="txt" & "`extraformat'"!="csv" & "`extraformat'"!=""  {
	noisily di as err "only 'txt' or 'csv' are admitted in the extraformat option"
	exit 198 
	}
noi di "	Done"	

*Program stops if exit variables are problematic		
* check if exitdeath and exitchange are binary (only if they exist) 

if "`exitdeath'"!="" {
	
	replace `exitdeath'=0 if `exitdeath'==.
	
	count if `exitdeath'!=0 & `exitdeath'!=1  
	if r(N)>0 {
		noisily di as err "the exitchange variable `exitchange' can take only 0/1 values"
		exit 198 
		}
	}

if "`exitchange'"!="" {
	
	replace `exitchange'=0 if `exitchange'==.
	
	count if `exitchange'!=0 & `exitchange'!=1  
	if r(N)>0 {
		noisily di as err "exitchange can take only 0/1 values"
		exit 198 
		}
	}

if "`exitdeath'"!="" & "`exitchange'"!="" {
	 count if `exitdeath'+`exitchange'==2
	 if r(N)>0 {
		noisily di as err "exitchange end exitdeath cannot be both equal to 1"
		exit 198 
		}
	}
	
if "`sectorchange'"!="" & ("`isic3'"=="" | "`isic4'"=="" | "`newindyear'"=="" )  {
	noisily di as err "if sectorchange option is selected, both isic3 and isic4 variables must be specified and also newindyear must be given"
	exit 198 
	}

* limit the sample according to if/in 

  if "`if'" ~= "" {
  keep  `if'
  }

  if "`in'" ~= "" {
  keep  `in'
  }
   
*************************************
**** PART 1: SOME DATA CLEANING  ****
*************************************

noisily display _n "Cleaning and basic definitions..."
cap cd "`outputdir'"

cap mkdir OUTPUT_TOSEND
cap cd OUTPUT_TOSEND

keep if `year'>=$minyear

keep `id' `employment' `turnover' `year' `birth' `isic3' `isic4'  `death'  `leftcensoring' `exitdeath' `exitchange'
duplicates drop

******** rename variables *******

// Store the industry variable in two variables if they are originally stored in one - create isic4 
if "`sectorchange'"!="" & "`isic3'"=="`isic4'"	 & "`isic3'"!="" {
	cap rename `isic3' isic3
	gen isic4 = isic3	
	}

foreach name in id employment turnover year birth isic3 isic4  death  leftcensoring exitdeath exitchange {
	cap rename ``name'' `name'
	}

* redefine locals for optional variables

foreach varname in turnover leftcensoring exitchange exitdeath {
	if "``varname''"!="" local `varname' `varname'
	}
	
// convert employment & turnover variable to integer and turn to missing if negative

foreach var in employment `turnover' {

	replace `var'=round(`var')
	replace `var'=. if `var'<0
	}

* replace ID variable with a standard one (from 1 to N)
egen long tmp=group(id)
drop id
rename tmp id


* drop duplicates for ID and year or give an error message if there are variables changing within ID-year
duplicates report id year
if r(unique_value)!=r(N) {
	duplicates tag id year, gen(dup)
	noisily display as text "Number of duplicated ids in the same year"
	noisily tab dup
	di as err "Error - there are duplicate observations for unit identifier and year - the program cannot continue, please check your data."
	exit 198 	
	}

// CONVERSION ISIC3/ISIC4  ********************


noi di "	Converting industry classification if needed..."

* Converting everything to 3 digit if not 3 digit yet
cap su isic4
loc max = r(max)
if `max'>5000 & `max' <10000	{	
	cap replace isic4=floor(isic4/10)	
	}

cap su isic3
loc max_isic3 = r(max)
if `max_isic3'>5000 & `max_isic3' <10000	{	
	cap replace isic3=floor(isic3/10)	
	}

if "`sectorchange'"=="" {
	
	* if all observations are in isic4 --> do mode calculation only
	if "`isic3'"=="" & "`isic4'"!=""	{
		noi di "		All observations are in isic4 --> calculating the mode"
		_ind_mode, ind_var(isic4)
		noi di "		Done"
	}
	
	* if all observations are in isic3
	if "`isic3'"!=""  & "`isic4'"=="" {
// industry conversion based on externally provided conversion table called changeover_database.dta, and to be saved in the current directory
		noi di "		All observations are in isic3 --> converting based on external converter"
		_industry_conversion, use_external_table(1) ind_old(isic3) ind_new(isic4)
		noi di "		Done"
	}
	
}
	
if "`sectorchange'"!="" { 
	noi di "		Changeover in `newindyear' from isic3 to isic4"
// industry conversion based on constructed conversion table based on the current data 		
		_industry_conversion, industry_change_year(`newindyear')	ind_old(isic3) ind_new(isic4) 
	noi di "		Done"
	}
	
drop if isic4==.
	
// creating 2-digit sector	for further analysis
cap su isic4
loc max = r(max)
if `max'>=100 & `max' <1000 {	
	gen sector2=floor(isic4/10)			
	}
if `max'>=1000 & `max' <10000	{	
	gen sector2=floor(isic4/100)
	}
if `max'>=10 & `max' <100	{	
	gen sector2=isic4
	}


// Generating macrosector classification: mfg-construction-services: NACE 2 / ISIC 4  only

gen macrosect=99
replace macrosect=1 if sector2>=10 & sector2<=33 // mfg NACE rev2
replace macrosect=2 if ((sector2>=45 & sector2<=63) | (sector2>=68 & sector2<=82)) // services NACE rev2 
replace macrosect=3 if sector2>=41 & sector2<=44   // construction NACE rev2


noi di "	Done"

noi di "Done"

compress

save temp_with_ms, replace

foreach ms in 1 2 3 99 { 
 
	use temp_with_ms if macrosect==`ms', clear
	
	if _N==0 continue
	
	* define initial and final year of the dataset
	summ year
	global inyear=r(min)
	global finyear=r(max)

	bys id: egen firstyear=min(year)


	* expand the dataset if the entry year is missing but the birthyear is within the observed period and also fill-in gaps in the data
		
	//	Checking for and tabulating gaps in the data

	sort id year
	by id : gen diff = year-year[_n-1]
	by id : replace diff = firstyear-birth+1 if birth<firstyear & _n==1
	by id : replace diff = firstyear-$inyear+1 if birth<firstyear & birth<$inyear  & _n==1
	by id : replace diff = firstyear-birth+1 if birth<firstyear & birth>=$inyear  & _n==1

	gen gap_diff = diff-1 if diff>=2 & diff!=.
		
	noisily display as text "Tabulation of gaps in the data:", _newline(2)  
	noisily tabstat gap_diff, by(year) s(count mean)
	tabout gap_diff using Dynemp_`country'_`unit'_tabgaps.txt, replace  stats(count mean)

	expand diff, gen(tagexp)

	replace tagexp=tagexp*-1

	forvalues q=$inyear/$finyear {
		bys id (year tagexp): replace year=year[_n+1]-1 if tagexp==-1
		}
		
	foreach var of varlist employment `turnover' `exitdeath' `exitchange' {
		replace `var'=. if tagexp==-1
		}
		
	drop gap_diff diff tagexp	

	* re-define firstyear

	drop firstyear
	bys id: egen firstyear=min(year)

	gen pos_emp = 1 if (employment> 0 & employment<.) 

	bys id: egen firstyear_posemp_tmp=min(year) if pos_emp==1
	bys id : egen firstyear_posemp=min(firstyear_posemp_tmp)
	drop firstyear_posemp_tmp
	 
	noi di _n "Defining entry and exit..."

	* Entry and exit
	replace birth=firstyear_posemp if firstyear_posemp>$inyear & birth==. & firstyear_posemp!=. // case 1: birth missing, replace by first year of appearance with positive employment (FYAPE)
	replace birth=firstyear_posemp if firstyear_posemp>$inyear & firstyear_posemp<birth // case 2: birth later than FYAPE, birth replaced by FYAPE 

	gen entry=(year==birth)

	drop firstyear_posemp

	* Create exit based on last year with positive employment

	bysort id pos_emp : egen max_year = max(year)
	gen exit= 1 if year == max_year & pos_emp==1 & year!=$finyear
	drop pos_emp 

	gen exityear=year if exit==1
	bys id : egen tmpexityear=min(exityear)
	replace exityear=tmpexityear 
	drop if year>exityear
	
	foreach var in tmpexityear tmpentryyear max_year exityear	{
		cap drop `var'	
		}	
	
	* redefine birth and exityear, following modifications just above

	drop firstyear 
	drop if year<birth & birth!=.
		
	* align exit for M&A and bankrupcty to the general exit

	foreach ex in `exitchange' `exitdeath' {

		bys id: egen idtag=max(`ex')
		replace `ex'=0
		replace `ex'=1 if exit==1 & idtag==1
		drop idtag
		}

	noi di "	Done"

	compress

	*** inspect data after cleaning
	noisily display as text "Inspecting the data after cleaning...", _newline(2)
	noisily describe
	noisily bys year: summ
	noisily inspect
	noisily display as text "Macrosectors included in the sample and number of observations in each sector", _newline(2)
	noisily tab macrosect
	noisily display as text "", _newline(2)
	noisily display as text "Years included in the sample and number of observations in each year", _newline(2)
	noisily tab year
	noisily display as text "", _newline(2)

	***


	***************************** 

	*  calculate yearly growth rates, absolute job/turnover variation, and Growth index - and then flag inconsistent employment for incumbents

	foreach var in employment {

		tsset id year 
		cap drop idgrowth1 
		gen double idgrowth1=(f1.`var'-`var')/(0.5*(f1.`var'+`var'))	

		* consistency checks on growth rates: qui replace employment at time t with interpolated employment if all following conditions hold (e.g. case 20-200-20)
		*1) the abs. value of 1-year growth rate at time t is >1.5
		*2) the abs. value of 1-year growth rate at time t+1 is >1.5
		*3) the two 1-year growth rates (at time t and t+1) have different sign 
		
		gen inconsist=0
		replace inconsist=1 if abs(idgrowth1)>1.5 & abs(l1.idgrowth1)>1.5 & idgrowth1*l1.idgrowth1<0 & idgrowth1!=. & l1.idgrowth1!=. & (l1.employment+employment+f1.employment)/3>=20

		* Interpolate employment
		gen varbis=`var' if inconsist==0
		bys id: ipolate  varbis year, gen (int_var)
		drop varbis	
		replace int_var=round(int_var)

		***

		tsset id year

		replace `var'=int_var if inconsist==1
		replace `var'=int_var if l1.inconsist==1

		drop int_var inconsist

		* similar procedure for entrants (e.g., 200-20-20)

		tsset id year 
		gen inconsist=0
		replace inconsist=1 if entry==1 & (f1.`var')/`var'<=0.1 &  (f2.`var'/`var')<=0.1 & employment>=20 & employment!=.
		gen f1_var=f1.`var'
		gen f2_var=f2.`var'
		egen mean_var=rowmean(f1_var f2_var) if inconsist==1 
		drop f*_var
		replace `var'=round(mean_var) if inconsist==1
		drop mean_var inconsist

		}
		
		
	* define HGFs

	tsset id year
	gen id_grtrad3=((f3.employment/employment)^(1/3))-1
	gen hgf=(id_grtrad3>0.20 & id_grtrad3!=. & employment>=10 & entry!=1)
	drop id_grtrad3

	* define 1-employee units

	bys id: egen maxemp=max(employment)
	gen oneemp=(maxemp<=1)
	drop maxemp

	* define 1-year units

	gen oneyear=(entry==1 & exit==1)
		
	**** SAVE TEMPORARY FILE WITH FULL CLEAN DATASET ****

	cap drop idgrowth1	

	noisily display _n "Saving temporary data file macrosector `ms'..."
	compress
	save dataall_ms`ms', replace
	noi di "	Done"

}
// end macrosect loop

clear 

foreach ms in 1 2 3 99 { 
	cap append using dataall_ms`ms'
	cap erase dataall_ms`ms'.dta
	}

tempfile dataall 
save "`dataall'"

erase temp_with_ms.dta

	
*****************************************
*** Transition matrices *****************
*****************************************

noisily display _n "Creating transition matrices..."

foreach tryear in $years_trans {

	foreach j in $jlist_trans {
		
		local tryear_j=`tryear'+`j'
		
		if inlist(`tryear',`yeardatalist')==0 continue // skip loop if t does not exist in the dataset
		if inlist(`tryear_j',`yeardatalist')==0 continue // skip loop if t+j does not exist in the dataset
		
		use "`dataall'" if year>=`tryear' & year <=`tryear_j', clear
		
		drop if macrosect==99
		
		if _N==0 continue
		
		_stan_a38, ind_var(sector2) 
		drop sector2
		
		tsset id year
		
		gen exit_t`j'=exit
		local k=`j'-1
		forvalues n=0/`k' {
			replace exit_t`j'=1 if f`n'.exit==1
			}
		
		replace exit_t`j'=0 if exit_t`j'==.
		
		foreach var in employment `turnover' {
		
			if "`var'"=="employment" local stub emp 
			if "`var'"=="turnover" local stub trn

			gen idgrowth`stub't1=(`var'-l1.`var')/(0.5*(`var'+l1.`var'))	
			bys id: egen volat_`stub'=sd(idgrowth`stub't1)
			drop idgrowth`stub't1
			
			bys id: egen av`stub'per=mean(`var') 
		
			}
		
		keep if year==`tryear' | year==`tryear'+`j' 		
						
		noi di "Creating transition matrix from year `tryear' to year `tryear_j'"
				
		keep id volat_* av*per employment year macrosect ind_a38 birth `leftcensoring' entry exit exit_t`j' hgf
		tsset id year
	
		gen f_employment=f`j'.employment
		
		drop if year!=`tryear'
		
		if _N==0 continue
		
		gen jobvar=f_employment-employment
		gen JC_surv=jobvar if jobvar>=0
		gen JD_surv=jobvar*(-1) if jobvar<0
		
		// gen age classes
		
		gen age=year-birth
		
		drop year
		
		replace age=0 if entry==1
		replace age=. if entry==0 & age==0
		
		if "`leftcensoring'"!="" { // if option is not empty 
			gen flagcens=(birth==leftcensoring)
			drop leftcensoring
			}
		else  {
			gen flagcens=0
			}						
		

		_ageclasses, nragecl(4) agevar(age) flagcens(flagcens)
		
		
		* gen size classes at time t and t+j, and then drop obs at time t+j	
		
		_sizeclasses, nrsizecl(61) sizevar(f_employment)
		
		rename sizeclass6 f_sizeclass6
		
		_sizeclasses, nrsizecl(61) sizevar(employment)
				
		replace f_sizeclass6=7 if exit_t`j'==1 // exit
			
		keep id volat_* av*per employment macrosect ind_a38 ageclass4 f_employment f_sizeclass6 sizeclass6 jobvar JC_surv JD_surv hgf
		
		gen idgrowthemp=(f_employment-employment)/(0.5*(employment+f_employment))	
		
		* generate transition frequencies
	
		expand 2, gen (tagexp)
		replace macrosect=9 if tagexp==1
		drop tagexp
		
		preserve
		
* 1-2. by ageclass, sizeclass, and f_sizeclass for entrants; by ageclass and sizeclass for non-entrants
		
		replace f_sizeclass6=9 if ageclass4!=0 & f_sizeclass6!=7 & f_sizeclass6!=99  // aggregated for surviving non-entrants 
		
		foreach var of varlist employment f_employment {
		
			if "`var'"=="employment" local stub emp 
			if "`var'"=="f_employment" local stub f_emp 
			
			_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4) medianvar(`var') stub(`stub') round
			
			}		
		
		// drop p99 and create weights for volatility measure
		
		foreach var in employment `turnover' {
		
			if "`var'"=="employment" local stub emp 
			if "`var'"=="turnover" local stub trn
			
			summ volat_`stub', d
			replace volat_`stub'=. if volat_`stub'>r(p99) 
						
			bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen tot`stub'W=sum(av`stub'per)
			gen W=av`stub'per/tot`stub'W
			replace volat_`stub'=volat_`stub'*W
			drop W tot`stub'W 
		
			}
		
		// gen top 10% JC var
		
		bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen p90gr=pctile(idgrowthemp), p(90)

		foreach var in JC_surv jobvar {
			gen `var'_top10=`var' if idgrowthemp>=p90gr
		}
		
		drop p90gr
	
		// stats on HGFs
		for varlist employment f_employment: gen X_hgf=X*hgf 	
		gen  idgrowthemp_hgf= idgrowthemp if hgf==1
		
		foreach var of varlist employment f_employment {
		
			if "`var'"=="employment" local stub emp 
			if "`var'"=="f_employment" local stub f_emp 
			
			_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4 hgf) medianvar(`var') stub(`stub'_hgf) round
			replace median`stub'_hgf=. if hgf==0
			
			}		
		
		***
		
		collapse (sum) volat_* totemp=employment f_totemp=f_employment JC_surv JD_surv *_top10 nrunit_hgf=hgf totemp_hgf=employment_hgf f_totemp_hgf=f_employment_hgf  ///	
			(mean) medianemp f_medianemp=medianf_emp medianemp_hgf f_medianemp_hgf=medianf_emp_hgf ///
			(count) nrunit=id (mean) meangrowth=idgrowthemp meangrowth_hgf=idgrowthemp_hgf, by (macrosect f_sizeclass6 sizeclass6 ageclass4)

		gen j=`j'
		gen year=`tryear'
		
		save tmp_trans_year`tryear'_j`j'.dta, replace
		
		restore
		
* 3. by ageclass only
		
		preserve
				
		replace sizeclass6=9 if sizeclass6!=99
		drop if sizeclass6==99
		
		if _N>0 {
		
			replace f_sizeclass6=9 if f_sizeclass6!=7 & f_sizeclass6!=99 // all survivors excl. exit and missing
						
			foreach var of varlist employment f_employment {
			
				if "`var'"=="employment" local stub emp 
				if "`var'"=="f_employment" local stub f_emp 
		
				_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4) medianvar(`var') stub(`stub') round
				
				}
			
			
			// drop p99 and create weights for volatility measure
			
			foreach var in employment `turnover' {
			
				if "`var'"=="employment" local stub emp 
				if "`var'"=="turnover" local stub trn
				
				summ volat_`stub', d
				replace volat_`stub'=. if volat_`stub'>r(p99) 
							
				bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen tot`stub'W=sum(av`stub'per)
				gen W=av`stub'per/tot`stub'W
				replace volat_`stub'=volat_`stub'*W
				drop W tot`stub'W 
			
				}
				
			// gen top 10% JC var
			
			bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen p90gr=pctile(idgrowthemp), p(90)

			foreach var in JC_surv jobvar {
				gen `var'_top10=`var' if idgrowthemp>=p90gr
			}
			
			drop p90gr
			
			// stats on HGFs
			
			for varlist employment f_employment: gen X_hgf=X*hgf 	
			gen  idgrowthemp_hgf= idgrowthemp if hgf==1
			
			foreach var of varlist employment f_employment {
			
				if "`var'"=="employment" local stub emp 
				if "`var'"=="f_employment" local stub f_emp 
				
				_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4 hgf) medianvar(`var') stub(`stub'_hgf) round
					
				replace median`stub'_hgf=. if hgf==0
				
				}		
			

			***
			
			collapse (sum) volat_* totemp=employment f_totemp=f_employment JC_surv JD_surv *_top10 nrunit_hgf=hgf totemp_hgf=employment_hgf f_totemp_hgf=f_employment_hgf  ///	
				(mean) medianemp f_medianemp=medianf_emp medianemp_hgf f_medianemp_hgf=medianf_emp_hgf ///
				(count) nrunit=id (mean) meangrowth=idgrowthemp meangrowth_hgf=idgrowthemp_hgf, by (macrosect f_sizeclass6 sizeclass6 ageclass4)
				
			gen j=`j'
			gen year=`tryear'
			save tmp_trans_year`tryear'_j`j'_TOT.dta, replace
		
			}
			// end if _N>0
		
		restore
		
		preserve
		
* 4.by sizeclass only
		
		replace ageclass4=9
		replace f_sizeclass6=9 if f_sizeclass6!=7 & f_sizeclass6!=99 // all survivors excl. exit and missing
		
		foreach var of varlist employment f_employment {

			if "`var'"=="employment" local stub emp 
			if "`var'"=="f_employment" local stub f_emp 
	
			_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4) medianvar(`var') stub(`stub') round

			}
		
		// drop p99 and create weights for volatility measure
		
		foreach var in employment `turnover' {
		
			if "`var'"=="employment" local stub emp 
			if "`var'"=="turnover" local stub trn
			
			summ volat_`stub', d
			replace volat_`stub'=. if volat_`stub'>r(p99) 
						
			bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen tot`stub'W=sum(av`stub'per)
			gen W=av`stub'per/tot`stub'W
			replace volat_`stub'=volat_`stub'*W
			drop W tot`stub'W 
		
			}

		// gen top 10% JC var
		
		bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen p90gr=pctile(idgrowthemp), p(90)

		foreach var in JC_surv jobvar {
			gen `var'_top10=`var' if idgrowthemp>=p90gr
		}
		
		drop p90gr
		
		collapse (sum) volat_* totemp=employment f_totemp=f_employment JC_surv JD_surv *_top10 (mean) medianemp f_medianemp=medianf_emp  (count) nrunit=id (mean) meangrowth=idgrowthemp, by (macrosect f_sizeclass6 sizeclass6 ageclass4)
						
		gen j=`j'
		gen year=`tryear'
		save tmp_trans_year`tryear'_j`j'_TOT2.dta, replace
		
		restore
		
		preserve
		
		
* 5.all size, entrants only
		
		
		keep if ageclass4==0
		
		if _N>0 {
		
			replace ageclass4=9
			replace f_sizeclass6=9 if f_sizeclass6!=7 & f_sizeclass6!=99 // all survivors excl. exit and missing
			
			foreach var of varlist employment f_employment {

				if "`var'"=="employment" local stub emp 
				if "`var'"=="f_employment" local stub f_emp 
		
				_median, byvar(macrosect f_sizeclass6 sizeclass6 ageclass4) medianvar(`var') stub(`stub') round

				}
			
			// drop p99 and create weights for volatility measure
			
			foreach var in employment `turnover' {
			
				if "`var'"=="employment" local stub emp 
				if "`var'"=="turnover" local stub trn
				
				summ volat_`stub', d
				replace volat_`stub'=. if volat_`stub'>r(p99) 
							
				bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen tot`stub'W=sum(av`stub'per)
				gen W=av`stub'per/tot`stub'W
				replace volat_`stub'=volat_`stub'*W
				drop W tot`stub'W 
			
				}

			// gen top 10% JC var
			
			bys macrosect f_sizeclass6 sizeclass6 ageclass4: egen p90gr=pctile(idgrowthemp), p(90)

			foreach var in JC_surv jobvar {
				gen `var'_top10=`var' if idgrowthemp>=p90gr
			}
			
			drop p90gr
			
			collapse (sum) volat_* totemp=employment f_totemp=f_employment JC_surv JD_surv *_top10 (mean) medianemp f_medianemp=medianf_emp (count) nrunit=id (mean) meangrowth=idgrowthemp, by (macrosect f_sizeclass6 sizeclass6 ageclass4)
							
			gen j=`j'
			gen year=`tryear'
			save tmp_trans_year`tryear'_j`j'_ENTRALL.dta, replace
			}
			// end if _N>0
			
		restore
	
* 6.all size, all age, ind_a38
		
		drop if macrosect==9
		
		replace ageclass4=9 if ageclass4>0 & ageclass4!=.
 		replace sizeclass6=9 if sizeclass6!=99
		drop if sizeclass6==99
		
		if _N>0 {
		
			replace f_sizeclass6=9 if f_sizeclass6!=7 & f_sizeclass6!=99 // all survivors excl. exit and missing
			drop macrosect
			
			foreach var of varlist employment f_employment {

				if "`var'"=="employment" local stub emp 
				if "`var'"=="f_employment" local stub f_emp 
		
				_median, byvar(ind_a38 f_sizeclass6 sizeclass6 ageclass4) medianvar(`var') stub(`stub') round

				}
			
			// drop p99 and create weights for volatility measure
			
			foreach var in employment `turnover' {
			
				if "`var'"=="employment" local stub emp 
				if "`var'"=="turnover" local stub trn
				
				summ volat_`stub', d
				replace volat_`stub'=. if volat_`stub'>r(p99) 
							
				bys ind_a38 f_sizeclass6 sizeclass6 ageclass4: egen tot`stub'W=sum(av`stub'per)
				gen W=av`stub'per/tot`stub'W
				replace volat_`stub'=volat_`stub'*W
				drop W tot`stub'W 
			
				}
			
			// gen top 10% JC var
			
			bys ind_a38 f_sizeclass6 sizeclass6 ageclass4: egen p90gr=pctile(idgrowthemp), p(90)

			foreach var in JC_surv jobvar {
				gen `var'_top10=`var' if idgrowthemp>=p90gr
			}
			
			drop p90gr
			
			collapse (sum) volat_* totemp=employment f_totemp=f_employment JC_surv JD_surv *_top10 (mean) medianemp f_medianemp=medianf_emp (count) nrunit=id (mean) meangrowth=idgrowthemp, by (ind_a38 f_sizeclass6 sizeclass6 ageclass4)
							
			gen j=`j'
			gen year=`tryear'
			save tmp_trans_year`tryear'_j`j'_SECT.dta, replace
			}
			// end if _N>0
		} 
		// end of j-list transition matrices
	} 
	// end of year-list transition matrices
	

	clear
	
	* append transition data files
	
	foreach tryear in $years_trans {
		foreach j in $jlist_trans {
			cap append using tmp_trans_year`tryear'_j`j'.dta
			cap erase tmp_trans_year`tryear'_j`j'.dta
			cap append using tmp_trans_year`tryear'_j`j'_TOT.dta
			cap erase tmp_trans_year`tryear'_j`j'_TOT.dta
			cap append tmp_trans_year`tryear'_j`j'_ENTRALL.dta
			cap erase tmp_trans_year`tryear'_j`j'_ENTRALL.dta
			cap append using tmp_trans_year`tryear'_j`j'_TOT2.dta
			cap erase tmp_trans_year`tryear'_j`j'_TOT2.dta
			cap append using tmp_trans_year`tryear'_j`j'_TOT3.dta
			cap erase tmp_trans_year`tryear'_j`j'_TOT3.dta
			cap append using tmp_trans_year`tryear'_j`j'_SECT.dta
			cap erase tmp_trans_year`tryear'_j`j'_SECT.dta
			
			}
		}
	
		if _N>0 {
		
			cap label drop ageclass4labels 
			label define ageclass4labels 0 "entry" 1 "1_2"  2 "3_5"  3  "6_10"  4 "11+" 9 "All" 99 "agemissing"  
			label values ageclass4 ageclass4labels
			
			cap label drop sizeclasstrlabels
			label define sizeclasstrlabels 1 "0_9"  2 "10_19" 3 "20_49"  4 "50_99" 5 "100_249" 6 "250+" 7 "exit" 9 "All"  99 "missing"
			label values sizeclass6 sizeclasstrlabels
			
			cap label drop f_sizeclasstrlabels
			label define f_sizeclasstrlabels 1 "0_9"  2 "10_19" 3 "20_49"  4 "50_99" 5 "100_249" 6 "250+" 7 "exit" 9 "All survivors"  99 "missing"
			label values f_sizeclass6 f_sizeclasstrlabels
			
			cap label drop macrosectlabels 
			label define macrosectlabels 1 "Manufacturing" 2 "Services"  3 "Construction" 9 "All"  
			label values macrosect macrosectlabels 
			
			
			noi di _n " -------------------------------------- " 
			noi di "Number of cells involving less or equal than ${mincell} observations in transition matrix over `j':"
			noi cou if nrunit<=$mincell		
			noi di " -------------------------------------- "  _n
			
					 // labels				
					cap for varlist	macrosect		:	label var X	"macro sectors"
					cap for varlist	ageclass4		:	label var X	"age class"
					cap for varlist	sizeclass6		:	label var X	"size class at time t"
					cap for varlist	f_sizeclass6	:	label var X	"size class at time t+j"
					cap for varlist	nrunit			:	label var X	"nr of units in the cell"
					cap for varlist	medianemp		:	label var X	"median employment at time t"
					cap for varlist	f_medianemp		:	label var X	"median employment at time t+j"
					cap for varlist	medianemp_hgf	:	label var X	"median employment of high-growth firms at time t"
					cap for varlist	f_medianemp_hgf :	label var X	"median employment of high-growth firms at time t+j"
					cap for varlist	totemp			:	label var X	"total employment at time t"				
					cap for varlist	f_totemp		:	label var X	"total employment at time t+j"
					cap for varlist	j				:	label var X	"j=3, 5, or 7"
					cap for varlist	year			:	label var X	"reference year"
					cap for varlist	volat_emp		:	label var X	"average (employment weighted) of unit-level employment growth volatility"
					cap for varlist	volat_trn		:	label var X	"average (turnover weighted) of unit-level turnover growth volatility"
					cap for varlist	meangrowth 		:	label var X	"average growth"
					cap for varlist	meangrowth_hgf 	:	label var X	"average growth of high-growth firms"
					cap for varlist JC_surv	: 	label var X	"gross job creation from t to t+j"
					cap for varlist JD_surv	: 	label var X	"gross job destruction from t to t+j"
					cap for varlist JC_surv_top10: 	label var X	"gross job creation from t to t+j - top 10% firms"
					cap for varlist jobvar_top10: 	label var X	"net job variation from t to t+j - top 10% firms"
					cap for varlist	nrunit_hgf		:	label var X	"nr of high-growth firms in the cell"
					cap for varlist	totemp_hgf		:	label var X	"total employment of high-growth firms at time t"				
					cap for varlist	f_totemp_hgf	:	label var X	"total employment of high-growth firms at time t+j"
					cap for varlist ind_a38			: label var X "2-digit sector STAN A38 classification"

					
			* CONFIDENTIALITY BLANKING

			if "`blank'"!="" {
				noi display "Now blanking statistics for cells containg less units than the set confidentiality threshold (equal to `conf')"
				gen blanked=(nrunit<`conf')
				label var blanked "=1 if suppressed for confidentialty (<`conf' units)"
				for varlist totem* f_tote* median* f_median* nruni* mean* volat_* JC* JD* job*: replace X=. if blanked==1
				}
			
			
			* SAVING 
		
			noi di _n "Saving dynemp_`country'_`unit'_trans_mat.dta..."	
			save dynemp_`country'_`unit'_trans_mat.dta, replace		
			if "`extraformat'"=="txt" {
				outsheet using dynemp_`country'_`unit'_trans_mat.txt, replace
			}	
			if "`extraformat'"=="csv" {
				outsheet using dynemp_`country'_`unit'_trans_mat.csv, comma replace
			}  
		
		} 
		// end if _N>0
		
		if _N==0 noisily display _n "--- WARNING: no observations in the transition matrices --- "
		
		
		
noisily display as text "	Done"

noisily display _n "--- Done with transition matrices --- "

noisily display _n _n "--- Now calculating job flows  --- "


*********************************
****** YEARLY FLOWS *************
*********************************

**** START LOOP BY YEAR AND J (in which groups are defined)


foreach ny in `yeart' { 		// loop for each year 

	if `ny'<$minyear	continue
	
	noisily display _n _n "Calculating job flow statistics for year `ny'..."
		
		
		if inlist(`ny', `yeardatalist')==1 {     // check if year=ny exists in the dataset 		 
			use "`dataall'" if  year>=`ny'-1 & year<=`ny', clear 			
					
					
			// mark unit that entry between t-1 and t

			tsset id year					
								
			gen entryin1=(entry==1 & year==`ny')
			
			// mark exits
			
			replace exit=0 if exit==.
			expand exit+1 if year==`ny'-1, gen(exitin1) 
			replace year=`ny' if exitin1==1						
			tsset id year				
			foreach var in `exitchange' `exitdeath' {
				gen `var'in1=(l1.`var'==1 & exitin1==1)
				local `var'in1 `var'in1
				}
			
					
			// mark survivors between t=0 and t-1			

			gen survin1=(exitin1==0 & entryin1==0)	
				foreach var in employment `turnover' {
				cap gen l1_`var'=l1.`var'	
				}
	
						
			// now we can keep only year=ny												
			drop if year!=`ny'
					
			tempfile datatemp_`ny'_1
			save "`datatemp_`ny'_1'"
						
			foreach vargroup in survin1 entryin1 exitin1 `exitchangein1' `exitdeathin1' {		
					
				clear															
					cap use "`datatemp_`ny'_1'" if `vargroup'==1, clear						
					cap  summ `vargroup'															
					if _rc==0 & r(sum)>0 { // if `vargroup' exist...
						gen group="`vargroup'"						


						// check if there are still obs in the dataset. If not, go ahead with the next group 						
						if _N==0 {
							continue
							}
						
					
						* generate age classes	
						
						gen age=year-birth
						
						if "`leftcensoring'"!="" { // if option is not empty 
							gen flagcens=(birth==leftcensoring)
							drop leftcensoring
							}
						else  {
							gen flagcens=0
							}
						
						
						gen agebis=age
						replace agebis=9999 if flagcens==1
						
						* generate size classes (using average of employment a t and t-1 when t-1 exists)
												
						gen m_emp=employment						
																		
						if  "`vargroup'"=="survin1" {
							replace m_emp=(employment+l1_employment)/2 
							replace m_emp=employment if m_emp==.
							}
													
						** START LOOP OVER N	
										
						foreach n in `levels' {
														
							*** gen aggregation level n							
							
						
							if (`n'==1 & "`vargroup'"!="survin1") | (`n'==1 & `ny'==$inyear) | (`n'==4 & "`vargroup'"!="survin1")  | (`n'==4 & `ny'==$inyear) | ///
								(`n'==1 & "`express'"!="") | (`n'==4 & "`express'"!="")  {
																						
								continue
								
								}
								
							preserve							

							
							if `n'==1  /* MACROSECTOR - PERCENTILES - AGE3 */ {
									
									drop if macrosect==99
									
									if _N==0 {
										restore										
										continue
										}
								
								
								// ageclasses
								
								_ageclasses, nragecl(3) agevar(age) flagcens(flagcens)
					
								// create "macrosect=all, ageclass=all" category
								expand 2, gen(tagexp)
								replace macrosect=9 if tagexp==1
								replace ageclass3=9 if tagexp==1
								summ id
								replace id=id+r(max) if tagexp==1
								drop tagexp
															
								// generate percentiles
								
								local var employment
								local stub emp
																
								gen double idgrowth`stub'=(`var'-l1_`var')/(0.5*(l1_`var'+`var'))	
								egen minabsgr=min(abs(idgrowth`stub')) if idgrowth`stub'!=0 
								egen  minabsgrtmp=min(minabsgr)
								replace minabsgr=minabsgrtmp
								drop minabsgrtmp
								gen double idgrowth`stub'2=idgrowth`stub'
								replace idgrowth`stub'2=idgrowth`stub'+uniform()*minabsgr if idgrowth`stub'==0
								drop minabsgr
								
								gen prc=.
								
								foreach nprc in 1 2 3 4 5 {
																			
									if `nprc'==1 { 	
										local pmin=0
										local pmax=10
										}
									
									if `nprc'==2 { 	
										local pmin=10
										local pmax=25
										}
										
									if `nprc'==3 { 	
										local pmin=25
										local pmax=75
										}
										
									if `nprc'==4 { 	
										local pmin=75
										local pmax=90
										}
										
									if `nprc'==5 { 	
										local pmin=90
										local pmax=100
										}
										

									so macrosect ageclass3
									
									if `pmin'==0 gen pmingr=-2
									if `pmin'!=0 by macrosect ageclass3: egen pmingr=pctile(idgrowth`stub'2),p(`pmin')
																					
									if `pmax'!=100 by macrosect ageclass3: egen pmaxgr=pctile(idgrowth`stub'2), p(`pmax')
									if `pmax'==100 gen pmaxgr=2.1 
														
									replace prc=`nprc' if idgrowth`stub'2>=pmingr & idgrowth`stub'2<pmaxgr & idgrowth`stub'2!=. & prc==.
																							
									drop pmaxgr pmingr
									}
									
									replace prc=99 if prc==.
									
									cap label drop labprc
									label define labprc 1 "0-10" 2 "11-25"  3 "26-75" 4 "76-90"  5 "91-00" 99 "missing" 
									label values prc labprc
									
									
									***
														
									if _N>1 egen level`n'=group(macrosect prc ageclass3)
									if _N==1 gen level`n'=1 /* Q: What is this for? */
								

								}
							// end of `n'==1
							
							
							if `n'==2 /* MACROSECTOR-SIZE6-AGE3 */ {
								
								drop if macrosect==99
									
									if _N==0 {
										restore										
										continue
										}
								// create "total" category
								expand 2, gen(tagexp)
								summ id
								replace id=id+r(max) if tagexp==1
								replace macrosect=9 if tagexp==1
								drop tagexp
								
								_ageclasses, agevar(age) nragecl(3) flagcens(flagcens) 
								drop age
								
								_sizeclasses, sizevar(m_emp) nrsizecl(6)
								drop m_emp
																
																
								if _N==0 {
									restore
									continue
									}
								
								if _N>1 egen level`n'=group(macrosect sizeclass6 ageclass3)
								if _N==1 gen level`n'=1
																
							}
							// end of `n'==2 
							
							if `n'==3 /* STAN-SIZE4-AGE3 */ {
								
								drop macrosect
								
								_stan_a38, ind_var(sector2) 
							
								if _N==0 {
									restore										
									continue
									}
																		
								_ageclasses, agevar(age) nragecl(3) flagcens(flagcens) 
								
								drop age
								
								_sizeclasses, sizevar(m_emp) nrsizecl(4)
								drop m_emp
																																				
																
								if _N==0 {
									restore
									continue
									}
								
								if _N>1 egen level`n'=group(ind_a38 sizeclass4 ageclass3)
								if _N==1 gen level`n'=1
																
							}
							// end of `n'==3
							
											
							if `n'==4 & "`vargroup'"=="survin1"  /* STAN-PERCENTILE */ {
								
								drop macrosect
								
								_stan_a38, ind_var(sector2) 
							
								if _N==0 {
									restore										
									continue
									}
																
								local var employment
								local stub emp
									
								// generate percentiles
									
															
								gen double idgrowth`stub'=(`var'-l1_`var')/(0.5*(l1_`var'+`var'))	
								
								egen minabsgr=min(abs(idgrowth`stub')) if idgrowth`stub'!=0
								egen  minabsgrtmp=min(minabsgr)
								replace minabsgr=minabsgrtmp
								drop minabsgrtmp
								gen double idgrowth`stub'2=idgrowth`stub'
								replace idgrowth`stub'2=idgrowth`stub'+uniform()*minabsgr if idgrowth`stub'==0
								drop minabsgr
								
								
								gen prc=.
								
								foreach nprc in 1 2 3 4 5 {
																			
									if `nprc'==1 { 	
										local pmin=0
										local pmax=10
										}
									
									if `nprc'==2 { 	
										local pmin=10
										local pmax=25
										}
										
									if `nprc'==3 { 	
										local pmin=25
										local pmax=75
										}
										
									if `nprc'==4 { 	
										local pmin=75
										local pmax=90
										}
										
									if `nprc'==5 { 	
										local pmin=90
										local pmax=100
										}
									
									so ind_a38
									
									if `pmin'==0 gen pmingr=-2
									if `pmin'!=0 by ind_a38: egen pmingr=pctile(idgrowth`stub'2),p(`pmin')
																					
									if `pmax'!=100 by ind_a38: egen pmaxgr=pctile(idgrowth`stub'2), p(`pmax')
									if `pmax'==100 gen pmaxgr=2.1 
																							
									replace prc=`nprc' if idgrowth`stub'2>=pmingr & idgrowth`stub'2<pmaxgr & idgrowth`stub'2!=. & prc==.
									
									drop pmaxgr pmingr
							
									}
									
									replace prc=99 if prc==.
									
									cap label drop labprc
									label define labprc 1 "0-10" 2 "11-25"  3 "26-75" 4 "76-90"  5 "91-00" 99 "missing" 
									label values prc labprc
									
									***
								
								if _N>1 egen level`n'=group(ind_a38 prc)
								if _N==1 gen level`n'=1
								
						
							}
							// end of `n'==4
			
							*** calculate  statistics at t=0: totals, median, etc. 									
							
							so level`n'
							by level`n': egen nrunit=sum(1)
							gen posemp=(employment>0 & employment!=.)
							by level`n': egen nrunit_posemp=sum(posemp)
							drop posemp
																			
							_median, byvar(level`n') medianvar(agebis) stub(age) round
							replace medianage=. if medianage>1000
							
							foreach var in employment `turnover' {
								if "`var'"=="employment" local stub emp 
								if "`var'"=="turnover" local stub trn
								
								_median, byvar(level`n') medianvar(`var') stub(`stub') round
																
								by level`n': egen mean`stub'=mean(`var')
								by level`n': egen tot`stub'=sum(`var')
								
								if "`var'"=="employment" { 
									bys level`n': egen emp1emp=sum(`var'*oneemp)
									bys level`n': egen nr1emp=sum(oneemp)
									}
																
								if "`var'"=="employment" & `n'!=1 & `n'!=4 { 
									bys level`n': egen emp1year=sum(`var'*oneyear)
									bys level`n': egen nr1year=sum(oneyear)
									}
																
								if "`var'"=="turnover" {
									by level`n': egen meantrnovemp=mean(turnover/employment) 
									
									gen trnovemp=turnover/employment
									_median, byvar(level`n') medianvar(trnovemp)									
									drop trnovemp
									
									by level`n': egen sdtrnovemp=sd(turnover/employment) 	
									by level`n': egen p90trnovemp=pctile(turnover/employment), p(90) 	
									by level`n': egen p10trnovemp=pctile(turnover/employment), p(10) 	
									gen p90p10trnovemp = p90trnovemp - p10trnovemp
									drop p90trnovemp p10trnovemp
									}
																								
								*** calculate statististics for incumbents only							
								
								if  "`vargroup'"=="survin1" { 	// if t+j exists and group is survivals:
																	
									if "`var'"=="employment" {
										by level`n': egen totemp_b=sum(employment) if l1_employment!=. 
										by level`n': egen nrunit_b=sum(1) if l1_employment!=. & employment!=.
										}
									
									tset id year
									
									cap gen idgrowth`stub'=(`var'-l1_`var')/(0.5*(l1_`var'+`var'))
									
									gen id_`stub'var1=`var'-l1_`var' 
																		
									cap drop pos neg
									
									gen pos=(id_`stub'var1>=0)
									gen neg=(id_`stub'var1<0)
																										
									bys level`n': egen grosscreat`stub'=sum(id_`stub'var1*pos)
									by level`n': egen grossdestr`stub'=sum(id_`stub'var1*neg*-1)
									
									_median, byvar(level`n') medianvar(idgrowth`stub') stub(growth`stub')
																	
									by level`n': egen meangrowth`stub'=mean(idgrowth`stub')
									by level`n': egen sd`stub'=sd(`var')
								
									_median, byvar(level`n') medianvar(l1_`var') stub(`stub't_1) round
								
								// statistics on HGFs
									
									count if hgf==1
									
									if r(N)>0 & `n'==2 {
									
										if "`var'"=="employment" by level`n': egen nrunit_hgf=sum(hgf) 
										if "`var'"=="employment" _median if hgf==1, byvar(level`n') medianvar(agebis) stub(age_hgf) round
										if "`var'"=="employment" replace medianage_hgf=. if medianage_hgf>1000
										by level`n': egen tot`stub'_hgf=sum(`var'*hgf)
										by level`n': egen mean`stub'_hgf=mean(`var') if hgf==1
										if "`var'"=="turnover" by level`n': egen trnovemp_hgf=mean(turnover/employment) if hgf==1
																																											
											drop idgrowth`stub' id_`stub'var1 
														
											foreach vartosp of varlist medianage_hgf totemp_hgf meanemp_hgf  {
												by level`n': egen `vartosp'tmp=mean(`vartosp')
												replace `vartosp'=`vartosp'tmp
												drop `vartosp'tmp
												}
										
											if "`var'"=="turnover" {
											
												foreach vartosp of varlist tottrn_hgf meantrn_hgf trnovemp_hgf {
													by level`n': egen `vartosp'tmp=mean(`vartosp')
													replace `vartosp'=`vartosp'tmp
													drop `vartosp'tmp
													}
												}
													
										} 
										// end if r(N)>0 & `n'==2
										
								
								
									} 
									// end if group is survivals loop
							
															
								} 
								// end loop employment/turnover 
										
											
							* spread the value all over the cell

						*** drop duplicates and collapse data									

						bys level`n': keep if _n==1

							
							drop id oneyear id* employment `turnover' `vargroup' isic* birth entry exit  l*_*  agebis level`n'   oneemp	 				
							cap drop age
							cap drop m_emp 
							cap drop l*_exit
							cap drop exit*
							cap drop surv*
							cap drop entry*
							cap drop hgf 
							cap drop flagcens
							cap drop mode
							cap drop sector2
							cap drop pos neg
								

																			
							save lev`n'`vargroup'year`ny', replace
																
							restore								
					
							} 
							// end n levels loop									
					} 
					// end if vargroup exists loop	
				} 
				// end vargroup loop					
			} 
			// end if ny+j<finyear loop						

		
			if _N>0	{	
				noi di "		Done"						
				}
			else	{	
				noi di "		No observations in the result data for year `ny'"						
				}
			
			noi di "	Appending and labelling output variables..."
	

	foreach n in `levels' { 
			
				noi display "`n'"

			
				clear			
				
					foreach vargroup in   survin1 entryin1 exitin1 `exitchangein1' `exitdeathin1' {						
						cap append using lev`n'`vargroup'year`ny'.dta
						cap erase lev`n'`vargroup'year`ny'.dta
						}
				
				if _N>0 {
				
				for varlist grosscreatemp grossdestremp: replace X=. if year==$inyear
				
				foreach exvar in exitchange exitdeath {
					replace group="`exvar'" if group=="`exvar'in1"
					}
				}				
					
	*** renaming variables with more intuitive names
	
	if _N>0 {
	
		 replace group="incumbents" if group=="survin1"
		 replace group="entering" if group=="entryin1"
		 replace group="exiting" if group=="exitin1"
		
		 cap replace grosscreatj=totemp if  group=="entering"
		 cap replace grossdestrj=totemp if  group=="exiting"
						
		noi di "	Saving year- and level- specific result dataset dynemp_`country'_`unit'_`ny'_lev`n'..."
		
		
		/* CONFIDENTIALITY BLANKING */
	
		if "`blank'"!="" {
			noi display "Now blanking statistics for cells containg less units than the set confidentiality threshold (equal to `conf')"
			gen blanked=(nrunit<`conf')
			label var blanked "=1 if suppressed for confidentialty (<`conf' units)"
			for varlist nruni* median* tot* median* grosscreat* grossdestr* emp* nr1emp  mean* sd* : replace X=. if blanked==1
			foreach var in nr1year growth1p* trn* p90p10trnovemp {
				cap replace `var'=. if blanked==1
				}
			}
		 save dynemp_`country'_`unit'_`ny'_lev`n', replace
		
		 
		 noi di "		Done"
		}
	else 	{
		noi di "		No observations in year `ny', level `n' results --> no result data saved."
		}
			
	} 
	// end the n loop
	noi di "	Done with year `ny'"		
			
} 
// end year loop


noi di _n "Appending across years"
foreach n in `levels' 	{
	clear
	noi di "	for aggregation level `n'..."
	foreach ny in `yeart'		{	
		cap append using "dynemp_`country'_`unit'_`ny'_lev`n'.dta"	
		cap erase "dynemp_`country'_`unit'_`ny'_lev`n'.dta"
		
		}
	if _N>0 {
		noi di _n " -------------------------------------- "
		noi di  "Number of cells involving less or equal than ${mincell} observations in job flows, level `n'"
		noi cou if nrunit<=$mincell		
		noi di " -------------------------------------- " _n
		
		 // labels				
				cap for varlist	macrosect		:	label var X	"broad sectors"
				cap for varlist	group			:	label var X	"group of units"
				cap for varlist	ageclass		:	label var X	"age class"
				cap for varlist sdemp 			: 	label var X	"standard dev. of employment at time t"
				cap for varlist prc 			: 	label var X	"growth percentile (employment)"
				cap for varlist	sizeclass		:	label var X	"size class"
				cap for varlist	nlevel			:	label var X	"level of aggregation"
				cap for varlist	cellid			:	label var X	"group(ageclass4"
				cap for varlist	nrunit			:	label var X	"nr of units in the cell"
				cap for varlist	nrunit_b		:	label var X	"nr of surviving units in the cell with empl. defined both at t and t-1"
				cap for varlist	medianemp		:	label var X	"median employment at time t"
				cap for varlist	meanemp			:	label var X	"average employment at time t"
				cap for varlist	totemp			:	label var X	"total employment at time t"				
				cap for varlist	totemp_b		:	label var X	"total employment at time t of units with empl. defined both at t and t-1"
				cap for varlist	totemptj		:	label var X	"total employment at time t+1"
				cap for varlist	medianemptj		:	label var X	"total employment at time t+1"
				cap for varlist	sdemptj			:	label var X	"standard dev of employment at time t+1"
				cap for varlist meangrowthtj 	:	label var X	"average growth index of units in the cell from t to t+1"	
				cap for varlist mediangrowthtj	:	label var X	"median growth index of units in the cell from t to t+1"	
				cap for varlist	grosscreatj 	:	label var X	"gross job creation from t to t+1"	
				cap for varlist	grossdestrj 	:	label var X	"gross job destruction from t to t+1"
				cap for varlist emp1emp			: 	label var X	"total employment of 1-employee units"
				cap for varlist no1emp			: 	label var X	"total number of 1-employee units"
				cap for varlist grosscreatemp	: 	label var X	"gross job creation from t-1 to t"
				cap for varlist grossdestremp	: 	label var X	"gross job destruction from t-1 to t"
				cap for varlist medianempt_1	: 	label var X	"median employment at time t-1"
				cap for varlist meangrowthp*emp	: 	"mean growth rate at the Pth percentile, employment"
				cap for varlist nrunit_hgf		:	 label var X "number of high-growth firms"
				cap for varlist medianage_hgf	:	 label var X "median age of high-growth firms"
				cap for varlist totemp_hgf		:	 label var X "total employment of high-growth firms"
				cap for varlist meanemp_hgf		:	 label var X "mean employment of high-growth firms" 
				cap for varlist grosscreat_emp_hgf	: label var X "gross job creation of high-growth firms"  
				cap for varlist grossdestr_emp_hgf	: label var X "gross job destruction of high-growth firms"  
				cap for varlist meangrowth_emp_hgf	: label var X "mean growth of high-growth firms" 
				cap for varlist mediantrn		:	 label var X "median turnover"
				cap for varlist meantrn			: 	label var X "mean turnover"
				cap for varlist tottrn			: 	label var X "total turnover"
				cap for varlist meantrnovemp	: 	label var X "mean turnover/employment ratio" 
				cap for varlist mediantrnovemp	:	label var X "median turnover/employment ratio" 
				cap for varlist sdtrnovemp		:	label var X "standard deviation of turnover/employment ratio"
				cap for varlist p90p10trnovemp	: 	label var X "difference between p90 and p10 of turnover/employment ratio"
				cap for varlist grosscreattrn	: 	label var X "gross turnover creation from t-1 to t"
				cap for varlist grossdestrtrn	:	label var X "gross turnover destruction from t-1 to t"
				cap for varlist mediantrnt_1	: 	label var X "median turnover at time t-1"
				cap for varlist tottrn_hgf		: 	label var X "total turnover of high-growth firms"
				cap for varlist meantrn_hgf		: 	label var X "mean turnover of high-growth firms"
				cap for varlist grosscreat_trn_hgf: label var X "gross turnover creation of high-growth firms"
				cap for varlist grossdestr_trn_hgf: label var X "gross turnover destruction of high-growth firms"
				cap for varlist meangrowth_trn_hgf: label var X "mean growth of turnover of high-growth firms"
				cap for varlist trnovemp_hgf	: 	label var X "turnover/employment ratio of high-growth firms"
				cap for varlist ind_a38			: label var X "2-digit sector STAN A38 classification"
				cap for varlist medianage		: label var X "median age of firms"
				cap for varlist nrunit_posemp	: label var X "number of units with positive employment"
				cap for varlist nr1emp			: label var X "number of units with employment less or equal to 1"
				cap for varlist emp1year		: label var X "employment in one-year firms"
				cap for varlist nr1year 		: label var X "number of one-year firms"
				cap for varlist mediangrowthemp : label var X "median growth of employment"
				cap for varlist meangrowthemp 	: label var X "mean growth of employment"
				cap for varlist mediangrowthtrn : label var X "median growth of turnover"
				cap for varlist meangrowthtrn 	: label var X "mean growth of turnover"
				cap for varlist sdtrn		 	: label var X "standard deviation of turnover"
				cap for varlist ageclass3	 	: label var X "ageclass"
				cap for varlist sizeclass6	 	: label var X "sizeclass"
				cap for varlist sizeclass4	 	: label var X "sizeclass"
	     	 
		
		cap label drop macrosectlabels 
		label define macrosectlabels 1 "Manufacturing" 2 "Services"  3 "Construction" 9 "All"  
		cap label values macrosect macrosectlabels
		
		cap label drop ageclass3labels
		cap label define ageclass3labels 1 "0-2" 2 "3-5" 3 "6+" 9 "All"  99 "agemissing"  
		cap label values ageclass3 ageclass3labels
			
		save "dynemp_`country'_`unit'_lev`n'.dta", replace
		
		if "`extraformat'"=="txt" {
			outsheet using "dynemp_`country'_`unit'_lev`n'.txt", replace
			noi di "	dynemp_`country'_`unit'_lev`n'.txt saved"
		}	
		
		if "`extraformat'"=="csv" {
			outsheet using "dynemp_`country'_`unit'_lev`n'.csv", comma replace
			noi di "	dynemp_`country'_`unit'_lev`n'.csv saved"
		}	
		
		noi di "	Done dynemp_`country'_`unit'_lev`n'.dta saved"
		}
	else {	
		noi di "	No observations in `country'_`unit'_lev`n'.dta"
		}
	}	
noisily display _n "--- Done with job flows --- "

***********************************
***** DISTRIBUTED REGRESSIONS *****
***********************************

if "`noreg'"==""  {
	
	
	use "`dataall'"
	
		if _N>1000 {
		
		// Cutting down the sample for the regressions if it's too large  
		local i=0
		while _N>`regsample' & `i'<8 {
			cap gen last_digit = id-floor(id/10)*10
			keep if last_digit!=`i'
			local i=`i'+1
		}
		
		cap drop last_digit
		
		if `i'>0 {
			loc dropped = (`i' + 1)*10
			noi di "`dropped' % of the sample has been dropped to easy computation"
		}
				
		noisily display _n "Now running regressions...(it may take a while)"
		
		set matsize 800
				
		if "`regyear'"!=""  {
			// adding commas
			local regyear=subinstr("`regyear'", " " ,  ", "  , 50)
			noi display "		years for which regressions are run: `regyear'"
			keep if inlist(year,`regyear')
			}
		
		tset id year

		replace exit=0 if exit==. & employment!=.
		replace exit=0 if year==$finyear & exit==1
	
		gen age=year-birth

		if "`leftcensoring'"!="" { // if option is not empty 
			gen flagcens=(birth==leftcensoring)
			drop leftcensoring
			}
		else  {
			gen flagcens=0
			}

	_ageclasses, agevar(age) nragecl(4) flagcens(flagcens)
		
		drop if ageclass4==99 | employment==.


		cap erase dynemp_`country'_`unit'_reggrowth.txt
		cap erase dynemp_`country'_`unit'_reggrowth.xml
		cap erase dynemp_`country'_`unit'_regexit.txt
		cap erase dynemp_`country'_`unit'_regexit.xml
		cap erase dynemp_`country'_`unit'_sizecont.txt
		cap erase dynemp_`country'_`unit'_sizecont.xml
		cap erase dynemp_`country'_`unit'_sizecont2.txt
		cap erase dynemp_`country'_`unit'_sizecont2.xml
		
		gen rec_dummy=(year==2008 | year==2009) // great recession dummy
		
		_hitech, ind_var(isic4) // gen hi-tech dummy
				
				
		noi display "now running growth regressions"
		

		gen idgrowthemp1=(employment-l1.employment)/(0.5*(l1.employment+employment))
		gen l1m_emp=(employment+l1.employment)*0.5
		cap	drop sizeclass6
		_sizeclasses, sizevar(l1m_emp) nrsizecl(6)


		
		cap areg idgrowthemp1 b6.sizeclass6 b$inyear.year, rob a(isic4)
		if _rc==0 {
			outreg2 using dynemp_`country'_`unit'_reggrowth, excel nocons  e(df_m r2_a)  ctitle(1)
			}
		
		cap areg idgrowthemp1 b5.ageclass4 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_reggrowth, excel nocons  e(df_m r2_a) ctitle(2)
			}
	
		cap areg idgrowthemp1 b6.sizeclass6#b5.ageclass4 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_reggrowth, excel nocons  e(df_m r2_a) ctitle(3)
			}
		
		cap areg idgrowthemp1 b6.sizeclass6#b5.ageclass4#b0.rec_dummy b$inyear.year, rob a(isic4)
		if _rc== 0 {
			outreg2 using dynemp_`country'_`unit'_reggrowth, excel nocons  e(df_m r2_a) ctitle(4)
			}
		
		cap areg idgrowthemp1 b6.sizeclass6#b5.ageclass4#b0.hitecd2 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_reggrowth, excel nocons  e(df_m r2_a) ctitle(5)
			}
		
		// regression of exit dummies
			
		noi display "now running exit regressions"
	
			
		_ageclasses, agevar(age) nragecl(11) flagcens(flagcens)
		
				
		cap areg exit b11.ageclass11 b6.sizeclass6 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_regexit, excel nocons  e(df_m r2_a) ctitle(1)
			}
		
		cap areg exit b5.ageclass4#b6.sizeclass6 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_regexit, excel nocons  e(df_m r2_a) ctitle(2)
			}
		
		cap areg exit b11.ageclass11#b0.rec_dummy b6.sizeclass6 b$inyear.year, rob a(isic4)
		if _rc == 0 {
			outreg2 using dynemp_`country'_`unit'_regexit, excel nocons  e(df_m r2_a) ctitle(3)
			}
		
		cap areg exit b11.ageclass11#b0.hitecd2 b6.sizeclass6 b$inyear.year, rob a(isic4) 
		if _rc ==0 {
			outreg2 using dynemp_`country'_`unit'_regexit, excel nocons  e(df_m r2_a) ctitle(4)
			}
		
		noi display "now running size-contigent policy regressions"
		
		// dummies size-contigent policies
		
		foreach nemp in 8 9 13 14 18 19 23 24 48 49 98 99 {
			gen demp`nemp'=(employment==`nemp')
			}
		
		cap drop  idgrowthemp1
		cap drop sizeclass6
		
		_sizeclasses, sizevar(employment) nrsizecl(6)	
						
		foreach t in 1 3 5 {

			gen idgrowthemp`t'=(f`t'.employment-employment)/(0.5*(f`t'.employment+employment))
			}
			
		loc ctitle = 0
		foreach depvar of varlist idgrowthemp* {
			loc ctitle= `ctitle'+1
			cap areg `depvar' demp* b6.sizeclass6#b5.ageclass4 b$inyear.year, rob a(isic4)
			if _rc==0 {
				outreg2 using dynemp_`country'_`unit'_sizecont, excel nocons  e(df_m r2_a) ctitle(`ctitle' `depvar')
				}
			}
		
		*** 2nd set of size cont. regressions: full sample ***
		
		clear
		use "`dataall'"
		
		tset id year
		
		gen idgrowthemp1=(employment-l1.employment)/(0.5*(l1.employment+employment))
		
		drop if employment>60 
		
		replace exit=0 if exit==. & employment!=.
		replace exit=0 if year==$finyear & exit==1
	
		gen age=year-birth

		if "`leftcensoring'"!="" { // if option is not empty 
			gen flagcens=(birth==leftcensoring)
			drop leftcensoring
			}
		else  {
			gen flagcens=0
			}

		_ageclasses, agevar(age) nragecl(4) flagcens(flagcens)
		
		drop age flagcens
		
		drop if ageclass4==99 | employment==.
				
		gen growing=(idgrowthemp1>0)
		gen stable=(idgrowthemp1==0)
		gen shrinking=(idgrowthemp1<0)
						
		collapse (mean) growing stable shrinking meangr=idgrowthemp1 (count) freq=id, by (employment ageclass4 year isic4)
		
		loc ctitle = 0		
		foreach depvar in growing stable shrinking meangr freq {
			loc ctitle = `ctitle'+1
			cap areg `depvar' b1.employment b5.ageclass4 b$inyear.year, rob a(isic4)
			if _rc == 0 {
				outreg2 using dynemp_`country'_`unit'_sizecont2, excel nocons  e(df_m r2_a) ctitle(`ctitle' `depvar')				
				}
			}
		
		} 
		// end if _N>1000
	
	
	noisily display _n " ----- Done with regressions!", _newline(2)

	
	} 
	// end if regressions
	

	
	***********************
	
clear

timer off 1
timer list 1
local t1=r(t1)
noisily display as text "", _newline(2)
noisily display as text "Seconds needed to execute the program: `t1'", _newline(2)

noi di _n _n " ****** Dynemp has run successfully! Many thanks for your collaboration! ****** " _newline(2)
	
} 
// end quietly

set varabbrev on

end

****************************************************************
******************* ADDITIONAL PROGRAMS ************************
****************************************************************

*************
** MEDIANS **
*************

capture program drop _median
program define _median

syntax [if] [in], byvar(varlist) medianvar(varname) [stub(string) round]

if "`stub'"=="" local stub `medianvar'

so `byvar' `medianvar'

forvalues n=-1(1)1 {
	local npos=`n'+1
	by `byvar' : gen med`npos'=`medianvar' if _n==round(_N/2-`n') 
	}

gen median`stub'=(med1+med0[_n+1]+med2[_n-1])/3
by `byvar': egen tmp=mean(median`stub')
replace median`stub'=tmp
drop tmp med0 med1 med2
if "`round'"!="" replace median`stub'=round(median`stub')

end

*****************
** AGE CLASSES **
*****************

cap label drop _all

capture program drop _ageclasses
program define _ageclasses
syntax, agevar(varname numeric)  flagcens(varname) [nragecl(integer 4)]

if `nragecl'==4 {

	gen ageclass4=.	
	replace ageclass4=0 if `agevar'==0
	replace ageclass4=1 if `agevar'>0 & `agevar'<=2
	replace ageclass4=2 if `agevar'>2 & `agevar'<=5
	replace ageclass4=3 if `agevar'>5 & `agevar'<=10
	replace ageclass4=4 if `agevar'>10 & `agevar'!=. 
	replace ageclass4=99 if  `agevar'==. | (`agevar'<=10 & `flagcens'==1)

	cap label define ageclass4labels 0 "entry" 1 "1_2"  2 "3_5"   3  "6_10"  4 "11+" 99 "agemissing"  
	label values ageclass4 ageclass4labels
	}

if `nragecl'==3 {

	gen ageclass3=.	
	replace ageclass3=1 if `agevar'<=2
	replace ageclass3=2 if `agevar'>=3 & `agevar' <=5
	replace ageclass3=3 if `agevar'>5 
	replace ageclass3=99 if  `agevar'==. | (`agevar'<=5 & `flagcens'==1)

	cap label define ageclass3labels 1 "0-2" 2 "3-5" 3 "6+"  99 "agemissing"  
	label values ageclass3 ageclass3labels
	}
	
if `nragecl'==2 {

	gen ageclass2=.	
	replace ageclass2=1 if `agevar'<=5
	replace ageclass2=2 if `agevar'>5 
	replace ageclass2=99 if  `agevar'==. | (`agevar'<=5 & `flagcens'==1)

	cap label define ageclass2labels 1 "<=5"  2 "6+"  99 "agemissing"  
	label values ageclass2 ageclass2labels
	}

if `nragecl'==11 {

	gen ageclass11=.	
	replace ageclass11=0 if `agevar'==0
	replace ageclass11=1 if `agevar'==1
	replace ageclass11=2 if `agevar'==2
	replace ageclass11=3 if `agevar'==3 
	replace ageclass11=4 if `agevar'==4
	replace ageclass11=5 if `agevar'==5
	replace ageclass11=6 if `agevar'==6
	replace ageclass11=7 if `agevar'==7
	replace ageclass11=8 if `agevar'==8
	replace ageclass11=9 if `agevar'==9
	replace ageclass11=10 if `agevar'>=10 & `agevar'!=.
	
	replace ageclass11=99 if  `agevar'==. | (`agevar'<=9 & `flagcens'==1)

	cap label define ageclass11labels 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10+" 99 "agemissing"    
	label values ageclass11 ageclass11labels
	}
	
	
end

******************
** SIZE CLASSES **
******************


** define program for sizeclasses **

capture program drop _sizeclasses
program define _sizeclasses
syntax,  sizevar(varname numeric) [nrsizecl(integer 6)]


if `nrsizecl'==7 {
			
	gen sizeclass6=99 if `sizevar'==.  // out
	replace sizeclass6=1 if `sizevar'<10 // 0-9
	replace sizeclass6=2 if `sizevar'>=10 & `sizevar'<20 // 10_19	
	replace sizeclass6=3 if `sizevar'>=20 & `sizevar'<50 // 20-50
	replace sizeclass6=4 if `sizevar'>=50 & `sizevar'<100 // 20-99
	replace sizeclass6=5 if `sizevar'>=100 & `sizevar'<250 // 100-249
	replace sizeclass6=6 if `sizevar'>=250 & `sizevar'<500 // 250-499
	replace sizeclass6=7 if `sizevar'>=500 & `sizevar'!=. // 500+
	
	cap label define sizeclass6labels 1 "0_9"  2 "10_19"  2 "20_49" 4 "50_99" 5 "100_249" 6 "250_499" 7 "500+" 99 "missing" 
	label values sizeclass6 sizeclass6labels
	}

if `nrsizecl'==6 {
			
	gen sizeclass6=99 if `sizevar'==.  // out
	replace sizeclass6=1 if `sizevar'<10 // 0-9
	replace sizeclass6=2 if `sizevar'>=10 & `sizevar'<50 // 10_49
	replace sizeclass6=3 if `sizevar'>=50 & `sizevar'<100 // 50-99
	replace sizeclass6=4 if `sizevar'>=100 & `sizevar'<250 // 100-249
	replace sizeclass6=5 if `sizevar'>=250 & `sizevar'<500 // 250-499
	replace sizeclass6=6 if `sizevar'>=500 & `sizevar'!=. // 500+
	
	cap label define sizeclass6labels 1 "0_9"  2 "10_49" 3 "50_99" 4 "100_249" 5 "250_499" 6 "500+" 99 "missing" 
	label values sizeclass6 sizeclass6labels
	}

if `nrsizecl'==61 {
			
	gen sizeclass6=99 if `sizevar'==.  // out
	replace sizeclass6=1 if `sizevar'<10 // 0-9
	replace sizeclass6=2 if `sizevar'>=10 & `sizevar'<20 // 10-19
	replace sizeclass6=3 if `sizevar'>=20 & `sizevar'<50 // 20-49
	replace sizeclass6=4 if `sizevar'>=50 & `sizevar'<100 // 50-99
	replace sizeclass6=5 if `sizevar'>=100 & `sizevar'<250 // 100-249
	replace sizeclass6=6 if `sizevar'>=250 & `sizevar'!=. // 250+
	
	cap label define sizeclass61labels 1 "0_9"  2 "10_19" 3 "20_49" 4 "50_99" 5 "100_249" 6 "250+" 99 "missing" 
	label values sizeclass6 sizeclass61labels
	}
	

if `nrsizecl'==4 {
			
	gen sizeclass4=99 if `sizevar'==.  // out
	replace sizeclass4=1 if `sizevar'<10 // 0-9
	replace sizeclass4=2 if `sizevar'>=10 & `sizevar'<50 // 10_49
	replace sizeclass4=3 if `sizevar'>=50 & `sizevar'<250 // 100-249
	replace sizeclass4=4 if `sizevar'>=250 & `sizevar'!=. // 250+
	
	cap label define sizeclass4labels 1 "0_9"  2 "10_49" 3 "50_249" 4 "250+" 99 "missing" 
	label values sizeclass4 sizeclass4labels
	}

if `nrsizecl'==3 {
			
	gen sizeclass3=99 if `sizevar'==.  // out
	replace sizeclass3=1 if `sizevar'<10 // 0-9
	replace sizeclass3=2 if `sizevar'>=10 & `sizevar'<20 // 10_19
	replace sizeclass3=3 if `sizevar'>=20 & `sizevar'!=. // 20+
	
	cap label define sizeclass3labels 1 "0_9"  2 "10_19" 3 "20+" 99 "missing" 
	label values sizeclass3 sizeclass3labels
	}

if `nrsizecl'==2 {
			
	gen sizeclass2=99 if `sizevar'==.  // out
	replace sizeclass2=1 if `sizevar'<10 // 0-9
	replace sizeclass2=2 if `sizevar'>=10 & `sizevar'!=. // 10+
	
	cap label define sizeclass2labels 1 "0_9"  2 "10+"  99 "missing" 
	label values sizeclass2 sizeclass2labels
	}
	
end


*************************
** INDUSTRY CONVERSION **
*************************

cap program drop _industry_conversion
program define _industry_conversion
syntax, 	[industry_change_year(integer 2008) use_external_table(integer 0) prepare_table_only(integer 0) ind_old(varname) ind_new(string)]

qui {
		
noi di _n _n 	"------------------------------"
noi di 			"Converting industry classifications"
noi di 			"------------------------------"



	
/* Options */
	gl drop_noise		1	 /* Dropping transition which are very infrequent and would create only noise */
	gl min_fraction 	0.1 /* The fraction of observations in the new classification system which is disregarded */
	
noi di "Step 0: Saving data"	
	save before_conversion, replace
	loc ind_vars_present
	foreach var in `ind_old' `ind_new' {
		cap de `var'
		if _rc==0	{
			loc ind_vars_present `ind_vars_present' `var'
		}
	}
	keep year `ind_vars_present' id 		/*ind_expected*/
	cap rename `ind_old' ind_old
	cap rename `ind_new' ind_new
	save only_conversion_vars, replace
		
if `use_external_table'==0 | "`use_external_table'"==""	| `prepare_table_only'==1 {
	
noi di "Step 1: Collapse and store changeover breakdowns (old-->new) to retrieve probabilities"

	keep if year==`industry_change_year' |  year==`industry_change_year'-1
	collapse (lastnm) ind_old (firstnm) ind_new, by(id)
	collapse (count) N_changeover_new=id, by(ind_old ind_new)	
	keep if ind_new!=. & ind_old!=.
	bysort ind_old : egen N_changeover_old= total(N_changeover_new)
	if $drop_noise == 1 {
		/* Dropping likely noise or insignificant fractions */
		drop if N_changeover_new<=2	
		gen fraction = N_changeover_new/N_changeover_old 
		bysort ind_old : egen max_fraction = max(fraction)
		drop if fraction < ${min_fraction} & max_fraction>${min_fraction} /* making sure we do not drop all the transitions */
		drop N_changeover_old
		bysort ind_old : egen N_changeover_old = sum(N_changeover_new) /* redefining the old in order to match the total */
		gen new_fraction = N_changeover_new/N_changeover_old 
		bysort ind_old : egen max_new_fraction = max(new_fraction)
	}
	
	save "${inputdata_dir}\\changeover_database.dta", replace

	
}

if `prepare_table_only'!=1	{

if `use_external_table'!=1	{		
noi di "Step 2: Convert and drive back in time for those firms which have an overlap year and indicate whether it's new or old"
		
	use id year ind_new ind_old 	/*ind_expected*/	using only_conversion_vars, clear
	
	gen 		ind_temp=ind_new 	if year>=`industry_change_year'
	gen 		new = 1 	if year>=`industry_change_year'	 /* 1 denotes new, 0 denotes 0 */
	
				loc industry_change_year_1 = `industry_change_year'-1
				su year
				loc year_min=r(min)
	
	sort id year		
	forvalues y=`industry_change_year_1'(-1)`year_min'	{		
		replace ind_temp 		= ind_temp[_n+1] 	///
			if year==`y' & id==id[_n+1] & (ind_old==ind_old[_n+1] | ind_old[_n+1]==.)		
		replace new 			=	1	///
			if year==`y' & id==id[_n+1] & ind_temp==ind_temp[_n+1] & ind_temp!=.
	}
	replace new = 0 					if new == .
	replace ind_temp = ind_old 	if ind_temp == . 


}

if `use_external_table'==1	{		
	preserve		
		insheet using "${inputdata_dir}\\changeover_database.txt", clear 
		save "${inputdata_dir}\\changeover_database.dta", replace
	restore
	gen ind_temp = ind_old
	gen new = 0
}

noi di "Step 3: Create latest mode, both in industry_var and the indicator_var"

	_ind_mode, ind_var(ind_temp)
	bys id : egen mode_new = mode(new)
	if r(N)>0	{
		replace mode_new=1 if mode_new==.
	}
	replace new = mode_new
	*order ind_expected, last
	collapse (firstnm) ind_temp new, by(id)
	save mode_done, replace


noi di "Step 4: 	If indicator of latest mode is new --> that firm is Done. Save that to industry_conversion_part1 "
	
	keep if new==1
	save industry_conversion_part1, replace	

	/*	If indicator of latest mode is old --> */
	use ind_temp id new if new==0 using mode_done, clear	
	ren ind_temp ind_old
			*a) Keep those and collapse them by firm - done already above
			*b) Create a random number by ind_old
	set seed 0
	gen random = runiform()
			*c) Sort by the random number
	sort ind_old random
			*d) bysort ind_old : gen n = _n
	bysort ind_old : gen n = _n
			*e) save 
	save industry_conversion_part2, replace
			*f) collapse (count) N_old=id, by(ind_old)
	collapse (count) N_old = id, by(ind_old)
			*g) save collapsed_to_ind_old	
	save collapsed_to_ind_old, replace


noi di "Step 5: Expand changeover database to match the sizes, by ind_old "
	
	use "${inputdata_dir}\\changeover_database.dta", clear
	merge m:1 ind_old using collapsed_to_ind_old, nogen keepusing(N_old) keep(3)
	gen N_new = round(new_fraction*N_old)
	bysort ind_old : egen N_new_check = total(N_new)
	bysort ind_old : egen min_N_changeover_new = min(new_fraction)
	bysort ind_old : egen max_N_changeover_new = max(new_fraction)
	* Decrease the minimum if total is larger than old
	replace N_new = N_new-(N_new_check-N_old) if N_new_check>N_old & new_fraction == min_N_changeover_new
	drop N_new_check
	bysort ind_old : egen N_new_check = total(N_new)
	* Increase the maximum if total is smaller than old
	replace N_new = N_new+(N_old-N_new_check) if N_new_check<N_old & new_fraction == max_N_changeover_new
	drop if N_new==0
	expand N_new
	bysort ind_old : gen n=_n
	order ind_old n N_old N_new 
				
noi di "Step 6: Merge the original data to the expanded correspondence table, by ind_old and n "
	
	cap drop _merge
	merge 1:n ind_old n using industry_conversion_part2, keepusing(id) update replace 
	drop if _merge!=3 /* making sure we drop those firms who we cannot convert */
	drop _merge
	order id n ind_old ind_new
	keep id n ind_old ind_new
	ren ind_new ind_temp

noi di "Step 7:  Append back these firms to the ones already done + merge with data"

	append using industry_conversion_part1
	ren ind_temp ind_var
	keep id ind_var
	drop if id==. 
	merge 1:n id using before_conversion.dta, nogen
	order id year `ind_old' /*ind_new ind_var ind_expected*/
	*count if ind_var!=ind_expected
	cap drop `ind_new' 
	ren ind_var `ind_new'	/* Final variable */
	sort id year
	*br if ind_var!=ind_expected

	
noi di _n "Industry classification converted."
	
noi di "Number of seconds taken:"

noi di "number of observations: "
	noi tab year
}

noi di "Step 8: Cleaning up temporary files"
foreach filename in before_conversion only_conversion_vars mode_done industry_conversion_part1 industry_conversion_part2 collapsed_to_ind_old	{
	cap erase `filename'.dta
}

noi di _n _n 	"----------------------------------------"
noi di 			"Industry classification conversion Done"
noi di 			"----------------------------------------"

}
/* end of prepare_table_only!=1 */

end

***************
** _ind_mode **
***************

cap program drop _ind_mode
program define _ind_mode
syntax, ind_var(varname numeric)

	bys id: egen mode=mode(`ind_var')	
// keep the most recent one if there are multiple modes
if r(N)>0 {
	bys id `ind_var': gen mode_candid=_N if mode==.
	bys id: egen maxmode_cand=max(mode_candid) if mode==.
	gen tag=(maxmode_cand==mode_candid) if mode==.
	bys id (tag year): gen t_finmode=`ind_var' if _n==_N & mode==.
	bys id: egen finmode=min(t_finmode) if mode==.
	
	replace mode=finmode if mode==.
	
	drop *finmode mode_candid maxmode_cand tag
	
	}	
	replace `ind_var' = mode
	drop mode

end


****************************
** HI TECH CLASSIFICATION **
****************************

// NB: based on isic4/nace2
// source: European Commission, January 2009

cap program drop _hitech
cap program define _hitech

syntax, ind_var(varname numeric)

gen hitec = 0

***Manuf high-tech***


replace hitec = 1 if `ind_var' >= 210 & `ind_var' < 220
replace hitec = 2 if `ind_var' >= 260 & `ind_var' < 270
replace hitec = 3 if `ind_var' == 303

***Manuf Medium High-tech***

replace hitec = 4 if `ind_var' >= 200 & `ind_var' < 210 
replace hitec = 5 if `ind_var' ==254
replace hitec = 6 if `ind_var' >= 270 & `ind_var' < 300
replace hitec = 7 if `ind_var' >= 340 & `ind_var' < 350
replace hitec = 8 if `ind_var' >= 300 & `ind_var' < 310 & `ind_var' != 301 & `ind_var' != 303
replace hitec = 9 if `ind_var' ==325

***Services KIS***

replace hitec = 10 if `ind_var' >= 500 & `ind_var' < 520
replace hitec = 11 if `ind_var' >= 580 & `ind_var' < 640
replace hitec = 12 if `ind_var' >= 640 & `ind_var' < 670
replace hitec = 13 if `ind_var' >= 690 & `ind_var' < 760 			
replace hitec = 14 if `ind_var' >= 780 & `ind_var' < 789 
replace hitec = 15 if `ind_var' >= 800 & `ind_var' < 810 
replace hitec = 16 if `ind_var' >= 840 & `ind_var' < 940


***Hitec1 dummy 0/1***
gen hitecd =(hitec>0) 


***Hitec2 dummy 0/1 for Manuf high-tech, KIS, HIgh-tech KIS***
gen hitecd2 =((hitec >0 & hitec <= 3) | (hitec >= 10 & hitec<= 16))

***Other method of classifying according to intensity*** 

gen ind2d=floor(`ind_var')

gen hitecd3 = inlist(ind2d, 10, 11, 20, 21, 23, 24, 26, 27, 28, 29, 30, 42, 46, 51, 52, 62)

end

*****************************
** STAN A38 CLASSIFICATION **
*****************************

/* Generating STAN A38 level of industry aggregation based on ISIC4 / NACE2 2 digit industries */
cap program drop _stan_a38
program define _stan_a38
syntax, ind_var(varname) [narrow_private_sector(integer 0)]

set more off

/* --- test --- */
/*
clear all
loc ind_var ind2
loc narrow_private_sector 0
gen ind2=1
set obs 100
forvalues i = 1/99 {
	replace ind2=`i' if _n==`i'
}
*/
/* --- end of test --- */

/* Making sure we have 2-digit ind_var */
	su `ind_var'
	loc ind_max = r(max)
	if `ind_max'>99 		& `ind_max'<999 	replace `ind_var' = floor( `ind_var' / 10 )
	if `ind_max'>999 		& `ind_max'<9998 	replace `ind_var' = floor( `ind_var' / 100 )
	
/* Excluding certain industries */
	/* drop if `ind_var'<=3 						/* Agriculture */  */
	/* drop if `ind_var'>=64 & `ind_var'<=66 	/* Financial sector */ 	*/
	drop if `ind_var'==84						/* Public sector */ 
	drop if `ind_var'>=97						/* Households, extraterritorial activities */ 
	
	if `narrow_private_sector'==1	{
		drop if `ind_var'==85 					/* Education */ 
		drop if `ind_var'==75 | ///
		(`ind_var'>=86	 & `ind_var'<=88) 	/* Healtchare */			
		drop if `ind_var'>=35 & `ind_var'<=39 	/* Utilities (electricity, water, waste) */ 	
		drop if `ind_var'>=90 & `ind_var'<=96	/* Arts, entertainment, recreation and other services */ 	
	}
	
/* Generating ind_a38 - values based on the starting 2-digit industry value of the interval */
	cap drop ind_a38
	gen ind_a38 = .	
	replace  ind_a38=1 if `ind_var'>=1 & `ind_var'<=3
	replace  ind_a38=5 if `ind_var'>=5 & `ind_var'<=9
	replace  ind_a38=10 if `ind_var'>=10 & `ind_var'<=12
	replace  ind_a38=13 if `ind_var'>=13 & `ind_var'<=15
	replace  ind_a38=16 if `ind_var'>=16 & `ind_var'<=18
	replace  ind_a38=19 if `ind_var'>=19 & `ind_var'<=19
	replace  ind_a38=20 if `ind_var'>=20 & `ind_var'<=20
	replace  ind_a38=21 if `ind_var'>=21 & `ind_var'<=21
	replace  ind_a38=22 if `ind_var'>=22 & `ind_var'<=23
	replace  ind_a38=24 if `ind_var'>=24 & `ind_var'<=25
	replace  ind_a38=26 if `ind_var'>=26 & `ind_var'<=26
	replace  ind_a38=27 if `ind_var'>=27 & `ind_var'<=27
	replace  ind_a38=28 if `ind_var'>=28 & `ind_var'<=28
	replace  ind_a38=29 if `ind_var'>=29 & `ind_var'<=30
	replace  ind_a38=31 if `ind_var'>=31 & `ind_var'<=33
	replace  ind_a38=35 if `ind_var'>=35 & `ind_var'<=35
	replace  ind_a38=36 if `ind_var'>=36 & `ind_var'<=39
	replace  ind_a38=41 if `ind_var'>=41 & `ind_var'<=43
	replace  ind_a38=45 if `ind_var'>=45 & `ind_var'<=47
	replace  ind_a38=49 if `ind_var'>=49 & `ind_var'<=53
	replace  ind_a38=55 if `ind_var'>=55 & `ind_var'<=56
	replace  ind_a38=58 if `ind_var'>=58 & `ind_var'<=60
	replace  ind_a38=61 if `ind_var'>=61 & `ind_var'<=61
	replace  ind_a38=62 if `ind_var'>=62 & `ind_var'<=63
	replace  ind_a38=64 if `ind_var'>=64 & `ind_var'<=66
	replace  ind_a38=68 if `ind_var'>=68 & `ind_var'<=68
	replace  ind_a38=69 if `ind_var'>=69 & `ind_var'<=71
	replace  ind_a38=72 if `ind_var'>=72 & `ind_var'<=72
	replace  ind_a38=73 if `ind_var'>=73 & `ind_var'<=75
	replace  ind_a38=77 if `ind_var'>=77 & `ind_var'<=82
	replace  ind_a38=84 if `ind_var'>=84 & `ind_var'<=84
	replace  ind_a38=85 if `ind_var'>=85 & `ind_var'<=85
	replace  ind_a38=86 if `ind_var'>=86 & `ind_var'<=86
	replace  ind_a38=87 if `ind_var'>=87 & `ind_var'<=88
	replace  ind_a38=90 if `ind_var'>=90 & `ind_var'<=93
	replace  ind_a38=94 if `ind_var'>=94 & `ind_var'<=96
	replace  ind_a38=97 if `ind_var'>=97 & `ind_var'<=98
	replace  ind_a38=99 if `ind_var'>=99 & `ind_var'<=99

	
	cap label drop ind_a38_labels	
	label def ind_a38_labels ///
1 "AGRICULTURE, FORESTRY AND FISHING [A]" ///
5 "Mining and quarrying [B]" ///
10 "Food products, beverages and tobacco [CA]" ///
13 "Textiles, wearing apparel, leather and related prodcuts [CB]" ///
16 "Wood and paper products, and printing [CC]" ///
19 "Coke and refined petroleum products [CD]" ///
20 "Chemicals and chemical products [CE]" ///
21 "Basic pharmaceutical products and pharmaceutical preparations [CF]" ///
22 "Rubber and plastics products, and other non-metallic mineral products [CG]" ///
24 "Basic metals and fabricated metal products, except machinery and equipment [CH]" ///
26 "Computer, electronic and optical products [CI]" ///
27 "Electrical equipment [CJ]" ///
28 "Machinery and equipment n.e.c. [CK]" ///
29 "Transport equipment [CL]" ///
31 "Furniture; other manufacturing; repair and installation of machinery and equipment [CM]" ///
35 "Electricity, gas, steam and air conditioning supply [D]" ///
36 "Water supply; sewerage, waste management and remediation activities [E]" ///
41 "CONSTRUCTION [F]" ///
45 "Wholesale and retail trade, repair of motor vehicles and motorcycles [G]" ///
49 "Transportation and storage [H]" ///
55 "Accommodation and food service activities [I]" ///
58 "Publishing, audiovisual and broadcasting activities [JA]" ///
61 "Telecommunications [JB]" ///
62 "IT and other information services [JC]" ///
64 "FINANCIAL AND INSURANCE ACTIVITIES [K]" ///
68 "REAL ESTATE ACTIVITIES [L]" ///
69 "Legal and accounting activities, etc [MA]" ///
72 "Scientific research and development [MB]" ///
73 "Advertising and market research; other professional, scientific and technical activities; veterinary activities [MC]" ///
77 "Administrative and support service activities [N]" ///
84 "Public administration and defence; compulsory social security [O]" ///
85 "Education [P]" ///
86 "Human health activities [QA]" ///
87 "Residential care and social work activities [QB]" ///
90 "Arts, entertainment and recreation [R]" ///
94 "Other service activities [S]" ///
97 "Activities of households as employers; undifferentiated activities of households for own use [T]" ///
99 "Activities of extraterritorial organizations and bodies [U]" 


	label values ind_a38 ind_a38_labels 

/* Dropping unassigned industries, if any */
	drop if ind_a38==.

end



