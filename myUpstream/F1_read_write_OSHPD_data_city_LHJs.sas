 
libname pdd    'G:\FusionData\0.Secure.Data\rawOSHPD\PDD' ; 
libname ed     'G:\FusionData\0.Secure.Data\rawOSHPD\ed'  ;
run;

data pdd.pdd_2016; set pdd.cdph_pdd_rln2016; 
 year = 2016;
 keep diag_p        
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2017; set pdd.cdph_pdd_ssn2017; 
 year = 2017;
 keep diag_p        
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2018; set pdd.cdph_pdd_ssn2018; 
 year = 2018;
 keep diag_p        
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2019; set pdd.cdph_pdd_rln2019; 
 year = 2019;
 keep diag_p        
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2020; set pdd.cdph_pdd_rln2020; 
 year = 2020;
 keep diag_p odiag1-odiag24      
      ccsr_diagp  
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2021; set pdd.cdph_pdd_ssn2021; 
 year = 2021;
 keep diag_p odiag1-odiag24      
      ccsr_diagp  
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2022; set pdd.cdph_pdd_rln2022; 
 year = 2022;
 keep diag_p odiag1-odiag24      
      ccsr_diagp  
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;





*-----------------------------------------------------------------------------------------------------------------------------------------;
data pdd.pdd_2022_ecode; set pdd.cdph_pdd_rln2022; year = 2022;keep ecm1-ecm12            patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2021_ecode; set pdd.cdph_pdd_ssn2021; year = 2021;keep ecm1-ecm12            patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2020_ecode; set pdd.cdph_pdd_rln2020; year = 2020;keep ecm1-ecm12            patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2019_ecode; set pdd.cdph_pdd_rln2019; year = 2019;keep ecm1-ecm12            patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2018_ecode; set pdd.cdph_pdd_ssn2018; year = 2018;keep ecode_p ecode1-ecode4 patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2017_ecode; set pdd.cdph_pdd_ssn2017; year = 2017;keep ecode_p ecode1-ecode4 patcnty sex agyrdsch race_grp year; run;
data pdd.pdd_2016_ecode; set pdd.cdph_pdd_rln2016; year = 2016;keep ecode_p ecode1-ecode4 patcnty sex agyrdsch race_grp year; run;


*===========================================================================================================================================;

data ed.ed_2016; set ed.cdph_ed_rln2016; year = 2016;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;
data ed.ed_2017; set ed.cdph_ed_ssn2017; year = 2017;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;
data ed.ed_2018; set ed.cdph_ed_ssn2018; year = 2018;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;

data ed.ed_2019(rename =(diag_p=dx_prin ccs_diagp=ccs_dx_prin patcnty=patco disp=dispn) 
      keep = year     diag_p     ccs_diagp    patcnty       disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_rln2019; 
 year = 2019;
 run;

data ed.ed_2020(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_rln2020; 
 year = 2020;
 run;

data ed.ed_2020_city(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       oshpd_id patzip disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_rln2020; 
 year = 2020;
 run;

data ed.ed_2021(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_ssn2021; 
 year = 2021;
 run;

data ed.ed_2021_city(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       oshpd_id patzip disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_ssn2021; 
 year = 2021;
 run;
data ed.ed_2022(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_rln2022; 
 year = 2022;
 run;

 data ed.ed_2022_city(rename =(diag_p=dx_prin  patcnty=patco disp=dispn) 
      keep = year     diag_p        patcnty       oshpd_id patzip disp sex race_grp agyrserv  payer ); ; 
 set ed.cdph_ed_rln2022; 
 year = 2022;
 run;



*-----------------------------------------------------------------------------------------------------------------------------------------;
data ed.ed_2022_ecode(keep = ecm1-ecm12      year patcnty sex race_grp agyrserv); set ed.cdph_ed_rln2022; year = 2022; run;
data ed.ed_2021_ecode(keep = ecm1-ecm12      year patcnty sex race_grp agyrserv); set ed.cdph_ed_ssn2021; year = 2021; run;
data ed.ed_2020_ecode(keep = ecm1-ecm12      year patcnty sex race_grp agyrserv); set ed.cdph_ed_rln2020; year = 2020; run;
data ed.ed_2019_ecode(keep = ecm1-ecm12      year patcnty sex race_grp agyrserv); set ed.cdph_ed_rln2019; year = 2019; run;
data ed.ed_2018_ecode(keep = ec_prin ec1-ec4 year patco sex race_grp agyrserv);   set ed.cdph_ed_ssn2018; year = 2018; run;
data ed.ed_2017_ecode(keep = ec_prin ec1-ec4 year patco sex race_grp agyrserv);   set ed.cdph_ed_ssn2017; year = 2017; run;
data ed.ed_2016_ecode(keep = ec_prin ec1-ec4 year patco sex race_grp agyrserv);   set ed.cdph_ed_rln2016; year = 2016; run;


/*
data ed.ed_work; set ed.ed_2017 ed.ed_2018 ed.ed_2019; run;

/*
NOTE: for ed_2019 data ccs_diagp is 4 character wide (not sure why) versus 3 in prior years
WARNING: Multiple lengths were specified for the variable ccs_dx_prin by input data set(s). This
         can cause truncation of data.
*/


*-----------------------------------------------------------------------------------------------------------------------------------------;


data pdd.pdd_2019_ALL_diag; set pdd.cdph_pdd_rln2019; 
 year = 2019;
 keep diag_p  odiag1-odiag24      
      pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2020_ALL_diag; set pdd.cdph_pdd_rln2020; 
 year = 2020;
 keep diag_p  odiag1-odiag24      
      pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2021_ALL_diag; set pdd.cdph_pdd_ssn2021; 
 year = 2021;
 keep diag_p  odiag1-odiag24      
      pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;

data pdd.pdd_2022_ALL_diag; set pdd.cdph_pdd_rln2022; 
 year = 2022;
 keep diag_p  odiag1-odiag24      
      pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los year; 
run;


proc contents data=ed.cdph_ed_ssn2018;run;
proc freq; table ccs_dx_prin;run;
