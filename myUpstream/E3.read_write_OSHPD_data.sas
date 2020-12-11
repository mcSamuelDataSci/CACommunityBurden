 
libname pdd    'G:\CCB\0.Secure.Data\rawOSHPD\PDD' ; 
libname ed     'G:\CCB\0.Secure.Data\rawOSHPD\ed'  ;
run;


data pdd.pdd_2016; set pdd.cdph_pdd_rln2016; 
 keep diag_p        odiag1-odiag24
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los; 
run;

data pdd.pdd_2017; set pdd.cdph_pdd_ssn2017; 
 keep diag_p        odiag1-odiag24
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los; 
run;

data pdd.pdd_2018; set pdd.cdph_pdd_ssn2018; 
 keep diag_p        odiag1-odiag24
      ccs_diagP ccs_odiag1-ccs_odiag24
      mdc msdrg charge pay_cat pay_type admtyr patcnty patzip sex agyrdsch race_grp oshpd_id los_adj los; 
run;

data pdd.pdd_work1; set pdd.pdd_2016 pdd.pdd_2016 pdd.pdd_2016; run;

data pdd.pdd_work2; set pdd.pdd_work1; 
 keep admtyr patcnty patzip sex agyrdsch race_grp ccs_diagP diag_p;  run;

data ed_2016; set ed.cdph_ed_rln2016; year = 2016;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;
data ed_2017; set ed.cdph_ed_ssn2017; year = 2017;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;
data ed_2018; set ed.cdph_ed_ssn2018; year = 2018;keep year dx_prin ccs_dx_prin patco sex race_grp agyrserv dispn payer; run;


data ed.ed_work; set ed_2016 ed_2017 ed_2018; run;
