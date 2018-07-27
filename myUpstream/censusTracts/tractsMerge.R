

infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\Deaths_County_Corrected.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\mssa00.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\mssa13.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\pov_2006_10.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\SVI_CDC_group_living.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
infile '\\pheecisilon00.file.cdphintra.ca.gov\exec\CCB\CensusTracts\Data_to_merge\tracts_tiger.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;



data merged;
merge deaths(in=a) HCI(in=b) group(in=c) shape(in=d) mssa2000(in=e) mssa2013(in=f);
by GEOID county;
if a then inDeaths=1;
if b then inHCI=1;
if c then inSVI=1;
if d then inShapeX=1;
if e then inCensus2000=1;
if f then inCensus2010=1; 
run;

