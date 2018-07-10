# title: education.make.r
# purpose: datasets of education for CBD project (tract)
# notes:
# You can choose between 2 different ACS tables in this worksheet (the code also includes handling for B15001):
#	table B15002 contains adults 25+ x education x sex (what HCI uses)
# table C15002A-I contains adults 25+ x education x sex and race: W,B,AIAN,A,NHPI,O,MR,W-NH,H
# The semicolon is just "do this; and then do this other thing"

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","tidycensus","readr") 
.inst <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  path and globals
.path  <- "H:/Ben's Work file/Projects/CBD/Education and Poverty"
setwd(.path)
.ckey 	<- read_file("census.api.key.txt")						                                       # Raw text file containing API key

## 2	DATASETS	----------------------------------------------------------------------

## 2.1  ACS data: variable names containing tract-level education frequencies by age and sex
## ACS API variables: "https://www.census.gov/data/developers/data-sets/acs-5year.2015.html"
acs.education.varlist <- c(sprintf("B15002_%03dE", c(1:35)))                                 # 25+ male and female x grade groupings including totals
# acs.education.varlist <- paste0("C15002",rep(c(LETTERS[1:9]),each=11),
#                               sprintf("_%03dE",c(1:11)))                                     # 25+ male and female x race x grade* (*different from B15) groupings including totals

## 2.2 	2015 Education ACS variable descriptive labels. 
## csv derived from ACS API variables html.
acs.education.labels <- setDT (read_csv("EduAttain_B15.C15_2015_labels.csv"),        
                               key="Name")                                                   # Define Name as the key for this file

## 2.3 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
acs.education.tracts <- setDT (get_acs(state = 06, geography = "tract",                      # Create ACS DT extract containing All tracts in CA (FIPS state code = 06)
                               survey = "acs5", year = 2015,                                 # ACS options
                               variables = acs.education.varlist,                            # ACS variables requested
                               key=.ckey, moe_level=90))                                     # Census API key and margin of error

## 2.4  Group Quarters (Prison, college, etc.) Census Tract Removal *replace with Dave's dataset later
## html with census API variables: "https://www.census.gov/data/developers/data-sets/decennial-census.html" 
dec.groupquarters.tracts <- get_decennial(state = 06, geography = "tract",year = 2010,
                                          variables = c("P0010001","P0420001"),key=.ckey)    # extract decennial general and group quarter populations
attach(dec.groupquarters.tracts)                                                             # attach dec.groupquarter.tracts to for quick column reference
dec.groupquarters.tracts$diff<-ave(value,GEOID,FUN=function(x) c(diff(x)))                   # create diff by subtracting the value of every row by GEOID 
dec.groupquarters.tracts<-setDT(dec.groupquarters.tracts)[diff==0]                           # create a DT containing only differences of 0
acs.education.tracts<-acs.education.tracts[!acs.education.tracts$GEOID %in%
                                             dec.groupquarters.tracts$GEOID,]                # remove tracts which match our groupquarters tracts  
detach(dec.groupquarters.tracts)                                                             # detach dec.groupquarter.tracts

##	3	ANALYSIS	----------------------------------------------------------------------

##	3.1		ACS datasets (tract)

## 	3.1.1 	Cleanup labels read from external file
if (substr(acs.education.varlist[1],0,6)=="B15001") {                                        #check if labels contains age (B15001)
  acs.education.labels[, c("type","strata","sex","age","description") 
                       := tstrsplit(Label, "!!") 		                                         # parse descriptive label using "!!" delimiter
                     ][age=="65 years and over", age := "65 to 999 years"                    # relabel age groups to provide upper, lower ages
                     ][, c("agell","ageul"):= tstrsplit(age," ", keep=c(1,3))                # parse age ranges out of age label
                     ][, age:=NULL]                                                          # drop full text age label
} else {
  acs.education.labels[, c("type","strata","sex","description") := tstrsplit(Label, "!!")    # parse descriptive label using "!!" delimiter
                     ][, c("trash1","race") := tstrsplit(Concept,'\\(') 		                 # parse concept label using "(" delimiter
                     ][,"description" := paste0("Age 25+ ",description) 		                 # create add Age 25+ to description
                     ][, race := substr(race,1,nchar(race)-1)]                               # remove "E" suffix from label for clean merging
  acs.education.labels <- acs.education.labels[,-"trash1"]
}
acs.education.labels[,sex:=toupper(sex)                                                      # capitalize sex 
                   ][,description:=toupper(description)                                      # and description for consistency
                   ][, Name := substr(Name,1,nchar(Name)-1)]                                 # remove "E" suffix from label for clean merging
acs.education.labels <- acs.education.labels[,-c("Label","Concept","Required",
                                                 "Attributes","Limit",
                                                 "Predicate Type","Group",
                                                 "Valid Value","type","strata")]             # remove unneeded elements, retain Name, sex, age, and description

## 	3.1.2 	Add age and sex labels. Merge labels and tract tables 
acs.education.tracts[, NAME:=NULL][, year := 2015]                                           # drop geography label (use GEOID only),add year
names(acs.education.tracts)[names(acs.education.tracts)== "variable"] = "Name"               # rename variable to Name (to merge with labels)
setkey(acs.education.tracts,"Name")                                                          # define Name as the key to merge 
acs.education.tracts<-merge(acs.education.tracts,                                            # merge ACS tract dataset with with age, sex labels
                            acs.education.labels, 
                            all.x=TRUE)

## 	3.1.3 	Denominator data subsetting
.denominator<-acs.education.tracts[description=="AGE 25+ NA",]                                # create table of denominator values
.denominator[,d_se := (moe/1.645)^2]                                                          # create denominator standard error
names(.denominator)[names(.denominator)== "estimate"] = "denominator"                          # rename estimate to denominator
names(.denominator)[names(.denominator)== "moe"] = "d_moe"                                     # rename moe to d_moe
.denominator <- .denominator[,-c("Name","description")]                                        # keep needed variables from denominator for merge

##  3.1.4   Numerator data subsetting
acs.education.tracts<-acs.education.tracts[description!="AGE 25+ NA",]                       # create table of numerator values
setkey(acs.education.tracts,description)                                                     # setting key for aggregating acs.education.tract groups
.numwithsex<-acs.education.tracts[.(c("AGE 25+ BACHELOR'S DEGREE",
                                      "AGE 25+ MASTER'S DEGREE",
                                      "AGE 25+ PROFESSIONAL SCHOOL DEGREE",
                                      "AGE 25+ DOCTORATE DEGREE")),
                                  .(numerator = sum(estimate),
                                  n_se = sum((moe/1.645)^2),
                                  description = "AGE 25+ FOUR-YEAR DEGREE +"),
                                  by=c("GEOID", "year", "race",  "sex")]                     # creating aggregate groups Bachelors+ w/ sex 
.numnosex<-acs.education.tracts[.(c("AGE 25+ BACHELOR'S DEGREE",
                                    "AGE 25+ MASTER'S DEGREE",
                                    "AGE 25+ PROFESSIONAL SCHOOL DEGREE",
                                    "AGE 25+ DOCTORATE DEGREE")),
                                .(numerator = sum(estimate),
                                n_se = (sum((moe/1.645)^2)),
                                description = "AGE 25+ FOUR-YEAR DEGREE +", sex = NA),
                                by=c("GEOID", "year", "race")]                               # creating aggregate groups Bachelors+ w/o sex      
setcolorder(.numnosex,c("GEOID","year","race","sex","numerator","n_se","description"))       # re-ordering numnosex for merge 
acs.education.tracts<-rbindlist(list(.numwithsex,.numnosex))                                 # merging aggregate groups


## 	3.1.5 	Numerator and denominator merging
setkey(acs.education.tracts, GEOID, sex, race, year)                                         # setting keys for merge                               
setkey(.denominator, GEOID, sex, race, year)
acs.education.tracts<-merge(acs.education.tracts,.denominator,all.x=TRUE)                     # merging numerators and denominators and keeping all denominator values
acs.education.tracts[, estimate := (numerator/denominator)*100][                             # add esimate
                     , se       := (1/denominator) * sqrt(n_se +
                                   (numerator^2/denominator^2) * d_se) * 100][               # add Standard error
                     , ll_95ci  := estimate-1.96*se][                                        # add lower 95% CI 
                     , ul_95ci  := estimate+1.96*se][                                        # add lower 95% CI 
                     ll_95ci < 0, ll_95ci := 0][                                             # make CIs less than 0 into 0
                     ul_95ci > 100, ul_95ci := 100][                                         # make CIs more than 100 into 100
                     , rse      := se/estimate*100][
                     estimate==0, rse := 0]                                                  # add relative standard error
acs.education.tracts <- acs.education.tracts[,-c("n_se","d_moe","d_se")]                     # remove unneeded elements



##	4	CLEANUP		----------------------------------------------------------------------

##	4.1	export results: county, tract, MSSA level datasets
saveRDS(acs.education.tracts, file="eduTract.rds")


# END

