# title: poverty.make.r
# purpose: datasets of poverty for CBD project (tract)
# notes:
# You can choose between 2 different ACS sets in this worksheet. Each set contains individuals below poverty level, and at-or-above poverty level.
#	table B17001 contains sex x ages: under 5,5,6-11,12-14,15,16-17,18-24,25-34,35-44,45-54,55-64,65-74,75+
#	table B17001# contains sex x ages x race: W,AA, AIAN, A, NHPI,O, B,WNH,H

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","tidycensus","readr") 
.inst <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  path and globals
.path  <- "H:/Ben's Work file/Projects/CBD/Education and Poverty"
setwd(.path)
.ckey 	<- read_file("census.api.key.txt")						                                                      # raw text file containing API key.

## 2	DATASETS	----------------------------------------------------------------------

## 2.1  ACS data: vector of variable names containing tract-level poverty frequencies by age/sex
## ACS API variables: "https://www.census.gov/data/developers/data-sets/acs-5year.2015.html"
acs.poverty.varlist <- sprintf("B17001_%03dE",c(1:59))                                                      # sex x age groupings 
#acs.poverty.varlist <- paste0("B17001",rep(c(LETTERS[1:9]),each=59),sprintf("_%03dE",c(1:59)))              # sex x race (race-nh not included) x age groupings
#ratio fpl tables (what hci is using for below 200% fpl)
#acs.poverty.varlist <- sprintf("C17002_%03dE",c(1:8))                                                       # TBD. income/poverty ratio groupings 
#acs.poverty.varlist <- sprintf("B17024_%03dE",c(1:131)))                                                    # TBD. income/poverty ratio x age groupings


## 2.2 	2015 Poverty ACS variable descriptive labels. 
## csv from API ACS 5 year 2015 data page:"https://www.census.gov/data/developers/data-sets.html"
acs.poverty.labels <- setDT (read_csv("upInfo/Pov12m_B17001_2015_labels.csv"),key="Name")
#MCS
labels2            <- read_csv("Pov12m_B17001_2015_labels.csv")



## 2.3 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
acs.poverty.tracts <- get_acs(state = 06, geography = "tract",                                              # all tracts in CA (FIPS state code = 06)
                              survey = "acs5", year = 2010,                                                 # ACS options
                              variables = acs.poverty.varlist,                                              # ACS variables requested
                              key=.ckey, moe_level=90)                                                      # Census API key and margin of error

## 2.4  Group Quarters (Prison, college, etc.) Census Tract Removal *replace with Dave's dataset later
## html with census API variables: "https://www.census.gov/data/developers/data-sets/decennial-census.html" 
dec.groupquarters.tracts <- get_decennial(state = 06, geography = "tract",year = 2010,
                                          variables = c("P0010001","P0420001"),key=.ckey)                   # extract decennial general and group quarter populations
attach(dec.groupquarters.tracts)                                                                            # attach dec.groupquarter.tracts to for quick column reference
dec.groupquarters.tracts$diff<-ave(value,GEOID,FUN=function(x) c(diff(x)))                                  # create diff by subtracting the value of every row by GEOID 
dec.groupquarters.tracts<-setDT(dec.groupquarters.tracts)[diff==0]                                          # create a DT containing only differences of 0
acs.poverty.tracts<-acs.poverty.tracts[!acs.poverty.tracts$GEOID %in%
                                             dec.groupquarters.tracts$GEOID,]                               # remove tracts which match our groupquarters tracts  
detach(dec.groupquarters.tracts)                                                                            # detach dec.groupquarter.tracts


####


##	3	ANALYSIS	----------------------------------------------------------------------

##	3.1		ACS datasets (tract)

#MCS
labelsTemp <- acs.poverty.labels


## 	3.1.1 	cleanup labels read from external file
acs.poverty.labels[, c("type","strata","description","sex","age") := tstrsplit(Label, "!!")] 		           # parse descriptive label using "!!" delimiter
#if (substr(acs.poverty.varlist[1],0,7)==paste0("B17001",c(LETTERS[1:9]))) {                               # check if labels contains age (B15001)
acs.poverty.labels[, c("trash1","race") := tstrsplit(Concept,'\\(')][,race := race][
                   ,race :=substr(race,1,nchar(race)-1)][                                                  # parse concept label using "(" delimiter for race
                   is.na(description), description := "Income in the past 12 months any poverty level"]
#acs.poverty.labels <- acs.poverty.labels[,-c("trash1")]
#}
#Michael test here
acs.poverty.labels[age=="Under 5 years", age := "0 to 4 years"][
                   age=="75 years and over", age := "75 to 999 years"][ 		                               # relabel age groups to provide upper, lower ages
                  ,c("agell","ageul"):= tstrsplit(age, " ", keep=c(1,3))][ 		                             # parse age ranges out of age label
                   is.na(ageul), ageul:= agell][, age:=NULL][ 		                                         # replace NA with the lower age, then drop full text age label
                  ,sex:=toupper(sex) ][,description:=toupper(description)][                                # capitalize sex and description
                  , Name := substr(Name,1,nchar(Name)-1)]                                                  # remove "E" suffix from label for clean merging
acs.poverty.labels <- acs.poverty.labels[,-c("trash1","Label","Concept","Required","Attributes",
                                             "Limit","Predicate Type","Group",
                                             "Valid Value","type","strata")]                               # remove unneeded elements, retain Name, sex, age, and description

## 	3.1.2 	Add age and sex labels. Merge labels and tract tables
setDT(acs.poverty.tracts) 
acs.poverty.tracts[, NAME:=NULL][, year := 2015]                                                           # drop geography label (use GEOID only),add year
names(acs.poverty.tracts)[names(acs.poverty.tracts)== "variable"] = "Name"                                 # rename variable to Name (to merge with labels)
setkey(acs.poverty.tracts,"Name")                                                                          # define Name as the key to merge 
acs.poverty.tracts <- merge(acs.poverty.tracts,acs.poverty.labels,all.x=TRUE)                              # merge ACS tract dataset with with age, sex labels
acs.poverty.tracts <- acs.poverty.tracts[,-c("Name")]                                                      # keep needed variables only

##  3.1.3   Numerator and Denominator aggregation
numerator<-acs.poverty.tracts[description!="INCOME IN THE PAST 12 MONTHS ANY POVERTY LEVEL",][
                              description!="INCOME IN THE PAST 12 MONTHS AT OR ABOVE POVERTY LEVEL",]      # create table of numerator values
b4function<-acs.poverty.tracts
#acs.poverty.tracts<-b4function
setkey(acs.poverty.tracts,agell)
setkey(numerator,agell)                                                                                    # setting key for aggregating acs.education.tract numerators
#Michael test here







NDaggr<-function(x1,x2){                                                                                   # aggregation function for numerators
  numINC<-numerator[                                                                                       # numerator aggregation including sex
                    .(c(paste(x1,sep=" "))),                                                               # select the range of values from the key column (agell)
                    .(numerator = sum(estimate),                                                           # create aggregate functions from this list, ".(", of items...
                      n_se = sum((moe/1.645)^2),
                      #n_se = sqrt(sum((moe/1.645)^2)),
                      description = "NUMERATOR",
                      agell = head(x1,1),
                      ageul = tail(x1,1)),
                    by=c("GEOID", "year", "race",  "sex")]                                                 # by these variables 
  numEXC<-numerator[                                                                                       # numerator aggregation excluding sex
                    .(c(paste(x1,sep=" "))),                                                               # select the range of values from the key column (agell)
                    .(numerator = sum(estimate),                                                           # create aggregate functions from this list, ".(", of items...
                      n_se = sum((moe/1.645)^2),
                      #n_se = sqrt(sum((moe/1.645)^2)),
                      description = "NUMERATOR",
                      agell = head(x1,1),
                      ageul = tail(x1,1),
                      sex   = NA),
                    by=c("GEOID", "year", "race")]                                                         # by these variables 
    setcolorder(numEXC,c("GEOID","year","race","sex","numerator","n_se","description","agell","ageul"))    # re-ordering numEXC for merge 
  denEXC<-acs.poverty.tracts[
                    .(c(paste(x1,sep=" "))),                                                               # select the range of values from the key column (agell)
                    .(numerator = sum(estimate),                                                           # create aggregate functions from this list, ".(", of items...
                      n_se = sum((moe/1.645)^2),
                      #n_se = sqrt(sum((moe/1.645)^2)),
                      description = x2,
                      agell = head(x1,1),
                      ageul = tail(x1,1),
                      sex   = NA),
                    by=c("GEOID", "year", "race")]
    setcolorder(denEXC,c("GEOID","year","race","sex","numerator","n_se","description","agell","ageul"))    # re-ordering denEXC for merge 
  rbindlist(list(numINC,numEXC,denEXC))
}
acs.poverty.tracts<-rbindlist(list(NDaggr(0:17 ,"INCOME IN THE PAST 12 MONTHS AGE 0-17"),
                                   NDaggr(0:999,"INCOME IN THE PAST 12 MONTHS ANY POVERTY LEVEL")))

## 	3.1.4 	Numerator and Denominator subsetting
numerator<-acs.poverty.tracts[description=="NUMERATOR",]
denominator<-acs.poverty.tracts[description!="NUMERATOR",]
names(denominator)[names(denominator)== "numerator"] = "denominator"                                      # rename variable to Name (to merge with labels)
names(denominator)[names(denominator)== "n_se"] = "d_se"                                                  # rename variable to Name (to merge with labels)
numerator   <- numerator[  ,description :=NULL]
denominator <- denominator[,sex :=NULL]

## 	3.1.5 	Numerator and denominator merging
setkey(numerator, GEOID, race, year,agell,ageul)                                                          # setting keys for merge                               
setkey(denominator, GEOID, race, year,agell,ageul)
acs.poverty.tracts<-merge(numerator,denominator,all.x=TRUE, allow.cartesian = TRUE)                       # merging numerators and denominators and keeping all denominator values
acs.poverty.tracts[, estimate := (numerator/denominator)*100][                             # add esimate
                   , se       := (1/denominator) * sqrt(n_se^2 -
                                 (numerator/denominator)^2 * d_se^2) * 100][               # add Standard error
                   ,seC       := (1/denominator) * (sqrt(n_se) -
                                 (numerator^2/denominator^2) * d_se) * 100][               # add Standard error
                   , ll_95ci  := estimate-1.96*se][                                        # add lower 95% CI 
                   , ul_95ci  := estimate+1.96*se][                                        # add lower 95% CI 
                   ll_95ci < 0, ll_95ci := 0][                                             # make CIs less than 0 into 0
                   ul_95ci > 100, ul_95ci := 100][                                         # make CIs more than 100 into 100
                   , rse      := se/estimate*100][
                   estimate==0, rse := 0]                                                  # add relative standard error
acs.poverty.tracts <- acs.poverty.tracts[,-c("n_se","d_se")]                                              # remove unneeded elements


##	4	CLEANUP		----------------------------------------------------------------------

##	4.1	export results: county, tract, MSSA level datasets
saveRDS(acs.poverty.tracts, file="povTract.rds")


# END

