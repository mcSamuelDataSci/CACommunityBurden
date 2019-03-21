#Education, Poverty, Any Internet access
#10 steps

#1 Setting Paths, and Packages
.path       <- #"H:/Ben's Work file/Projects/CBD/Education and Poverty"
                "C:/Users/Benjamin/Desktop/work/cbd"
setwd(.path)
.packages	  <- c("tidycensus",    #load_variables, get_acs
                 "tidyr",         #separate
                 "readr",         #read_file
                 "dplyr",         #select
                 "stringr")       #str_detect
.inst       <- .packages %in% installed.packages() 
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst]) 
lapply(.packages, require, character.only=TRUE)           
.censuskey 	<- read_file("census.api.key.txt")

#2 User Input Variables
#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html
#UserLabels' regular expressions https://stat545.com/block022_regular-expression.html
#https://www.regular-expressions.info/numericranges.html
ACSYear     <- 2017
ACSSurvey   <- "acs5" # 5 year (acs5), or 1 year (acs1) data
Labels      <- load_variables(ACSYear,ACSSurvey) # view to see topics and labels
UserTopics  <-  c("total"         ,"poverty"       , "education" , "netuse")
UserLabels  <-  c("B01001_001"    ,"B17001_00[1-2]", "B15001_001", "B28002_")

#3 Creating User Selected ACS Label Files.
LabelListSubset  <- function(UT,UL){
  assign(paste0("acs.",UT),subset(Labels,grepl(UL,Labels$name)),env=.GlobalEnv)}
mapply(LabelListSubset,UserTopics,UserLabels)

#4 Pull Labels and Add as "Label" List Subsets.
Labels <- mget(ls(pattern='acs.+'))
Labels <- lapply(Labels,separate,label,sep="!!",c("TOTAL","ESTIMATE","temp1","temp2","temp3"))
Labels <- lapply(Labels,select,-TOTAL,-ESTIMATE)

#5 Pull Data and Add as "RawData" List Subsets. 
RawData <- lapply(Labels,function(subtable) {
  get_acs(state = 06, geography = "tract", survey = ACSSurvey,
          year = ACSYear, variables = subtable$name, key=.ckey, moe_level=90)})
RawData <- lapply(RawData,select,-NAME)
RawData <- lapply(RawData,rename,'name'='variable','numerator'='estimate','moe_numer'='moe')

#6 Merging "Label" and "RawData" Subsets. Write to Global.
for (subtable in 1:length(Labels)) {
assign(names(Labels[subtable]),merge(Labels[[subtable]],RawData[[subtable]],by="name",all=T))}

#7 Functions for creating denominator and denominator moe values
CountTemps<- function(dataframe){
  ncol(dataframe[,grepl("temp",names(dataframe))])}
RowLvls<- function(dataframe,lvl){
  #Returns conditional phrase (eg. !is.na(temp1) &  is.na(temp2) )
  LvlPhrase<-paste0(rep(c("!",""),times=c(lvl-1,(CountTemps(dataframe)+1)-lvl )),
                    sprintf("is.na(temp%1d)",1:CountTemps(dataframe)),collapse=" & ")
  #Evaluates phrase in an index (ie. with(dataframe,dataframe[phrase]) )
  eval(parse(text=paste0("with(",deparse(substitute(dataframe)),
                                  ",which(",LvlPhrase,"))")))}
LvlFill<- function(dataframe,lvl){  
  Textname  <- deparse(substitute(dataframe))
  ForMerge1 <- dataframe[RowLvls(dataframe,lvl),]
  ForMerge1 <- subset(ForMerge1,select=-c(numerator,moe_numer))
  ForMerge2 <- dataframe[RowLvls(dataframe,(lvl-1)),]
  Merged    <- left_join(ForMerge1, ForMerge2,
                    eval(parse(text=ifelse(lvl==2,
                    #phrase containing columns names except name and temp
                    paste0("by=c(",paste0("'",colnames(select(ForMerge1,-name,-temp1)),"'",
                                          collapse=","),")"),
                    #eg. phrase "by=c("temp2","temp3")
                    paste0("by=c(",paste0(sprintf("'temp%1d'",(lvl-2):1),collapse=","),")")
                    ))))
  dataframe$denominator[RowLvls(dataframe,lvl)]<- Merged$numerator
  dataframe$moe_denom[RowLvls(dataframe,lvl)]<- Merged$moe_numer
  assign(Textname,dataframe,envir = .GlobalEnv)}

ForMerge1<-Values[RowLvls(Values,1),]
ForMerge2 <- dataframe[RowLvls(dataframe,(lvl-1)),]
#rename formerge2 numerator columns
#https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
Merged    <- ifelse(lvl=1, left_join(ForMerge1,lvl1Set,by=GEOID),
  left_join(ForMerge1, ForMerge2,
                       eval(parse(text=ifelse(lvl==2,
                                              #phrase containing columns names except name and temp
                                              paste0("by=c(",paste0("'",colnames(select(ForMerge1,-name,-temp1)),"'",
                                                                    collapse=","),")"),
                                              #eg. phrase "by=c("temp2","temp3")
                                              paste0("by=c(",paste0(sprintf("'temp%1d'",(lvl-2):1),collapse=","),")")
                       )))))

#Denominator and MoE Creation
Values <- mget(ls(pattern='acs.+'))
Values <- lapply(Values,function(x){x$denominator <- NA; x})
Values <- lapply(Values,function(x){x$moe_denom <- NA; x})
Values <- for (i in 2:CountTemps()){
          mapply(LvlFill,Values,lvl=i)}
Values <- mapply(LvlFill,Values, function(i){i=2:CountTemps(Values)})


LvlFill(acs.poverty,2)
#Estimates and Value Creation
Values <- lapply(Values,function(x){x$estimates <- (numerator/denominator)*100})
Values <- lapply(Values,function(x){x$moe       <- with(Values,
                                    moe_ratio(numerator,denominator,moe_numer,moe_denom))})
Values <- lapply(Values,function(x){x$ll_95ci   <- with(Values,estimate-moe)})
Values <- lapply(Values,function(x){x$ul_95ci   <- with(Values,estimate+moe)})
Values <- lapply(Values,function(x){x$se        <- with(Values,moe/1.96)})
list2env(lapply(Values,`[`),envir=.GlobalEnv)

# return list items to global environment
list2env(lapply(Labels,`[`),envir=.GlobalEnv)
list2env(lapply(RawData,`[`),envir=.GlobalEnv)
list2env(lapply(Values,`[`),envir=.GlobalEnv)



LvlFill(acs.netuse,2)




#Calculate estimates, moe, ll_95ci, ul_95ci
Values$estimates <- with(Values,numerator/denominator)*100
Values$moe       <- with(Values,moe_ratio(numerator,denominator,moe_numer,moe_denom))
Values$ll_95ci   <- with(Values,estimate-moe)
Values$ul_95ci   <- with(Values,estimate+moe)
Values$se        <- with(Values,moe/1.96)
  


# creating temporary dataset to test #############
list2env(lapply(Labels,`[`),envir=.GlobalEnv)
Values<-acs.poverty
Values$numerator<-(seq(1,nrow(Values)))*2
Values$moe_numer<-(seq(1,nrow(Values)))/2
Values$denominator<-NA
Values$moe_denom<-NA


LvlFill(Values,3)


dataframe=Values
lvl=2




LvlFill<- function(dataframe,lvl){  
  Textname  <- deparse(substitute(dataframe))
  ForMerge1 <- dataframe[RowLvls(dataframe,lvl),]
  ForMerge1 <- subset(ForMerge1,select=-c(numerator,moe_numer))
  ForMerge2 <- dataframe[RowLvls(dataframe,(lvl-1)),]
  Merged    <- left_join(ForMerge1, ForMerge2,
                         eval(parse(text=ifelse(lvl==2,
                                                #phrase containing columns names except name and temp
                                                paste0("by=c(",paste0("'",colnames(select(ForMerge1,-name,-temp1)),"'",
                                                                      collapse=","),")"),
                                                #eg. phrase "by=c("temp2","temp3")
                                                paste0("by=c(",paste0(sprintf("'temp%1d'",(lvl-2):1),collapse=","),")")
                         ))))
  dataframe$denominator[RowLvls(dataframe,lvl)]<- Merged$numerator
  dataframe$moe_denom[RowLvls(dataframe,lvl)]<- Merged$moe_numer
  assign(Textname,dataframe,envir = .GlobalEnv)}




























# List Rules ####################

# return object (by string name)
Estimates[["temp2"]]
Qvsr<-`[[` #quick vector string reference function
Qvsr(Estimates,paste0("temp",i))
# return object (by name) from list
Labels[2]
# return values of object from list
Labels[[2]]
# return vector from values of object from list
Labels[[2]][1]
Labels$acs.education$name
# return object from list, then return objects name 
names(Labels[1])
# return object names from list, then return object name
names(Labels)[1]
# return object names from list
ls(Labels)
# preview list objects
glimpse(RawData)
# renaming all objects in list using regular expressions
for (i in 1:length(RawData)){
  names(RawData)[i]<-paste0("ext.",sub(".*[.]","",names(RawData)[i]))}
# conditionally renaming variables in object using apply
rename(my.data,
       sex = colnames(my.data[which(sapply(my.data, function(x) any(x=="female")))]))

my.data[which(sapply(my.data, function(x) any(x=="female")))]

grepl(".+male",my.data$strata3)
ifelse(grepl("^.+to+.+years",column),rename(column), rename(priorcolumnname))
#return row number given column
contains("10 years and over",vars=my.data$strata3)
#return column name given exact match criteria
colnames(my.data[which(sapply(my.data, function(x) any(x=="female")))])
#return column name given regular expression criteria
names(acs.education[,str_detect(acs.education,pattern="years")])
# adding column to sublist with given value
Labels <- lapply(Labels, function(x) cbind(x,title="future name"))
#levels function using eval parse that can't pull objects :-/
LvlValues <- function(level){
  if (level==1) {with(Estimates,estimate[  is.na(temp1) &  is.na(temp2) &  is.na(temp3) ])}
  else if (level==2) {with(Estimates,estimate[ !is.na(temp1) &  is.na(temp2) &  is.na(temp3) ])}
  else if (level==3) {with(Estimates,estimate[ !is.na(temp1) & !is.na(temp2) &  is.na(temp3) ])}
  else if (level==4) {with(Estimates,estimate[ !is.na(temp1) & !is.na(temp2) & !is.na(temp3) ])}}
LvlValues(4)



#Returns rows
LvlRows <- function(dataframe,level){ #returns the row #s of a given
  LvlPhrase<-paste(rep(c("!",""),times=c(level-1,(ntemps+1)-level )),
                   sprintf("is.na(temp%1d)",1:ntemps),sep="",collapse=" & ")
  eval(parse(text=paste0("with(",deparse(substitute(dataframe)),",which(",
                         LvlPhrase,"))")))
}

#Returns values
LvlRef <- function(dataframe,level){ #returns the row #s of a given
  LvlPhrase<-paste(rep(c("!",""),times=c(level-1,(ntemps+1)-level )),
                   sprintf(paste0("is.na(",deparse(substitute(dataframe)),
                                  "$temp%1d)"),1:ntemps),sep="",collapse=" & ")
  eval(parse(text=paste0("with(",deparse(substitute(dataframe)),",estimate[",LvlPhrase,"])")))}
LvlRef(Estimates,2)

#Returns subset
LvlSubset <- function(dataframe,level){ #returns the row #s of a given
  LvlPhrase<-paste(rep(c("!",""),times=c(level-1,(ntemps+1)-level )),
                   sprintf(paste0("is.na(","temp%1d)"),1:ntemps),sep="",collapse=" & ")
  eval(parse(text=paste0(deparse(substitute(dataframe)),"[with(",
                         deparse(substitute(dataframe)),",which(",
                         LvlPhrase,")),]")))}


#very helpful regular expressions guide https://rpubs.com/iPhuoc/stringr_manipulation


# return list items to global environment
list2env(lapply(Labels,`[`),envir=.GlobalEnv)
list2env(lapply(RawData,`[`),envir=.GlobalEnv)
list2env(lapply(Values,`[`),envir=.GlobalEnv)

# Junk ##########################

popStandard     <-  tDat %>% filter(year == 2015, county == "California")


strrep(paste0("is.na(temp",i,") & "),i)

substr("abcdef", 2, 4)
substring("abcdef", 1:6, 1:6)

sprintf("B01001_%03dE",c(3:26,28:49))

paste(sprintf("is.na(temp%1d) %2s",c(1:(i-1)),"&"),"is.na(temp")

noquote(paste0(rep("!",each=2),sprintf("is.na(temp%1d) %2s",c(1:3),"&")))

rep("!",each=2)

sprintf("is.na(temp%1d)%2s is.na(temp%3d)",c(1:(i-1)),"&",i)

vars <- c("SR", "PL")
vis <- c(1,2,3)
paste(rep(vars, each = length(c(1,2))), vis, sep = ".")

paste(rep("-",each=length(c(1,2))),vis)

rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,3))     # same as second.

rep(a,each=3)

names(Estimates2[,!(names(Estimates2)) %in% c("estimate","temp2")])
Estimates[,!(names(Estimates)) %in% c("temp1")]
paste(names(acs.education[,!(names(acs.education)) %in% c("estimate","temp1")]),sep=",")



ntemps   <-ncol(Values[,grepl("temp",names(Values))])

RowLvls <- function(dataframe,level){ #returns the row #s of a given level
  LvlPhrase<-paste0(rep(c("!",""),times=c(level-1,(ntemps+1)-level )),
                    sprintf("is.na(temp%1d)",1:ntemps),collapse=" & ")
  eval(parse(text=paste0("with(",deparse(substitute(dataframe)),",which(",
                         LvlPhrase,"))")))}




check<-left_join(Values[RowLvls(Values,lvl),] , Values[RowLvls(Values,(lvl-1)),],
                 eval(parse(text=ifelse(lvl==2,"","by='temp0'"))))
#exlude (lvl-1)and up
Values$denominator[RowLvls(Values,4)] <-check$numerator.y



# in progress ########################

