
#dMeasuresShort <- ....[1,3,5,7,8]



lMeasuresShort   <- lMeasures[c(1,5,3,7,8)] #for ranking factors

###This works for creating a variable to be used to assign long names to short names in the function. However, if we use that same input in the dashboard, it uses the short names (the names)
#instead of the long names (the values). 


# lMeasuresCShort <- lMeasuresC[c(1,5,3,7,8)]

#For replacing values with full names in function
#names(lMeasuresCShort) <- lMeasuresShort  


#Names in dashboard dropdown

lMeasures_dashboard <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

lMeasures_dashboardShort <- lMeasures_dashboard[c(1,5,3,7,8)]













#-----------------------


lMeasures <- c("Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate", "mean.age","SMR")


#  (SOME DAY) edit measure names to standards (and similar nomenclature for confidence intervals):
#  1  YLL                     yll                   
#  2  YLLper                  yll.rate              
#  3  YLL.adj.rate  e         yll.adjusted.rate     
#  4  Ndeaths                 ndeaths               
#  5  cDeathRate              death.rate
#  6  aRate                   death.adjusted.rate
#  7  mean.age                mean.age.at.death
#  8  SMR                     SMR 

lMeasuresShort   <- lMeasures[c(1,5,3,7,8)] #for ranking factors

###This works for creating a variable to be used to assign long names to short names in the function. However, if we use that same input in the dashboard, it uses the short names (the names)
#instead of the long names (the values). 
lMeasuresC <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

lMeasuresCShort <- lMeasuresC[c(1,5,3,7,8)]

#For replacing values with full names in function
names(lMeasuresCShort) <- lMeasuresShort  


#Names in dashboard dropdown

lMeasures_dashboard <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

lMeasures_dashboardShort <- lMeasures_dashboard[c(1,5,3,7,8)]

#Given these issues, seems more efficient to define:
if(1==2){
  #For ordering factors in function
  lMeasures <- c("Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate", "mean.age","SMR") 
  lMeasuresShort   <- lMeasures[c(1,5,3,7,8)] #for ranking factors
  
  #For assigning variables long names in function:
  lMeasures_Names <- c(
    Ndeaths = "Number of deaths",
    cDeathRate = "Crude Death Rate per 100,000 population",
    aRate = "Age-Adjusted Death Rate",
    YLL = "Years of Life Lost (YLL)",
    YLLper = "YLL Rate per 100,000 population",
    YLL.adj.rate = "Age-Adjusted YLL Rate",
    mean.age = "Mean Age at Death",
    SMR ="Standard Mortality Ratio")
  
  lMeasures_NamesShort <- lMeasures_Names[c(1,5,3,7,8)]
  
  #For dashboard dropdown:
  lMeasures_dashboard <- c(
    "Number of deaths",
    "Crude Death Rate per 100,000 population",
    "Age-Adjusted Death Rate",
    "Years of Life Lost (YLL)",
    "YLL Rate per 100,000 population",
    "Age-Adjusted YLL Rate",
    "Mean Age at Death",
    "Standard Mortality Ratio")
  
  lMeasures_dashboardShort <- lMeasures_dashboard[c(1,5,3,7,8)]
}