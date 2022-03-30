# ===================================================================================
# Functions for Showing & Hiding sidebar Inputs.  (actual inputs are in input_widgets.R)
# Jonah Golden, October 8 2019
# ===================================================================================


# Constants =========================================================================

# List of all show/hide inputs (except 'textHomeTab', and 'textNotHomeTab', which are handled in server)
INPUTS <- c("tabHelp", "display","yearRange","level","year","sex","metric","measure","myCAUSE","myLHJ","myGeo","myYear", "myYearRank", "myYearRange", "myYearDemo", "mySex",
            "myRace", "mySexMult",
            "myLev", "myLevShort", "myBroadGroups", "myStateCut","myN", "myN_topTrends", "myMeasure","myMeasureShort","myYearGrouping","myCutSystem","myLabName","myCI","myRefLine",
            "myData", "myStrata", "mySort", "myMeasureAgeRaceFocus","myOlderFocus", "myScale", "myLiveborn",
            "myLogTrans","myMultiRace","myLifeRace","myX","myAddN","myAddRate","myAddRR","myOSHPDtype","myOSHPDtype_mdcdrg","myPosition","myprimetype","myGeoHelpText",
            "myMultiRaceHelpText", "ourDownloads", "rankCauseDownloads", "suppressionNote", "recentYearNote", "myCompare","ourOnlyPNGDownload")

# Tab to inputs mapping: Associates each tab with it's input widgets
TAB_INPUTS <- list("homeTab"=c(),
                   "aboutTab"=c(),
                   "techDocTab"=c(),
                   "otherLinksTab"=c(),
                   "urlParametersTab"=c(),
                   "lifeExpectancyTab"=c("ourDownloads","myLHJ","mySexMult","myRace","myYearGrouping","myCI"),
                   "interactiveMapTab"=c("ourDownloads","myCAUSE","myLHJ","myGeo","mySex","myStateCut","myMeasure","myCutSystem", "suppressionNote", "recentYearNote"),   # "tabHelp", -- REMOVED FROM ALL
                   "staticMapTab"=c("tabHelp", "myCAUSE", "myLHJ", "myGeo", "myMeasure", "myCutSystem", "myLabName", "mySex", "myStateCut"),
                   "rankByCauseTab"=c("ourDownloads", "myLHJ", "mySex", "myLev", "myN", "myMeasureShort", "suppressionNote", "recentYearNote"),
                   "rankByCauseAndSexTab"=c("tabHelp", "myLev", "myN", "myMeasure"),
                   "rankByGeographyTab"=c("ourDownloads","myCAUSE", "myLHJ", "mySex", "myMeasure", "myRefLine", "suppressionNote", "recentYearNote"),
                   "ageRaceFocusTab" = c("ourDownloads", "myLHJ", "myData", "myStrata","mySort", "myMeasureAgeRaceFocus", "myLiveborn","myOlderFocus","myScale", "suppressionNote"),
                   "deathHospEDTab" = c("ourDownloads", "myLHJ", "myStrata","mySort", "myMeasureAgeRaceFocus", "myLiveborn","suppressionNote"),
                   "sexTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "myMeasure", "myYearGrouping", "myLogTrans", "suppressionNote", "recentYearNote"),
                   "ageTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "myMeasure", "myLogTrans", "suppressionNote"),
                   "raceTrendTab"=c("ourDownloads","myCAUSE", "myLHJ", "myMeasure","myLogTrans", "myMultiRace", "suppressionNote"),
                   "disparitiesTab"=c("ourDownloads","myCAUSE", "myLHJ", "myCompare","myAddN","myAddRate","myAddRR","suppressionNote"),
                   "educationTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "mySex", "myMeasure", "suppressionNote"),
                   "topTrendsTab"=c("myLHJ", "myYearRank", "myYearRange", "myMeasure", "myLevShort", "myN_topTrends", "myBroadGroups", "myLogTrans", "suppressionNote"),
                   "dataTableTab"=c("myLHJ", "suppressionNote"),
                   "sdohTab"=c("myCAUSE", "myGeo", "mySex", "myMeasure", "myX", "suppressionNote"),
                   "hospitalDischargeTab"=c("ourDownloads", "myLHJ", "mySex", "myN", "myOSHPDtype","myLiveborn"),
                 # "MDC/DRGTab"=c("myLHJ", "mySex", "myN","myVar"),
                   "hospitalPrimaryAnyTab"=c("ourDownloads", "myLHJ", "myPosition"),
                 # "hospitalMapTab"=c("myCAUSE", "myLHJ", "mySex"),
                   "arrowsTab"=c("display", "level", "measure", "yearRange", "sex", "metric"),
                   "riskByCauseTab"=c("level", "year", "sex", "metric", "measure"), 
                   "demographicsTab" = c("myLHJ", "myYearDemo")
)

# Functions =========================================================================

# Self Explanatory
hideAllInputs <- function() {
  for (input in INPUTS) { hide(input) }
}

# Main function for updating input widgets ------------------------------------------


updateInputsOnTabId <- function(tabID, myGeo="", myLHJ="", myMeasure="", myMultiRace="") {
  
  hideAllInputs()
  
  # 1. Build list of desired inputs
  curr_tab_inputs = get(tabID, TAB_INPUTS)
  curr_tab_inputs = c(curr_tab_inputs,
                      updateMyYearInput(tabID, myGeo, myLHJ),
                      updateMyCiInput(tabID, myMeasure),
                      updateMyGeoHelpText(tabID, myGeo),
                      updateMyMultiRaceHelpText(tabID, myMultiRace))
  
  # 2. Show desired inputs, hide rest. (smoother transition in the UI if you show THEN hide):
  for (input in curr_tab_inputs) { show(input) }  
 #for (input in setdiff(INPUTS, curr_tab_inputs)) { hide(input) } "opposite approach"
}


# Helper functions to determine if specific INPUT, including a message, is shown based on conditionals --------------------

updateMyYearInput <- function(tabID, myGeo, myLHJ) {
  if ( (tabID %in% c("rankByCauseTab","dataTableTab")) ||
       ((tabID %in% c("interactiveMapTab","staticMapTab")) && (myGeo=="County")) ||
       ((tabID %in% c("rankByGeographyTab")) && (myLHJ=="CALIFORNIA"))
  ) { return("myYear") }
}


updateMyCiInput <- function(tabID, myMeasure) {
  if ( (tabID %in% c("rankByGeographyTab")) &&
       (myMeasure %in% c("cDeathRate", "aRate"))
  ) { return("myCI") }
}

updateMyGeoHelpText <- function(tabID, myGeo) {
  if ( (tabID %in% c("interactiveMapTab","staticMapTab","hospitalMapTab")) &&
       (myGeo=="Census Tract")
  ) { show("myGeoHelpText") }
}

updateMyMultiRaceHelpText <- function(tabID, myMultiRace) {
  if ( (tabID=="raceTrendTab") && (myMultiRace)
  ) { return("myMultiRaceHelpText") }
}



