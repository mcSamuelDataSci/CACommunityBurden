library(dplyr)
library(stringr)
library(readxl)

# Function update: set ageGroups to NA if you want Age, but no age groups


# if (server) popPath <-  "/mnt/projects/"
# if(!server) popPath <-  "G:/"
# 
# 
# 
# securePlace    <-  paste0(highPath,"FusionData/0.Secure.Data/")  
# 
# fusionPlace    <- paste0(highPath,"FusionData/")
# standardsPlace <- paste0(highPath,"FusionData/Standards/")
# sdohPlace      <- paste0(highPath,"FusionData/SDOH/")
# 
# if (path == "mnt") {
#   myPath <- "/mnt/projects/FusionData/"
# } else {
#   myPath <- "G:/FusionData/"
# }

# source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

populationExtract <- function(County = F, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                              Race   = F,
                              Sex    = F, 
                              Age    = F,
                              Year      = 2016:2018,
                              ageGroups = "age4", # Add NULL to not include ageGroups
                              ageLabels,
                              raceLabel = "raceName", 
                              CA    = T, 
                              Total = T,
                              multiYear = F, 
                              popData = NULL, # Previously NA, now NULL since NA (all of a sudden) starting causing an error in if-else
                              server = TRUE) { 
  
  
  
  # argsList <- c(county, race, sex, age)
  # nameList <- c("countyName", raceLabel, "sex", "ageGroup")
  # groupBy_list <- append("year", nameList[which(argsList == T)])
  # dots <- lapply(groupBy_list, as.symbol)
  
  # Set working directory to differentiate between local and server pro
  
  
  
  if(is.null(popData)) {
    popRaw <- data.table::fread(paste0(fusionPlace, "Population Data/P3_Complete.csv")) %>%
      mutate(sex = str_to_title(sex)) %>%
      rename(age = agerc, population = perwt)
  } else {
    popRaw <- popData
  }
  
  raceLink <- readxl::read_xlsx(paste0(standardsPlace, "raceLink.xlsx")) %>%
    filter(!is.na(race7)) %>%
    select(raceLabel, race7)# %>%
  # rename(raceName = raceLabel)
  
  countyLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
    select(countyName, FIPSCounty) %>%
    mutate(FIPSCounty = as.numeric(paste0("6", FIPSCounty))) %>%
    rename(county = countyName)
  
  if (length(ageGroups) == 1){
    if (!is.null(ageGroups)) {
      ageLink    <- read_excel(paste0(standardsPlace, "ageLink.xlsx"),sheet=ageGroups)
      ageBreaks  <- c(-1, ageLink$uAge)  
      ageLabels  <- ageLink$ageName
    }
    
  } else {
    
    ageBreaks <- ageGroups
  }
  
  columnList <- c("year", "county", raceLabel, "ageGroup", "sex")
  dots <- lapply(columnList, as.symbol)
  
  popDenoms <- popRaw %>%
    filter(year %in% Year) %>%
    mutate(ageGroup = if(!is.null(ageGroups)) cut(age, breaks = ageBreaks, labels = ageLabels, right = TRUE) else as.character(age)) %>%
    full_join(raceLink, by = "race7") %>%
    full_join(countyLink, by = c("fips" = "FIPSCounty")) %>%
    #group_by(year, countyName, raceLabel, ageGroup, sex) %>% # what if we want raceCode?
    group_by(.dots = dots) %>% # Resolves above issue, but is there a way to call a global environment variable in group_by function?
    summarise(population = sum(population))# %>%
  #rename(Sex = sex)
  
  
  # 1) Get California
  
  not_county <- popDenoms %>%
    group_by(.dots = dots[-2]) %>% # Excludes countyName
    summarise(population = sum(population)) %>%
    mutate(county = "CALIFORNIA")
  
  new1 <- bind_rows(popDenoms, not_county)
  
  # 2) Get race total
  
  not_race <- new1 %>%
    group_by(.dots = dots[-3]) %>% # Excludes raceLabel
    summarise(population = sum(population)) %>%
    mutate({{ raceLabel }} := "Total")
  
  new2 <- bind_rows(new1, not_race)
  
  
  # Get age total
  
  not_age <- new2 %>%
    group_by(.dots = dots[-4]) %>% # Excludes ageGroup
    summarise(population = sum(population)) %>%
    mutate(ageGroup = "Total")
  
  
  new3 <- bind_rows(new2, not_age)
  
  # Get sex total
  
  not_sex <- new3 %>%
    group_by(.dots = dots[-5]) %>% # Excludes sex
    summarise(population = sum(population)) %>%
    mutate(sex = "Total")
  
  final <- bind_rows(new3, not_sex) %>%
    arrange(!!!dots)
  
  
  final_filtered <- final %>%
    filter(if (!Sex) sex=="Total" else if(Sex & !Total) sex != "Total" else sex==sex) %>%
    filter(if (!Age) ageGroup=="Total" else if(Age & !Total) ageGroup != "Total" else ageGroup==ageGroup) %>%
    filter(if (!Race) !!as.symbol(raceLabel)=="Total" else if(Race & !Total) !!as.symbol(raceLabel) != "Total" else !!as.symbol(raceLabel)==!!as.symbol(raceLabel)) %>%
    filter(if (!County) county=="CALIFORNIA" else if(County & !CA) county != "CALIFORNIA" else county==county)
  
  if (multiYear) {
    
    final_filtered <- final_filtered %>%
      group_by(county, !!as.symbol(raceLabel), ageGroup, sex) %>%
      summarise(population = sum(population)) %>%
      ungroup() %>%
      mutate(year = as.character(paste0(Year[1], "-", Year[length(Year)])))
    
  }
  

  return(final_filtered)
  
  
}

# check <- populationExtract(County = F, # all uppercase in arguments; explain the ageGroups better; raceCode;
#                            Race   = F,
#                            Sex    = T,
#                            Age    = F,
#                            Year      = 2016:2018,
#                            ageGroups = "age4", # Add NA to not include ageGroups
#                            ageLabels,
#                            raceLabel = "raceName",
#                            CA    = T,
#                            Total = T,
#                            multiYear = F,
#                            popData = NA,
#                            server = TRUE)
