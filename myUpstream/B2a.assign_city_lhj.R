# Documentation =================================================================================

# About:
# - 2nd part of B2.process_California_raw_death_data.R
# - The script assigns city LHJs (Berkeley, Long Beach, Pasadena) and 2 HDs (Alameda HD, Los Angeles HD)

# Notes:
# - Assignment of city LHJs is not automated.
# - Only looking at cases where county is Alameda or LA
#   - Cases where city is one of three city LHJs, but corresponding county is not Alameda or LA. These cases are not addressed


# Load standards ============================================================================
server <- TRUE
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Read in processed death data ==============================================================
cbdDat0 <- readRDS(paste0(securePlace, "myData/ccb_processed_deaths.RDS"))

cbdDat0_00_04 <- cbdDat0 %>% 
  filter(year %in% 2000:2004)

cbdDat0_05 <- cbdDat0 %>% 
  filter(year > 2004)

# LHJ Details ===============================================================================
# lhj names: berkeley, pasadena, longbeach
# lhj codes: 06000, 56000, 43000
berkeley <- "berkeley"
berkeleyCode <- "06000"

lb <- "longbeach"
lbCode <- "43000"
lbCities <- c("belmontshore", "bixbyknolls", "naples", "northlongbeach") # Identified these cities that are in long beach.. Records with these cities will be assigned as Long Beach

pasadena <- "pasadena"
pasadenaCode <- "56000"

lhjCounty <- c("Alameda", "Los Angeles")

# LHJ Assignment =============================================================================

## Filter on Alameda and Los Angeles County. Process cityText field ---------------------------

datAlamedaLA <- cbdDat0_05 %>% 
  filter(county %in% lhjCounty) %>% 
  mutate(cityText = str_to_lower(cityText), # Make all city text lower case
         cityText = gsub("[[:space:]]", "", cityText), # Remove all white spaces
         cityText = ifelse(cityText %in% lbCities, lb, cityText) # Change city text in lbCities to 'longbeach'
         )

# Check NA values
colSums(is.na(datAlamedaLA))

datAlameda <- datAlamedaLA %>% 
  filter(county == "Alameda")

datLA <- datAlamedaLA %>% 
  filter(county == "Los Angeles")


## Alameda County ==========================================================

### Assign city LHJ: Berkeley or Alameda HD

# 1. If (cityText == "berkeley") & (cityCode == "06000") -> "Berkeley"
# 2. If (cityText == "berkeley") & (cityCode != "06000") -> "Berkeley"
#   - Only 25 cases. All correspond to cityCodes of "00000" or "99999". All seem to be in Berkeley after inspecting residence addresses
# 3. If (cityText != "berkeley") & (cityCode == "06000") -> NO SUCH CASES
# 4. ALL OTHER CASES: If (cityText != "berkeley) & (cityCode != "06000") -> "Alameda HD"

# 1.
datAlameda_cityCode <- datAlameda %>% 
  filter(
    ((cityText == berkeley) & (cityCode == berkeleyCode))
  ) %>% 
  mutate(city_lhj = "Berkeley")


# 2.
datAlameda_city_notCode <- datAlameda %>% 
  filter(
    ((cityText == berkeley) & (cityCode != berkeleyCode))
  ) %>% 
  mutate(city_lhj = "Berkeley")

# 3. 
datAlameda_code_notCity <- datAlameda %>% 
  filter(
    ((cityText != berkeley) & (cityCode == berkeleyCode))
  )

datAlameda_notCode_notCity <- datAlameda %>% 
  filter(
    ((cityText != berkeley) & (cityCode != berkeleyCode))
  ) %>% 
  mutate(city_lhj = "Alameda HD")

# Data Quality Checks
(nrow(datAlameda_cityCode) + nrow(datAlameda_city_notCode) + nrow(datAlameda_code_notCity) + nrow(datAlameda_notCode_notCity)) == nrow(datAlameda)

# Bind rows
datAlameda_final <- bind_rows(datAlameda_cityCode, datAlameda_city_notCode, datAlameda_code_notCity, datAlameda_notCode_notCity)
table(datAlameda_final$city_lhj, useNA = "ifany")


## Los Angeles County - Long Beach ==========================================================

### Assign city LHJ: Long Beach or Los Angeles HD

# 1. If (cityText == "longbeach") & (cityCode == "43000") -> "Long Beach"
# 2. If (cityText == "longbeach") & (cityCode != "43000") -> "Long Beach"
#   - 123 cases. All correspond to cityCodes of "00000" or "99999". All seem to be in Long Beach after inspecting residence addresses
# 3. If (cityText != "longbeach") & (cityCode == "43000") -> "Los Angeles HD"
#   - Only 1 case. Does not seem to be in Long Beach

# 1.
datLALB_cityCode <- datLA %>% 
  filter(
    ((cityText == lb) & (cityCode == lbCode))
  ) %>% 
  mutate(city_lhj = "Long Beach")


# 2.
datLALB_city_notCode <- datLA %>% 
  filter(
    ((cityText == lb) & (cityCode != lbCode))
  ) %>% 
  mutate(city_lhj = "Long Beach")

# 3. 
datLALB_code_notCity <- datLA %>% 
  filter(
    ((cityText != lb) & (cityCode == lbCode))
  ) %>% 
  mutate(city_lhj = "Los Angeles HD")

datLALB_final <- bind_rows(datLALB_cityCode, datLALB_city_notCode, datLALB_code_notCity)

## Los Angeles County - Pasadena ==========================================================

### Assign city LHJ: Pasadena or Los Angeles HD

# 1. If (cityText == "pasadena") & (cityCode == "56000") -> "Pasadena"
# 2. If (cityText == "pasadena") & (cityCode != "56000") -> "Pasadena"
#   - Only 72 cases. All correspond to cityCodes of "00000" or "99999" except for one ("40040"). All seem to be in Pasadena after inspecting residence addresses
# 3. If (cityText != "pasadena") & (cityCode == "56000") -> "Los Angeles HD"
#   - Only 8 cases. All are "raymond" which is not in Pasadena.
#       - Note: "raymond" is mostly in Madera, and some Mariposa.. But these 8 cases are assigned LA.. Leave unchanged for now

# 1.
datLAPasadena_cityCode <- datLA %>% 
  filter(
    ((cityText == pasadena) & (cityCode == pasadenaCode))
  ) %>% 
  mutate(city_lhj = "Pasadena")


# 2.
datLAPasadena_city_notCode <- datLA %>% 
  filter(
    ((cityText == pasadena) & (cityCode != pasadenaCode))
  ) %>% 
  mutate(city_lhj = "Pasadena")

# 3. 
datLAPasadena_code_notCity <- datLA %>% 
  filter(
    ((cityText != pasadena) & (cityCode == pasadenaCode))
  ) %>% 
  mutate(city_lhj = "Los Angeles HD")

# Bind rows
datLAPasadena_final <- bind_rows(datLAPasadena_cityCode, datLAPasadena_city_notCode, datLAPasadena_code_notCity)


# Assign remaining codes as "Los Angeles HD"
datLA_notCode_notCity <- datLA %>% 
  filter(
    ((!cityText %in% c(lb, pasadena)) & (!cityCode %in% c(lbCode, pasadenaCode)))
  ) %>% 
  mutate(city_lhj = "Los Angeles HD")

# Bind rows
datLA_final <- bind_rows(datLALB_final, datLAPasadena_final, datLA_notCode_notCity)
nrow(datLA_final) == nrow(datLA)
table(datLA_final$city_lhj, useNA = "ifany")


# Final data ==========================================================

datAlamedaLA_final <- bind_rows(datAlameda_final, datLA_final)
nrow(datAlamedaLA_final) == nrow(datAlamedaLA)
length(unique(datAlamedaLA_final$SFN)) == nrow(datAlamedaLA)
table(datAlamedaLA_final$county, datAlamedaLA_final$city_lhj, useNA = "ifany")

cbdDat0_final <- cbdDat0_00_04 %>% 
  bind_rows(filter(cbdDat0_05, !county %in% lhjCounty)) %>% 
  bind_rows(datAlamedaLA_final)
nrow(cbdDat0_final) == nrow(cbdDat0)
length(unique(cbdDat0_final$SFN)) == (nrow(cbdDat0_05) + 1) # 2000-2004 does not have SFNs

table(filter(cbdDat0_final, year > 2004)$county, filter(cbdDat0_final, year > 2004)$city_lhj, useNA = "ifany") 

# Save data ================================================================
saveRDS(cbdDat0_final, file= paste0(securePlace,"/myData/ccb_processed_deaths.RDS"))

11416 + 59412 + 21485
