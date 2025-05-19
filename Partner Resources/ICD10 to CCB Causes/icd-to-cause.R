# Load libraries ===========================================================================
library(fuzzyjoin) # Join/merge through regular expressions
library(dplyr) # Data wrangling
library(tidyr) # Data reshaping
library(readxl) # Read in excel files
library(ggplot2) # Create charts

# Read in files =============================================================================
icdCause <- read_excel("ICD10-to-CCB-Cause.xlsx",sheet=2) # read 2nd sheet of crosswalk file
var_names <- read_excel("death.File.Vars.xlsx") # Link Sequence IDs to variable names

sampleData <- read.csv("sample-death-data.csv", header = TRUE) # Sampled Death data with three columns: F19 (Sex), F24 (Year), F144 (ICD10)


# Standardize variable names=================================================================

# Match column names from 'sampleData' to var_names'
new_names <- var_names$varName[match(names(sampleData), var_names$seqID1)]

# Rename columns
names(sampleData) <- new_names

# Link ICD10 Codes to CCB Causes ===============================================================

#  Perform a left-join based on a regular expression match between the "ICD10" column in sampleData and "icd10_regex" column in icdCause
# "See Example.pptx" to learn more about regular expressions
linkedData <- sampleData %>% 
  regex_left_join(icdCause, by = c("ICD10" = "icd10_regex"))

# Check ==================
nrow(sampleData) == nrow(linkedData) # Should return TRUE; otherwise duplicates were created

# Look at NA 'cause_code': These are records where corresponding ICD10 codes did not map to a CCB cause; ICD10 codes may be blank, "000", or "0000"
linkedData %>% 
  filter(is.na(cause_code)) %>% 
  select(ICD10)

# Set missing values to Unknown
linkedData <- linkedData %>% 
  replace_na(list(
    cause_code = "Z02", cause_name = "Unknown/Missing Value", broad_condition_group = "Ill-Defined")
  )


# Count up number of deaths per cause ========================================================

nDeaths_per_cause <- linkedData %>% 
  count(cause_code, cause_name, broad_condition_group, name = "nDeaths")

head(nDeaths_per_cause)


# Create bar chart: Top 10 Causes based on number of deaths ===================================================================

nDeaths_per_cause %>% 
  slice_max(nDeaths, n = 10) %>%  # Filter on causes in top 10 for nDeaths
  ggplot(aes(x = nDeaths, y = reorder(cause_name, nDeaths))) + 
  geom_col(fill = "blue") +
  labs(x = "Number of Deaths", y = "Cause", title = "Top 10 Causes based on Number of Deaths") 
