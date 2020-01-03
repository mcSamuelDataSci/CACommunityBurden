library(summarytools)

oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) 

freq(oshpd16$patcnty)

# 81,283 (2.12%) of 3,842,621 are patcnty = "00"


Field Name: patcnty
Definition: The patient’s county of residence. OSHPD assigns the county of residence based on the patient’s reported ZIP Code. Because ZIP Codes can cross county boundaries, OSHPD assigns the county with the greatest population in the respective ZIP Code. Invalid, blank, and unknown ZIP Codes as well as patients residing outside California and the homeless are assigned a county code value of “00”.
Variable Type: Character

Field Name: patzip
Definition: The patient’s 5-digit ZIP Code of residence. If the ZIP Code is unknown it is assigned a value of “XXXXX”. Foreign residents are assigned a ZIP Code of “YYYYY” and homeless are assigned a ZIP Code of “ZZZZZ”. If only the city of residence is known, the first three digits of the ZIP Code are reported followed by two zeros. Invalid and blank ZIP Codes are set to “00000”.
Variable Type: Character