# Changelog

Please view "changelog.md" to see the history of updates made to the CCB Condition List.

# California Community Burden of Disease (CCB) Condition List

The "California Community Burden of Disease Condition List" is the
hierarchical list of grouped causes of death, or "conditions", that can
be examined and explored in the CCB. The CCB Condition List has three
"levels": The "Top Level", the "Public Health Level", and the "Detail
Level"

1.  The "Top Level" of the Condition list includes 6 broad mutually
    exclusive and exhaustive groups: 1) Communicable, maternal, and
    nutritional conditions 2) Cancer/Malignant neoplasms 3)
    Cardiovascular diseases 4) Other Chronic Conditions 5) Injuries 6)
    Perinatal Conditions
2.  The next level, the "Public Health" level, splits the top level into
    around 75 mutually exclusive and exhaustive conditions. These
    conditions are based on meaningful public health programmatic
    groupings and all have large enough numbers for useful exploration.
3.  The final "Detail Level" includes several conditions of special
    interest or significance, but for which the numbers are insufficient
    or inappropriate to include as a unique "Public Health Level"
    category.

Please visit the [CCB Technical Documentation
page](https://skylab.cdph.ca.gov/communityBurden/?tab=technicaldocumentation)
to learn more about the CCB Condition List.

# About this folder

This folder provides resources and R code for mapping ICD-10 death codes to "Public
Health" level causes in the CCB Condition List. A description of each
file is below:

-   **ICD10-to-CCB-Cause.xlsx**:

    -   First sheet: Public Health Level Link

        -   This sheet lists out which ICD-10 codes are in each of the
            CCB "Public Health" level. Columns include:

            -   cause_code: Code for the CCB "Public Health"
                level cause

            -   cause_name: Name of the CCB "Public Health" level cause

            -   broad_condition_group: Broader "Top Level" group that the
                corresponding CCB cause is under.

            -   icd10_code: ICD-10 codes included in the corresponding
                CCB cause.

            -   icd10_regex: Regular expression syntax for ICD-10 codes.

    -   Second sheet: More Detailed Cause Link

        -   Similar to the first sheet, but with even more detailed
            conditions included.

            -   For example, this sheet lists the ICD-10 codes for
                several specific STDs that roll up into the "Public
                Health" level cause "A02 - HIV/ and other Sexually
                transmitted diseases (STDs)".

-   **Sample-death-data.csv**

    -   25,000 death records containing ICD-10 codes, county of
        residence, and sex. These records were randomly sampled from the
        raw death data. This data file is read into the R script as an
        example to show the mapping process.

-   **Icd-to-cause.R**

    -   R script showing how to map ICD-10 codes to the CCB "Public
        Health" level causes.

        -   The mapping approach is based on the use of regular
            expressions (regex).

-   **Example.pptx**

    -   Explains regular expressions (regex) and contains an example of
        merging two tables based on regex.
        
<br>
<br>


### *Date Created: July 1, 2024*
### *Last Modified: March 28, 2025*