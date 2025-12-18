# Changelog

All notable changes to the CCB Condition List are documented in this file.

# December 18, 2025

The CCB Condition List was updated with the following changes:

-   New “Public Health level” conditions added under the broad "Perinatal conditions" group.

    -   P.01 - Short gestation, low birth weight, slow growth

    -   P.02 - Perinatal respiratory distress/conditions

    -   P.03 - Perinatal cardiovascular disorders

    -   P.04 - Infections and other disorders of newborn

    -   P.05 - Sudden unexplained death syndrome

        -   Note: ICD10 codes *R99* and *W75* are classified as "Sudden unexplained death syndrome" only among infants (\< 1 year old). In all other cases (\>= 1 year old), *R99* is mapped to "Z01 - Symptoms, signs and ill-defined conditions, not elsewhere classified" and *W75* to "E06 - Other unintentional injuries". Refer to the "icd-to-cause.R" file for details on how these mappings are applied.

    -   P.06 - Newborn affected by complications of pregnancy

    -   P.07 - Congenital anomalies

        -   Note: "Congenital anomalies" was previously under the broad "Other Chronic" group.

    -   Basis for change:

        -   Aggregation of ICD-10 codes for the public health level perinatal conditions were based on reviewing all *individual* ICD-10 in codes in the [CDPH Maternal, Child & Adolescent Health Division (MCAH)/CDC National Center for Health Statistics’ List of 130 Selected Causes of Infant Death system](https://www.cdc.gov/nchs/data/nvsr/nvsr74/nvsr74-10.pdf), inspecting death counts for these individual codes and aggregations of these codes, and developing a final list based on meaningful groupings with adequate numbers for assessing these perinatal causes of death in California. This system differs from the aggregation system used in the [MCAH Infant Mortality dashboard](https://www.cdph.ca.gov/Programs/CFH/DMCAH/surveillance/Pages/Infant-Mortality.aspx) which only comprises perinatal causes of deaths.

Other updates:

-   ICD10-to-CCB-Cause.xlsx

    -   The following columns in the "More Detailed Cause Link" sheet were renamed:

        -   cause_code -\> cause_code_public_health

        -   cause_name -\> cause_name_public_health

    -   Broad condition groups were relabeled for greater clarity.

-   icd-to-cause.R

    -   New code added for mapping ICD10 codes to "P.05 - Sudden unexplained death syndrome". Please see note further above.

# March 28, 2025

The CCB Condition List was updated with the following changes:

1.  Cervical cancer (code B10), which had been inaccurately labeled recently, was correctly renamed to Uterine/Cervical cancer. 

2.  New detail level causes added under the Public Health Level cause Uterine/Cervical cancer (code B10)

    -   B10a – Cervical

    -   B10b – Uterine

3.  New detail level causes added under the Public Health Level cause Brain and nervous system cancers (code B15)

    -   B15a – Malignant

    -   B15b – Non-malignant

4.  There are now two “catch-all” Public Health Level causes for Cancer:

    -   B98 – Other malignant neoplasms

    -   B99 – Other neoplasms

        -   Includes most D00-D48 ICD-10 codes, which represent in-situ, benign, or neoplasms with unknown behavior. These codes were previously under D99 – Other Chronic conditions.
