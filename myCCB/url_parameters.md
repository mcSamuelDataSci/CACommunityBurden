#### A URL can be constructed and shared that links to a specific "tab" on the CCB with some specific parameters selected.

<br>

-   For example the following URL links to the cause of death rankings in Fresno County in 2018:

    -   <a target="_blank" rel="noopener noreferrer" href="https://skylab.cdph.ca.gov/communityBurden/?tab=rankbycause&county=fresno&year=2018">https://skylab.cdph.ca.gov/communityBurden/?tab=rankbycause&county=fresno&year=2018</a>
    
<br>

#### Constructing the URLs is based on the following structure and values:

-   Base URL

    -   <a target="_blank" rel="noopener noreferrer" href="https://skylab.cdph.ca.gov/community/Burden">https://skylab.cdph.ca.gov/community/Burden</a>

-   Four query parameters

    -   tab

        -   Table below shows possible value inputs

    -   county

        -   Full county name

    -   cause

        -   Inputs cause codes, not cause names.
        -   **Cause codes can be found in the 'Cause of Death' dropdown**, located on most tabs (e.g. Trends -\> Sex Trends)
        -   The cause code is the letter(s) and numbers before the cause name
        -   Use just the letter(s) and number; no periods or spaces

    -   year

        -   Four digit number

-   General query pattern to add at the end of the url:

    -   ?parameter1=value1&parameter2=value2&parameter3=value3

-   Specific CCB query pattern:

    -   ?tab=tabvalue&county=countyvalue&cause=causevalue&year=yearvalue
    -   Not all parameters are relevant for all tabs
    -   Not all parameters for a given tab need to be specified. If no parameter is given, a default value will be used.

-   Example 1 - Linking to Sex Trend Tab, Alameda County, Ischemic Heart Disease

    -   <a target="_blank" rel="noopener noreferrer" href="https://skylab.cdph.ca.gov/communityBurden/?tab=sextrend&county=alameda&cause=c02">https://skylab.cdph.ca.gov/communityBurden/?tab=sextrend&county=alameda&cause=c02</a>

-   Example 2 - Linking to Race Trend Tab

    -   <a target="_blank" rel="noopener noreferrer" href="https://skylab.cdph.ca.gov/communityBurden/?tab=racetrend">https://skylab.cdph.ca.gov/communityBurden/?tab=racetrend</a>

        -   This defaults to California since no county is specified

<br>

#### Tab Parameters

| CCB Tab                                       | Parameter Value        |
|-----------------------------------------------|------------------------|
| INTERACTIVE MAP                               | interactivemap         |
| RANK BY CAUSE - DEATHS                        | rankbycause            |
| RANK BY GEOGRAPHY - DEATHS                    | rankbygeography        |
| MULTIPLE CAUSES OF DEATH                      | mcod                   |
| AGE RACE FOCUS                                | ageracefocus           |
| DEATH HOSP ED                                 | deathhosped            |
| ATTRIBUTABLE RISKS - IHME                     | risksihme              |
| TWO-YEAR IHME RANKINGS                        | ihmerankings           |
| SEX TREND                                     | sextrend               |
| AGE TREND                                     | agetrend               |
| RACE TREND                                    | racetrend              |
| EDUCATION TREND                               | educationtrend         |
| LIFE EXPECTANCY                               | lifeexpectancy         |
| LEADING CAUSES                                | leadingcausestrend     |
| MORTALITY (MULTIPLE LENSES)                   | mortalitylens          |
| MORBIDITY (MULTIPLE LENSES)                   | morbiditylens          |
| YLD AND RISK (MULTIPLE LENSES)                | risklens               |
| DISPARITIES                                   | disparities            |
| SOCIAL DETERMINANTS                           | sdoh                   |
| HOSPITAL DISCHARGE                            | hospitaldischarge      |
| HOSPITAL DISCHARGE--PRIMARY AND ANY DIAGNOSES | hospitalprimaryany     |
| DEMOGRAPHICS                                  | demographics           |
| OVERVIEW                                      | overview               |
| TECHNICAL DOCUMENTATION                       | technicaldocumentation |
| LINKS TO OTHER DATA                           | otherlinks             |
| CCB URL PARAMETERS                            | urlparameters          |
