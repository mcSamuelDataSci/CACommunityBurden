#Technical Notes for each Page
#
#By: John Pugliese, PhD - 3/19/2018

techList <- list()

techList[[1]] <- paste0("The CAUSES in the app are currently based on an alphabetical arrangement of 36 mutually exclusive 
                			   and exhaustive conditions from the Global Burden of Disease Study— The 36 are an ad hoc list, thought
                			   to be generally relatable to California Public Health priorities.  But, the app could, and likely will, 
                			   include the full list of Global Burden of Disease conditions, and/or other Global Burden of Disease subsets; 
                			   and/or subsets from 2011 National Center of Health Statistics condition groupings (e.g. 133 or 39 causes).")

techList[[2]] <- paste0('<p>The current MEASURES of deaths are:</p> 
                         <ui style="list-style-type:circle">
                              <li style="margin-left: 80px">Years of Life Lost (YLL)</li>
                              <li style="margin-left: 80px">Mean YLL</li>
                              <li style="margin-left: 80px">Years of Life Lost per 100,000 population</li>
                              <li style="margin-left: 80px">Number of Deaths</li>
                              <li style="margin-left: 80px">Death Rate (Deaths per 100,000 population)</li>
                              <li style="margin-left: 80px">Median Age at Death</li>
                              <li style="margin-left: 80px">Excess Relative Risk (Standard Mortality Ratio)</li>
                         </ui>
                         <br>
                         <p>No one measure is best--each measure provides a different view or perspective into 
                  			 the impact of the condition. For example, <b>Number of deaths</b> is the simplest, most direct measure, and has 
                  			 clear intuitive meaning, and, other things being equal, will be larger in areas with larger populations.  
                  			 <b>Death Rate</b> takes the size of the population into account, and is the number of deaths divided by the number 
                  			 of people in the population (multiplied by a constant, 100,000, for interpretability). <b>Mean Years of Life Lost</b> 
                  			 is the average number of years of life lost among all people that die from that condition (so will be higher for 
                  			 conditions differentially impacting young people. <b>Years of Life Lost</b> sums all the years of life lost across all 
                  			 people that die from that condition, and is influenced by the age at which people die from the condition and the 
                  			 number of people that die from that condition.  <b>Median Age</b> shows the median age at death for all persons who die 
                  			 from that condition. It is not a common measure, but is intuitive and clear, showing  the <i>average age</i> at death, 
                  			 and can be useful for easily and clearly highlighting disparities. <b>Excess Relative Risk</b> is an <i>ad hoc</i> measure, 
                  			 and shows the county rate of condition divided by the rate for the condition in the State overall. This measure will 
                  			 highlight counties that have especially high (or low) rates of a condition compared to the State rate.   Measures 
                  			 to be added in the near future include age-specific rates and life expectancy.')

techList[[3]] <- paste0("The MAP (INTERACTIVE) tab is a “placeholder” for now, and using a different approach to mapping, 
		                     where the user can zoom in and zoom out.  Based on user input, this tab will be updated soon.")

techList[[4]] <- paste0("<p>The MAP (STATIC) tab shows the geographic distribution of CONDITIONS. The initial or “default” map 
                  		   shows YLL (Years of Life Lost) from Diabetes deaths in 2015 in each of the 58 California counties.   
                  		   The user can select different Causes, Years and Measures from the drop down lists.</p>  
                         <br>
                         <p>The <b>Geo Level</b> options allow the user to change the display from county, to community, to census tract.  This selection 
                  		   is one of the key concepts behind the app—Place Matters!--and insights into the burden of disease must 
                  		   be explored at multiple geographic levels, especially granular community levels. The user can click 
                  		   <b>Zoom to County</b>, and the map for just that county will be displayed.  For a county map, the user will 
                  		   want to choose the <b>Community</b> or <b>Census Tract</b> geographic level.</p>
                         <br>
                         <p>The <b>State-based Cutpoints</b> button changes the way the Measure is broken down or grouped. With the box checked, the cutpoints are based on 
                  		   the State data overall (so many/most communities in a given county might be in the highest category, if 
                  		   that condition tended to be high in that county in general). If the box is unchecked, the cut points will 
                  		   be based on the data in just that county, so the distribution of the condition throughout just that one 
                  		   county may be easier to see and understand (and take action!).  The CCB team is exploring a number of 
                  		   options where the user can choose different methods for determining the cut points (stay tuned, or contact 
                  		   us to help!).</p>")

techList[[5]] <- paste0("Medical Service Study Areas (MSSAs) are sub-city and sub-county geographical units used to organize and 
		     display population, demographic and physician data.")

techList[[6]] <- paste0("The RANKING tab shows the ranking of causes of death in the selected geography for the selected year.   
			   At the moment, the bar chart shows the ranking within the selected geography for four of the measures.  
			   The order of the bars is based on the selected measures, and different measures will generally show very 
			   different rankings.  This is a key concept of the app.   Ranking on numbers of deaths provides very different 
			   insight than ranking on median age at death.  These different rankings have implications for program periodization 
			   and selection of interventions.")

techList[[7]] <- paste0("Ranking on Excess Relative Risk provides a special window in the potentially unique priority of a condition in the 
			   selected geography.  A large Excess Relative Risk means the condition is especially high in that geography relative 
			   to the State average, even if the condition does not have a large number of deaths.  This is an important way to detect 
			   conditions that, while perhaps not super common, are unusually high (or low) in a county or community in which one is 
			   interested.  To aid in using this measure, the vertical red line is at 1.2, corresponding to the measure being 120% 
			   higher in the selected geography than the State average.  The green line is at 0.8, 80% of the State average and the 
			   grey line is at 1.0, right on the State average.")

techList[[8]] <- paste("The RANK GEOGRAPHIES tab displays the ranking of geographies for a selected condition, and lets the user explore 
			which geographies have the “highest” ranks for a selected condition for a selected measure.")

techList[[9]] <- paste0("The TRENDS tab displays the trend over time (currently from 2001 to 2015) of the selected measure for a selected condition 
		      in a selected geography.  This line chart is all about assessing whether the condition is getting better or worse over time.")



techList[[10]] <- paste0("YEAR:  At the County and State levels of geography, YEAR is the individual year of death, with current 
		     data from 2001 to 2015.  At the Community and Census Tract levels of geography, all data are displayed 
		     for the years 2011-2015 combined.  These years are combined for “statistical stability”, so that for these 
		     more granular levels of geography, the displayed data are still meaningful, and not just the result of 'random fluctuations'.   
	             We do not currently have 'geocoded' data for years prior to 2011, so cannot construct 2001-2005 and 2006-2010 community/census 
		     tract-level aggregation.  This is in our project 'road map.'")