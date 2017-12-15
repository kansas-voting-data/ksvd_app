# Kansas Voting Data Application Files
Files for fetching, organizing, displaying, and deploying data on the Kansas Voting Data Shiny application  

## Table of Contents
- **acs**: Files for fetching and organizing data from the American Community Survey  
- **app**: The Kansas Voting Data Shiny application  
- **results**: Data files from the Kansas (Secretary of State Website)[http://www.kssos.org/elections/elections_statistics.html] for election results. .pdf files were downloaded from this website, converted to .csv files using Tabula, and manually cleaned; they were all combined using the `combining_results.R` script  
- **turnout**: Data files from the Kansas (Secretary of State Website)[http://www.kssos.org/elections/elections_statistics.html] for election turnout. Excel files were downloaded from this website, converted to .csv files and manually cleaned; they were all combined using the `combining_turnout.R` script  