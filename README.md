Project name:
Analysis of North Korea's Trash Balloon Landings


Project description:
Analyse the dataset of 167 recorded North Korea's balloon landings in South Korea between May and November 2024. The goal is to produce a data-driven infographic on this event, following the style guidelines of The Dallas Morning news.


Data source:
The dataset used in this analysis was obtained from the Beyond Parallel Project by the Center for Strategic and International Studies (CSIS). It is publicly available to be downloaded as a csv file on their website, and was accessed via the "Get the data" button at:
https://beyondparallel.csis.org/map-of-north-koreas-garbage-filled-balloons/


Desciption of each variable:
1. wave_number – which wave the balloon was launched
2. location – location formatted in town, city, province
3. lat – latitude coordinate where the balloon landed
4. long – longitude coordinate where the balloon landed
5. province – the administrative division in South Korea where the balloon was recorded
6. korean_name – Korean name of the landing location
7. date – the recorded date when the balloon landed
8. time – the recorded time of day when the balloon landed
9. source – the source of the data for the balloon landing
10. distance_to_seoul – the calculated distance in km from the landing location to Seoul
11. wave_group – categorical variable that takes value "Waves 1-3" or "Later waves" depending on the wave number


Data processing:
Although there are missing values in the "time" column, the column is not used in the analysis and no action is performed to address the missing values. Columns are converted to snake_case for consistency.


Packages used:
1. geosphere – Haversine distance calculations
2. ggplot2 – data visualization
3. leaflet – interactive map
4. dplyr – data manipulation
5. rnaturalearth – to obtain the base map of South Korea


Analysis performed:
1. Displayed all landing locations on a map
2. Calculated the distance from Seoul, visualised on a map
3. Investigated the trend of targeting Seoul over time
4. Identified which provinces are the most badly affected


Final output:
An infographic pdf created using Canva, based on the visualisations created using Rstudio. If you encounter any difficulties in reproducing the results, please refer to the Support section below.


Reference:
1. Data source: https://beyondparallel.csis.org/map-of-north-koreas-garbage-filled-balloons/
2. Haversine distance formulation: https://en.wikipedia.org/wiki/Haversine_formula
3. Style guidelines: https://policyviz.com/wp-content/uploads/2021/03/tdmn_graphics.pdf?srsltid=AfmBOor_O2WCwoPSBuB8SY0iTO82d4mOaGWBR-lNDwczbzo3rj1gGZHq
4. CMYK to HEX colour code converter: https://colordesigner.io/convert/cmyktohex

Author:
CID: 06011902


Support:
Please report issues to 06011902@ic.ac.uk

