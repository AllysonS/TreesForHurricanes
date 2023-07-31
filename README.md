# TreesForHurricanes
Analyze the hurricane wind resistance of tree inventories and predict new tree wind ratings

## Overview
Hurricane damage to trees in towns and cities increases cleanup costs and decreases the benefits provided by the urban forest. There are many reasons why a particular tree will be damaged during a hurricane, but we do know that different tree species are more or less resistant to damage from severe winds. 

This website contains a set of tools to help communities understand the ability of tree species in their urban forests to resist damage from wind. Research by Duryea et al. (2007a, b) created a wind resistance rating system which was later expanded on by Salisbury et al. (2003a, b) form the foundation for these tools. 

First, the Estimating Tree Community Hurricane Resistance Tool (ETCHER) v.01 can determine the proportion of a community’s tree inventory that is made of Low, Medium Low, Medium High, and High wind resistant species. It can also be used to search for the wind resistance rating of a tree species. 

Second, since many tree species lack a wind resistance rating, we developed a predictive model that relies on tree species characteristics and observations of hurricane damage. If a user has the requisite data, they can use the Wind Resistance Rating Predictive Model R script to assign a rating to their species of interest. 

We intend for these tools to help communities plan for and mitigate hurricane damage to trees. Completely preventing hurricane tree damage in all scenarios is impossible, but these tools can complement other management activities that can help to better balance the costs and benefits of the urban forest.

## Estimating Tree Community Hurricane Resistance Tool (ETCHR)
We designed the Estimating Tree Community Hurricane Resistance (ETCHR) Tool to help communities evaluate the hurricane wind resistance rating of their tree species. One way to use the ETCHR Tool is to determine the proportion of a tree inventory that is made of Low, Medium Low, Medium High, and High wind resistant species. The Tree Inventory Instructions explain this process below. You can also use the Tool to simply search for the rating of a species. These ratings create a foundation for understanding one aspect of tree resistance to damage from hurricanes and should be used to supplement practitioner experience and knowledge of local conditions. 

First, begin by downloading the most up to date version of ETCHR: 
[ETCHR_workbook_v01.00.xlsx](https://github.com/AllysonS/TreesForHurricanes/files/12197234/ETCHR_workbook_v01.00.xlsx)

Instructions for using ETCHR are included in the "Instructions" page of the spreadsheet as well as in the companion pdf guide: 
[ETCHR_guide_v01.pdf](https://github.com/AllysonS/TreesForHurricanes/files/12173655/ETCHR_guide_v01.pdf)

And in a companion how-to video: https://youtu.be/IKMXXAHk7sA

## Random Forest Model for Predicting Wind Resistance Ratings
You can use the R script GH_wind_model_final.R to fit the random forest model for predicting wind resistance ratings based on the original research by Duryea et al. (2007a, b). Once trained, the model can be used to predict the rating for new species if the following information is available: 1) species wood density, 2) leaf mass per area, 3) species overall maximum height, 4) proportion of the species killed and/or damaged during a hurricane or tropical cyclone, 5) the latitude and 6) longitude of the study location, 7) if the study site is in a urban area, 8) if the species is an angiosperm or gymnosperm, 9) if the species have evergreen or (semi)deciduous leaves, 10) the study site biome, 11) and the time since a previous hurricane or tropical cyclone impacted the study area. 

The following files contain data for training the random forest model: 01_spp_damage_trait_env.csv, Species_common_names.csv, and Wind_resistance_methods.xlsx. 

## Resources
Urban Forest Hurricane Recovery Program Series https://edis.ifas.ufl.edu/collections/series_urban_forest_hurricane_recovery_program 

Duryea, M. L., Kampf, E., and Littell, R. C. (2007a). Hurricanes and the Urban Forest: I. effects on southeastern United States coastal plain tree species. Arboriculture & Urban Forestry 33, 83–97. doi: https://doi.org/10.48044/jauf.2007.010 

Duryea, M. L., Kampf, E., Littell, R., and Rodríguez-Pedraza, C. (2007b). Hurricanes and the urban forest: II. Effects on tropical and subtropical tree species. Arboriculture & Urban Forestry 33, 98–112. https://doi.org/10.48044/jauf.2007.011 

Salisbury, A. B., Koeser, A. K., Andreu, M. G., Chen, Y., Freeman, Z., Miesbauer, J. W., et al. (2023). Predictors of tropical cyclone-induced urban tree failure: an international scoping review. Frontiers in Forests and Global Change. https://doi.org/10.3389/ffgc.2023.1168495 


