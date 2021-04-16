# Tomales_Great_Egret_habitat_selection_2020
Analysis of habitat selection by GPS tagged Great Egrets on Tomales Bay, CA



## code  

*main analysis code*  
The analysis is composed of 4 steps, each with their own code file:  

* analysis_1_prep_data.R: Combine necessary data sources. Assign habitat and elevation values to each GPS location. Calculate time-specific, location-specific water depth. Basic pre-analysis data summaries and visualization.  

* analysis_2_logrss.R - fit iSSF models to estimate relative habitat selection. Saves each model object as RDS. uses methods of:   
  + Avgar, T., Potts, J. R., Lewis, M. A., & Boyce, M. S. (2016). Integrated step selection analysis: bridging the gap between resource selection and animal movement. Methods in Ecology and Evolution, 7(5), 619–630. https://doi.org/10.1111/2041-210X.12528  
  Avgar, T., Lele, S. R., Keim, J. L., & Boyce, M. S. (2017). Relative Selection Strength: Quantifying effect size in habitat- and step-selection inference. Ecology and Evolution, 7(14), 5322–5330. https://doi.org/10.1002/ece3.3122  
  
* analysis_3_steplength.R - test for differences in steplength between wetland types. Extends the models from analysis_2_logrss.R to include the interaction between wetland type and step length.  

* analysis_4_odba.R - test for differences in Overall Dynamic Body Acceleration (ODBA) between wetland types. Fit mixed effects linear models with a random effect for bird ID.

*helper code*

* spatial_data_read_clean.R - make rasters for habitat and DEM same resolution, combine. Other assorted tasks related to summarizing spatial data.  

* utility_functions.R - define support functions and create some simple objects that are used at multiple places in the analysis  

* misc_tasks.R - descriptive file name. various small tasks tangential to actual analysis.  