# Tomales_Great_Egret_habitat_selection_2020
Analysis of habitat selection by GPS tagged Great Egrets on Tomales Bay, CA



## code  

*main analysis code*  
The analysis has 2 parts, each part having multiple code files.  

1. habitat selection  

* hab_sel_1_combine_data_for_iSSF.R - First step in habitat selection analysis. Assign habitat and elevation values to each GPS location  

* hab_sel_2_fit_models.R - fit iSSF models to estimate relative habitat selection. Saves each model object as RDS  
  + uses methods of Avgar, T., Potts, J. R., Lewis, M. A., & Boyce, M. S. (2016). Integrated step selection analysis: bridging the gap between resource selection and animal movement. Methods in Ecology and Evolution, 7(5), 619–630. https://doi.org/10.1111/2041-210X.12528  
  Avgar, T., Lele, S. R., Keim, J. L., & Boyce, M. S. (2017). Relative Selection Strength: Quantifying effect size in habitat- and step-selection inference. Ecology and Evolution, 7(14), 5322–5330. https://doi.org/10.1002/ece3.3122  
  
* hab_sel_3_model_results.R - Read RDS model objects, view coefficients, etc. Generate plots to show relative selection of each habitat.

2. movement/behavior 

* behav_1_combine_data.R - combine ODBA (calculated in hetp_data_work/code_HETP/data_management/add_covariates.R) with habitat values


*helper code*

* spatial_data_read_clean.R - make rasters for habitat and DEM same resolution, combine. Other assorted tasks related to summarizing spatial data.  

* utility_functions.R - define support functions and create some simple objects that are used at multiple places in the analysis  

* misc_tasks.R - descriptive file name. various small tasks tangential to actual analysis.  