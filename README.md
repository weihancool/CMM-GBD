Analytical code for Cardiometabolic Multimorbidity Aggregation Patterns, Network Mechanisms, and Disease Burden Projections to 2040
This repository contains the R code supplied for the accepted manuscript. The scripts have been organized and named for public archiving. No analytical statements, functions, model settings, thresholds, seeds, or output specifications were removed or revised. The only changes inside the R scripts are file-path replacements. All input and output paths now point to D:/GBD_data.

## Repository structure

- `R/01_Core_analysis_Figures_1_2_3_4AB_Table_1_Supp_Tables_2_5.R`  
  Core analyses for the main figures, main table, and supplementary tables.

- `R/02_Figure_4_panel_C_Sankey.R`  
  Code for Figure 4C.

- `R/03_Figure_4_panel_D_Transition_table_and_country_analysis.R`  
  Code for Figure 4D and associated country-level transition analyses.

- `R/04_Additional_overall_trend_plots.R`  
  Code for additional trend plots.

- `CITATION.cff`  
  Citation metadata for the repository.


Suggested run order
Prepare the required data files and pre-existing objects/functions.
Run R/01_Core_analysis_Figures_1_2_3_4AB_Table_1_Supp_Tables_2_5.R in its original order.
Ensure risk_2021.csv and risk_2040.csv are present in D:/GBD_data.
Run R/02_Figure_4_panel_C_Sankey.R.
Run R/03_Figure_4_panel_D_Transition_table_and_country_analysis.R.
Run R/04_Additional_overall_trend_plots.R only when the additional trend outputs are needed.
R packages
The scripts call the following packages directly or through pacman::p_load: openxlsx, bnlearn, Rgraphviz, readr, stringr, corrplot, ranger, ggplot2, igraph, dplyr, ciTools, pROC, kernelshap, shapviz, plotly, htmlwidgets, flextable, officer, tidyr, and the packages listed in R/04_Additional_overall_trend_plots.R.

Version
Initial archival release: v1.0.0.
