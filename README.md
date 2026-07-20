# Analytical code for *Cardiometabolic Multimorbidity Aggregation Patterns, Network Mechanisms, and Disease Burden Projections to 2040*

This repository contains the R code supplied for the accepted manuscript. The scripts have been organized and named for public archiving. **No analytical statements, functions, model settings, thresholds, seeds, or output specifications were removed or revised. The only changes inside the R scripts are file-path replacements.** All input and output paths now point to `D:/GBD_data`.

## Repository structure

- `R/01_Core_analysis_Figures_1_2_3_4AB_Table_1_Supp_Tables_2_5.R` - core analysis in its original execution order.
- `R/02_Figure_4_panel_C_Sankey.R` - Figure 4C.
- `R/03_Figure_4_panel_D_Transition_table_and_country_analysis.R` - Figure 4D and the associated country-transition analyses.
- `R/04_Additional_overall_trend_plots.R` - five additional standalone trend plots.
- `CODE_INDEX.csv` - manuscript-order mapping from each figure/table to its script and line range.
- `docs/PATH_CHANGES.md` - complete path-replacement audit.
- `docs/DOI_RELEASE_GUIDE_CN.md` - GitHub/Zenodo release checklist in Chinese.
- `CITATION.cff` - software citation metadata for GitHub and Zenodo.

## Code index in manuscript order

### Main figures

1. **Figure 1, panels A-B** - `R/01...R`, lines 14-73. The code computes the disease quartiles and five CMM pattern labels used for the maps. The uploaded R scripts do not contain the final map-rendering or composite-assembly commands.
2. **Figure 2, panel A** - `R/01...R`, lines 75-89. Random-forest SHAP analysis. The disease index `i` must be supplied externally for IHD, stroke, and T2DM.
3. **Figure 2, panel B** - `R/01...R`, lines 91-124. Adjusted Bayesian network construction.
4. **Figure 2, panel C** - `R/01...R`, lines 125-151. Network-restricted Pearson correlation matrix; output: `D:/GBD_data/cor_lower.pdf`.
5. **Figure 3, panel A** - `R/01...R`, lines 261-292. Global trend fitting and projection data.
6. **Figure 3, panels B-F** - `R/01...R`, lines 294-353. Trend fitting and projection data for IST, IS, IT, ST, and Others. The uploaded R scripts do not contain the final composite-plot assembly commands.
7. **Figure 4, panels A-B** - `R/01...R`, lines 355-399. Preparation of 2021/2040 high-risk-factor counts and grades for map visualization. The uploaded R scripts do not contain the final map-rendering commands.
8. **Figure 4, panel C** - `R/02...R`, lines 1-295. Sankey diagram; output: `D:/GBD_data/Figure_C_Sankey_2021_2040.html`.
9. **Figure 4, panel D** - `R/03...R`, lines 1-951. Frequency/transition table and supporting country-level analyses. The original internal Word-table title is retained unchanged because no analytical content was edited.
10. **Figure 5** - schematic analytical framework; no R code or numerical source data.

### Main table

- **Table 1** - `R/01...R`, lines 153-260. AUC comparison between adjusted and classical Bayesian networks.

### Supplementary tables

- **Supplementary Table 1** - manually compiled exposure definitions; no generating code was supplied.
- **Supplementary Table 2** - `R/01...R`, lines 54-62, computes the numerical quantiles. Check-mark annotations and final formatting are not coded.
- **Supplementary Table 3** - `R/01...R`, lines 75-89, contains the underlying SHAP computation. Final mean-absolute-SHAP extraction/export is not explicitly present.
- **Supplementary Table 4** - `R/01...R`, lines 153-171, defines the distal/intermediate/proximal matrix.
- **Supplementary Table 5** - `R/01...R`, lines 91-124, creates the bootstrap strength/direction objects.

### Additional code

`R/04_Additional_overall_trend_plots.R` generates five standalone plots for air pollution, two behavioral groups, metabolic factors, and outcomes. These outputs are not directly identified as a final main figure or supplementary table in the supplied final manuscript.

## Required files in `D:/GBD_data`

Place the following files directly in the folder:

- `cause_id.RData`
- `factor_id.RData`
- `risk_2021.csv`
- `risk_2040.csv`
- `air pollution.csv`
- `Behavior1.csv`
- `Behavior2.csv`
- `Metabolic.csv`
- `Outcomes.csv`

## Required pre-existing R objects/functions

The supplied core script assumes that the following are already available in the R session, but their creation code was not included in the uploaded files:

- objects: `resultall`, `arcgis`, `pres`
- scalar/index: `i` for selecting the SHAP outcome
- functions: `cor.mtest`, `adjbn`
- functions/packages used without an explicit `library()` call: `kernelshap`, `shapviz`

The repository preserves this behavior rather than adding or changing analytical code. For full independent reproducibility, the missing object-generation and custom-function code should be added in a future release if available.

## Suggested run order

1. Prepare the required data files and pre-existing objects/functions.
2. Run `R/01_Core_analysis_Figures_1_2_3_4AB_Table_1_Supp_Tables_2_5.R` in its original order.
3. Ensure `risk_2021.csv` and `risk_2040.csv` are present in `D:/GBD_data`.
4. Run `R/02_Figure_4_panel_C_Sankey.R`.
5. Run `R/03_Figure_4_panel_D_Transition_table_and_country_analysis.R`.
6. Run `R/04_Additional_overall_trend_plots.R` only when the additional trend outputs are needed.

## R packages

The scripts call the following packages directly or through `pacman::p_load`: `openxlsx`, `bnlearn`, `Rgraphviz`, `readr`, `stringr`, `corrplot`, `ranger`, `ggplot2`, `igraph`, `dplyr`, `ciTools`, `pROC`, `kernelshap`, `shapviz`, `plotly`, `htmlwidgets`, `flextable`, `officer`, `tidyr`, and the packages listed in `R/04_Additional_overall_trend_plots.R`.

## Version

Initial archival release: `v1.0.0`.
