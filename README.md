# SMART indicators Data
Functions to collect ecosystem indicator information and compile into a SMART (Specific, Measurable, Achievable, Relevant, Time-bound) indicator reporting format

Automation inspired by the NOAA-EDAB Ecosystem Context for Stock Assessment project: https://github.com/NOAA-EDAB/ECSA/blob/master/README.md and by the code for rendering the NOAA-EDAB Ecosystem Indicator Catalog: https://github.com/NOAA-EDAB/catalog/blob/master/R/make_rmd.R

The draft basic workflow so far applies to existing Northeast US Ecosystem Indicators familiar to the Council from [Northeast Fisheries Science Center State of the Ecosystem Reports](https://www.fisheries.noaa.gov/new-england-mid-atlantic/ecosystems/state-ecosystem-reports-northeast-us-shelf).

However, other indicator sources can be added.

At present:

1. Information from the most recent version of [`ecodata`](https://noaa-edab.github.io/ecodata/index.html) and the current version of the associated [Indicator Catalog](https://noaa-edab.github.io/catalog/index.html) is retrieved and stored using the functions `collect_ecodata_indicator_stats` and `collect_ecodata_catalog_data`.
2. Stored datasets `ecodatadat.rds` and `catalogdat.rds` are used by the function `create_template` and the generic `SMART_template.rmd` to create a page template specific to each indicator.
3. Indicator templates can then be rendered directly to an html book and posted online, or rendered to a google doc for hand editing.

To do:

1. Equivalent functions retrieving information from the current version of the ecodata [Technical Documentation](https://noaa-edab.github.io/tech-doc/) will be written to retrieve key methods attributes.
2. Stored methods datasets will be incorporated into `create_template` and the generic `SMART_template.rmd`.
3. Depending on the amount of hand editing needed, the functions and templates will be modified following the Ecosystem Context for Stock Assessment functionality that begins with a draft template (https://github.com/NOAA-EDAB/ECSA/blob/master/templates/generic_template.rmd), writes to google, allows data entry within specified fields, then reads edited sections back into the final indicator template page (https://github.com/NOAA-EDAB/ECSA/blob/master/R/merge_to_bookdown.R
4. Attributes from the final SMART template will be saved for each indicator in a comprehensive dataset that can be used for interactive browsing in a shiny app, to be developed in https://github.com/MAFMC/SMARTindicatorsDashboard 
5. Automate rendering to on online book using a function similar to https://github.com/NOAA-EDAB/ECSA/blob/master/R/render_ecsa.R 

