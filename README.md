# SMART indicators Data
Functions to collect ecosystem indicator information and compile into a SMART (Specific, Measurable, Achievable, Relevant, Time-bound) indicator reporting format

Automation inspired by the NOAA-EDAB Ecosystem Context for Stock Assessment project: https://github.com/NOAA-EDAB/ECSA/blob/master/README.md and by the code for rendering the NOAA-EDAB Ecosystem Indicator Catalog: https://github.com/NOAA-EDAB/catalog/blob/master/R/make_rmd.R

The draft basic workflow now applies to both existing Northeast US Ecosystem Indicators familiar to the Council from [Northeast Fisheries Science Center State of the Ecosystem Reports](https://www.fisheries.noaa.gov/new-england-mid-atlantic/ecosystems/state-ecosystem-reports-northeast-us-shelf) and to other indicator sources.

The workflow is described in the vignette Workflow, currently available in the `doc` folder when the package is installed.

At present:

1. Information from the most recent version of [`ecodata`](https://noaa-edab.github.io/ecodata/index.html) and the current version of the associated [Indicator Catalog](https://noaa-edab.github.io/catalog/index.html) is retrieved and stored using the functions `collect_ecodata_indicator_stats` and `collect_ecodata_catalog_data`.
2. Stored datasets `ecodatadat.rds` and `catalogdat.rds` are used by the function `create_template_SOE` and the generic `SMART_template_SOE.rmd` to create a page template specific to each indicator.
3. Indicator templates can then be rendered directly to an html book and posted online, or rendered to a google doc for hand editing.
4. A generic template `SMART_template_generic.rmd` can now be rendered to the drafts folder and to google doc using `create_template_generic` to hand-enter all fields for non-SOE indicators. 
5. Once edited, the filled google doc for a particular indicator is pulled back into the full set of indicator rmds using `gdoc_to_filledrmd`.
6. The function `renderall` calls `render_smartind` for any rmd file in docs to build the book. The `index.Rmd` file now includes SOE and non SOE indicators.
7. Equivalent functions retrieving information from the current version of the ecodata [Technical Documentation](https://noaa-edab.github.io/tech-doc/) have been written to retrieve key methods attributes.
8. Stored methods datasets have been incorporated into `create_template_SOE` and `SMART_template_SOE.rmd`.
9. Automated initial SMART ratings were added to `SMART_template_SOE.rmd` and a summary rating dataset is output for each indicator.


To do:

1. The SOE specific `create_template_SOE` function and template will be modified to write to google docs to allows data entry within the remaining specified fields (comments), then reads edited sections back into the final indicator template page
2. Attributes from the final SMART template will be saved for each indicator in a comprehensive dataset that can be used for interactive browsing in a shiny app, to be developed in https://github.com/MAFMC/SMARTindicatorsDashboard 
3. Automate rendering to on online book using a function similar to https://github.com/NOAA-EDAB/ECSA/blob/master/R/render_ecsa.R 

