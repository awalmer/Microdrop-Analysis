# Microdrop-Analysis

The Microdrop Analysis Tool is an R Shiny application that I developed at Eli Lilly & Company. The Microdrop assay is an experimental method that helps to reveal certain characteristics about antibodies, such as solution solubility. The app facilitates Microdrop analysis by automating the process of result capture, as well as providing visualizations for the user. It includes parsed and pivoted data, a heat table, curve fit plots sample and buffer, as well as individual buffer-sample combination plots, downloadable Excel and PDF reports, and methodology documentation. 

One aim of Microdrop Analysis is to see how long it takes for a given sample solution to drop in protein concentration under specific conditions. That point of change is called the onset value, which is an outcome of interest for Microdrop. The greater the onset, the better.

The R script for the application is app.R, and the required supplmental script of functions sourced by the application is functions.R.

The original application is intended for usage by Eli Lilly scientists and is hosted on Eli Lilly's server. An open source version of this application can be found here: https://awalmer.shinyapps.io/microdrop_analysis/. Note: This version is hosted on shinyapps.io and has a 25 Active Hour limit per month.

## Features of the Application:
Parsed Data: Transforms the input data file into a cleaned table.
Pivot by Sample, Pivot by Buffer: Tranforms the input data file into multiple pivoted tables, showing values of interest by Sample, and by Buffer (respectively).
Onset Heat Table: 
