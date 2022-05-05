# Microdrop Analysis

The Microdrop Analysis Tool is an R Shiny application that I developed at Eli Lilly & Company. The Microdrop assay is an experimental method that helps to reveal certain characteristics about antibodies, such as solution solubility. The app facilitates Microdrop analysis by automating the process of result capture, as well as providing visualizations for the user. It includes parsed and pivoted data, a heat table, curve fit plots sample and buffer, as well as individual buffer-sample combination plots, downloadable Excel and PDF reports, and methodology documentation. 

One aim of Microdrop Analysis is to see how long it takes for a given sample solution to drop in protein concentration under specific conditions. That point of change is called the onset value, which is an outcome of interest for Microdrop. The greater the onset, the better. There are two primary approaches within the assay: 24-hour onset and 3-hour onset. While the 24-hour onset approach aims to find where the protein concentration drops,  the 3-hour onset approach conversely aims to capture where the absorbance (350 nm) distinctly increases. For both approaches, the user may either have a full plate or half plate of samples. The data folder in this repository contains examples of all four possible input file types. 

The R script for the application is app.R, and the required supplmental script of functions sourced by the application is functions.R.

The original application is intended for usage by Eli Lilly scientists and is hosted on Eli Lilly's server. An open source version of this application can be found here: https://awalmer.shinyapps.io/microdrop_analysis/. Note: This version is hosted on shinyapps.io and has a 25 Active Hour limit per month.
