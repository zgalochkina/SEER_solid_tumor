rm(list = ls())
while(!is.null(dev.list())) dev.off()

# DOWNLOAD! --------------------------------------------------------------

# https://seer.cancer.gov/data/access.html 
# Export ALL cancer 2004+ records as text file from SEER Case Listing 
# Matrix (.slm) and save as caselisting_data.dic & caselisting_data.txt
# to ./data/case_listing/ folder as:
# "caselisting_data.dic"  - dictionary part ;
# "caselisting_data.txt"  - data part ;
# To save time, you might want preselect columns and years in SEER*Stat
# in advance. See below the list of preselected variables in README file.

# WHAT CAN MODIFY ---------------------------------------------------------

# Can modify the following parts of this R file:
# * CANCER ATTRIBUTES:     modifiable variables 1-7;
# * ADDITIONAL ATTRIBUTES: modifiable variables 8-12;
# Running 'attributes.R' will catch all necessary files except SEER data,
# which has to be downloaded by the user due to the need of signing agreement form
# with SEER program.

# PACKAGES ----------------------------------------------------------------

# required packages
pkg <- c("plyr", "tidyverse", "SEER2R", "survival", "scales", "stats", "stringr", "forcats", 
         "gridExtra", "plotrix", "maptools", "mapproj", "rgeos", "rgdal", "rmarkdown", "knitr", "R.utils", "utils")

# check if packages are installed
newpkg <- pkg[!(pkg %in% installed.packages())]

# install packages
if (length(newpkg)) {
  install.packages(newpkg, repos = "http://cran.rstudio.com")
}

# load packages
# Note: 'plyr' package has to be loaded before 'dplyr' package!
library(plyr)       # for 'ddply' function
library(tidyverse)  # filter, transform, plot data with 'ggplot2', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr' packages
library(SEER2R)     # read SEER data files
library(survival)   # Kaplan-Meier, Cox
library(scales)     # transform data scale on plot
library(stats)      # for 'fisher.test', 'aov' functions
library(stringr)    # for 'str_replace_all' function
library(forcats)    # for 'fct_recode' function
library(gridExtra)  # for 'grid.arrange' function
library(plotrix)    # for 'addtable2plot' function
library(maptools)   # map data to 50 states
library(mapproj)    # map data to 50 states
library(rgeos)      # map data to 50 states
library(rgdal)      # map data to 50 states
library(rmarkdown)  # for reports
library(knitr)      # for reports
library(R.utils)    # to unzip .gz population data file
library(utils)      # to unzip .zip GIS files for maps


# * CANCER ATTRIBUTES: ----------------------------------------------------
# can change

# ------------ 1 ---------------
# years: 2004+

years <- 2004:2014

# ------------ 2 ---------------
# age groups as shown in
# : https://seer.cancer.gov/popdata/popdic.html

age_groups <- 0:18
# select codes from: 0, 1, 2, 3, ..., 18, e.g. age_gr <- 4:16 means 20-
# which are the codes for
# "00 years", "01-04 years", "05-09 years","10-14 years", "15-19 years", "20-24 years",
# "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
# "60-64 years, "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years" age groups

# ------------ 3 ---------------
# select ICD-O-3 codes
# : http://seer.cancer.gov/siterecode/icdo3_dwhoheme/

icdo3_1 <- c(180:189, 260, 199, 209)

# if need to exclude different histology types for different icd-o-3 code groups 
# as in Brain cancer; by default subsequent options are disabled by "NULL" assignment
icdo3_2 <- NULL
icdo3_3 <- NULL

# ------------ 4 ---------------
# exclude histology types
# : http://seer.cancer.gov/siterecode/icdo3_dwhoheme/

hist_excl_1 <- "9050:9055, 9140, 9590:9992"
# if need to exclude different histology types for different icd-o-3 code groups 
# as in Brain cancer; by default subsequent options are disabled by "NULL" assignment
hist_excl_2 <- NULL
hist_excl_3 <- NULL

# ------------ 5 ---------------
# matched recodes for selected ICD-O-3 codes
# : http://seer.cancer.gov/siterecode/icdo3_dwhoheme/
# for all groups together, in right order: icdo3_1, icdo3_2, icdo3_3

site_recode_new <- c('Cecum', 'Appendix', 'Ascending Colon', 'Hepatic Flexure',
                     'Transverse Colon', 'Splenic Flexure', 'Descending Colon',
                     'Sigmoid Colon', rep('Large Intestine, NOS', 3), 
                     'Rectosigmoid Junction', 'Rectum')


# ------------ 6 ---------------
# cancer type name to use in titles & subfolders' names

cancer_type <- "Colorectal"

# ------------ 7 ----------------
# switcher: 
# staging: AJCC vs. Grade

ajcc_grade <- "AJCC"
# select from: 
# "AJCC", "Grade"


# * ADDITIONAL ATTRIBUTES: ------------------------------------------------
# can change

# ------------ 8 ---------------
# the registry which will be compared to other 17 registries

registry <- "New Mexico"
# select from:
# "San Francisco-Oakland SMSA", "Connecticut", "Detroit (Metropolitan)", 
# "Hawaii", "Iowa", "Seattle (Puget Sound)", "Utah", "Atlanta (Metropolitan)", 
# "Alaska Natives", "San Jose-Monterey", "Los Angeles", "Rural Georgia", 
# "California excluding SF/SJM/LA", "Kentucky", "Louisiana", "New Jersey", 
# "Georgia excluding Atlanta/Rural Georgia", "New Mexico"

# ------------ 9 ---------------
# switcher: 
# AJCC editions: 6th edition only vs. mix of 6th[for 2004-2009] and 7th[for 2010+] editions
# if selected "AJCC" option above

ajcc_ed <- "6th edition"
# select from: 
# "6th edition", "6th and 7th editions"

# ------------ 10 --------------
# switcher: 
# answer to the question: "Does cancer type subsetting happen for the first time with selected attributes?"
# Note: has to be "Yes" at least for the first run with selected attributes!
# if answered "Yes" then 'subset.R' code will be run within 'analyze.R' file and it will increase time of calculations,
# otherwise 'analyze.R' will omit run of 'subset.R' part and will use the previous output;
# 'subset.R' code subsets data, makes Flow Charts, Table 1;

subset_yn <- "Yes"         
# select from: "Yes", "No"

# ------------ 11 --------------
# population data (for age-adjusted rates)
# : https://seer.cancer.gov/popdata/download.html - County-Level Population Files - 19 Age Groups
# from column "1990-2015 4 Expanded Races by Origin"

url_population <- "https://seer.cancer.gov/popdata/yr1990_2015.19ages/us.1990_2015.19ages.adjusted.txt.gz"

# ------------ 12 --------------
# 2000 standard population (for age-adjusted rates)
# : https://seer.cancer.gov/stdpopulations/
# "Standard Populations - 19 Age Groups (0, 1-4, 5-9, 10-14, ..., 85+)"

url_std_population <- "https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt"

# ----------- Note -------------

# currently code works only for comparison of these 3 groups (subject to change in a new code release):
# "American Indian or Alaska Native", "Hispanic", "Non-Hispanic White", "Other"

# --------------~---------------



# CALL 'analyze.R' --------------------------------------------------------
# do not change
# 'analyze.R' will produce all predefined analyses and produce report
source("./code/generic/analyze.R")


# REPORTS -----------------------------------------------------------------
# go to the reports section and implement change in code chunk called 'change'

# Word
rmarkdown::render(file.path(paste0("./code/specific/", cancer_type), "report_docx_html.Rmd"),
                  word_document(reference_docx ="styles_docx.docx"))
# Note: there will be a Warning message because of scaling PNG plots. Ignore it. 
# Width control was intentionally added, but it can only be applided to HTML report.

# HTML
rmarkdown::render(file.path(paste0("./code/specific/", cancer_type), "report_docx_html.Rmd"), 
                  html_document(theme = "default"))

# PDF
# There is a complication caused by too wide tables (which are truncated on right side when call R Markdown file 
# to get PDF report).
# It's better if you use fully customizable reports through R + LaTex compilation.
# Or you can try 'kableExtra()' package in R Markdown which most probably will help to format wide tables in PDFs 
# and HTML reports (but not in DOCX).
# Do not recommend to use 'pander::pandoc.table()' instead of 'knitr::kable()', as it will not solve 
# wide-tables truncation problem in PDFs but will just worsen the report's appearance in this particular case.

# Presentations
# R Markdown produces html and latex style presentations, not ppt.
# If you really need to produce PPT presentation from R try 'ReporteRs' package.