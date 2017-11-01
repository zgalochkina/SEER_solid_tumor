# INPUT FILES -------------------------------------------------------------

# caselisting_data.dic  - dictionary part of exported SEER Case Listing Matrix
# caselisting_data.txt  - data part of exported SEER Case Listing Matrix
# us.1990_2015.19ages.adjusted.txt - population data (auto downloaded from Internet)
# stdpop.19ages.txt                - standard population data (auto downloaded from Internet)
# functions.R

# OUTPUT FILES ------------------------------------------------------------
# flow_charts.Rdata     - flow charts of records selection
# table1.Rdata          - characteristics of cancer patients
# icdo3_recode.Rdata    - to show in the report ICD-O-3 codes selected (if needed)
# popdata.Rdata
# pop_std.Rdata
# records in all SEER registries except registry of interest: 
# us_inc.csv            - for incidence analysis 
# us_surv.csv           - for survival analysis  
# records in registry of interest:
# reg_inc.csv           - for incidence analysis
# reg_surv.csv          - for survival analysis


# CALL FUNCTIONS.R --------------------------------------------------------

# user-defined functions
source("./code/generic/functions.R")


# ICD-O-3 RECODE TABLE ----------------------------------------------------

(icdo3_recode <- data.frame(Primary_Site = c(icdo3_1, icdo3_2, icdo3_3), Site_recode = site_recode_new, 
                            Histology_exclude = c(rep(hist_excl_1, length(icdo3_1)), 
                                                  rep(hist_excl_2, length(icdo3_2)), 
                                                  rep(hist_excl_3, length(icdo3_3)))))
# save for report
save(icdo3_recode, file = file.path(paste0("./data/intermediate/", cancer_type), "icdo3_recode.Rdata"))


# SEER*STAT DATA ----------------------------------------------------------

ptm <- proc.time() # track process time

# preselect variables in advance in SEER session to save time
seer = read.SeerStat(DICfileName = "./data/case_listing/caselisting_data.dic", 
                     TXTfileName = "./data/case_listing/caselisting_data.txt", 
                     UseVarLabelsInData = TRUE)
names(seer) <- str_replace_all(names(seer), "/", "_")
names(seer)

proc.time() - ptm # show process time


# SELECT VARIABLES --------------------------------------------------------
# potentially useful variables to produce current report

seer <- seer %>% dplyr::select( 
               Age_recode_with_1_year_olds, 
               Sex, 
               Year_of_diagnosis, 
               SEER_registry, 
               County, 
               Statecounty, 
               CHSDA_2012, 
               State, 
               Site_recode_ICDO3_WHO_2008, 
               Behavior_recode_for_analysis,
               Primary_Site_labeled, 
               Primary_Site, 
               Histologic_Type_ICDO3, 
               Grade, 
               Diagnostic_Confirmation, 
               ICDO3_Hist_behav,
               ICDO3_Hist_behav_malignant, 
               Derived_AJCC_Stage_Group_7th_ed_2010,
               Derived_AJCC_Stage_Group_6th_ed_2004, 
               Breast_Adjusted_AJCC_6th_Stage_1988,
               Derived_AJCC_Flag_2004, 
               Summary_stage_2000_1998, 
               SEER_historic_stage_A, 
               COD_to_site_recode, 
               SEER_causespecific_death_classification,
               SEER_other_cause_of_death_classification, 
               Survival_months,
               Survival_months_flag, 
               Vital_status_recode_study_cutoff_used,
               Type_of_followup_expected, 
               Sequence_number, 
               Age_recode_with_single_ages_and_85, 
               Race_recode_W_B_AI_API,
               Origin_recode_NHIA_Hispanic_NonHisp, 
               Age_at_diagnosis, 
               Year_of_birth,
               Month_of_diagnosis, 
               Patient_ID,
               Type_of_Reporting_Source
               )


# + AJCC / GRADES [2004+] -------------------------------------------------

# add column 'Stage_Grade'
seer <- fun.ajcc_grade(seer)


# + CAUSE OF DEATH --------------------------------------------------------

# add two columns: 'CON_CS', 'COD_NCS'

# cause-specific
# 1 = dead due to Cause-Specific cause, 0 = reasons of death other than Cause-Specific or alive

seer[seer$SEER_causespecific_death_classification %in% c("Dead (attributable to this cancer dx)"), "COD_CS"] <- 1
seer$COD_CS[is.na(seer$COD_CS)] <- 0

# not cause-specific
# 1 = dead, 0 = alive
seer[!(seer$COD_to_site_recode %in% c("Alive")), "COD_NCS"] <- 1
seer$COD_NCS[is.na(seer$COD_NCS)] <- 0


# + TIME SURVIVED ---------------------------------------------------------

# add column Time_Survived
# (Survival Months + 1) due to 'floor' function used in SEER

seer$Survival_months[seer$Survival_months == "Unknown"] <- NA
seer$Survival_months <- droplevels(seer$Survival_months)
seer$TimeSurv = as.numeric(as.character(seer$Survival_months)) + 1


# + RACE / ETHNICITY ------------------------------------------------------

# add column Race_Ethnicity
# AIAN, NHW, Hisp, Other
seer$Race_Ethnicity <- NA
seer$Race_Ethnicity[(seer$Origin_recode_NHIA_Hispanic_NonHisp %in% c("Non-Spanish-Hispanic-Latino")) & 
                          (seer$Race_recode_W_B_AI_API %in% c("White"))] <- "Non-Hispanic White"
seer$Race_Ethnicity[(seer$Origin_recode_NHIA_Hispanic_NonHisp %in% c("Spanish-Hispanic-Latino")) & 
                          (seer$Race_recode_W_B_AI_API %in% c("White"))] <- "Hispanic"
seer$Race_Ethnicity[(seer$Race_recode_W_B_AI_API %in% "American Indian/Alaska Native")] <- "American Indian or Alaska Native"
seer$Race_Ethnicity[is.na(seer$Race_Ethnicity)] <- "Other"

seer$Race_Ethnicity <- as.factor(seer$Race_Ethnicity)


# + SITE RECODE -----------------------------------------------------------

# add column Site_recode as a substitute for Site_recode_ICD_O_3_WHO_2008
seer <- left_join(seer, icdo3_recode, by = "Primary_Site")
seer <- seer %>% dplyr::select(-(Site_recode_ICDO3_WHO_2008))


# MODIFY AGE AT DIAGNOSIS -------------------------------------------------

# convert Age_at_diagnosis to numeric variable
seer$Age_at_diagnosis <- gsub("Unknown", "NA", seer$Age_at_diagnosis)
seer$Age_at_diagnosis <- suppressWarnings(as.numeric(seer$Age_at_diagnosis))

# % of unknown Age_at_diagnosis for any cancer type
sum(is.na(seer$Age_at_diagnosis)) / nrow(seer) * 100


# + AGE CODES -------------------------------------------------------------

seer$Age_recode_with_1_year_olds <-  as.character(seer$Age_recode_with_1_year_olds)

age_gr <- tribble(
  ~ Age_recode_with_1_year_olds, ~ Age,
  "00 years"    , 0,
  "01-04 years" , 1,
  "05-09 years" , 2,
  "10-14 years" , 3,
  "15-19 years" , 4,
  "20-24 years" , 5,
  "25-29 years" , 6,
  "30-34 years" , 7,
  "35-39 years" , 8,
  "40-44 years" , 9,
  "45-49 years" , 10,
  "50-54 years" , 11,
  "55-59 years" , 12,
  "60-64 years" , 13,
  "65-69 years" , 14,
  "70-74 years" , 15,
  "75-79 years" , 16,
  "80-84 years" , 17,
  "85+ years"   , 18
)

seer <- left_join(seer, age_gr, by = "Age_recode_with_1_year_olds")


# SELECT OBSERVATIONS: ----------------------------------------------------

list_of_diag_conf <- c("Positive histology", "Positive exfoliative cytology, no positive histology",
  "Positive microscopic confirm, method not specified",
  "Pos hist AND immunophenotyping AND/OR pos genetic studies")


# for USA -----------------------------------------------------------------

us_dim <- NA

# ~~~~~~~~~~~~ 1 ~~~~~~~~~~~~~~~
# All cancer diagnoses in SEER registries for selected age groups and years

us <- seer %>% 
  dplyr::filter(Age %in% age_groups) %>% 
  dplyr::filter(Year_of_diagnosis %in% years) %>% 
  dplyr::filter(!grepl(pattern = registry, x = SEER_registry))

us_dim <- nrow(us)

# ~~~~~~~~~~~~ 2 ~~~~~~~~~~~~~~~
# cancer type selection

# select histology type codes in accordance with icd-o-3 codes
us <- filter(us, Primary_Site %in% icdo3_recode$Primary_Site)

us$Histology_select <- NA
for(i in 1:nrow(us)){
  if(us$Histologic_Type_ICDO3[i] %in% eval(parse(text=paste0("c(", us$Histology_exclude[i], ")")))){
    us$Histology_select[i] <- "exclude" 
  } else us$Histology_select[i] <- "select" 
}

us <- filter(us, Histology_select %in% c("select"))
us_dim <- c(us_dim, nrow(us))

# ~~~~~~~~~~~~ 3 ~~~~~~~~~~~~~~~
# malignant

us <- filter(us, Behavior_recode_for_analysis %in% c("Malignant"))
us_dim <- c(us_dim, nrow(us))

# + REGION
us$Region <- rep("USA", nrow(us))

# save subset for incidence analysis
write_csv(us, path = file.path(paste0("./data/case_listing/", cancer_type), "us_inc.csv"))

# ~~~~~~~~~~~~ 4 ~~~~~~~~~~~~~~~
# microscopically confirmed

us <- filter(us, Diagnostic_Confirmation %in% list_of_diag_conf)
us_dim <- c(us_dim, nrow(us))

# ~~~~~~~~~~~~ 5 ~~~~~~~~~~~~~~~
# first primary

us <- filter(us, Sequence_number %in% c("One primary only", "1st of 2 or more primaries"))
us_dim <- c(us_dim, nrow(us))

# ~~~~~~~~~~~~ 6 ~~~~~~~~~~~~~~~
# active follow-up

us <- filter(us, Type_of_followup_expected %in% c("Active follow-up"))
us_dim <- c(us_dim, nrow(us))

# ~~~~~~~~~~~~ 7 ~~~~~~~~~~~~~~~
# reporting source

us <- filter(us, ! Type_of_Reporting_Source %in% c("Autopsy only", "Death certificate only"))
us_dim <- c(us_dim, nrow(us))

# save subset for survival analysis
write_csv(us, path = file.path(paste0("./data/case_listing/", cancer_type), "us_surv.csv"))


# for Registry ------------------------------------------------------------

reg_dim <- NA

# ~~~~~~~~~~~~ 1 ~~~~~~~~~~~~~~~
# All cancer diagnoses in SEER registries for selected age groups and years

reg <- seer %>% 
  dplyr::filter(Age %in% age_groups) %>% 
  dplyr::filter(Year_of_diagnosis %in% years) %>% 
  dplyr::filter(grepl(pattern = registry, x = SEER_registry))

reg_dim <- nrow(reg)

# ~~~~~~~~~~~~ 2 ~~~~~~~~~~~~~~~
# cancer type selection

# select histology type codes in accordance with icd-o-3 codes
reg <- filter(reg, Primary_Site %in% icdo3_recode$Primary_Site)

reg$Histology_select <- NA
for(i in 1:nrow(reg)){
  if(reg$Histologic_Type_ICDO3[i] %in% eval(parse(text=paste0("c(", reg$Histology_exclude[i], ")")))){
    reg$Histology_select[i] <- "exclude" 
  } else reg$Histology_select[i] <- "select" 
}

reg <- filter(reg, Histology_select %in% c("select"))
reg_dim <- c(reg_dim, nrow(reg))

# ~~~~~~~~~~~~ 3 ~~~~~~~~~~~~~~~
# malignant

reg <- filter(reg, Behavior_recode_for_analysis %in% c("Malignant"))
reg_dim <- c(reg_dim, nrow(reg))

# + REGION
reg$Region <- rep(paste0(registry), nrow(reg))

# save subset for incidence analysis
write_csv(reg, path = file.path(paste0("./data/case_listing/", cancer_type), "reg_inc.csv"))

# ~~~~~~~~~~~~ 4 ~~~~~~~~~~~~~~~
# microscopically confirmed

reg <- filter(reg, Diagnostic_Confirmation %in% list_of_diag_conf)
reg_dim <- c(reg_dim, nrow(reg))

# ~~~~~~~~~~~~ 5 ~~~~~~~~~~~~~~~
# first primary

reg <- filter(reg, Sequence_number %in% c("One primary only", "1st of 2 or more primaries"))
reg_dim <- c(reg_dim, nrow(reg))

# ~~~~~~~~~~~~ 6 ~~~~~~~~~~~~~~~
# active follow-up

reg <- filter(reg, Type_of_followup_expected %in% c("Active follow-up"))
reg_dim <- c(reg_dim, nrow(reg))

# ~~~~~~~~~~~~ 7 ~~~~~~~~~~~~~~~
# reporting source

reg <- filter(reg, ! Type_of_Reporting_Source %in% c("Autopsy only", "Death certificate only"))
reg_dim <- c(reg_dim, nrow(reg))

# save subset for survival analysis
write_csv(reg, path = file.path(paste0("./data/case_listing/", cancer_type), "reg_surv.csv"))


# FLOW CHARTS: ------------------------------------------------------------

years_chr <- paste(min(years), "-", max(years))
select_incidence <- c(rep("o", 3), rep("x", 4))
select_survival  <- rep("o", 7)


# for USA -----------------------------------------------------------------

select_criteria_us <- c(paste("All cancer diagnoses in SEER registries for selected age groups,", years_chr),
                        paste(cancer_type, "Cancer"),
                        paste("Malignant"),
                        paste("Microscopically confirmed"),
                        paste("First primary: 'One primary only' or '1st of 2 or more primaries'"),
                        paste("Active follow-up"),
                        paste("Reporting Source NOT 'Autopsy only' and NOT 'Death certificate only'")
)

flow_chart_us <- data.frame(`Step`               = seq_along(us_dim),
                            `Selection Criteria` = select_criteria_us,
                            `Incidence Rates`    = select_incidence,
                            `Survival Analyses`  = select_survival,
                            `Number Selected`    = format(us_dim, big.mark = ","),
                            `Number Excluded`    = NA
                            )
# column 'Number Excluded'
flow_chart_us[1, 6] = 0
for(i in 1:(length(us_dim) - 1)){
  flow_chart_us[i+1,6] = format(us_dim[i] - us_dim[i+1], big.mark = ",")
}

names(flow_chart_us) <- c("Step", "Selection Criteria", "Incidence Rates", "Survival Analyses", 
                          "Number Selected", "Number Excluded")
flow_chart_us


# for Registry ------------------------------------------------------------
select_criteria_reg    <- select_criteria_us
select_criteria_reg[1] <- paste("All cancer diagnoses in", registry, "registry, for selected age groups", years_chr)
  
flow_chart_reg <- data.frame(`Step`              = seq_along(reg_dim),
                            `Selection Criteria` = select_criteria_reg,
                            `Incidence Rates`    = select_incidence,
                            `Survival Analyses`  = select_survival,
                            `Number Selected`    = format(reg_dim, big.mark = ","),
                            `Number Excluded`    = NA
)  

# column 'Number Excluded'
flow_chart_reg[1, 6] = 0
for(i in 1:(length(reg_dim) - 1)){
  flow_chart_reg[i+1,6] = format(reg_dim[i] - reg_dim[i+1], big.mark = ",")
}

names(flow_chart_reg) <- c("Step", "Selection Criteria", "Incidence Rates", "Survival Analyses", 
                           "Number Selected", "Number Excluded")
flow_chart_reg

# save Flow Charts 
# as workspace for use in the report
save(flow_chart_us, flow_chart_reg, file = file.path(paste0("./data/intermediate/", cancer_type), "flow_charts.Rdata"))


# TABLES 1 ----------------------------------------------------------------
# based on subsets for incidence analysis

us_inc   <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "us_inc.csv"))
reg_inc  <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "reg_inc.csv"))

# order levels of Summary_stage_2000_1998 factor
# "In situ" level is not among malignant cases
us_inc$Summary_stage_2000_1998  <- factor(us_inc$Summary_stage_2000_1998, 
                                          levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))
reg_inc$Summary_stage_2000_1998 <- factor(reg_inc$Summary_stage_2000_1998,
                                          levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))


(tbl1_ethn_us <- fun.tableone(var_cont   = "Age_at_diagnosis", 
                        var_cat    = c("Sex", "Summary_stage_2000_1998", "Stage_Grade"), 
                        var_strata = "Race_Ethnicity", us_inc))

(tbl1_ethn_reg <- fun.tableone(var_cont   = "Age_at_diagnosis", 
                        var_cat    = c("Sex", "Summary_stage_2000_1998", "Stage_Grade"), 
                        var_strata = "Race_Ethnicity", reg_inc))

(tbl1_sex_us <- fun.tableone(var_cont   = "Age_at_diagnosis", 
                              var_cat    = c("Summary_stage_2000_1998", "Stage_Grade"), 
                              var_strata = "Sex", us_inc))

(tbl1_sex_reg <- fun.tableone(var_cont   = "Age_at_diagnosis", 
                               var_cat    = c("Summary_stage_2000_1998", "Stage_Grade"), 
                               var_strata = "Sex", reg_inc))


df_inc <- rbind(us_inc, reg_inc)
(tbl1_region <- fun.tableone(var_cont   = "Age_at_diagnosis", 
                              var_cat    = c("Sex", "Summary_stage_2000_1998", "Stage_Grade"), 
                              var_strata = "Region", df_inc))

# save different Table 1's
# as workspace for use in the report
save(tbl1_ethn_us, tbl1_ethn_reg, tbl1_sex_us, tbl1_sex_reg, tbl1_region,
     file = file.path(paste0("./data/intermediate/", cancer_type), "table1.Rdata"))


# POPULATION DATA ---------------------------------------------------------

# dictionary: https://seer.cancer.gov/popdata/popdic.html 
# webpage: https://seer.cancer.gov/popdata/download.html - from column "1990-2015 4 Expanded Races by Origin"
# file: https://seer.cancer.gov/popdata/yr1990_2015.19ages/us.1990_2015.19ages.adjusted.txt.gz

# download file
file <- basename(url_population)

if(! file.exists(gsub(".gz", "", file.path("./data/public", file)))){
  download.file(url_population, file.path("./data/public", file))
  R.utils::gunzip(file.path("./data/public", file))
  }

# SEER areas Population information 
# "us.1990_2015.19ages.adjusted.txt" file
pop <- read_fwf(file = file.path(gsub(".gz", "", file.path("./data/public", file))), 
                fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                           col_names=c("Year", "State", "StateFIPS", "CountyFIPS", "Registry", "Race", 
                                       "Origin", "Sex", "Age", "Population")))

# Race, 1990+ data:
# 1 = White; 2 = Black; 3 = American Indian/Alaska Native; 4 = Asian or Pacific Islander

# Origin, Applicable to 1990+ data:
# 0 = Non-Hispanic; 1 = Hispanic; 9 = Not applicable in 1969-2011 W,B,O files

# Sex:
# 1 = Male; 2 = Female

# Registry: 
# 99 = Registry for non-SEER area  (exclude)
# 33 (AZ) = Arizona Indians        (exclude, because not available yet)
# 29 (AK) = Alaska Natives         (keep only for Race==3)

# subset
pop <- pop %>% 
  dplyr::filter(Year %in% c(years)) %>% 
  dplyr::filter(!(Registry %in% c(99, 33))) %>% 
  dplyr::filter(!(Registry %in% c(29) & Race %in% c(1, 2, 4)))

pop$Population <- as.numeric(pop$Population)

pop[pop$Sex == 1, 'Gender'] <- "Male"
pop[pop$Sex == 2, 'Gender'] <- "Female"

pop[pop$Race == 3, 'Race_Ethnicity'] <- "American Indian or Alaska Native"
pop[pop$Race == 1 & pop$Origin == 1, 'Race_Ethnicity'] <- "Hispanic"
pop[pop$Race == 1 & pop$Origin == 0, 'Race_Ethnicity'] <- "Non-Hispanic White"
pop$Race_Ethnicity[is.na(pop$Race_Ethnicity)] <- "Other"

pop <- pop %>% dplyr::mutate(Age = fct_recode(Age,
#      new      /  old 
  "00 years"    = "00",
  "01-04 years" = "01", 
  "05-09 years" = "02", 
  "10-14 years" = "03", 
  "15-19 years" = "04", 
  "20-24 years" = "05",
  "25-29 years" = "06", 
  "30-34 years" = "07", 
  "35-39 years" = "08", 
  "40-44 years" = "09", 
  "45-49 years" = "10", 
  "50-54 years" = "11", 
  "55-59 years" = "12", 
  "60-64 years" = "13", 
  "65-69 years" = "14",
  "70-74 years" = "15", 
  "75-79 years" = "16", 
  "80-84 years" = "17", 
  "85+ years"   = "18"
))

# save
save(pop, file = file.path(paste0("./data/intermediate/", cancer_type), "popdata.Rdata")) 


# STANDARD POPULATION DATA ------------------------------------------------

# dictionary : https://seer.cancer.gov/stdpopulations/stdpopdic.html
# webpage: https://seer.cancer.gov/stdpopulations/ - 2000 standard population (for age-adjusted rates)
# "Standard Populations - 19 Age Groups (0, 1-4, 5-9, 10-14, ..., 85+)"
# file: https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt

# download file
file <- basename(url_std_population)

if(! file.exists(gsub(".gz", "", file.path("./data/public", file)))){
  download.file(url_std_population, file.path("./data/public", file))
  }

# 2000 US Standard Population (Census P25-1130)
# "stdpop.19ages.txt" file
pop_std <- read_fwf(file = file.path("./data/public/stdpop.19ages.txt"), 
                fwf_widths(c(3, 3, 8),
                           col_names=c("Standard", "Age", "Population")))

pop_std$Population <- as.numeric(pop_std$Population)

pop_std <- pop_std %>% dplyr::mutate(Age = fct_recode(Age,
#      new    /   old 
"00 years"    = "000",
"01-04 years" = "001", 
"05-09 years" = "002", 
"10-14 years" = "003", 
"15-19 years" = "004", 
"20-24 years" = "005",
"25-29 years" = "006", 
"30-34 years" = "007", 
"35-39 years" = "008", 
"40-44 years" = "009", 
"45-49 years" = "010", 
"50-54 years" = "011", 
"55-59 years" = "012", 
"60-64 years" = "013", 
"65-69 years" = "014",
"70-74 years" = "015", 
"75-79 years" = "016", 
"80-84 years" = "017", 
"85+ years"   = "018"
))

# use code 203
pop_std <- pop_std %>% dplyr::filter(Standard %in% c("203")) %>% 
  dplyr::select(-Standard)

# save 
save(pop_std, file = file.path(paste0("./data/intermediate/", cancer_type), "pop_std.Rdata")) 


# CLEAN MEMORY ------------------------------------------------------------

rm(list = ls()[!(ls() %in% c('years', 'age_groups', 'cancer_type', 'registry', 'subset_yn', 'race_ethn_yn', 
                             'map_yn', 'ajcc_grade', 'icdo3_recode'))])
gc()
