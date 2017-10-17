# INPUT FILES -------------------------------------------------------------

# subset.R
# functions.R
# mapcoord.Rdata
# generated during 'subset.R' run: 
# us_inc.csv     - subset of particular cancer cases for incidence analysis in 17 registries other then the registry of interest
# reg_inc.csv    - subset of particular cancer cases for incidence analysis in the registry of interest
# us_surv.csv    - subset of particular cancer cases for survival analysis in 17 registries other then the registry of interest
# reg_surv.csv   - subset of particular cancer cases for survival analysis in the registry of interest
# popdata.Rdata  - subset of population data
# pop_std.Rdata  - 2000 standard population


# OUTPUT FILES ------------------------------------------------------------
# PNG plots
# results.RData


# CREATE SUBFOLDERS -------------------------------------------------------
# additional subfolders

ifelse(!dir.exists(paths = file.path("./data")), 
       dir.exists(paths = file.path("./data")), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path("./figures")), 
       dir.exists(paths = file.path("./figures")), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path("./data/case_listing")), 
       dir.exists(paths = file.path("./data/case_listing")), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path("./data/intermediate")), 
       dir.exists(paths = file.path("./data/intermediate")), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path(paste0("./data/case_listing/", cancer_type))), 
       dir.create(path  = file.path(paste0("./data/case_listing/", cancer_type))), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path(paste0("./data/intermediate/", cancer_type))), 
       dir.create(path  = file.path(paste0("./data/intermediate/", cancer_type))), 
       paste0("Folder already exists"))

ifelse(!dir.exists(paths = file.path(paste0("./figures/", cancer_type))), 
       dir.create(path  = file.path(paste0("./figures/", cancer_type))), 
       paste0("Folder already exists"))


# CALL 'subset.R' ---------------------------------------------------------

# modifiable variable from 'attributes.R'
if(subset_yn == "Yes"){
  source("./code/generic/subset.R")} 


# CALL 'functions.R' ------------------------------------------------------

# some user-defined functions
source("./code/generic/functions.R")


# LOAD SUBSETS ------------------------------------------------------------

# these subsets were generated in 'subset.R' part of the code

# for incidence analysis
us_inc   <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "us_inc.csv"))
reg_inc  <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "reg_inc.csv"))

# for survival analysis
us_surv  <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "us_surv.csv"))
reg_surv <- read.csv(file = file.path(paste0("./data/case_listing/", cancer_type), "reg_surv.csv"))

# for Regions comparison
df_inc <- rbind(us_inc, reg_inc)
df_surv <- rbind(us_surv, reg_surv)


# LOAD POPDATA ------------------------------------------------------------

load(file = file.path(paste0("./data/intermediate/", cancer_type), "popdata.Rdata"))


# LOAD STD POP ------------------------------------------------------------

load(file = file.path(paste0("./data/intermediate/", cancer_type), "pop_std.Rdata"))


# LOAD FLOW CHARTS --------------------------------------------------------

load(file = file.path(paste0("./data/intermediate/", cancer_type), "flow_charts.Rdata"))


# LOAD TABLE 1 ------------------------------------------------------------

load(file = file.path(paste0("./data/intermediate/", cancer_type), "table1.Rdata"))


# ORDER LEVELS OF FACTORS -------------------------------------------------

#  1 
# for 'Summary_stage_2000_1998' (incidence subsets)

# for incidence analysis
us_inc$Summary_stage_2000_1998   <- factor(us_inc$Summary_stage_2000_1998, 
                                           levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))
reg_inc$Summary_stage_2000_1998  <- factor(reg_inc$Summary_stage_2000_1998,
                                           levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))
# for survival analysis
us_surv$Summary_stage_2000_1998  <- factor(us_surv$Summary_stage_2000_1998, 
                                           levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))
reg_surv$Summary_stage_2000_1998 <- factor(reg_surv$Summary_stage_2000_1998,
                                           levels = c("Localized", "Regional", "Distant", "Unknown/unstaged", "Blank(s)"))

#  2 
# for 'COD_to_site_recode' (survival subsets)
alive_level <- "Alive"
other_levels_us <- levels(us_surv$COD_to_site_recode)[which(!(levels(us_surv$COD_to_site_recode) == "Alive"))]
other_levels_reg <- levels(reg_surv$COD_to_site_recode)[which(!(levels(reg_surv$COD_to_site_recode) == "Alive"))]

us_surv$COD_to_site_recode <- factor(us_surv$COD_to_site_recode, levels= c(alive_level, other_levels_us))
reg_surv$COD_to_site_recode <- factor(reg_surv$COD_to_site_recode, levels= c(alive_level, other_levels_reg))

#  3
# for 'Region'
df_inc$Region  <- factor(df_inc$Region, levels = c("USA", registry))
df_surv$Region <- factor(df_surv$Region, levels = c("USA", registry))


# * BASED ON INCIDENCE SUBSETS: -------------------------------------------

# CONTINGENCY TABLES ------------------------------------------------------

# w/ind. rows: by Year and Gender
(year_sex_us  <- fun.tableone_by_rows(df = us_inc, var_cat = "Year_of_diagnosis", var_strata = "Sex"))
(year_sex_reg <- fun.tableone_by_rows(df = reg_inc, var_cat = "Year_of_diagnosis", var_strata = "Sex"))

# w/ind. columns: by Site and Race/Ethnicity
(site_ethn_us  <- fun.tableone(df = us_inc, var_cat = "Site_recode", var_strata = "Race_Ethnicity", var_cont = NULL))
(site_ethn_reg <- fun.tableone(df = reg_inc, var_cat = "Site_recode", var_strata = "Race_Ethnicity", var_cont = NULL))

# w/ind. columns: by Race/Ethnicity and Gender
(ethn_sex_reg <- fun.tableone(df = reg_inc, var_cat = "Race_Ethnicity", var_strata = "Sex", var_cont = NULL))
(ethn_sex_us  <- fun.tableone(df = us_inc, var_cat = "Race_Ethnicity", var_strata = "Sex", var_cont = NULL))

# w/ind. columns: by Summary Stage and Gender
(sumstage_sex_reg <- fun.tableone(df = reg_inc, var_cat = "Summary_stage_2000_1998", var_strata = "Sex", var_cont = NULL))
(sumstage_sex_us  <- fun.tableone(df = us_inc, var_cat = "Summary_stage_2000_1998", var_strata = "Sex", var_cont = NULL))

# w/ind. columns: by Stage/Grade and Gender
(stagegrade_sex_reg <- fun.tableone(df = reg_inc, var_cat = "Stage_Grade", var_strata = "Sex", var_cont = NULL))
(stagegrade_sex_us  <- fun.tableone(df = us_inc, var_cat = "Stage_Grade", var_strata = "Sex", var_cont = NULL))

# w/ind. columns: by Summary Stage and Race/Ethnicity
(sumstage_ethn_us  <- fun.tableone(df = us_inc, var_cat = "Summary_stage_2000_1998", var_strata = "Race_Ethnicity", var_cont = NULL))
(sumstage_ethn_reg <- fun.tableone(df = reg_inc, var_cat = "Summary_stage_2000_1998", var_strata = "Race_Ethnicity", var_cont = NULL))

# w/ind. columns: by Stage/Grade and Race/Ethnicity
(stagegrade_ethn_us  <- fun.tableone(df = us_inc, var_cat = "Stage_Grade", var_strata = "Race_Ethnicity", var_cont = NULL))
(stagegrade_ethn_reg <- fun.tableone(df = reg_inc, var_cat = "Stage_Grade", var_strata = "Race_Ethnicity", var_cont = NULL))


# SUMMARY TABLES ----------------------------------------------------------

# for Age_at_diagnosis

(age_region  <- fun.summary_group(df = df_inc, var1 = Region, var2 = Age_at_diagnosis))

# for Age_at_diagnosis by Race/Ethnicity
# anova p-value
(age_ethn_us      <- fun.summary_group(df = us_inc, var1 = Race_Ethnicity, var2 = Age_at_diagnosis))

(pval_age_ethn_us <- fun.pval_anova(df = us_inc, var1 = "Race_Ethnicity", var2 = "Age_at_diagnosis",
                                    levels1 = c("American Indian or Alaska Native", "Hispanic", "Non-Hispanic White")))

(age_ethn_reg      <- fun.summary_group(df = reg_inc, var1 = Race_Ethnicity, var2 = Age_at_diagnosis))

(pval_age_ethn_reg <- fun.pval_anova(df = reg_inc, var1 = "Race_Ethnicity", var2 = "Age_at_diagnosis",
                                     levels1 = c("American Indian or Alaska Native", "Hispanic", "Non-Hispanic White")))

# for Age_at_diagnosis by Stage/Grade
# anova p-value
(age_stagegrade_us      <- fun.summary_group(df = us_inc, var1 = Stage_Grade, var2 = Age_at_diagnosis))

(pval_age_stagegrade_us <- fun.pval_anova(df = us_inc, var1 = "Stage_Grade", var2 = "Age_at_diagnosis",
                                          levels1 = c("1", "2", "3", "4")))

(age_stagegrade_reg      <- fun.summary_group(df = reg_inc, var1 = Stage_Grade, var2 = Age_at_diagnosis))

(pval_age_stagegrade_reg <- fun.pval_anova(df = reg_inc, var1 = "Stage_Grade", var2 = "Age_at_diagnosis",
                                           levels1 = c("1", "2", "3", "4")))


# DESCRIPTIVE PLOTS -------------------------------------------------------

# Plot: Proportion of Sites

sites_distr_us <- us_inc %>% 
  dplyr::group_by(Site_recode) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::arrange(-counts) %>% 
  dplyr::mutate(Site_recode = factor(Site_recode, Site_recode)) %>% 
  dplyr::mutate(prop = counts / sum(counts)) 

sites_distr_reg <- reg_inc %>% 
  dplyr::group_by(Site_recode) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::arrange(-counts) %>% 
  dplyr::mutate(Site_recode = factor(Site_recode, Site_recode)) %>% 
  dplyr::mutate(prop = counts / sum(counts)) 


plot.distr_sites <- function(df, title){
  ggplot(data = df, aes(x = Site_recode, y = prop)) +
    geom_bar(stat="identity", fill = "grey") +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(df$prop)), breaks = seq(0, max(df$prop), by = 0.05)) +
    geom_text(aes(y = 0.02, label = paste0(roundHAFZ(prop*100), '%')),   
              position = position_dodge(width = .9), size = 3, color="black") +
    labs(title = title, x = NULL, y = NULL) +
    coord_flip() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

png(file = file.path(paste0("./figures/", cancer_type, "/distr_sites.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.distr_sites(sites_distr_us, "USA"), 
                   plot.distr_sites(sites_distr_reg, registry),
                   nrow = 1, ncol = 2))
dev.off()


# Plot: Frequency by Gender and Year

sex_year_freq_us <- us_inc %>% 
  dplyr::group_by(Sex, Year_of_diagnosis) %>% 
  dplyr::summarize(counts = n()) 

sex_year_freq_reg <- reg_inc %>% 
  dplyr::group_by(Sex, Year_of_diagnosis) %>% 
  dplyr::summarize(counts = n()) 

plot.freq_sex_year <- function(df, title){
  ggplot(data = df, aes(x = Year_of_diagnosis, y = counts, label = counts)) +
    geom_line(aes(color = Sex), size = 1) +
    geom_point(aes(shape = Sex, color = Sex), size = 2) +
    geom_text(size = 3, vjust = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, max(df$counts))) +
    labs(title = title, x = NULL, y = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom") 
}

png(file = file.path(paste0("./figures/", cancer_type, "/freq_sex_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.freq_sex_year(sex_year_freq_us, "USA"), 
                   plot.freq_sex_year(sex_year_freq_reg, registry),
                   nrow = 1, ncol = 2))
dev.off()


# Plot: Frequency by Race/Ethn and Year

ethn_year_freq_us <- us_inc %>% 
  dplyr::group_by(Race_Ethnicity, Year_of_diagnosis) %>% 
  dplyr::summarize(counts = n()) 

ethn_year_freq_reg <- reg_inc %>% 
  dplyr::group_by(Race_Ethnicity, Year_of_diagnosis) %>% 
  dplyr::summarize(counts = n()) 


plot.freq_ethn_year <- function(df, title){
  ggplot(data = df, aes(x = Year_of_diagnosis, y = counts)) +
    geom_line(aes(color = Race_Ethnicity), size = 1) +
    geom_point(aes(shape = Race_Ethnicity, color = Race_Ethnicity), size = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, max(df$counts))) +
    labs(title = title, x = NULL, y = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE, title = "Race/Ethnicity")) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE, title = "Race/Ethnicity"))
}

png(file = file.path(paste0("./figures/", cancer_type, "/freq_ethn_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.freq_ethn_year(ethn_year_freq_us, "USA"), 
                   plot.freq_ethn_year(ethn_year_freq_reg, registry),
                   nrow = 1, ncol = 2))
dev.off()


# Plot: Proportion by Race/Ethn and Year

plot.prop_ethn_year <- function(df, title){
  ggplot(data = df, aes(x = as.factor(df$Year_of_diagnosis), y = counts, fill = Race_Ethnicity)) +
    geom_bar(stat="identity", position = "fill") +
    labs(title = title, x = NULL, y = "Proportion") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, title = "Race/Ethnicity"))
}

png(file = file.path(paste0("./figures/", cancer_type, "/prop_ethn_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.prop_ethn_year(ethn_year_freq_us, "USA"), 
                   plot.prop_ethn_year(ethn_year_freq_reg, registry),
                   nrow = 1, ncol = 2))
dev.off()


# Plot: Distribution of Age by Race/Etnicity 

plot.distr_age_ethn <- function(df){
  ggplot(df, aes(x = Age_at_diagnosis)) + 
    geom_histogram(aes(y = ..density..), binwidth=5, colour="black", fill="white") +
    labs(x = "Age at diagnosis") +
    facet_wrap(Region ~ Race_Ethnicity, nrow=2) +
    theme_minimal()
}

png(file = file.path(paste0("./figures/", cancer_type, "/distr_age_ethn.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.distr_age_ethn(df_inc))
dev.off()


# Plot: Distribution of Age by Stage/Grade

plot.distr_age_stage <- function(df){
  ggplot(df, aes(x = Age_at_diagnosis)) + 
    geom_histogram(aes(y = ..density..), binwidth=5, colour="black", fill="white") +
    labs(x = "Age at diagnosis") +
    facet_wrap(Region ~ Stage_Grade, nrow=2) +
    theme_minimal()
}

png(file = file.path(paste0("./figures/", cancer_type, "/distr_age_stage.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.distr_age_stage(df_inc))
dev.off()


# AGE-ADJUSTED RATES ------------------------------------------------------

# : https://seer.cancer.gov/popdata/popdic.html
registry_code <- tribble(
  ~ Registry_name, ~Registry,
  "San Francisco-Oakland SMSA",              1,
  "Connecticut",                             2, 
  "Detroit (Metropolitan)",                  20, 
  "Hawaii",                                  21, 
  "Iowa",                                    22,
  "New Mexico",                              23,
  "Seattle (Puget Sound)",                   25,
  "Utah",                                    26,
  "Atlanta (Metropolitan)",                  27,
  "Alaska Natives",                          29, 
  "San Jose-Monterey",                       31,
  "Los Angeles",                             35, 
  "Rural Georgia",                           37, 
  "California excluding SF/SJM/LA",          41,
  "Kentucky",                                42, 
  "Louisiana",                               43,
  "New Jersey",                              44,
  "Georgia excluding Atlanta/Rural Georgia", 47 
)

pop <- left_join(pop, registry_code, by = "Registry")

pop_us  <- pop %>% dplyr::filter(!(Registry_name == registry))
pop_reg <- pop %>% dplyr::filter(Registry_name == registry)


# for All cases -----------------------------------------------------------

pop_all_us <- pop_us %>% 
  dplyr::group_by(Age, Year) %>% 
  dplyr::summarise(Population = sum(Population)) %>% 
  tidyr::spread(key = Year, value = Population)

pop_all_reg <- pop_reg %>% 
  dplyr::group_by(Age, Year) %>% 
  dplyr::summarise(Population = sum(Population)) %>% 
  tidyr::spread(key = Year, value = Population)

(aarate_all_us  <- fun.aarate(df = us_inc,  pop = pop_all_us))
names(aarate_all_us) <- c("Year", paste0(names(aarate_all_us)[-1], " (All)"))
(aarate_all_reg <- fun.aarate(df = reg_inc, pop = pop_all_reg))
names(aarate_all_reg) <- c("Year", paste0(names(aarate_all_reg)[-1], " (All)"))


# by Sex ------------------------------------------------------------------

levels_sex <- levels(as.factor(pop_us$Gender))

fun.sex_aarates <- function(df, df_pop){
  temp <- data.frame(matrix(nrow = length(years) + 1, ncol = 0))
  temp$Year <- c(years, "APC (p-value)")
  for(i in 1:length(levels_sex)){
    pop_temp <-  df_pop %>% 
      dplyr::filter(Gender == levels_sex[i]) %>% 
      dplyr::group_by(Age, Year) %>% 
      dplyr::summarise(Population = sum(Population)) %>% 
      tidyr::spread(key = Year, value = Population)
    aarates <- fun.aarate(df = subset(df, Sex == levels_sex[i]),  pop = pop_temp)
    names(aarates) <- c("Year", paste0(names(aarates)[-1], " (", levels_sex[i], ")"))
    temp  <- left_join(temp, aarates, by = "Year") 
  }
  return(temp)
}

(aarate_sex_us <- fun.sex_aarates(df = us_inc, df_pop = pop_us))
(aarate_sex_reg <- fun.sex_aarates(df = reg_inc, df_pop = pop_reg))


# by Race/Ethnicity -------------------------------------------------------

levels_ethn <- levels(as.factor(pop_us$Race_Ethnicity))

fun.ethn_aarates <- function(df, df_pop){
  temp <- data.frame(matrix(nrow = length(years) + 1, ncol = 0))
  temp$Year <- c(years, "APC (p-value)")
  for(i in 1:length(levels_ethn)){
    pop_temp <-  df_pop %>% 
      dplyr::filter(Race_Ethnicity == levels_ethn[i]) %>% 
      dplyr::group_by(Age, Year) %>% 
      dplyr::summarise(Population = sum(Population)) %>% 
      tidyr::spread(key = Year, value = Population)
    aarates <- fun.aarate(df = subset(df, Race_Ethnicity == levels_ethn[i]),  pop = pop_temp)
    names(aarates) <- c("Year", paste0(names(aarates)[-1], " (", abbreviate(levels_ethn[i]), ")"))
    temp  <- left_join(temp, aarates, by = "Year") 
  }
  return(temp)
}

(aarate_ethn_us <- fun.ethn_aarates(df = us_inc, df_pop = pop_us))
(aarate_ethn_reg <- fun.ethn_aarates(df = reg_inc, df_pop = pop_reg))


# by Sites ----------------------------------------------------------------

levels_site <- levels(as.factor(us_inc$Site_recode))

fun.site_aarates <- function(df, df_pop){
  temp <- data.frame(matrix(nrow = length(years), ncol = 0))
  temp$Year <- as.character(c(years))
  for(i in 1:length(levels_site)){
    aarates <- fun.aarate(df = subset(df, Site_recode == levels_site[i]),  pop = df_pop)[, 1:2]
    aarates <- aarates %>% dplyr::filter(Year %in% years)
    names(aarates) <- c("Year", levels_site[i])
    temp  <- left_join(temp, aarates, by = "Year") 
  }
  return(temp)
}

(aarate_site_us <- fun.site_aarates(df = us_inc, df_pop = pop_all_us))
(aarate_site_reg <- fun.site_aarates(df = reg_inc, df_pop = pop_all_reg))


# PREPARE AGE-ADJUSTED RATE' TABLES TO PLOT RATES -------------------------


# for All cases -----------------------------------------------------------

aarate_all_us2 <- aarate_all_us %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::select(Year, contains("All")) %>% 
  dplyr::filter(Year %in% years)
names(aarate_all_us2) <- c("Year" , "All")

aarate_all_reg2 <- aarate_all_reg %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::select(Year, contains("All")) %>% 
  dplyr::filter(Year %in% years)
names(aarate_all_reg2) <- c("Year" , "All")


# by Gender ---------------------------------------------------------------

aarate_sex_us2 <- aarate_sex_us %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::select(-contains("All")) %>% 
  dplyr::filter(Year %in% years)
names_aarate_sex_us2  <- names(aarate_sex_us2)
names(aarate_sex_us2) <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", names_aarate_sex_us2, perl=T)
names(aarate_sex_us2)[1] <- "Year" 
aarate_sex_us2 <- aarate_sex_us2 %>% 
  tidyr::gather(names(aarate_sex_us2)[-1], key = "group", value = "rates")

aarate_sex_reg2 <- aarate_sex_reg %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::select(-contains("All")) %>% 
  dplyr::filter(Year %in% years)
names_aarate_sex_reg2  <- names(aarate_sex_reg2)
names(aarate_sex_reg2) <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", names_aarate_sex_reg2, perl=T)
names(aarate_sex_reg2)[1] <- "Year" 
aarate_sex_reg2 <- aarate_sex_reg2 %>% 
  tidyr::gather(names(aarate_sex_reg2)[-1], key = "group", value = "rates")


# by Race/Ethnicity -------------------------------------------------------

aarate_ethn_us2 <- aarate_ethn_us %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::filter(Year %in% years)
names_aarate_ethn_us2 <- names(aarate_ethn_us2)
names(aarate_ethn_us2) <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", names_aarate_ethn_us2, perl=T)
names(aarate_ethn_us2)[1] <- "Year" 
aarate_ethn_us2 <- aarate_ethn_us2 %>% 
  tidyr::gather(names(aarate_ethn_us2)[-1], key = "group", value = "rates")

aarate_ethn_reg2 <- aarate_ethn_reg %>% 
  dplyr::select(Year, contains("rate")) %>% 
  dplyr::filter(Year %in% years)
names_aarate_ethn_reg2<- names(aarate_ethn_reg2)
names(aarate_ethn_reg2) <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", names_aarate_ethn_reg2, perl=T)
names(aarate_ethn_reg2)[1] <- "Year" 
aarate_ethn_reg2 <- aarate_ethn_reg2 %>% 
  tidyr::gather(names(aarate_ethn_reg2)[-1], key = "group", value = "rates")


# by Sites ----------------------------------------------------------------

aarate_site_us2 <- aarate_site_us %>% 
  tidyr::gather(names(aarate_site_us)[-1], key = "group", value = "rates")

aarate_site_reg2 <- aarate_site_reg %>% 
  tidyr::gather(names(aarate_site_reg)[-1], key = "group", value = "rates")


# PLOT AGE-ADJUSTED RATES -------------------------------------------------

ylim_max_1 <- 1.1* max(aarate_all_us2$All, aarate_sex_us2$rates, aarate_ethn_us2$rates)


# for All cases -----------------------------------------------------------

plot.aarate_all_year <- function(df, title){
  ggplot(data = df, aes(x = as.numeric(Year), y = All, label = All)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(size = 3, vjust = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, ylim_max_1)) +
    labs(title = title, x = NULL, y = "Age-adjusted rates, per 100,000") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom") 
}

png(file = file.path(paste0("./figures/", cancer_type, "/aarate_all_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.aarate_all_year(aarate_all_us2, "USA"), 
                   plot.aarate_all_year(aarate_all_reg2, registry),
                   nrow = 1, ncol = 2))
dev.off()


# by Gender ---------------------------------------------------------------

plot.aarate_sex_year <- function(df, title){
  ggplot(data = df, aes(x = as.numeric(Year), y = rates, label = rates)) +
    geom_line(aes(color = group), size = 1) +
    geom_point(aes(shape = group, color = group), size = 2) +
    geom_text(size = 3, vjust = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, ylim_max_1)) +
    labs(title = title, x = NULL, y = "Age-adjusted rates, per 100,000", color = "Sex", shape = "Sex") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom")
}

png(file = file.path(paste0("./figures/", cancer_type, "/aarate_sex_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.aarate_sex_year(aarate_sex_us2, "USA"), 
                   plot.aarate_sex_year(aarate_sex_reg2, registry),
                   nrow = 1, ncol = 2))
dev.off()


# by Race/Ethnicity -------------------------------------------------------

plot.aarate_ethn_year <- function(df, title){
  ggplot(data = df, aes(x = as.numeric(Year), y = rates)) +
    geom_line(aes(color = group), size = 1) +
    geom_point(aes(shape = group, color = group), size = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, ylim_max_1)) +
    labs(title = title, x = NULL, y = "Age-adjusted rates, per 100,000", color = "Race/Ethnicity", shape = "Race/Ethnicity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE)) 
}

png(file = file.path(paste0("./figures/", cancer_type, "/aarate_ethn_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.aarate_ethn_year(aarate_ethn_us2, "USA"), 
                   plot.aarate_ethn_year(aarate_ethn_reg2, registry),
                   nrow = 1, ncol = 2))
dev.off()


# by Sites ----------------------------------------------------------------

ylim_max_2 <- 1.1* max(aarate_site_us[, -1])

plot.aarate_site_year <- function(df, title){
  ggplot(data = df, aes(x = as.numeric(Year), y = rates)) +
    geom_line(aes(color = group), size = 1) +
    geom_point(aes(color = group), size = 2) +
    scale_x_continuous(limits = c(min(years), max(years)), breaks = seq(min(years), max(years), by = 1)) +
    scale_y_continuous(limits = c(0, ylim_max_2)) +
    labs(title = title, x = NULL, y = "Age-adjusted rates, per 100,000", color = "Sites") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 4, byrow = TRUE))
}

png(file = file.path(paste0("./figures/", cancer_type, "/aarate_site_year.png")), 
    width = 10, height = 5, units = "in", res = 300, bg = "transparent")
print(grid.arrange(plot.aarate_site_year(aarate_site_us2, "USA"), 
                   plot.aarate_site_year(aarate_site_reg2, registry),
                   nrow = 1, ncol = 2))
dev.off()


# * BASED ON SURVIVAL SUBSETS: --------------------------------------------

# FREQUENCY OF SURVIVAL ---------------------------------------------------
# combination of 2 variables: COD_to_site_recode + SEER_causespecific_death_classification

# USA

alive_us <- us_surv %>% 
  dplyr::filter(COD_to_site_recode == "Alive") %>% 
  dplyr::summarize(Frequency = n())

not_alive_us <- us_surv %>% 
  dplyr::group_by(SEER_causespecific_death_classification) %>% 
  dplyr::summarize(Frequency = n()) %>% 
  dplyr::rename(Vital_status = SEER_causespecific_death_classification)

alive_dead_us <- rbind(cbind(Vital_status = "Alive", Frequency = alive_us), not_alive_us)
alive_dead_us$Vital_status <- as.character(alive_dead_us$Vital_status)
alive_dead_us[2, "Vital_status"] <- "Dead of other cause"
alive_dead_us[2, "Frequency"] <- alive_dead_us[2, "Frequency"] - alive_dead_us[1, "Frequency"]

(alive_dead_us <- alive_dead_us %>% 
    dplyr::mutate(Sum = sum(Frequency)) %>% 
    dplyr::mutate(`%` = roundHAFZ(Frequency / Sum * 100)) %>% 
    dplyr::select(-Sum))
alive_dead_us$Frequency <- format(alive_dead_us$Frequency, big.mark = ",")


# REGISTRY
alive_reg <- reg_surv %>% 
  dplyr::filter(COD_to_site_recode == "Alive") %>% 
  dplyr::summarize(Frequency = n())

not_alive_reg <- reg_surv %>% 
  dplyr::group_by(SEER_causespecific_death_classification) %>% 
  dplyr::summarize(Frequency = n()) %>% 
  dplyr::rename(Vital_status = SEER_causespecific_death_classification)

alive_dead_reg <- rbind(cbind(Vital_status = "Alive", Frequency = alive_reg), not_alive_reg)
alive_dead_reg$Vital_status <- as.character(alive_dead_reg$Vital_status)
alive_dead_reg[2, "Vital_status"] <- "Dead of other cause"
alive_dead_reg[2, "Frequency"] <- alive_dead_reg[2, "Frequency"] - alive_dead_reg[1, "Frequency"]

(alive_dead_reg <- alive_dead_reg %>% 
    dplyr::mutate(Sum = sum(Frequency)) %>% 
    dplyr::mutate(`%` = roundHAFZ(Frequency / Sum * 100)) %>% 
    dplyr::select(-Sum))
alive_dead_reg$Frequency <- format(alive_dead_reg$Frequency, big.mark = ",")


# FREQUENCY OF DEATH REASONS ----------------------------------------------

# REGISTRY
(death_reason <- fun.tableone(var_cont = NULL, var_cat = "COD_to_site_recode", var_strata = "Race_Ethnicity", df = reg_surv))


# K-M PLOTS ---------------------------------------------------------------

# by Region ---------------------------------------------------------------

png(file = file.path(paste0("./figures/", cancer_type, "/KM_registries_CS.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = df_surv, 
                     strataName = "Region", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_registries_NCS.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = df_surv, 
                     strataName = "Region", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


(KM_registries_cs_tbl <- fun.KM_tbl(df = df_surv, strataName = "Region", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL))
(KM_registries_ncs_tbl <- fun.KM_tbl(df = df_surv, strataName = "Region", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL))


# by Sex ------------------------------------------------------------------

# CS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_sex_CS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv, 
                     strataName = "Sex", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "USA, Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_sex_CS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv, 
                     strataName = "Sex", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

# NCS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_sex_NCS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv, 
                     strataName = "Sex", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "USA, Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_sex_NCS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv, 
                     strataName = "Sex", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Not Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


KM_sex_cs_tbl_us <- fun.KM_tbl(df = us_surv, strataName = "Sex", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_sex_ncs_tbl_us <- fun.KM_tbl(df = us_surv, strataName = "Sex", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

KM_sex_cs_tbl_reg <- fun.KM_tbl(df = reg_surv, strataName = "Sex", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_sex_ncs_tbl_reg <- fun.KM_tbl(df = reg_surv, strataName = "Sex", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

(KM_sex_tbl_us <- rbind(KM_sex_cs_tbl_us, KM_sex_ncs_tbl_us))
(KM_sex_tbl_reg <- rbind(KM_sex_cs_tbl_reg, KM_sex_ncs_tbl_reg))


# by Race/Ethnicity -------------------------------------------------------

# CS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_ethn_CS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv, 
                     strataName = "Race_Ethnicity", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "USA, Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_ethn_CS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv, 
                     strataName = "Race_Ethnicity", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

# NCS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_ethn_NCS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv, 
                     strataName = "Race_Ethnicity", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "USA, Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_ethn_NCS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv, 
                     strataName = "Race_Ethnicity", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Not Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


KM_ethn_cs_tbl_us <- fun.KM_tbl(df = us_surv, strataName = "Race_Ethnicity", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_ethn_ncs_tbl_us <- fun.KM_tbl(df = us_surv, strataName = "Race_Ethnicity", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

KM_ethn_cs_tbl_reg <- fun.KM_tbl(df = reg_surv, strataName = "Race_Ethnicity", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_ethn_ncs_tbl_reg <- fun.KM_tbl(df = reg_surv, strataName = "Race_Ethnicity", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

(KM_ethn_tbl_us <- rbind(KM_ethn_cs_tbl_us, KM_ethn_ncs_tbl_us))
(KM_ethn_tbl_reg <- rbind(KM_ethn_cs_tbl_reg, KM_ethn_ncs_tbl_reg))


# by Summary stage --------------------------------------------------------

us_surv_summstage <- us_surv %>% 
  dplyr::filter(!(Summary_stage_2000_1998 == "Blank(s)"))
us_surv_summstage$Summary_stage_2000_1998 <- droplevels(us_surv_summstage$Summary_stage_2000_1998)

reg_surv_summstage <- reg_surv %>% 
  dplyr::filter(!(Summary_stage_2000_1998 == "Blank(s)"))
reg_surv_summstage$Summary_stage_2000_1998 <- droplevels(reg_surv_summstage$Summary_stage_2000_1998)

# CS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_summstage_CS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_summstage, 
                     strataName = "Summary_stage_2000_1998", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "USA, Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_summstage_CS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_summstage, 
                     strataName = "Summary_stage_2000_1998", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

# NCS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_summstage_NCS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_summstage, 
                     strataName = "Summary_stage_2000_1998", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "USA, Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_summstage_NCS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_summstage, 
                     strataName = "Summary_stage_2000_1998", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Not Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


KM_summstage_cs_tbl_us <- fun.KM_tbl(df = us_surv_summstage, strataName = "Summary_stage_2000_1998", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_summstage_ncs_tbl_us <- fun.KM_tbl(df = us_surv_summstage, strataName = "Summary_stage_2000_1998", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

KM_summstage_cs_tbl_reg <- fun.KM_tbl(df = reg_surv_summstage, strataName = "Summary_stage_2000_1998", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_summstage_ncs_tbl_reg <- fun.KM_tbl(df = reg_surv_summstage, strataName = "Summary_stage_2000_1998", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

(KM_summstage_tbl_us <- rbind(KM_summstage_cs_tbl_us, KM_summstage_ncs_tbl_us))
(KM_summstage_tbl_reg <- rbind(KM_summstage_cs_tbl_reg, KM_summstage_ncs_tbl_reg))


# by Stage/Grade ----------------------------------------------------------

us_surv_stagegrade <- us_surv %>% 
  dplyr::filter(Stage_Grade %in% c(1, 2, 3, 4))
us_surv_stagegrade$Stage_Grade <- droplevels(us_surv_stagegrade$Stage_Grade)

reg_surv_stagegrade <- reg_surv %>% 
  dplyr::filter(Stage_Grade %in% c(1, 2, 3, 4))
reg_surv_stagegrade$Stage_Grade <- droplevels(reg_surv_stagegrade$Stage_Grade)

# CS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_stagegrade_CS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_stagegrade, 
                     strataName = "Stage_Grade", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "USA, Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_stagegrade_CS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_stagegrade, 
                     strataName = "Stage_Grade", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

# NCS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_stagegrade_NCS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_stagegrade, 
                     strataName = "Stage_Grade", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "USA, Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_stagegrade_NCS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_stagegrade, 
                     strataName = "Stage_Grade", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Not Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


KM_stagegrade_cs_tbl_us <- fun.KM_tbl(df = us_surv_stagegrade, strataName = "Stage_Grade", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_stagegrade_ncs_tbl_us <- fun.KM_tbl(df = us_surv_stagegrade, strataName = "Stage_Grade", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

KM_stagegrade_cs_tbl_reg <- fun.KM_tbl(df = reg_surv_stagegrade, strataName = "Stage_Grade", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_stagegrade_ncs_tbl_reg <- fun.KM_tbl(df = reg_surv_stagegrade, strataName = "Stage_Grade", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

(KM_stagegrade_tbl_us <- rbind(KM_stagegrade_cs_tbl_us, KM_stagegrade_ncs_tbl_us))
(KM_stagegrade_tbl_reg <- rbind(KM_stagegrade_cs_tbl_reg, KM_stagegrade_ncs_tbl_reg))


# by Sites ----------------------------------------------------------------

# top 5 (or less) sites by frequency

freq_sites_us <- us_surv %>% 
  dplyr::group_by(Site_recode) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::arrange(-counts) %>% 
  dplyr::mutate(sequence = 1:n()) %>% 
  dplyr::filter(sequence %in% 1:5) %>% 
  dplyr::select(Site_recode)

freq_sites_reg <- reg_surv %>% 
  dplyr::group_by(Site_recode) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::arrange(-counts) %>% 
  dplyr::mutate(sequence = 1:n()) %>% 
  dplyr::filter(sequence %in% 1:5) %>% 
  dplyr::select(Site_recode)

us_surv_freqsites <- us_surv %>% 
  dplyr::filter(Site_recode %in% freq_sites_us$Site_recode)
us_surv_freqsites$Site_recode <- droplevels(us_surv_freqsites$Site_recode)

reg_surv_freqsites <- reg_surv %>% 
  dplyr::filter(Site_recode %in% freq_sites_us$Site_recode)
reg_surv_freqsites$Site_recode <- droplevels(reg_surv_freqsites$Site_recode)


# CS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_freqsites_CS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_freqsites, 
                     strataName = "Site_recode", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = "USA, Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_freqsites_CS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_freqsites, 
                     strataName = "Site_recode", COD = "COD_CS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

# NCS
png(file = file.path(paste0("./figures/", cancer_type, "/KM_freqsites_NCS_us.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = us_surv_freqsites, 
                     strataName = "Site_recode", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = "USA, Not Cause-Specific survival", xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()

png(file = file.path(paste0("./figures/", cancer_type, "/KM_freqsites_NCS_reg.png")), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.KM_strata(df = reg_surv_freqsites, 
                     strataName = "Site_recode", COD = "COD_NCS", survTimeVar = "TimeSurv", 
                     title = paste0(registry, ", Not Cause-Specific survival"), xlab = "Time in months", xTicksBy = 12, xshift = 6, 
                     survTicks = c(12, 24, 60, 120), cutoff = NULL))
dev.off()


KM_freqsites_cs_tbl_us <- fun.KM_tbl(df = us_surv_freqsites, strataName = "Site_recode", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_freqsites_ncs_tbl_us <- fun.KM_tbl(df = us_surv_freqsites, strataName = "Site_recode", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

KM_freqsites_cs_tbl_reg <- fun.KM_tbl(df = reg_surv_freqsites, strataName = "Site_recode", COD = "COD_CS", COD_abbr = "CS", survTimeVar = "TimeSurv", cutoff = NULL)
KM_freqsites_ncs_tbl_reg <- fun.KM_tbl(df = reg_surv_freqsites, strataName = "Site_recode", COD = "COD_NCS", COD_abbr = "NCS", survTimeVar = "TimeSurv", cutoff = NULL)

(KM_freqsites_tbl_us <- rbind(KM_freqsites_cs_tbl_us, KM_freqsites_ncs_tbl_us))
(KM_freqsites_tbl_reg <- rbind(KM_freqsites_cs_tbl_reg, KM_freqsites_ncs_tbl_reg))


# COX PH REGRESSION -------------------------------------------------------

(COX_cs <- fun.Cox_multiv(df = df_surv, varNames =  c("Age_at_diagnosis", "Region", "Race_Ethnicity", "Sex"), 
                          COD = "COD_CS", survTimeVar = "TimeSurv"))
(COX_ncs <- fun.Cox_multiv(df = df_surv, varNames =  c("Age_at_diagnosis", "Region", "Race_Ethnicity", "Sex"), 
                           COD = "COD_NCS", survTimeVar = "TimeSurv"))


# MAP SEER REGISTRIES -----------------------------------------------------

source("./code/generic/map_SEER.R")


# SAVE R OBJECTS ----------------------------------------------------------

save(list = ls(all = TRUE), file =  file.path(paste0("./data/intermediate/", cancer_type), "results_of_analyze.RData"))


# CLEAN MEMORY ------------------------------------------------------------

rm(list = ls()[!(ls() %in% c('cancer_type'))])
gc()
