# ROUND VALUES ------------------------------------------------------------

# R uses 'Round half to even' rule for rounding.
# 'Round half to even' is a tie-breaking rule that is less biased.
# x = c(1.85, 1.54, 1.65, 1.75, 1.85, 1.84, 1)
# (round(x, 1))

# 'Round half away from zero' will be used for results output.
# https://github.com/pedroguarderas/learnR/blob/master/R_base/session_26.R 
roundHAFZ <- function(x, n = 1) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
# (roundHAFZ(x))


# ROUND P-VALUE -----------------------------------------------------------

roundPVAL <- function(pval){
  # present small p-values as "<0.001",
  # other p-values will have 3 digits after decimal point
  
  if(length(pval) == 1){
  pval = roundHAFZ(pval, 3)
  if(pval == 0){
    pval = paste("<0.001")
  } 
  } else{
    for(i in 1:length(pval)){
      if(pval[i] == 0){pval[i] = "<0.001"}
      else pval[i] = pval[i]
    }
  }

  return(pval)
}


# AJCC / GRADES 2004+ -----------------------------------------------------
# only works specifically for SEER data

fun.ajcc_grade <- function(df){
  # creates 'Stage_Grade' variable with 5 possible values = { 1 / 2 / 3 / 4 / Unknown / Other }
  # to unify levels of 2 different variables: AJCC Stage 6th / 7th ed. and Grade
  
  # example:
  # df = seer
  
  if(min(df$Year_of_diagnosis) < 2004){
    stop("Minimum 'Year of diagnosis' has to be not less than 2004.")
  }
  
  if(ajcc_grade == "AJCC" && ajcc_ed == "6th and 7th editions"){
    
    df$Stage_Grade_6th <- NA
    df$Stage_Grade_6th[str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "IV") == 1] <- "4"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "III") == 1] <- "3"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "II")  == 1] <- "2"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "I")   == 1] <- "1"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th)] <- as.character(df$Derived_AJCC_Stage_Group_6th_ed_2004[is.na(df$Stage_Grade_6th)])
    
    df$Stage_Grade_7th <- NA
    df$Stage_Grade_7th[str_count(df$Derived_AJCC_Stage_Group_7th_ed_2010, "IV") == 1] <- "4"
    df$Stage_Grade_7th[is.na(df$Stage_Grade_7th) & str_count(df$Derived_AJCC_Stage_Group_7th_ed_2010, "III") == 1] <- "3"
    df$Stage_Grade_7th[is.na(df$Stage_Grade_7th) & str_count(df$Derived_AJCC_Stage_Group_7th_ed_2010, "II")  == 1] <- "2"
    df$Stage_Grade_7th[is.na(df$Stage_Grade_7th) & str_count(df$Derived_AJCC_Stage_Group_7th_ed_2010, "I")   == 1] <- "1"  
    df$Stage_Grade_7th[is.na(df$Stage_Grade_7th)] <-  as.character(df$Derived_AJCC_Stage_Group_7th_ed_2010[is.na(df$Stage_Grade_7th)])
    
    df$Stage_Grade <- NA
    df$Stage_Grade[df$Year_of_diagnosis %in% 2004:2009] <- df$Stage_Grade_6th[df$Year_of_diagnosis %in% 2004:2009]
    df$Stage_Grade[df$Year_of_diagnosis >= 2010]        <- df$Stage_Grade_7th[df$Year_of_diagnosis >= 2010]
    
    # "UNK Stage" - cases that do not have enough information to be staged.
    df$Stage_Grade[df$Stage_Grade == "UNK Stage"] <- "Unknown"

    # "Other" than 1 / 2 / 3 / 4 / "Unknown": includes "Not applicable" & "OCCULT"
    # "NA" (not applicable) is not an AJCC stage category. It is used as a placeholder for histologies that AJCC does not stage. 
    # In "OCCULT" stage NSCLC, cancer cells are found in the sputum (mucus from the lungs), but tumors are not immediately visible. 
    df$Stage_Grade[!(df$Stage_Grade %in% c("1", "2", "3", "4", "Unknown"))] <- "Other"

  } else if(ajcc_grade == "AJCC" && ajcc_ed == "6th edition"){
    
    df$Stage_Grade_6th <- NA
    df$Stage_Grade_6th[str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "IV") == 1] <- "4"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "III") == 1] <- "3"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "II")  == 1] <- "2"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th) & str_count(df$Derived_AJCC_Stage_Group_6th_ed_2004, "I")   == 1] <- "1"
    df$Stage_Grade_6th[is.na(df$Stage_Grade_6th)] <- as.character(df$Derived_AJCC_Stage_Group_6th_ed_2004[is.na(df$Stage_Grade_6th)])
    
    df$Stage_Grade_7th <- NA
    
    df$Stage_Grade <- df$Stage_Grade_6th
    
    # "UNK Stage" - cases that do not have enough information to be staged.
    df$Stage_Grade[df$Stage_Grade == "UNK Stage"] <- "Unknown"
    
    # "Other" than 1 / 2 / 3 / 4 / "Unknown": includes "Not applicable" & "OCCULT"
    # "NA" (not applicable) is not an AJCC stage category. It is used as a placeholder for histologies that AJCC does not stage. 
    # In "OCCULT" stage NSCLC, cancer cells are found in the sputum (mucus from the lungs), but tumors are not immediately visible. 
    df$Stage_Grade[!(df$Stage_Grade %in% c("1", "2", "3", "4", "Unknown"))] <- "Other"
    
  } else if(ajcc_grade == "Grade"){
    
    df$Stage_Grade_6th <- NA
    df$Stage_Grade_7th <- NA

    df$Stage_Grade <- NA
    df$Stage_Grade[str_count(df$Grade, "IV") == 1] <- "4"
    df$Stage_Grade[is.na(df$Stage_Grade) & str_count(df$Grade, "III") == 1] <- "3"
    df$Stage_Grade[is.na(df$Stage_Grade) & str_count(df$Grade, "II")  == 1] <- "2"
    df$Stage_Grade[is.na(df$Stage_Grade) & str_count(df$Grade, "I")   == 1] <- "1"
    df$Stage_Grade[is.na(df$Stage_Grade)] <- as.character(df$Grade[is.na(df$Stage_Grade)])
    
    # Includes: 'T-cell' / 'B-cell; pre-B; B-precursor' / 'Null cell; non T-non B' / 'NK cell; natural killer cell (1995+)'
    df$Stage_Grade[!(df$Stage_Grade %in% c("1", "2", "3", "4", "Unknown"))] <- "Other"
  }
  return(df)
}


# TABLE 1 -----------------------------------------------------------------

fun.tableone <- function(df, var_cont, var_cat, var_strata){
  
  # example:
  # var_cont = "Age_at_diagnosis"
  # var_cat  = c("Sex", "Summary_stage_2000_1998", "Stage_Grade")
  # var_strata = "Race_Ethnicity"
  # df = reg_inc
  
  if(!is.null(var_cont)){
  df_cont <- data.frame(matrix(nrow = 0, ncol = length(levels(df[, var_strata])) + 2))
  
  for(i in 1:length(var_cont)){
    # continuous variables
    cont_strata <-  data.frame(groups =           aggregate(df[, var_cont[i]] ~ df[, var_strata], df,  mean)[, 1],
                               mean   = roundHAFZ(aggregate(df[, var_cont[i]] ~ df[, var_strata], df,  mean)[, 2]),
                               sd     = paste0("(", roundHAFZ(aggregate(df[, var_cont[i]] ~ df[, var_strata], df,  sd)[, 2]), ")"))
    
    cont_all    <- data.frame(groups = "Overall",
                              mean   = roundHAFZ(mean(df[, var_cont[i]])),
                              sd     = paste0("(", roundHAFZ(sd(df[, var_cont[i]])), ")"))
    
    cont           <- data.frame(rbind(cont_strata, cont_all))
    cont$stat      <- paste(format(cont$mean, big.mark = ","), cont$sd)
    cont           <- cont[, c("groups", "stat")]
    var_name       <- names(df)[which(names(df) %in% var_cont[i])]
    cont           <- t(rbind(cbind(groups = "Characteristics", stat = paste(var_name, "/ (mean(sd))")), cont))
    col_name       <- cont[1, ]
    cont           <- data.frame(cont, row.names = NULL)[-1, ]
    names(cont)    <- col_name
    names(df_cont) <- col_name
    df_cont <- rbind(df_cont, cont)
  } 
  } else df_cont <- NULL
  
  if(!is.null(var_cat)){
  df_cat <- data.frame(matrix(nrow = 0, ncol = length(levels(df[, var_strata])) + 2))
  
  for(i in 1:length(var_cat)){
    # categorical variables
    cat_strata          <- data.frame(xtabs(~ df[, var_cat[i]] + df[, var_strata], addNA = TRUE))
    names(cat_strata)   <- c("var_name", "strata_name", "freq")
    cat_all             <- data.frame(xtabs(~ df[, var_cat[i]], addNA = TRUE))
    names(cat_all)      <-  c("var_name", "freq")
    cat_all$strata_name <- "Overall"
    cat                 <- rbind(cat_strata, cat_all)
    cat_sum        <- aggregate(cat$freq ~ cat[, 2], cat, sum) 
    names(cat_sum) <- c("strata_name", "sum")
    cat            <- left_join(cat, cat_sum, by = "strata_name") 
    cat$percent    <- paste0("(", roundHAFZ(cat$freq / cat$sum * 100), ")")
    cat$stat       <- paste(format(cat$freq, big.mark = ","), cat$percent)
    cat            <- cat[, c("var_name", "strata_name", "stat")]
    var_name       <- names(df)[which(names(df) %in% var_cat[i])]
    cat_name       <- levels(cat$strata_name)
    cat            <- rbind(cbind(var_name = paste(var_name, "/ N(%)"), strata_name = cat_name, stat = ""), cat)
    cat            <- reshape(cat, timevar = "strata_name", idvar = "var_name", direction = "wide")
    col_name       <- c("Characteristics", cat_name)
    names(cat)     <- col_name
    names(df_cat)  <- col_name
    df_cat         <- rbind(df_cat, cat)
  } 
  } else df_cat <- NULL
  
  df_n             <- data.frame(matrix(nrow = 0, ncol = length(levels(df[, var_strata])) + 2))
  n_strata         <- data.frame(xtabs(~ df[, var_strata], addNA = TRUE))
  names(n_strata)  <- c("strata_name", "freq") 
  n_all            <- data.frame(strata_name = "Overall", freq = nrow(df))
  n_strata         <- rbind(n_strata, n_all)
  n_strata$sum     <- sum(n_strata$freq[-nrow(n_strata)])
  n_strata$percent <- paste0("(", roundHAFZ(n_strata$freq / n_strata$sum  * 100), ")")
  n_strata$stat    <- paste(format(n_strata$freq, big.mark = ","), n_strata$percent)
  n_strata         <- n_strata[, c("strata_name", "stat")]
  n_strata         <- t(rbind(cbind(strata_name = "Characteristics", stat = "N(%)"), n_strata))
  col_name         <- n_strata[1, ]
  n_strata         <- data.frame(n_strata, row.names = NULL)[-1, ]
  names(n_strata)  <- col_name
  names(df_n)      <- col_name
  df_n             <- rbind(df_n, n_strata)
  
  tableone <- data.frame(rbind(df_n, df_cont, df_cat), row.names = NULL, check.names = FALSE)
  
  return(tableone)
}


# TABLE 1 BY ROWS ---------------------------------------------------------

fun.tableone_by_rows <- function(df, var_cat, var_strata){
  # %s are calculated across rows
  # 'cat_var', 'var_strata' are categorical
  
  # example:
  # var_cat = "Year_of_diagnosis"
  # var_strata = "Sex"
  # df   = reg_inc
  
  if(!is.null(var_cat)){
    df_cat <- data.frame(matrix(nrow = 0, ncol = length(levels(df[, var_strata])) + 2))
    
    for(i in 1:length(var_cat)){
      
      cat_strata          <- data.frame(xtabs(~ df[, var_cat] + df[, var_strata], addNA = TRUE))
      names(cat_strata)   <- c("var_name", "strata_name", "freq")
      strata_sum          <- aggregate(cat_strata$freq ~ cat_strata$var_name, cat_strata, sum) 
      names(strata_sum)   <- c("var_name", "sum")
      cat_all             <- strata_sum
      names(cat_all)      <- c("var_name", "freq")
      cat_all$strata_name <- "Overall"
      cat                <- rbind(cat_strata, cat_all)
      cat                <- left_join(cat, strata_sum, by = "var_name")
      cat$percent        <- paste0("(", roundHAFZ(cat$freq / cat$sum * 100), ")")
      cat$stat           <- paste(format(cat$freq, big.mark = ","), cat$percent)
      cat                <- cat[, c("var_name", "strata_name", "stat")]
      strata_name        <- levels(cat$strata_name)
      cat                <- reshape(cat, timevar = "strata_name", idvar = "var_name", direction = "wide")
      col_name           <- c("Characteristics", strata_name)
      names(cat)         <- col_name
      var_name           <- names(df)[which(names(df) %in% var_cat)]
      var_line           <- data.frame(t(c(paste(var_name, "/ N(%)"), rep("", ncol(cat) - 1))))
      names(var_line)    <- col_name
      names(df_cat)      <- col_name
      cat$Characteristics <- as.character(cat$Characteristics)
      df_cat             <- rbind(df_cat, var_line)
      df_cat             <- rbind(df_cat, cat)
    } 
  } else df_cat <- NULL
  
  df_n             <- data.frame(matrix(nrow = 0, ncol = length(levels(df[, var_strata])) + 2))
  n_strata         <- data.frame(xtabs(~ df[, var_strata], addNA = TRUE))
  names(n_strata)  <- c("strata_name", "freq") 
  n_all            <- data.frame(strata_name = "Overall", freq = nrow(df))
  n_strata         <- rbind(n_strata, n_all)
  n_strata$sum     <- sum(n_strata$freq[-nrow(n_strata)])
  n_strata$percent <- paste0("(", roundHAFZ(n_strata$freq / n_strata$sum  * 100), ")")
  n_strata$stat    <- paste(format(n_strata$freq, big.mark = ","), n_strata$percent)
  n_strata         <- n_strata[, c("strata_name", "stat")]
  n_strata         <- t(rbind(cbind(strata_name = "Characteristics", stat = "N(%)"), n_strata))
  col_name         <- n_strata[1, ]
  n_strata         <- data.frame(n_strata, row.names = NULL)[-1, ]
  names(n_strata)  <- col_name
  names(df_n)      <- col_name
  df_n             <- rbind(df_n, n_strata)
  
  tableone <- data.frame(rbind(df_n, df_cat), row.names = NULL, check.names = FALSE)
  
  return(tableone)
}


# FISHER's TEST -----------------------------------------------------------

fun.pval_fisher <- function(df, var1, var2, levels1 = "all", levels2 = "all"){
  # calculates p-value of Fisher's test for particular levels of variables
  # 'var1', 'var2' are categorical
  
  # example:
  # df = reg_inc
  # var1 = "Summary_stage_2000_1998"
  # var2 = "Race_Ethnicity"
  # levels1 = c("Localized", "Regional", "Distant")
  # levels2 = c("American Indian or Alaska Native", "Hispanic", "Non-Hispanic White")
  
  if(sum(levels1 == "all") < 1 && sum(levels2 == "all") >= 1){
    counts <- xtabs(~ df[, var1] + df[, var2])
    counts <- counts[levels1, ]
  } else if(sum(levels1 == "all") >= 1 && sum(levels2 == "all") < 1){
    counts <- xtabs(~ df[, var1] + df[, var2])
    counts <- counts[, levels2]
  } else if(sum(levels1 == "all") < 1 && sum(levels2 == "all") < 1){
    counts <- xtabs(~ df[, var1] + df[, var2])
    counts <- counts[levels1, levels2]    
  } else counts <- xtabs(~ df[, var1] + df[, var2])

  # increase the workspace argument
  test <- fisher.test(counts, simulate.p.value=TRUE, B=1e5)
  pval <- roundPVAL(test$p.value)
  return(pval)
}


# SUMMARY TABLES ----------------------------------------------------------
# for 'Age_at_diagnosis'

fun.summary_group <- function(df, var1, var2){
  # shows summary statistics: n, mean, sd, median, `min-max` for 'Age_at_diagnosis'
  # 'var1' is categorical, 'var2' is continuous
  
  # example:
  # df = df_inc
  # var1 = Region
  # var2 = Age_at_diagnosis
  
var1 <- enquo(var1)
var2 <- enquo(var2)

  df %>% 
    group_by(!!var1) %>% 
    dplyr::summarize(
      n         = format(n(), big.mark = ","),
      Mean      = roundHAFZ(mean((!!var2), na.rm = TRUE)),
      SD        = roundHAFZ(sd((!!var2), na.rm = TRUE)),
      Median    = roundHAFZ(median((!!var2), na.rm = TRUE)),
      `Min-Max` = paste(roundHAFZ(min((!!var2), na.rm = TRUE)), "-", 
                        roundHAFZ(max((!!var2), na.rm = TRUE)))
    )
}


# ANOVA P-VALUE -----------------------------------------------------------

fun.pval_anova <- function(df, var1, var2, levels1 = "all"){
  # calculates p-value of Fisher's test for particular levels of variables
  # 'var1' is categorical, 'var2' is continuous
  
  # example:
  # df = reg_inc
  # var1 = "Race_Ethnicity"
  # var2 = "Age_at_diagnosis"
  # levels1 = c("American Indian or Alaska Native", "Hispanic", "Non-Hispanic White")
  
  if(sum(levels1 == "all") < 1){
    index <- which(df[, var1] %in% levels1)
    df <- df[index, ]
  }
  
  test <- aov(df[, var2] ~ df[, var1])
  pval <- roundPVAL(summary(test)[[1]][["Pr(>F)"]][[1]])
  return(pval)
}


# AGE-ADJUSTED RATES ------------------------------------------------------

fun.aarate <- function(df, pop){
  # age-adjusted incidence rates for analyzed cancer type and APC
  # : http://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm 
  # : https://seer.cancer.gov/seerstat/WebHelp/Trend_Algorithms.htm
  
  # example:
  # df = subset(reg_inc, Sex == "Male")
  # pop = pop_male_reg
  
  # these 2 columns are enough  
  df <- df[, c("Age_recode_with_1_year_olds", "Year_of_diagnosis")]
  # drop 'Unknown' level in Age_recode_with_1_year_olds
  df <- df[!is.na(df$Age_recode_with_1_year_olds %in% "Unknown"), ]
  df$Age_recode_with_1_year_olds <- droplevels(df$Age_recode_with_1_year_olds)
  
  # as some age groups may be not presented in data subset
  age_grs <- levels(df$Age_recode_with_1_year_olds)
  pop2    <- pop[pop$Age %in% age_grs, ]
  pop_sum <- sum(pop_std$Population)
  pop_std <- pop_std[pop_std$Age %in% age_grs, ]
  
  df <- xtabs(~ Age_recode_with_1_year_olds + Year_of_diagnosis, df)
  
  aarate <- colSums(df / pop2[, -1] * 100000 * (pop_std$Population / pop_sum))
  SE     <- sqrt(colSums((df / (pop2[, -1])^2) * ((pop_std$Population / pop_sum)^2))) * 100000
  LCL    <- roundHAFZ(aarate - 1.96 * SE)
  UCL    <- roundHAFZ(aarate + 1.96 * SE)
  
  # suppress if counts < 15 or population < 10,000 - for stability of aarates
  df_suppress  <- which(colSums(df[, 2:ncol(df)]) < 15)
  pop_suppress <- which(colSums(pop2[, 2:ncol(pop2)]) < 10000)
  suppress <- df_suppress | pop_suppress
  
  # APC (Annual Percent Change)
  Year <- as.numeric(names(aarate))
  
  if(sum(Year == years) >= 1 & length(suppress) == 0){
    fit  <- lm(log(aarate) ~ Year)
    beta <- summary(fit)$coefficients[2,1]
    pval <- roundPVAL(summary(fit)$coefficients[2,4])
    apc  <- roundHAFZ(100 * (exp(beta) - 1))
    
    if(pval <= 0.05){
      pval <- paste0("(", pval, ")*")
    } else pval <- paste0("(", pval, ")")
  } else {
    apc  <- ""
    pval <- ""
  }
  
  aarate <- roundHAFZ(aarate)
  SE     <- roundHAFZ(SE)
  LCL    <- roundHAFZ(LCL)
  UCL    <- roundHAFZ(UCL)
  
  aarate <- data.frame(Year = Year, aarate = aarate, SE = SE, LCL = LCL, UCL = UCL, row.names = NULL)
  aarate[nrow(aarate) + 1, 1] <- "APC (p-value)"
  aarate[nrow(aarate), 2] <- apc
  aarate[nrow(aarate), 3] <- pval
  aarate[nrow(aarate), 4:5] <- ""
  
  # surppress if counts < 15 or population < 10,000
  if(length(suppress) > 0){
    aarate[suppress, 2]   <- "x"
    aarate[suppress, 3:5] <- ""
  }
  
  return(aarate)
}


# K-M SURVIVAL PLOTS ------------------------------------------------------

plot.KM_strata <- function(df, strataName, COD, survTimeVar, title, xlab, xTicksBy, xshift, survTicks, cutoff = NULL){

  # example:
  # df = df_surv
  # strataName = "Region"
  # COD = "COD_CS"
  # survTimeVar = "TimeSurv"
  # title = "Registries"
  # xlab = "Time in months"
  # xTicksBy = 12
  # xshift = 6
  # survTicks = c(12, 24, 60, 120)
  # cutoff = NULL
  
  
  # levels of variable to be plotted
  category <- df[, strataName]
  n        <- length(levels(as.factor(category)))
  time     <- df[, survTimeVar]
  COD      <- df[, COD]
  
  if(!is.null(cutoff)){
    # truncate data
    # adjust Time
    time_cut <- NA
    time_cut[time <= cutoff] <- time[time <= cutoff]
    time_cut[is.na(time_cut)] <- cutoff
    
    # adjust COD
    COD_cut <- NA
    COD_cut[time <= cutoff] <- df$COD[time <= cutoff]
    COD_cut[is.na(COD_cut)] <- 0
    
    time <- time_cut
    COD  <- COD_cut
  }
  
  # Kaplan-Meier estimates
  fitKM     <- survfit(Surv(time, COD)~ category, conf.type="log-log")
  fitKMdiff <- survdiff(Surv(time, COD)~ category, rho=0)  # Log-rank
  
  # Log-Rank p-value
  pval1 <- 1 - pchisq(fitKMdiff$chisq, length(fitKMdiff$n) - 1)  
  pval1 <- roundPVAL(pval1)

  # S(t) for particular time points & factor levels
  percentsLong <- data.frame(time        = summary(fitKM, times=c(0, survTicks))$time,
                             persentSurv = roundHAFZ(summary(fitKM, times=c(0, survTicks))$surv*100),
                             levels      = summary(fitKM, times=c(0, survTicks))$strata)
  percentsLong$levels <- gsub("category=", "", percentsLong$levels)
  
  # Long -> Wide format
  percentsWide <- reshape(percentsLong, idvar = "levels", timevar = "time", direction = "wide")
  namesWide    <- data.frame(levels = percentsWide[ , 1])
  names(namesWide) <- paste("% of survived")
  percentsWide     <- data.frame(percentsWide[, 2:ncol(percentsWide)])
  groupsNames  <- levels(as.factor(category))
  
  # Plot
  par(mai=c(1, 1, 1, 2.5)) # increase right margin space for a legend
  plot(fitKM, lty=1:n, col = 1:n, lwd = 2, xlab = xlab, ylab = 'Survival probability', cex.lab = 1.2, axes = FALSE)  
  legend("topright", inset = c(-0.5, 0), xpd = TRUE, xjust = 0, yjust = 0.5, box.lty = 1, box.lwd = 1, groupsNames, lty = 1:n, col = 1:n, lwd = 2)
  abline(h = 0.5, lty = "dotted", col = "red", lwd = 1)
  xmax = max(time)
  axis(side = 1, at = seq(0, xmax, xTicksBy), cex.axis = 0.8)
  axis(side = 2, at = seq(0, 1, 0.1), cex.axis = 0.8)
  
  # Add Text on plot
  mylabel = paste("Log-rank p-value = ", pval1, sep = "")
  mtext(mylabel, 3, line = 0, adj = 0)
  title(main = title, line = 2)
  addtable2plot(par('usr')[2], par('usr')[1], cex = 0.8, display.colnames = T, table = namesWide)
  for(i in 1:length(survTicks)){
    lines(x = rep(survTicks[i], 2), 
          y = c(-1, 0), lty = "dotted")
  }
  
  # every time need to shift output of % of survived, otherwise it's not exactly where want it to be;
  # it's practically impossible to have <1 year of survival, thus can omit such scenario
  if(ncol(percentsWide) == 2){
    addtable2plot(survTicks[1] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 2]), bg="transparent")
  } else if(ncol(percentsWide) == 3){
    addtable2plot(survTicks[1] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 2]), bg="transparent")
    addtable2plot(survTicks[2] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 3]), bg="transparent")
  } else if(ncol(percentsWide) == 4){
    addtable2plot(survTicks[1] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 2]), bg="transparent")
    addtable2plot(survTicks[2] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 3]), bg="transparent")
    addtable2plot(survTicks[3] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 4]), bg="transparent")
  } else if(ncol(percentsWide) == 5){
    addtable2plot(survTicks[1] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 2]), bg="transparent")
    addtable2plot(survTicks[2] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 3]), bg="transparent")
    addtable2plot(survTicks[3] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 4]), bg="transparent")
    addtable2plot(survTicks[4] - xshift, par('usr')[1], cex=0.8, display.colnames=F, table= data.frame(percentsWide[, 5]), bg="transparent")
  }
}



# K-M SURVIVAL TABLES -----------------------------------------------------

fun.KM_tbl <- function(df, strataName, COD, COD_abbr, survTimeVar, cutoff = NULL){
  
  # example:
  # df = df_surv
  # strataName = "Region"
  # COD = "COD_CS"
  # COD_abbr = "CS"
  # survTimeVar = "TimeSurv"
  # cutoff = NULL
  
  
  # levels of variable to be plotted
  strata <- df[, strataName]
  n        <- length(levels(as.factor(strata)))
  time     <- df[, survTimeVar]
  COD      <- df[, COD]
  
  if(!is.null(cutoff)){
    # truncate data
    # adjust Time
    time_cut <- NA
    time_cut[time <= cutoff] <- time[time <= cutoff]
    time_cut[is.na(time_cut)] <- cutoff
    
    # adjust COD
    COD_cut <- NA
    COD_cut[time <= cutoff] <- df$COD[time <= cutoff]
    COD_cut[is.na(COD_cut)] <- 0
    
    time <- time_cut
    COD  <- COD_cut
  }
  
  # Kaplan-Meier estimates
  fitKM <- survfit(Surv(time, COD)~ strata, conf.type="log-log")
  
  tbl <- roundHAFZ(summary(fitKM)$table[, c(1, 4:9)])

  rownames(tbl) <- paste0(rownames(tbl), " (", COD_abbr, ")") 
  
  return(tbl)
}


# Cox PH  -----------------------------------------------------------------

fun.Cox_multiv <- function(df, varNames, COD, survTimeVar){
  
  # example:
  # df = df_surv
  # varNames = c("Age_at_diagnosis", "Region", "Race_Ethnicity", "Sex")
  # COD = "COD_CS"
  # survTimeVar = "TimeSurv"
  # long = TRUE
  
  CoxMult <- df[, varNames]
  CoxMult <- data.frame(CoxMult, df[, c(survTimeVar, COD)])
  names(CoxMult) <- c(varNames, "survTimeVar", "COD")
  
  fit <- coxph(Surv((survTimeVar), COD) ~ ., CoxMult)
  fitCoxMulti <- summary((coxph(Surv((survTimeVar), COD) ~ ., CoxMult)))
  # Schoenfeld residuals (test violation of proportionality)
  test <- cox.zph(fit, transform = "rank") 
  
  # want specific output from Proportional Hazards Regression Model in a matrix format
  # exp(coef) 
  HR <- roundHAFZ(as.matrix(fitCoxMulti$coefficients)[, 2], n = 3)
  # CI
  CI <- roundHAFZ(as.matrix(fitCoxMulti$conf.int)[, c(3, 4)], n = 3) 
  # p-value
  pval <- roundPVAL(roundHAFZ(as.matrix(fitCoxMulti$coefficients)[, 5], n = 3))
  
  # combine all necessary coefficients
  CoxMultiCoef <- NA
  CoxMultiCoef <-  as.matrix(cbind(HR, 
                                   paste(CI[, 1], "-", CI[, 2], sep=""), 
                                   pval))
  
  # PH assumption p-value
  ph <- as.vector(test$table[-dim(test$table)[1], 3], mode="numeric")
  ph <- roundHAFZ(ph, n = 3)
  for(i in 1:length(ph)){
    if(ph[i] == 0){ph[i] = "<0.001"}
  }
  
  CoxMultiCoef <- data.frame(cbind(CoxMultiCoef, ph))
  names(CoxMultiCoef) <- c("Multivariable HR", "95% CI", "p-value", "P(PH)")
  
  return(CoxMultiCoef)
}


