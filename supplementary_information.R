
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%                   Replication code for the Supplementary Information file of "Estimating the                     %%#
#%%                 impact of airport wildlife hazards management on realized wildlife strike risk"                  %%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



## CHANGE **LINE 44** TO THE USER-SPECIFIC PATH OF THE "airport_wildlife_hazards_management" FOLDER. ##               



# TABLE OF CONTENTS:
#   1. SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA...............................................32-103
#   2. TABLE S1. Data summary statistics.........................................................................107-123
#   3. TABLE S2. Estimated dynamic effects of wildlife hazards management on join, civil-, and military-specific 
#                wildlife strike metrics (estimates presented visually in Figure 3 of manuscript)................127-182
#   4. TABLE S3. Estimated impact of management intervention on wildlife strike counts a la Wooldridge (2022)....186-210
#   5. TABLE S4. Sensitivity of estimated ATTs to alternative model specifications...............................214-296
#   6. TABLE S5. Sensitivity of estimated ATTs when subsetting to only those wildlife strikes that are confirmed 
#                within the airport operations area (AOA)........................................................300-373
#   7. TABLE S6. Sensitivity of estimated ATTs to the exclusion of PDX, PHX, and SLC.............................377-404
#   8. FIGURE S1. Distribution of pre-management and management observations across time relative to AWHP 
#                 management intervention........................................................................408-433
#   9. FIGURE S2. Average number of staff years funded over relative time at those airports with below- (low) 
#                 and above-median (high) Airport Wildlife Hazards Program staff years throughout the management
#                 period.........................................................................................437-486
# END



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA ----

# Set working directory:
setwd("C:/Users/LAltringer/OneDrive - USDA/Desktop/Projects/airport_wildlife_hazards_management")

# Install (if necessary) and Load required packages:
# List of required packages:
required_packages <- c("dplyr", "tidyr", "zoo", "doBy", # cleaning packages
                       "ggplot2", "ggthemes", "hrbrthemes", "ggpubr", "ggrepel", "sf", # plotting packages
                       "gridExtra", "scales", "sjPlot", "grid", "cowplot", "ggtext", # plotting packages
                       "fixest", "did", "etwfe", "bacondecomp", "psych") # modeling packages
# Install packages that aren't already installed:
installed_packages <- required_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(required_packages[!installed_packages])
}
rm(installed_packages)
# Load packages:
invisible(lapply(required_packages, library, character.only = TRUE))
# Remove list of required packages:
rm(required_packages)

# Bringing in data which is pre-cleaned and was drawn from a variety of sources including:
#   - USDA APHIS WS Airport Wildlife Hazards Management Program (AWHP)
#   - Air Force Safety Automated System (AFSAS)
#   - FAA National Wildlife Strike Database (NWSD)
#   - FAA Air Traffic Activity Data System (ATADS)
df <- read.csv("data.csv")

# Generating aircraft operations measures required in the analysis:
df$civ_operations <- df$itinerant_air_carrier + df$itinerant_air_taxi + df$itinerant_general_aviation + df$local_civil
df$mil_operations <- df$itinerent_military + df$local_military
df$share_commercial <- (df$itinerant_air_carrier + df$itinerant_air_taxi)/df$civ_operations
df$share_military <- df$mil_operations/df$total_operations

# Generating overall strike and strike rate measures required in the analysis:
df$joint_strike <- df$civ_strike + df$mil_strike
df$joint_strike_rate <- (df$joint_strike/df$total_operations)*5000
df$civ_strike_rate <- (df$civ_strike/df$civ_operations)*5000
df$mil_strike_rate <- (df$mil_strike/df$mil_operations)*5000

# Generating disruptive strike and strike rate measures required in the analysis:
df$joint_disruptive_strike <- df$civ_disruptive_strike + df$mil_disruptive_strike
df$joint_disruptive_strike_rate <- (df$joint_disruptive_strike/df$total_operations)*5000
df$civ_disruptive_strike_rate <- (df$civ_disruptive_strike/df$civ_operations)*5000
df$mil_disruptive_strike_rate <- (df$mil_disruptive_strike/df$mil_operations)*5000
df$joint_damage_strike <- df$civ_damage_strike + df$mil_damage_strike

# Generating strike-induced economic costs measures required in the analysis:
df$joint_strike_costs <- (df$civ_reported_cost + df$mil_reported_cost)/1000000
df$civ_strike_costs <- df$civ_reported_cost/1000000
df$mil_strike_costs <- df$mil_reported_cost/1000000

# Generate AWHP management timing variables:
df$treatment_year <- ifelse(is.na(df$ws_intervention_year), 0, df$ws_intervention_year)
df$time_to_treatment <- df$year - df$treatment_year
df$treatment <- ifelse(df$time_to_treatment>0 & !is.na(df$ws_intervention_date), 1, 0)

# Generate Low/High management groups based on AWHP staff-years employed:
temp <- subset(df, time_to_treatment>=0 & time_to_treatment<=14)
temp <- temp %>% group_by(airport) %>%
  mutate(years = mean(staff_years, na.rm=T)) %>%
  slice(1) %>%
  select(airport, years)
median(temp$years)
staff_years_low <- subset(temp, years<=median(temp$years))$airport
staff_years_high <- subset(temp, years>median(temp$years))$airport; rm(temp)
df$het_treat_years_cat <- ifelse(df$airport %in% staff_years_low, "Low", 
                                 ifelse(df$airport %in% staff_years_high, "High", "None")) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S1 ----
# Summary statistics for the variables of interest

# Variables for summary statistics
summary_df <- df[,c("total_operations", "civ_operations", "mil_operations", "share_commercial", "share_military",
                    "joint_strike", "civ_strike", "mil_strike",
                    "joint_strike_rate", "civ_strike_rate", "mil_strike_rate",
                    "joint_disruptive_strike", "civ_disruptive_strike", "mil_disruptive_strike",
                    "joint_disruptive_strike_rate", "civ_disruptive_strike_rate", "mil_disruptive_strike_rate",
                    "joint_damage_strike", "civ_damage_strike", "mil_damage_strike",
                    "joint_strike_costs", "civ_strike_costs", "mil_strike_costs")]

# Get summary statistics
describe(summary_df); rm(summary_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S2 ----
## Estimated dynamic effects of wildlife hazards management on join, civil-, and military-specific wildlife strike 
## metrics (estimates presented visually in Figure 3 of manuscript).

# Function to obtain the estimated dynamic average treatment effects:
print_dynamic_estimates <- function(dep_var, type, covariates) {
  
  df$var <- df[,paste0(type, "_", dep_var)]
  
  if (length(covariates)==2) {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1], " + ", covariates[2], 
                " | airport + year")
  } else if (length(covariates)==1) {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1],
                " | airport + year")
  } else {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year")
  }
  
  model <- feols(formula(f), df, se = "cluster")
  
  plot_df <- as.data.frame(model$coeftable[c(2:10),c(1,2,4)])
  plot_df <- plot_df %>%
    mutate(`Conf. Int. 95%` = paste0("[", round(Estimate - `Std. Error`*1.960, digits = 3), ", ",
                                     round(Estimate + `Std. Error`*1.960, digits = 3), "]")) %>%
    mutate(`Stan. Err.` = paste0("(", round(`Std. Error`, digits = 3), ")")) %>%
    mutate(Estimate = ifelse(`Pr(>|t|)` <0.01, paste0(round(Estimate, digits = 3),"***"),
                             ifelse(`Pr(>|t|)` >=0.01 & `Pr(>|t|)`<0.05, paste0(round(Estimate, digits = 3),"**"),
                                    ifelse(`Pr(>|t|)` >=0.05 & `Pr(>|t|)`<0.1, paste0(round(Estimate, digits = 3),"*"), 
                                           paste0(round(Estimate, digits = 3)))))) %>%
    mutate(P_Value = round(`Pr(>|t|)`, digits = 3)) %>%
    select(Estimate, `Stan. Err.`, `Conf. Int. 95%`, P_Value)
  
  return(plot_df)
  
}

# Panel A: Dependent variable is total wildlife strikes per 5,000 movements
print_dynamic_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
print_dynamic_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
print_dynamic_estimates(dep_var = "strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel B: Dependent variable is disruptive wildlife strikes per 5,000 movements
print_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
print_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
print_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel C: Dependent variable is wildlife strike costs (Millions, 2023 $)
print_dynamic_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike")) # Joint
print_dynamic_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike")) # Civil
print_dynamic_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike")) # Military (ANG)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S3 ----
## Estimated impact of management intervention on wildlife strike counts a la Wooldridge (2022).

# Redefine treatment year to match the definition of treatment in the manuscript (initial year of management is the 
# first full year of management:
df$treatment_year_new <- ifelse(is.na(df$ws_intervention_year), 0, df$ws_intervention_year+1)

# Panel A: Dependent variable is count of all wildlife strikes
model = etwfe(fml = joint_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Joint
model = etwfe(fml = civ_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Civil
model = etwfe(fml = mil_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Military (ANG)

# Panel B: Dependent variable is count of disruptive wildlife strikes
model = etwfe(fml = joint_disruptive_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Joint
model = etwfe(fml = civ_disruptive_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Civil
model = etwfe(fml = mil_disruptive_strike ~ 1, tvar = year, gvar = treatment_year_new, data = df, vcov = ~airport, family="poisson")
round(as.data.frame(emfx(model)[,c(4,5,7,9,10)]), digits = 3) # Military (ANG)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S4 ----
## Sensitivity of estimated ATTs to alternative model specifications

# Function to obtain the estimated average treatment effects:
alternative_specification_estimates <- function(dep_var, type, covariates) {
  
  model_df <- df
  
  model_df$var <- model_df[,paste0(type, "_", dep_var)]
  
  if (length(covariates)==2) {
    y <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1], " + ", covariates[2], 
                " | airport + year")
  } else if (length(covariates)==1) {
    y <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1],
                " | airport + year")
  } else {
    y <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year")
  }
  
  if (length(covariates)==2) {
    fy <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                 covariates[1], " + ", covariates[2], 
                 " | airport + flyway^year")
  } else if (length(covariates)==1) {
    fy <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                 covariates[1],
                 " | airport + flyway^year")
  } else {
    fy <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + flyway^year")
  }
  
  model <- feols(var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year, model_df, se = "cluster")
  est_1 <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                  ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                         ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                                paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  se_1 <- paste0("(",round(summary(model, agg = "att")$coeftable[1,2], digits = 3),")")
  
  model <- feols(formula(y), model_df, se = "cluster")
  est_2 <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                  ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                         ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                                paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  se_2 <- paste0("(",round(summary(model, agg = "att")$coeftable[1,2], digits = 3),")")
  
  model <- feols(formula(fy), model_df, se = "cluster")
  est_3 <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                  ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                         ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                                paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  se_3 <- paste0("(",round(summary(model, agg = "att")$coeftable[1,2], digits = 3),")")
  
  temp <- data.frame(Airport_FEs = c("Yes", "", "Yes", "", "Yes", ""),
                     Year_FEs = c("Yes", "", "Yes", "", "No", ""),
                     Flyway_by_Year_FEs = c("No", "", "No", "", "Yes", ""),
                     Covariates = c("No", "", "Yes", "", "Yes", ""),
                     Label = c("ATT", "SE", "ATT", "SE", "ATT", "SE"),
                     Estimates = c(est_1, se_1, est_2, se_2, est_3, se_3))
  
  return(temp)
  
}

# Panel A: Dependent variable is total wildlife strikes per 5,000 movements
alternative_specification_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
alternative_specification_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
alternative_specification_estimates(dep_var = "strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel B: Dependent variable is disruptive wildlife strikes per 5,000 movements
alternative_specification_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
alternative_specification_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
alternative_specification_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel C: Dependent variable is wildlife strike costs (Millions, 2023 $)
alternative_specification_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike")) # Joint
alternative_specification_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike")) # Civil
alternative_specification_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike")) # Military (ANG)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S5 ----
## Sensitivity of estimated ATTs when subsetting to only those wildlife strikes that are confirmed within the airport 
## operations area (AOA)

# Generate new dataframe to use in modeling:
model_df <- df

# Generate strike rates that only include AOA-confirmed strikes:
model_df$joint_strike_rate <- ((model_df$civ_strike_aoa + model_df$mil_strike_aoa)/model_df$total_operations)*5000
model_df$joint_disruptive_strike_rate <- ((model_df$civ_disruptive_strike_aoa + model_df$mil_disruptive_strike_aoa)/model_df$total_operations)*5000
model_df$joint_strike_costs <- (model_df$civ_reported_cost_aoa + model_df$mil_reported_cost_aoa)/1000000

# Generate disruptive strike rates that only include AOA-confirmed strikes:
model_df$civ_strike_rate <- (model_df$civ_strike_aoa/model_df$civ_operations)*5000
model_df$civ_disruptive_strike_rate <- (model_df$civ_disruptive_strike_aoa/model_df$civ_operations)*5000
model_df$civ_strike_costs <- model_df$civ_reported_cost_aoa/1000000

# Generate strike-induced economic costs that only include AOA-confirmed strikes:
model_df$mil_strike_rate <- (model_df$mil_strike_aoa/model_df$mil_operations)*5000
model_df$mil_disruptive_strike_rate <- (model_df$mil_disruptive_strike_aoa/model_df$mil_operations)*5000
model_df$mil_strike_costs <- model_df$mil_reported_cost_aoa/1000000

# Function to obtain the estimated average treatment effects:
get_estimates <- function(dep_var, type, covariates) {
  
  model_df$var <- model_df[,paste0(type, "_", dep_var)]
  
  if (length(covariates)==2) {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1], " + ", covariates[2], 
                " | airport + year")
  } else if (length(covariates)==1) {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1],
                " | airport + year")
  } else {
    f <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year")
  }
  
  model <- feols(formula(f), model_df, se = "cluster")
  est <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                       ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                              paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  se <- round(summary(model, agg = "att")$coeftable[1,2], digits = 3)
  ci <- paste0("[",round(confint(summary(model, agg = "att"), level = .95)[1,1], digits = 3),", ",round(confint(summary(model, agg = "att"), level = .95)[1,2], digits = 3),"]")
  
  temp <- data.frame(estimates = c(est, se, ci, model$nobs_origin, round(r2(model)[2], digits = 3), round(r2(model)[6], digits = 3)))
  rownames(temp) <- c("ATT", "SE", "95% CI", "N", "R2", "Within R2")
  
  return(temp)
  
}

# Panel A: Dependent variable is total wildlife strikes per 5,000 movements
get_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_estimates(dep_var = "strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel B: Dependent variable is disruptive wildlife strikes per 5,000 movements
get_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel C: Dependent variable is wildlife strike costs (Millions, 2023 $)
get_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike")) # Joint
get_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike")) # Civil
get_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike")) # Military (ANG)

# Remove altered dataframe:
rm(model_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE S6 ----
## Sensitivity of estimated ATTs to the exclusion of PDX, PHX, and SLC.

# Remove disproportionately large airports from the sample:
model_df <- subset(df, !(airport %in% c("PDX", "PHX", "SLC")))

# The get_estimates() function is defined above.

# Panel A: Dependent variable is total wildlife strikes per 5,000 movements
get_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_estimates(dep_var = "strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel B: Dependent variable is disruptive wildlife strikes per 5,000 movements
get_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel C: Dependent variable is wildlife strike costs (Millions, 2023 $)
get_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike")) # Joint
get_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike")) # Civil
get_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike")) # Military (ANG)

# Remove altered dataframe:
rm(model_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## FIGURE S1 ----
## Distribution of pre-management and management observations across time relative to AWHP management intervention. In 
## our analysis, relative time values are bottom- and top-coded at -8 and 10, respectively. Further, for relative time 
## values [-8, … , -1] and [1, … , 10], we bin adjacent periods (e.g., [-8, -7], [-6, -5], up to [9, 10]) with the 
## initial year of management intervention (i.e., relative time period 0) being defined as the reference period.

# Plot dataframe
plot_df <- subset(df, !is.na(ws_intervention_year))

# Plot
png("figure_s1.png", width = 6, height = 4, units = "in", res = 300)
ggplot(plot_df, aes(x = time_to_treatment)) + 
  geom_histogram(binwidth = 1, fill = scales::alpha("#E55537", 0.9), color = "#F4AC9D") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_ipsum(axis_title_size = 12) +
  coord_cartesian(ylim = c(0.5,12.8)) +
  scale_x_continuous(breaks = seq(-12,15,3)) + 
  scale_y_continuous(breaks = seq(0,12,2)) +
  labs(y = "Number of airports\n", x = "\nYears since management intervention") 
dev.off()

# Remove plot dataframe:
rm(plot_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## FIGURE S2 ----
## Average number of staff years funded over relative time at those airports with below- (low) and above-median (high) 
## Airport Wildlife Hazards Program staff years throughout the management period. The airports categorized as 
## low-management airports are BAF, BHM, FOE, GTF, LMT, MTN, and STJ. This group of airports experienced an average 
## 0.748 funded  staff years in the management period. Airports categorized as high-management airports are BTV, FWA, 
## PIA, SLC, SUX, and SWF. This group of airports experienced an average 1.212 funded staff years in the management 
## period.

# Generate plot dataframe:
plot_df <- subset(df, !is.na(ws_intervention_year))[,c("airport", "year", "ws_intervention_year", "time_to_treatment", "staff_years", "het_treat_years_cat")]
temp <- plot_df %>% mutate(count = 1) %>% group_by(time_to_treatment, het_treat_years_cat) %>% mutate(count = sum(count)) %>% 
  select(time_to_treatment, count) %>% slice(1)
plot_df <- summaryBy(staff_years ~ time_to_treatment + het_treat_years_cat, data = plot_df, FUN = c(mean, sd), na.rm = T)
plot_df <- merge(plot_df, temp, by=c("time_to_treatment","het_treat_years_cat"), all = T); rm(temp)
plot_df$mean <- plot_df$staff_years.mean
plot_df$staff_years.sd <- ifelse(is.na(plot_df$staff_years.sd), 0, plot_df$staff_years.sd)
plot_df$se <- plot_df$staff_years.sd/sqrt(plot_df$count)
plot_df <- subset(plot_df, time_to_treatment %in% seq(-8,10,1))

# Generate plot:
png("figure_s2.png", width = 6, height = 4, units = "in", res = 300)
ggplot(data = plot_df) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x=time_to_treatment, ymin=mean-se, ymax=mean+se, fill = het_treat_years_cat), alpha = 0.8) + 
  geom_line(aes(x=time_to_treatment, y=mean, linetype = het_treat_years_cat), size = 0.5, color = "grey35") + 
  theme_ipsum(axis_title_size = 12) + 
  scale_y_continuous(limits = c(-0.03,2), labels = scales::comma, breaks = seq(0,1.5, .25)) +
  scale_x_continuous(breaks = seq(-8,10,2)) +
  scale_fill_manual(name = "", values = c(scales::alpha("#E55537", 0.9), 
                                          scales::alpha("#F4AC9D", 0.9)), 
                    labels = c("Mean among high-\nmanagement airports (+/- SE)", 
                               "Mean among low-\nmanagement airports (+/- SE)")) +
  scale_linetype_manual(name = "", values = c(1, 2), labels = c("Mean among high-\nmanagement airports (+/- SE)", 
                                                                "Mean among low-\nmanagement airports (+/- SE)")) +
  coord_cartesian(ylim = c(-0.03,1.5)) + 
  labs(y = "AWHP management staff years funded\n", x = "\nYears to WS management intervention") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        legend.position = c(0.225,0.88),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.2, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))
dev.off()

# Remove plot dataframe:
rm(plot_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%                                                         END                                                      %%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

