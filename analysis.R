

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%                                Replication code for "Estimating the impact of airport                            %%#
#%%                             wildlife hazards management on realized wildlife strike risk"                        %%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



## CHANGE **LINE 44** TO THE USER-SPECIFIC PATH OF THE "airport_wildlife_hazards_management" FOLDER. ##               



# TABLE OF CONTENTS:
#   1. SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA...............................................29-100
#   2. FIGURE 1. Conceptualizing the relationship wildlife strike risk, wildlife strike metrics, and the hypothesized 
#                impact of airport wildlife hazards management...................................................104-111
#   3. FIGURE 2. The location (A) and timing (B) of AWHP management intervention among sample airports...........115-189
#   4. TABLE 1. Estimated impact of AWHP management intervention on joint, civil-, and military-specific wildlife 
#               strike metrics...................................................................................193-245
#   5. FIGURE 3. Estimated change in joint, civil, and military wildlife strike metrics before and after management 
#                intervention. ..................................................................................249-403
#   6. TABLE 2. Heterogeneity in the estimated impact of wildlife hazards management intervention when partitioning 
#               managed airports into low versus high management presence (staff years employed).................407-484
# END



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA ----

# Set working directory:
setwd("C:/Users/LAltringer/OneDrive - USDA/Desktop/Projects/airport_wildlife_hazards_management")

# Install (if necessary) and Load required packages:
# List of required packages:
required_packages <- c("dplyr", "tidyr", "zoo", # cleaning packages
                       "ggplot2", "ggthemes", "hrbrthemes", "ggpubr", "ggrepel", "sf", # plotting packages
                       "gridExtra", "scales", "sjPlot", "grid", "cowplot", "ggtext", # plotting packages
                       "fixest", "did", "etwfe", "bacondecomp") # modeling packages
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
## FIGURE 1 ---- 
## Conceptualizing the relationship wildlife strike risk, wildlife strike metrics, and the hypothesized 
## impact of airport wildlife hazards management.

# This figure was generated in MS PowerPoint and can be accessed/downloaded in the digital version of the manuscript.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## FIGURE 2 ----
## The location (A) and timing (B) of AWHP management intervention among sample airports.

# Dataframes required to plot the ocation of sample airports:
flyways <- st_crop(st_read("shapefiles/waterfowl_flyways/WaterfowlFlyways.shp"), xmin = -127, xmax = -65, ymin = 20.5, ymax = 52)
states <- st_crop(st_read("shapefiles/states/cb_2018_us_state_500k.shp"), xmin = -127, xmax = -65, ymin = 20.5, ymax = 52)
coords <- read.csv("https://pkgstore.datahub.io/core/airport-codes/airport-codes_csv/data/e07739e49300d125989ee543d5598c4b/airport-codes_csv.csv")
coords <- subset(coords, iata_code %in% unique(df$airport))
coords <- coords %>% separate(coordinates, sep = ", ", into = c("long","lat")) %>% dplyr::select(iata_code, long, lat)
coords$long <- as.numeric(coords$long)
coords$lat <- as.numeric(coords$lat)
colnames(coords)[1] <- "airport"
plot_df <- subset(df, year==2021, select = c("airport","ws_intervention_year"))
plot_df$treated_airport <- ifelse(is.na(plot_df$ws_intervention_year), 0, 1)
plot_df <- merge(plot_df, coords, by="airport"); rm(coords)

# Plotting the location of sample airports:
location_plot <- ggplot() + 
  geom_sf(data = states, color = "grey", fill = "white") +
  geom_sf(data = flyways, color = "grey35", fill = alpha("grey", 0.01)) +
  geom_point(data = plot_df, aes(x = long, y = lat), size = 1.65, color = "grey35", alpha = 0.9) +
  geom_point(data = plot_df, aes(x = long, y = lat, fill = as.factor(treated_airport), color = as.factor(treated_airport)), pch=21, size = 1.6, stroke = 0.85) +
  geom_text_repel(data=plot_df, aes(x = long, y = lat, label=airport), size = 2, color = "grey15",  box.padding = 0.14) +
  theme_void() +
  scale_fill_manual(values = c("grey", alpha("#E55537", 0.9)), labels = c("Non-AWHP airport", "AWHP airport")) +
  scale_color_manual(values = c(alpha("grey35", 0.8), alpha("#F4AC9D", 0.9)), labels = c("Non-AWHP airport", "AWHP airport"),
                     guide = guide_legend(label.hjust=0)) +
  xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.87,0.28),
        legend.direction =  "vertical",
        legend.key.height = unit(0.24, 'cm'),
        legend.key.width = unit(0.01, 'cm'),
        legend.text = element_text(size = 6,  color = "grey15", margin = margin(l = 3)))
rm(states, flyways, plot_df)

# Dataframe to plot the presence and, if applicable, the timing of AWHP management intervention across sample airports:
plot_df <- df
plot_df$treated_airport <- ifelse(is.na(plot_df$ws_intervention_year), 0, 1)
plot_df$ws_intervention_year <- ifelse(is.na(plot_df$ws_intervention_year), 2023, plot_df$ws_intervention_year)
plot_order <- (plot_df %>% group_by(airport) %>% slice(1) %>% select(airport, ws_intervention_year) %>% arrange(ws_intervention_year, airport))$airport
plot_df$airport <- factor(plot_df$airport, levels = rev(plot_order))
plot_df$start <- ifelse(plot_df$ws_intervention_year==2023, 2030, plot_df$ws_intervention_year)
plot_df$end <- 2021
plot_df$end <- ifelse(plot_df$ws_intervention_year==2023, 2035, plot_df$end)
plot_df$line_start <- 2000
plot_df$line_end <- 2021
plot_df <- plot_df %>% group_by(airport) %>% slice(1) %>% dplyr::select(airport, treated_airport, start, end, line_start, line_end)

# Plotting the presence and timing of AWHP management:
timing_plot <- ggplot() +
  geom_segment(data = plot_df, aes(x = start, y = airport, xend = end, yend = airport), linewidth=3.4, color = "grey85", alpha = 0.6) +
  geom_point(data = plot_df, aes(x = start, y = airport), size = 3, color = "grey35", alpha = 0.9) +
  geom_point(data = plot_df, aes(x = start, y = airport), size = 2.6, fill = alpha("#E55537", 0.9) , pch=21, color = "#F4AC9D", stroke = 0.7) +
  coord_cartesian(xlim=c(2005.5, 2020.3)) +
  theme_ipsum() +
  scale_x_continuous(breaks = seq(2005,2021,2))+
  xlab("\nYear") + ylab("Airport\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10))
rm(plot_df, plot_order)

# Combine the locationa and timing plots into one figure:
png("figure_2.png", width = 4, height = 5.5, units = "in", res = 600)
ggdraw() +
  draw_plot(location_plot, x = -.01, y = .505, width = 1, height = .5) +
  draw_plot(timing_plot, x = 0.10, y = 0, width = 0.75, height = 0.56) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0.07, 0.12), y = c(0.99, 0.6))
dev.off()
rm(location_plot, timing_plot)
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE 1 ----
## Estimated impact of AWHP management intervention on joint, civil-, and military-specific wildlife strike metrics.

# Function to obtain the estimated average treatment effects:
get_estimates <- function(dep_var, type, covariates) {
  
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
  est <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                       ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                              paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  se <- round(summary(model, agg = "att")$coeftable[1,2], digits = 3)
  p <- round(summary(model, agg = "att")$coeftable[1,4], digits = 3)
  ci <- paste0("[",round(confint(summary(model, agg = "att"), level = .95)[1,1], digits = 3),", ",round(confint(summary(model, agg = "att"), level = .95)[1,2], digits = 3),"]")
  
  temp <- data.frame(estimates = c(est, se, p, ci, model$nobs_origin, round(r2(model)[2], digits = 3), round(r2(model)[6], digits = 3)))
  rownames(temp) <- c("ATT", "SE", "p-value", "95% CI", "N", "R2", "Within R2")
  
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## FIGURE 3 ----
## Estimated change in joint, civil, and military wildlife strike metrics before and after management intervention.

# Function to obtain and plot estimated dynamic treatment effects:
plot_dynamic_estimates <- function(dep_var, type, covariates, plot_limits, plot_breaks) {
  
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
  
  print(model$coeftable)
  
  plot_df <- as.data.frame(model$coeftable[c(2:10),])
  plot_df[nrow(plot_df)+1,] <- c(0, 0, 0, 0)
  plot_df <- plot_df %>%
    mutate(time_to_treatment = c(-8, -6, -4, -2, 2, 4, 6, 8, 10, 0)) %>%
    mutate(ci_90_low = Estimate - `Std. Error`*1.645) %>%
    mutate(ci_90_high = Estimate + `Std. Error`*1.645) %>%
    mutate(ci_95_low = Estimate - `Std. Error`*1.960) %>%
    mutate(ci_95_high = Estimate + `Std. Error`*1.960) 
  
  if (dep_var=="strike_rate" & type=="joint") {
    plot <- ggplot(data = plot_df, aes(x = time_to_treatment, y = Estimate)) +
      geom_hline(yintercept = 0, linetype ="solid", size = 0.5) +
      geom_vline(xintercept = 0, linetype ="dashed", size = 0.5) +
      geom_ribbon(aes(ymin = ci_95_low, ymax = ci_95_high, fill = "95% C.I."), size = 3, alpha = 0.7) + 
      geom_ribbon(aes(ymin = ci_90_low, ymax = ci_90_high, fill = "90% C.I."), size = 3, alpha = 0.7) + 
      geom_line(size = 0.5, color = "grey35") +
      geom_point(shape = 21, size =1.5, color = "grey35", fill = "white", stroke=1) +
      scale_fill_manual(values = c("#E55537", "#F4AC9D"), name = "") +
      scale_y_continuous(breaks = plot_breaks) +
      coord_cartesian(ylim = plot_limits) +
      #scale_x_continuous(expand = c(.1, .1)) +
      labs(x ="", y = "") +
      theme_ipsum() +
      theme( plot.margin = margin(t = 0, r = 1, b = 1, l = 1),
             legend.position = c(0.235,0.94),
             legend.key.height = unit(0.2, "cm"),
             legend.key.width = unit(0.3, "cm"),
             legend.text = element_text(size = 8), 
             axis.text.y = element_text(size = 9), 
             axis.text.x = element_text(size = 9))
  } else {
    plot <- ggplot(data = plot_df, aes(x = time_to_treatment, y = Estimate)) +
      geom_hline(yintercept = 0, linetype ="solid", size = 0.5) +
      geom_vline(xintercept = 0, linetype ="dashed", size = 0.5) +
      geom_ribbon(aes(ymin = ci_95_low, ymax = ci_95_high, fill = "95% C.I."), size = 3, alpha = 0.7) + 
      geom_ribbon(aes(ymin = ci_90_low, ymax = ci_90_high, fill = "90% C.I."), size = 3, alpha = 0.7) + 
      geom_line(size = 0.5, color = "grey35") +
      geom_point(shape = 21, size =1.5, color = "grey35", fill = "white", stroke=1) +
      scale_fill_manual(values = c("#E55537", "#F4AC9D"), name = "") +
      scale_y_continuous(breaks = plot_breaks) +
      coord_cartesian(ylim = plot_limits) +
      #scale_x_continuous(expand = c(.1, .1)) +
      labs(x ="", y = "") +
      theme_ipsum() +
      theme(legend.position = "none",
            plot.margin = margin(t = 1, r = 1, b = 1, l = 1), 
            axis.text.y = element_text(size = 9), 
            axis.text.x = element_text(size = 9))
  }
  
  return(plot)
  
}

# Parts A, B, and C, pertaining to overall wildlife strikes per 5,000 aircraft movements
plot_a <- plot_dynamic_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial"), plot_limits = c(-1.25, 3), plot_breaks = seq(-1,3,1))
plot_b <- plot_dynamic_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial"), plot_limits = c(-1, 3.4), plot_breaks = seq(-1,3,1))
plot_c <- plot_dynamic_estimates(dep_var = "strike_rate", type = "mil", covariates = c(), plot_limits = c(-12.5, 12.5), plot_breaks = seq(-15,15,5))

strike_plot <- 
  ggdraw() +
  draw_plot_label(label = c("A", "B", "C"),
                  x = c(0.06, 0.38, 0.69), y = c(0.98, 0.98, 0.98), 
                  size = 15) +
  draw_label("Dep. var.: Joint wildlife strikes\nper 5,000 movements", hjust = 0, size = 8, x = 0.07, y = 0.84) +
  draw_label("Dep. var.: Civil wildlife strikes\nper 5,000 movements", hjust = 0, size = 8, x = 0.39, y = 0.84) +
  draw_label("Dep. var.: Military wildlife strikes\nper 5,000 movements", hjust = 0, size = 8, x = 0.7, y = 0.84) +
  draw_plot(plot_a, x =.04, y = .06, width = .31, height = .7) +
  draw_plot(plot_b, x = .355, y = .06, width = .31, height = .7) +
  draw_plot(plot_c, x = .67, y = .06, width = .31, height = .7) +
  draw_label("Estimated effect of management intervention", hjust = 0.5, size = 9, angle = 90,
             x = 0.035, y = 0.45) +
  draw_label("Years since management intervention", hjust = 1, size = 9, angle = 0,
             x = 0.98, y = 0.06)
rm(plot_a, plot_b, plot_c)

# Parts D, E, and F, pertaining to disruptive wildlife strikes per 5,000 aircraft movements
plot_d <- plot_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial"), plot_limits = c(-0.4, 0.4), plot_breaks = seq(-0.4, 0.4,0.2))
plot_e <- plot_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial"), plot_limits = c(-0.3, 0.3), plot_breaks = seq(-0.3, 0.3,0.15))
plot_f <- plot_dynamic_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c(), plot_limits = c(-6, 6), plot_breaks = seq(-6, 6,3))

disruptive_plot <- 
  ggdraw() +
  draw_plot_label(label = c("D", "E", "F"),
                  x = c(0.06, 0.38, 0.69), y = c(0.98, 0.98, 0.98), 
                  size = 15) +
  draw_label("Dep. var.: Joint disruptive wildlife\nstrikes per 5,000 movements", hjust = 0, size = 8, x = 0.07, y = 0.84) +
  draw_label("Dep. var.: Civil disruptive wildlife\nstrikes per 5,000 movements", hjust = 0, size = 8, x = 0.39, y = 0.84) +
  draw_label("Dep. var.: Military disruptive wildlife\nstrikes per 5,000 movements", hjust = 0, size = 8, x = 0.7, y = 0.84) +
  draw_plot(plot_d, x =.04, y = .06, width = .31, height = .7) +
  draw_plot(plot_e, x = .355, y = .06, width = .31, height = .7) +
  draw_plot(plot_f, x = .67, y = .06, width = .31, height = .7) +
  draw_label("Estimated effect of management intervention", hjust = 0.5, size = 9, angle = 90,
             x = 0.035, y = 0.45) +
  draw_label("Years since management intervention", hjust = 1, size = 9, angle = 0,
             x = 0.98, y = 0.06)
rm(plot_d, plot_e, plot_f)

# Parts G, H, and I pertaining to wildlife strike costs (Millions, 2023 $)
plot_g <- plot_dynamic_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike"), plot_limits = c(-4.3, 2.2), plot_breaks = seq(-4,2,2))
plot_h <- plot_dynamic_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike"), plot_limits = c(-2, 2), plot_breaks = seq(-2,2,1))
plot_i <- plot_dynamic_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike"), plot_limits = c(-3.5, 1), plot_breaks = seq(-3,1,1))

costs_plot <- 
  ggdraw() +
  draw_plot_label(label = c("G", "H", "I"),
                  x = c(0.06, 0.38, 0.695), y = c(0.98, 0.98, 0.98), 
                  size = 15) +
  draw_label("Dep. var.: Joint wildlife strike costs\n(Millions, 2023 $)", hjust = 0, size = 8, x = 0.07, y = 0.84) +
  draw_label("Dep. var.: Civil wildlife strike costs\n(Millions, 2023 $)", hjust = 0, size = 8, x = 0.39, y = 0.84) +
  draw_label("Dep. var.: Military wildlife strike costs\n(Millions, 2023 $)", hjust = 0, size = 8, x = 0.7, y = 0.84) +
  draw_plot(plot_g, x =.04, y = .06, width = .31, height = .7) +
  draw_plot(plot_h, x = .355, y = .06, width = .31, height = .7) +
  draw_plot(plot_i, x = .67, y = .06, width = .31, height = .7) +
  draw_label("Estimated effect of management intervention", hjust = 0.5, size = 9, angle = 90,
             x = 0.035, y = 0.45) +
  draw_label("Years since management intervention", hjust = 1, size = 9, angle = 0,
             x = 0.98, y = 0.06)
rm(plot_g, plot_h, plot_i)

# Combining the variable specific plots into one figure
png("figure_3.png", width = 6.5, height = 9, units = "in", res = 300)
ggdraw() +
  draw_plot(strike_plot, x =0, y = .66, width = 1, height = .33) +
  draw_plot(disruptive_plot, x = 0, y = .33, width = 1, height = .33) +
  draw_plot(costs_plot, x = 0, y = 0, width = 1, height = .33) 
dev.off()
rm(strike_plot, disruptive_plot, costs_plot)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## TABLE 2 ----
## Heterogeneity in the estimated impact of wildlife hazards management intervention when partitioning managed airports 
## into low versus high management presence (staff years employed).


# Function to obtain the estimated average treatment effects by treatment group:
get_heterogeneous_estimates <- function(dep_var, type, covariates) {
  
  model_df <- df
  
  model_df$var <- model_df[,paste0(type, "_", dep_var)]
  
  if (length(covariates)==2) {
    f_low <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1], " + ", covariates[2], 
                " | airport + year")
    f_high <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                    covariates[1], " + ", covariates[2], 
                    " | airport + year")
  } else if (length(covariates)==1) {
    f_low <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1],
                " | airport + year")
    f_high <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) + ", 
                covariates[1],
                " | airport + year")
  } else {
    f_low <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-12:-9), c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year")
    f_high <- paste0("var ~ sunab(treatment_year, time_to_treatment, ref.p = 0, bin.p = list(c(-8,-7), c(-6,-5), c(-4,-3), c(-2,-1), c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11:14))) | airport + year")
  }
  model <- feols(formula(f_low), subset(model_df, het_treat_years_cat!="High") , se = "cluster")
  low_est <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                    ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                           ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                                  paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  low_se <- round(summary(model, agg = "att")$coeftable[1,2], digits = 3)
  low_p <- round(summary(model, agg = "att")$coeftable[1,4], digits = 3)
  low_ci <- paste0("[",round(confint(summary(model, agg = "att"), level = .95)[1,1], digits = 3),", ",round(confint(summary(model, agg = "att"), level = .95)[1,2], digits = 3),"]")
  low_n <- model$nobs_origin
  low_r2 <- round(r2(model)[2], digits = 3)
  
  model <- feols(formula(f_high), subset(model_df, het_treat_years_cat!="Low") , se = "cluster")
  high_est <- ifelse(summary(model, agg = "att")$coeftable[1,4] <0.01, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"***"),
                     ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.01 & summary(model, agg = "att")$coeftable[1,4]<0.05, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"**"),
                            ifelse(summary(model, agg = "att")$coeftable[1,4] >=0.05 & summary(model, agg = "att")$coeftable[1,4]<0.1, paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3),"*"), 
                                   paste0(round(summary(model, agg = "att")$coeftable[1,1], digits = 3)))))
  high_se <- round(summary(model, agg = "att")$coeftable[1,2], digits = 3)
  high_p <- round(summary(model, agg = "att")$coeftable[1,4], digits = 3)
  high_ci <- paste0("[",round(confint(summary(model, agg = "att"), level = .95)[1,1], digits = 3),", ",round(confint(summary(model, agg = "att"), level = .95)[1,2], digits = 3),"]")
  high_n <- model$nobs_origin
  high_r2 <- round(r2(model)[2], digits = 3)
  
  temp <- data.frame(estimates = c(low_est, low_p, low_se, low_ci,  low_n, low_r2,
                                   high_est, high_p, high_se, high_ci, high_n, high_r2))
  rownames(temp) <- c("Low ATT", "Low p-value", "Low SE", "Low 95% CI", "Low Obs.", "Low R2",
                      "High ATT", "High p-value", "High SE", "High 95% CI", "High Obs.", "High R2")
  
  return(temp)
  
}

# Panel A: Dependent variable is total wildlife strikes per 5,000 movements
get_heterogeneous_estimates(dep_var = "strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_heterogeneous_estimates(dep_var = "strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_heterogeneous_estimates(dep_var = "strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel B: Dependent variable is disruptive wildlife strikes per 5,000 movements
get_heterogeneous_estimates(dep_var = "disruptive_strike_rate", type = "joint", covariates = c("share_military", "share_commercial")) # Joint
get_heterogeneous_estimates(dep_var = "disruptive_strike_rate", type = "civ", covariates = c("share_commercial")) # Civil
get_heterogeneous_estimates(dep_var = "disruptive_strike_rate", type = "mil", covariates = c()) # Military (ANG)

# Panel C: Dependent variable is wildlife strike costs (Millions, 2023 $)
get_heterogeneous_estimates(dep_var = "strike_costs", type = "joint", covariates = c("mil_damage_strike", "civ_damage_strike")) # Joint
get_heterogeneous_estimates(dep_var = "strike_costs", type = "civ", covariates = c("civ_damage_strike")) # Civil
get_heterogeneous_estimates(dep_var = "strike_costs", type = "mil", covariates = c("mil_damage_strike")) # Military (ANG)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%                                                         END                                                      %%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

