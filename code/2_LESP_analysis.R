
# Load libraries ----

library(tidyverse) # for data formatting and plotting
library(readxl)    # for importing xlsx
library(jagsUI)    # for analysis
library(mgcv)      # for creating jagam object (bayesian gams)
library(scales)    # for plotting
library(here)      # for sanity
library(patchwork) # for stacking plots real nice
library(mapview)   # for quick looks at spatial data
library(sf)        # for maps
library(ggspatial) # for maps

rm(list=ls())

# Define theme for plotting ----


CustomTheme <- theme_set(theme_bw())
CustomTheme <- theme_update(legend.key = element_rect(colour = NA), 
                            legend.key.height = unit(1.2, "line"),
                            panel.grid.major = element_line(colour = 'transparent'),
                            panel.grid.minor = element_line(colour = 'transparent'),
                            panel.border = element_rect(linetype = "solid",
                                                        colour = "black",
                                                        size = 1, fill = NA),
                            axis.line = element_line(colour = "black"),
                            strip.text = element_text(size = 12, colour = "black"),
                            strip.background = element_rect(colour = "black",
                                                            fill = "grey90",
                                                            linetype = "solid"),
                            axis.title.y = element_text(margin = margin(0,10,0,0),size=14),
                            axis.title.x = element_text(margin = margin(10,0,0,0),size=14),
                            axis.text.y = element_text(size=12),
                            axis.text.x = element_text(size=12),
                            panel.background = element_rect(fill = "white"))


# Custom Functions ----


`%!in%` <- Negate(`%in%`)


# Set working directory ----


setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Seabirds/Petrel_Puffin_Trend/code")


# Read/modify data ----


spdat = read_xlsx("../data/LESP_ATPU_counts.xlsx", sheet = 1) %>%
  subset(Species == "LESP") %>%
  dplyr::rename(Count = Mature_Individuals) %>% subset(!is.na(Count)) #%>% subset(Colony_Name_For_Trends != "Machias Seal Island, NB")

# Calculate SE for counts with upper and lower confidence limit but no SE
for(i in 1:nrow(spdat)){
  if(is.na(spdat$SE[i]) & !(is.na(spdat$Lower_CI_95[i]))){
    spdat$SE[i] <- (spdat$Upper_CI_95[i] - spdat$Lower_CI_95[i])/3.92
  }
}

spdat <- spdat[,c("Colony_Name_For_Trends","Year","Count","SE")]

# Restrict to date ranges
spdat <- subset(spdat, Year >= 1965)

# align variables with original code
names(spdat) <- tools::toTitleCase(names(spdat))

# Load colony coordinates (for ordering plots by latitude) ----
colony_coords <- read_xlsx("../data/LESP_ATPU_coords.xlsx", sheet = 1) %>%
  subset(Species == "LESP")

spdat <- left_join(spdat, colony_coords[,c("Colony_Name_For_Trends","Latitude","Longitude")])
spdat <- spdat %>% 
  dplyr::rename(Colony = Colony_Name_For_Trends) %>%
  arrange(Latitude,Year)

spdat$Colony <- factor(spdat$Colony, levels = unique(spdat$Colony))

# Prepare data for analysis ----

# Tables that link year index to actual year
spdat$year_numeric <- spdat$Year - min(spdat$Year) + 1
year_table = data.frame(Year = min(spdat$Year):max(spdat$Year))
year_table$year_numeric = 1:nrow(year_table)

colonies_to_include = spdat %>%
  group_by(Colony) %>%
  summarize(mean_count = mean(Count),
            recent_count = Count[which.max(Year)],
            first_survey = min(Year),
            last_survey = max(Year),
            n_surveys = length(unique(Year)),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>%
  subset(n_surveys > 1)

# Omit the smallest colony (Machias Seal) that is strongly influencing uncertainty
spdat = subset(spdat, Colony %in% colonies_to_include$Colony)

# Tables that link colony numbers to colony names
spdat$colony_numeric <- as.integer(factor(spdat$Colony))
colony_name_table = unique(spdat[,c("Colony","colony_numeric")])

# Plot raw data ----
ggplot(data = spdat,aes(x = Year, y = Count))+
  geom_point()+
  geom_line(linetype = 2)+
  facet_wrap(Colony~., scales = "free_y")+
  scale_y_continuous(labels = comma)

# Empirical relationship between log SE and log count
# Note: this should probably go in an appendix

ggplot(data = spdat, aes(x = log(Count), y = log(SE)))+
  geom_point()+
  ggtitle("Empirical relationship between log(Count) and log(SE)") + 
  CustomTheme

a = spdat
a$logc <- log(a$Count)
a$logSE <- log(a$SE)
lm(logSE ~ logc, data = a)

# Fit model ----


# Data for import into jags
nknots = 12
year <- spdat$Year - min(year_table$Year) + 1
ymax <- max(year_table$year_numeric)
nyears = length(1:ymax)
colony = spdat$colony_numeric
ncolony <- max(colony)
count <- round(spdat$Count) # Must be integer
ncounts = length(count)

# Use jagam to prepare basis functions
nyearspred = length(1:ymax)
preddat = data.frame(yrs = 1:ymax,count = 1)
form = as.formula(paste("count ~ s(yrs,k =",nknots,")"))
gamprep = jagam(formula = form,
                data = preddat,
                file = "../code/tempgam.txt",
                centred = T)

# Package data into a list for JAGS
jags_data = list(X = gamprep$jags.data$X,
                 S1 = gamprep$jags.data$S1,
                 zero = gamprep$jags.data$zero,
                 colony = colony,
                 ncounts = ncounts,
                 ncolony = ncolony,
                 count = count,
                 nknots = nknots,
                 nyearspred = nyearspred,
                 year = year,
                 survey_count = spdat$Count,
                 survey_SE = spdat$SE)

# Fit model using JAGS
parameters.to.save = c(
  
  # Estimate of annual process variance
  "ProcVar_sd",  
  
  # Parameters for estimating standard error for surveys that don't have SE estimates available
  "intercept_SE",
  "slope_SE",
  "sd_SE",
  
  # Magnitude of observation error for each survey
  "survey_SE",
  
  # Variance in GAM parameters
  "sdbeta",
  
  # GAM components
  "C",
  "beta.X",
  "B.X",
  
  # Annual population indices
  "population_index",
  
  # Simulated counts
  "simulated_count",
  
  # Discrepancy measures for posterior predictive checks (goodness-of-fit testing)
  "RMSE_actual",
  "RMSE_simulated",
  
  # Penalty measures
  'lambda'
)

if (!file.exists("../output/model_output/LESP_fitted_new.rds")){
  
  out <- jags(data = jags_data,
              parameters.to.save = parameters.to.save,
              inits = NULL,
              n.iter = 11000000*2,
              n.burnin = 1000000,
              n.thin = 1000,
              model.file = "Seabird_Model.jags",
              n.chains = 3,
              parallel = TRUE)
  
  # Save fitted model
  saveRDS(out, file = "../output/model_output/LESP_fitted_new.rds")
}

# Load fitted model ----
out <- readRDS(file = "../output/model_output/LESP_fitted_new.rds")

# How long it took to fit the model
out$mcmc.info$elapsed.mins

# Model convergence ----

# Which parameters have not converged?
unlist(out$Rhat)[which(unlist(out$Rhat)>1.1)] # Fully converged

# Which parameters have effective sample sizes less than 1000?
n.eff <- unlist(out$n.eff)
n.eff <- n.eff[-which(n.eff == 1)]
n.eff[which(n.eff < 1000)]


# Goodness-of-fit assessments ----

# ~Posterior predictive check~
# Calculate Bayesian p-value by comparing discrepancy measures from simulated datasets
# under a 'perfectly specified model' to discrepancy measures from the empirical data
# (Note: Bayesian p-values between 0.3 and 0.7 imply that the model fits the data reasonably well
#  i.e., it produces simulated datasets that "look like" the empirical dataset)

Bayesian_pval <- mean(out$sims.list$RMSE_actual > out$sims.list$RMSE_simulated) %>% round(2)
Bayesian_pval # 0.72

# Plot results of Posterior predictive check: We want to see values clustered along the 1:1 line
lim <- range(c(out$sims.list$RMSE_actual,out$sims.list$RMSE_simulated))
RMSE_df = data.frame(actual = out$sims.list$RMSE_actual, simulated = out$sims.list$RMSE_simulated)
PostPredCheckPlot <- ggplot(data = RMSE_df,
                aes(x = actual, y = simulated )) +
  geom_hex(binwidth = diff(lim)/50) +
  scale_fill_gradientn(colors = c("gray95","darkblue"), name = "Number of\nsimulated\ndatasets\n") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  coord_cartesian(xlim = lim,ylim=lim)+
  xlab("RMSE Actual Datasets")+
  ylab("RMSE Simulated Datasets")+
  theme_bw() + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0),size=14),
        axis.title.x = element_text(margin = margin(10,0,0,0),size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        legend.spacing.y = unit(1, "mm"), legend.direction="vertical",
        legend.box="vertical",
        legend.box.background = element_rect(color = "black",fill = "white"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

PostPredCheckPlot

ggsave(filename="../output/figures/goodness_of_fit/LESP_PPC.png", plot=PostPredCheckPlot, 
       device="png", dpi=300, units="cm", width=20, height=15)

# ~Plot observed counts versus estimated annual indices at each colony~
# Do they track the 1:1 line?

# Estimates of SE for each survey (fills in missing ones)
spdat$SE_est = out$mean$survey_SE

# Extract predictions of annual indices in dataframe format
fit_samples_colony = reshape2::melt(out$sims.list$population_index) %>%
  rename(samp = Var1, year_numeric = Var2, colony_numeric = Var3, N_pred = value) %>%
  full_join(colony_name_table) %>% full_join(year_table)

# Summarize
annual_summary_colony = fit_samples_colony %>% 
  group_by(Colony, Year) %>%
  summarize(q025 = quantile(N_pred,0.025),
            q50 = quantile(N_pred,0.500),
            mean = mean(N_pred),
            q975 = quantile(N_pred,0.975))

# Join with observed data
obs_vs_pred <- annual_summary_colony %>% left_join(spdat,.) %>% mutate(Count_lcl = Count - 1.96*SE_est, Count_ucl = Count + 1.96*SE_est)
obs_vs_pred$Count_lcl[obs_vs_pred$Count_lcl<=0]<-0.1
ObsPredPlot <- ggplot(obs_vs_pred, 
       aes(x = Count, xmin = Count_lcl, xmax = Count_ucl, 
           y = q50, ymin = q025, ymax = q975, 
           col = as.factor(Colony)))+
  geom_abline(slope = 1, intercept = 0, linetype="dashed")+
  geom_point()+
  geom_errorbar(width=0)+
  geom_errorbarh(height=0)+
  scale_colour_manual(values=viridis::plasma(length(unique(annual_summary_colony$Colony)))) +
  xlab("Observed Count (log-scale)")+
  ylab("Estimated Population Index (log-scale)")+
  scale_y_continuous(trans="log10", labels = comma)+
  scale_x_continuous(trans="log10", labels = comma)+
  theme(legend.position="none") +
  theme(axis.title.y = element_text(margin = margin(0,10,0,0),size=14),
        axis.title.x = element_text(margin = margin(10,0,0,0),size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12))+
  facet_wrap(Colony~.)

ObsPredPlot

ggsave(filename="../output/figures/goodness_of_fit/LESP_Obs_vs_Pred.png", plot=ObsPredPlot, 
       device="png", dpi=300, units="cm", width=20, height=20)


# Plot colony trajectories ----

# Extract predictions of annual indices in dataframe format
fit_samples_colony = reshape2::melt(out$sims.list$population_index) %>%
  rename(samp = Var1, year_numeric = Var2, colony_numeric = Var3, N_pred = value) %>%
  full_join(colony_name_table) %>% full_join(year_table)

annual_summary_colony = fit_samples_colony %>% 
  group_by(Colony, Year) %>%
  summarize(N_med = quantile(N_pred,0.500),
            N_q025 = quantile(N_pred,0.025),
            N_q975 = quantile(N_pred,0.975))

# Create plot of colony trajectories, overlaid with raw data and a symbol for whether the count had an SE

# reorder the colony factor such that plotting order is by descending latitude (north to south)
spdat <- spdat %>% mutate(Colony = fct_reorder(Colony, Latitude, .desc = TRUE))
annual_summary_colony <- annual_summary_colony %>% mutate(Colony = factor(Colony, levels = levels(spdat$Colony)))
fit_samples_colony <- fit_samples_colony %>% mutate(Colony = factor(Colony, levels = levels(spdat$Colony)))

colony_trajectory_plot <- ggplot()+

  # # Thick dashed line for 95% CI
   geom_ribbon(data = annual_summary_colony, 
               aes(x = Year, ymin=N_q025, ymax = N_q975),
               fill = "gray80", col = "transparent", 
               linetype = 2, linewidth = 0.5)+
  # 
  # # Thick line for posterior median
   geom_line(data = annual_summary_colony, 
             aes(x = Year, y= N_med), linewidth = 1, col = "gray50")+
   
  
  # Observed counts
  geom_point(data = spdat, aes(x = Year, y = Count), size = 2)+
  geom_errorbar(data = spdat, aes(x = Year, ymin = Count-1.96*SE_est, ymax = Count+1.96*SE_est), width = 0, linewidth = 1)+
  geom_point(data = subset(spdat, is.na(SE)), aes(x = Year, y = Count), size = 5, shape=1)+
  
  scale_y_continuous(labels = comma)+
  scale_x_continuous(limits=c(1965,2025), expand=c(0,0)) + 
  
  scale_color_manual(values=rep("grey50",length(unique(fit_samples_colony$samp))), 
                     guide = "none")+
  
  ylab("Index of Abundance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Colony, scales = "free_y", ncol=3)

colony_trajectory_plot

ggsave(filename="../output/figures/trajectory_and_trend_plots/LESP_trajectory_colony_all_years.png", plot=colony_trajectory_plot, 
       device="png", dpi=300, units="cm", width=30, height=25)

# Plot regional trajectories ----


fit_samples_regional = fit_samples_colony %>% 
  group_by(samp,Year) %>%
  summarize(N_pred = sum(N_pred))

# Summarize the median and 95% CI
annual_summary_regional <- fit_samples_regional %>%
  group_by(Year) %>%
  summarize(N_med = median(N_pred),
            N_q025 = quantile(N_pred,0.025),
            N_q975 = quantile(N_pred,0.975))

# Choose 1000 samples from the posterior to visualize on plot
samples_to_plot <- sample(unique(fit_samples_regional$samp),1000)

# Choose years that are considered "the most reliable" for summarizing trends
# (i.e., the year range where surveys are available at the largest colonies)
t_start <- min(annual_summary_colony$Year)
t_end <- 2023

# Choose y limits for plot
ylim <- c(0,max(annual_summary_regional$N_q975))

# turn off scientific notation
options(scipen=999)

# Create plot of regional trajectory
regional_trajectory_plot <- ggplot()+
  
  # Thick darkblue dashed line for 95% CI
  geom_ribbon(data = subset(annual_summary_regional,Year >= t_start & 
                              Year <= t_end), 
              aes(x = Year, ymin=N_q025/1000000, ymax = N_q975/1000000),
              fill = "gray90", col = "black", 
              linetype = 2, linewidth = 0.5)+
  
  # Plot 1000 trajectories from Bayesian posterior
  geom_line(data = subset(fit_samples_regional, 
                          samp %in% samples_to_plot & 
                            Year >= t_start & 
                            Year <= t_end), 
            aes(x = Year, y = N_pred/1000000, col = factor(samp)),alpha = 0.1)+
  
  # Thick darkblue line for posterior median
  geom_line(data = subset(annual_summary_regional, 
                          Year >= t_start & 
                            Year <= t_end), 
            aes(x = Year, y= N_med/1000000), linewidth = 1, col = "black")+

  
  #coord_cartesian(ylim = c(ylim$min/1000000,ylim$max/1000000))+
  
  scale_y_continuous(breaks = seq(0, 15, by = 5)) +
  scale_x_continuous(limits=c(1965,2025), breaks = seq(1970, 2020, by = 10), expand = c(0, 0))+
  coord_cartesian(ylim=ylim/1000000)+
  scale_color_manual(values=rep("grey50",length(unique(fit_samples_regional$samp))), 
                     guide = "none")+
  
  ylab("Index of Abundance\n(millions)")

regional_trajectory_plot

ggsave(filename="../output/figures/trajectory_and_trend_plots/LESP_trajectory_regional_all_years.png", plot=regional_trajectory_plot, 
       device="png", dpi=300, units="cm", width=20, height=20)


# Trend analysis ----
#   - summarize geometric mean rates of change across specific time intervals



# Rolling 5-year windows ----


trend_5_yr_windows <- fit_samples_regional %>%
  group_by(samp) %>%
  arrange(Year) %>%
  summarize(Year = Year, 
            N_pred_start = lag(N_pred, 2),  # 2 years before
            N_pred_end = lead(N_pred, 2),   # 2 years ahead
            trend = 100 * ((N_pred_end/N_pred_start)^(1/(4))-1)) %>% # 5-year window
  filter(!is.na(N_pred_start), !is.na(N_pred_end)) 

trend_5_yr_windows_summary <- trend_5_yr_windows %>% group_by(Year) %>%
  summarize(trend_med = median(trend),
            trend_q025 = quantile(trend,c(0.025)),
            trend_q975 = quantile(trend,c(0.975)))

trend_5_yr_plot <- ggplot()+
  geom_line(data = trend_5_yr_windows, aes(x = Year, y= trend, col = factor(samp)),alpha = 0.1)+
  scale_color_manual(values=rep("grey50",length(unique(trend_5_yr_windows$samp))), 
                     guide = "none")+
  geom_line(data = trend_5_yr_windows_summary, aes(x = Year, y= trend_med), linewidth = 1, col = "black")+
  geom_ribbon(data = trend_5_yr_windows_summary, aes(x = Year, ymin=trend_q025, ymax = trend_q975),
              fill = "transparent", col = "black", 
              linetype = 2, linewidth = 0.5)+
  geom_hline(yintercept=0) +
  scale_x_continuous(limits=c(1965,2025), breaks = seq(1970, 2020, by = 10),expand = c(0, 0))+
  scale_y_continuous(limits = c(-20,30))+
  ylab("5-year Trend\n(% change per year)")

trend_5_yr_plot

ggsave(filename="../output/figures/trajectory_and_trend_plots/LESP_trend_5_yr_windows.png", plot=trend_5_yr_plot, 
       device="png", dpi=300, units="cm", width=20, height=20)

# Where does median cross 0 (implying change from growing to declining, or vice versa)
trend_5_yr_windows_summary %>%
  mutate(next_trend = c(trend_med[-1],NA)) %>%
  subset(sign(trend_med) != sign(next_trend))
# 1986


# 3-generation trend ----


# Generation Length = 14.8 years; COSEWIC
t_end <- 2023
t_start <- floor(t_end - (14.8*3))

regional_indices_t_start <- subset(fit_samples_regional, Year == t_start)
regional_indices_t_end <- subset(fit_samples_regional, Year == t_end)
trend_3Gen <- data.frame(Trend = 100 * ((regional_indices_t_end$N_pred/regional_indices_t_start$N_pred)^(1/(t_end-t_start))-1))

trend_label = paste0("Posterior Summary\n",round(mean(trend_3Gen$Trend),2), "% (",round(quantile(trend_3Gen$Trend,0.025),2)," to ",round(quantile(trend_3Gen$Trend,0.975),2),")\nProb(Trend > 0) = ",round(mean(trend_3Gen$Trend > 0),2))
trend_3Gen_plot <- ggplot()+
  geom_vline(xintercept = 0, col = "gray80", linewidth = 1)+
  geom_density(data = trend_3Gen,aes(x = Trend), fill = "gray50", col = "transparent", alpha = 0.5)+
  
  xlab("3-Generation Trend")+
  ylab("Probability Density")+
  coord_cartesian(xlim=c(-8,8))

ylim <- layer_scales(trend_3Gen_plot)$y$range$range

trend_3Gen_plot <- trend_3Gen_plot +
  geom_text(aes(x = -8, y = 0),label = trend_label, hjust = 0,vjust=-0.5, size = 3)
trend_3Gen_plot

ggsave(filename="../output/figures/trajectory_and_trend_plots/LESP_Trend_3Gen_Posterior.png", plot=trend_3Gen_plot, 
       device="png", dpi=300, units="in", width=7, height=4)

# Calculate overall percent change in population across 3 generations
percent_change <- 100 * (regional_indices_t_end$N_pred - regional_indices_t_start$N_pred)/regional_indices_t_start$N_pred
mean(percent_change)
quantile(percent_change,c(0.025,0.975))



# Save workspace ----


save.image("../output/model_output/LESP_wksp.RData")


# Percent of regional population in each colony ----


proportions <- fit_samples_colony %>%
  group_by(samp, Year) %>%
  mutate(prop = N_pred/sum(N_pred)) %>%
  group_by(Colony,Year) %>%
  summarize(prop_mean = mean(prop),
            prop_lcl = quantile(prop,0.05),
            prop_ucl = quantile(prop,0.95))

ggplot(data = proportions,aes(x = Year, ymin = prop_lcl, ymax = prop_ucl, y = prop_mean)) + 
  geom_ribbon(fill = "lightblue")+
  geom_line()+
  theme_bw()+
  facet_wrap(Colony~.)

# Proportion on largest 3 colonies
proportions %>%
  group_by(Year) %>%
  summarize(prop_sum = sum(prop_mean[Colony %in% c("Great Island, NF","Gull Island, NF", "Baccalieu Island, NF")])) %>%
  as.data.frame()


spdat %>% group_by(Colony) %>% summarize(mean = mean(Count))
