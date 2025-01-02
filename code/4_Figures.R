# Load libraries ----

library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(png)
library(grid)
library(ggspatial)
library(rnaturalearth)

rm(list=ls())


# Set working directory and graphic options ----


setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Seabirds/Petrel_Puffin_Trend/code")

options(scipen = 999)
sf_use_s2(FALSE)

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

# Figure 1: Maps of colony locations ----


provinces <- ne_states(country = c("Canada","United States of America"), returnclass = "sf")

basemap2 <- ggplot(data=provinces) + 
  geom_sf(fill="gray90", col = "gray60", size=0.001) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), 
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"), 
        panel.grid.major = element_line(color = "gray80", linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "white")) + 
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE) +
  annotation_scale(location = "br", width_hint = 0.4, bar_cols = c("gray40", "white"), line_col="gray40") +
  annotate("text", label = "LB", x=-60, y=52.5, size=3, col = "gray30") +
  annotate("text", label = "QC", x=-60, y=51.5, size=3, col = "gray30") +
  annotate("text", label = "NF", x=-56, y=48.5, size=3, col = "gray30") +
  annotate("text", label = "NS", x=-63.5, y=45, size=3, col = "gray30") +
  annotate("text", label = "NB", x=-66, y=46, size=3, col = "gray30") 
  
basemap2 


# *Load relevant seabird data and prepare for plotting ----


# Coordinates of LESP and ATPU colonies
colony_coords <- read_xlsx("../data/LESP_ATPU_coords.xlsx", sheet = 1) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),crs=4326) %>%
  dplyr::rename(Count = Most_Recent_Count_Individuals)

# Repeat counts at colonies monitored for trend
colony_counts <- read_xlsx("../data/LESP_ATPU_counts.xlsx", sheet = 1)

most_recent_counts <- colony_counts %>%
  group_by(Species,Colony_Name_For_Trends) %>%
  summarize(Recent_Count = Mature_Individuals[Year == max(Year)],
            Most_Recent_Year = max(Year))

# Identify colonies that were used for trend analysis; ensure they use most updated information
colony_coords <- most_recent_counts[,c("Species","Colony_Name_For_Trends","Recent_Count","Most_Recent_Year")] %>% 
  mutate(Used_For_Trend = 1) %>%
  left_join(colony_coords,.)

colony_coords$Count[!is.na(colony_coords$Recent_Count)] = colony_coords$Recent_Count[!is.na(colony_coords$Recent_Count)]
colony_coords$Most_Recent_Count_Year[!is.na(colony_coords$Most_Recent_Year)] = colony_coords$Most_Recent_Year[!is.na(colony_coords$Most_Recent_Year)]

colony_coords$Count[colony_coords$Count == "present"] <- 1
colony_coords$Count <- as.numeric(colony_coords$Count)

colony_coords <- colony_coords %>% dplyr::select(-Recent_Count,Most_Recent_Year)


# *Construct maps for each species ----


colony_coords <- colony_coords %>% 
  subset(Count > 0) %>%
  mutate(Count_Factor = case_when(
    Count == 1 ~ "present",
    Count %in% 2:1000 ~ "<1,000",
    Count %in% 1000:5000 ~ "1,000 to 5,000",
    Count %in% 5000:10000 ~ "5,000 to 10,000",
    Count %in% 10000:50000 ~ "10,000 to 50,000",
    Count %in% 50000:100000 ~ "50,000 to 100,000",
    Count %in% 100000:500000 ~ "100,000 to 500,000",
    Count %in% 500000:1000000 ~ "500,000 to 1,000,000",
    Count > 1000000 ~ "> 1,000,000"))

# make it an ordered factor
colony_coords$Count_Factor <- factor(colony_coords$Count_Factor, 
                                     levels=c("present",
                                              "<1,000",
                                              "1,000 to 5,000",
                                              "5,000 to 10,000",
                                              "10,000 to 50,000",
                                              "50,000 to 100,000",
                                              "100,000 to 500,000",
                                              "500,000 to 1,000,000",
                                              "> 1,000,000"))

# Colour palette
col_pal <- rev(viridis::plasma(10))[3:10] %>%colorRampPalette()


# *LESP ----


LESP_clipart <- readPNG("../data/images/LESP_clipart.png", native=TRUE) %>%
  rasterGrob(interpolate=TRUE)

Fig1A <- basemap2 + 
  
  # All colonies
  geom_sf(data=subset(colony_coords, Species=="LESP"), 
          aes(geometry=geometry, color = Count_Factor, size = Count_Factor), 
          alpha=0.8, shape=20, show.legend = TRUE) + 
  
  # Colonies monitored for trend
  geom_sf(data=subset(colony_coords, Species=="LESP" & Used_For_Trend == 1), 
          aes(geometry=geometry), shape=1, size=6) + 
  
  scale_color_manual(values=col_pal(length(unique(colony_coords$Count_Factor))), drop = FALSE) +
  scale_size_manual(values=seq(3,7,0.5)) +
  
  labs(color = "Most Recent Count", size = "Most Recent Count") +
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE)+
  annotate("text", label = "A", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8)+
  theme(legend.position="none")+
  annotation_custom(LESP_clipart, xmin=-67, xmax=-62, ymin = 51, ymax=56)

Fig1A


# *ATPU ----


ATPU_clipart <- readPNG("../data/images/ATPU_clipart.png", native=TRUE) %>%
  rasterGrob(interpolate=TRUE)

Fig1B <- basemap2 + 
  
  # All colonies
  geom_sf(data=subset(colony_coords, Species=="ATPU"), 
          aes(geometry=geometry, color = Count_Factor, size = Count_Factor), 
          alpha=0.8, shape=20, show.legend = TRUE) + 
  
  # Colonies monitored for trend
  geom_sf(data=subset(colony_coords, Species=="ATPU" & Used_For_Trend == 1), 
          aes(geometry=geometry), shape=1, size=6) + 
  
  scale_color_manual(values=col_pal(length(unique(colony_coords$Count_Factor))), 
                     drop = FALSE) +
  scale_size_manual(values=seq(3,7,0.5), drop = FALSE) +
  
  labs(color = "Most Recent Count", size = "Most Recent Count") +
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE)+
  annotate("text", label = "B", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8)+
  theme(legend.spacing.y = unit(1, "mm"), legend.direction="vertical",
        legend.box="vertical",
        #legend.position="none", 
        legend.box.background = element_rect(color = "black",fill = "white"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))+
  annotation_custom(ATPU_clipart, xmin=-67.5, xmax=-61, ymin = 51, ymax=55)

Fig1B


# *Combine into single figure and save ----


Fig1 <- Fig1A|Fig1B
Fig1

ggsave(filename="../output/figures/maps/Fig1_maps.png", plot=Fig1, 
       device="png", dpi=300, units="cm", width=30, height=15)




# Figure 2: Trajectories of each LESP colony ----



# - produced by script 2_LESP_analysis.R, can load via
load("../output/model_output/LESP_wksp.RData")
colony_trajectory_plot

# Figure 3: Trajectories of each ATPU colony ----



# - produced by script 3_ATPU_analysis.R, can load via
load("../output/model_output/ATPU_wksp.RData")
colony_trajectory_plot


# Figure 4: Regional trajectories and trends for both species ----



# Load LESP workspace
load("../output/model_output/LESP_wksp.RData")
LESP_clipart <- readPNG("../data/images/LESP_clipart_rotated.png", native=TRUE) %>%
  rasterGrob(interpolate=TRUE)

plot_margins <- theme(plot.margin = margin(0.1,0.1,0.5,0.5, "cm"))
Fig4A <- regional_trajectory_plot + annotate("text", label = "A", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(LESP_clipart, xmin=2010, xmax=2022, ymin = 10, ymax=14)
Fig4C <- trend_5_yr_plot + annotate("text", label = "C", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(LESP_clipart, xmin=2010, xmax=2022, ymin = 14, ymax=29)
Fig4E <- trend_3Gen_plot + annotate("text", label = "E", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(LESP_clipart, xmin=5, xmax=8, ymin = 0.55, ymax=0.8)


# Load ATPU workspace
load("../output/model_output/ATPU_wksp.RData")
ATPU_clipart <- readPNG("../data/images/ATPU_clipart_rotated.png", native=TRUE) %>%
  rasterGrob(interpolate=TRUE)

Fig4B <- regional_trajectory_plot + annotate("text", label = "B", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(ATPU_clipart, xmin=2006, xmax=2018, ymin = 1.9, ymax=2.5)
Fig4D <- trend_5_yr_plot + annotate("text", label = "D", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(ATPU_clipart, xmin=2008, xmax=2018, ymin = 16, ymax=28)
Fig4F <- trend_3Gen_plot + annotate("text", label = "F", x=-Inf, y=Inf, vjust=2, hjust=-1, size=6) + plot_margins +
  annotation_custom(ATPU_clipart, xmin=4.4, xmax=7.5, ymin = 0.27, ymax=0.5)

# Combine into single 6-panel figure
Fig4 <- Fig4A + Fig4B + Fig4C + 
  Fig4D + Fig4E + Fig4F +
  plot_layout(nrow=3, heights = c(2, 2, 2))

ggsave(filename="../output/figures/trajectory_and_trend_plots/Fig4.png", plot=Fig4, 
       device="png", dpi=300, units="cm", width=20, height=20)



# Figures for Appendices ----

# Figure A1 ----

# load simulation results
simulation_results <- readRDS("../output/model_output/simulation_results.rds")

# make coverage a factor for the 50 year and 10 year trend results
simulation_results$cov_50yr_f <- factor(simulation_results$cov_50yr, labels = c("No", "Yes"))
simulation_results$cov_10yr_f <- factor(simulation_results$cov_10yr, labels = c("No", "Yes"))

# make a factor for whether the simulation had correlated temporal trajectories (shared environmental variation)
simulation_results$corr <- ifelse(simulation_results$sd_shared==0, "Shared Environmental Covariate", "Unshared Environmental Covariate")

# restructure the dataframe to have a factor for 10 year or 50 year trend
sims50 <- simulation_results[, c(3,4,5,6,14,16)]
sims50$trend <- "50-year trend"
sims10 <- simulation_results[, c(8,9,10,11,15,16)]
sims10$trend <- "10-year trend"

colnames(sims50) <- c("trend_true","trend_est_q025", "trend_est_q500", "trend_est_q975", "cov_f", "corr" ,"trend" )
colnames(sims10) <- c("trend_true","trend_est_q025", "trend_est_q500", "trend_est_q975", "cov_f", "corr" ,"trend" )
sims <- rbind(sims10, sims50)

col_pal <- viridis::plasma(9)
col_pal <- col_pal[c(6,2)]

FigA1 <- ggplot(data = sims, aes(x = trend_true, y = trend_est_q500, 
                                    ymin = trend_est_q025, ymax = trend_est_q975,
                                    col=cov_f))+
  geom_abline(intercept=0,slope=1,col="black", linetype="dashed")+
  geom_vline(xintercept=0,col="black")+
  geom_hline(yintercept=0,col="black")+
  geom_errorbar(width=0,lwd=0,show.legend = FALSE)+
  geom_point(size=0)+
  geom_point(data = subset(sims, cov_f=="Yes"), aes(x = trend_true, y = trend_est_q500), color=col_pal[2], size=0.5)+
  geom_errorbar(data = subset(sims, cov_f=="Yes"), aes(x = trend_true, y = trend_est_q500,
                                                                ymin = trend_est_q025, ymax = trend_est_q975), color=col_pal[2],width=0, lwd=0.1)+
  geom_point(data = subset(sims, cov_f=="No"), aes(x = trend_true, y = trend_est_q500),color=col_pal[1], size=0.5)+
  geom_errorbar(data = subset(sims, cov_f=="No"), aes(x = trend_true, y = trend_est_q500,
                                                               ymin = trend_est_q025, ymax = trend_est_q975), color=col_pal[1],width=0, lwd=0.1)+
  theme_bw()+
  xlab("True (simulated) regional trend")+
  ylab("Estimated regional trend")+
  scale_color_manual(values=col_pal, name = "CRI Coverage")+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(axis.title.y = element_text(margin = margin(0,10,0,0),size=14),
        axis.title.x = element_text(margin = margin(10,0,0,0),size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size = 12, colour = "black"),
        strip.background = element_rect(colour = "black",
                                        fill = "grey90",
                                        linetype = "solid"),
        legend.spacing.y = unit(1, "mm"), legend.direction="vertical",
        legend.box="vertical",
        legend.box.background = element_rect(color = "black",fill = "white"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14)) +
  facet_grid(trend ~ corr)

ggsave(filename="../output/figures/trajectory_and_trend_plots/FigA1.png", plot=FigA1, 
       device="png", dpi=300, units="cm", width=25, height=20)


# Figures A2 & A3 ----

# posterior predictive check and observed vs. predicted counts for petrels

# Load LESP workspace
load("../output/model_output/LESP_wksp.RData")

# Plot results of Posterior predictive check
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

# ~Plot observed counts versus estimated annual indices at each colony

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
annual_summary_colony <- annual_summary_colony %>% full_join(spdat)

ObsPredPlot <- ggplot(annual_summary_colony, 
                      aes(x = Count, xmin = Count - 1.96*SE_est, xmax = Count + 1.96*SE_est, 
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
        axis.text.x = element_text(size=12))

ObsPredPlot

ggsave(filename="../output/figures/goodness_of_fit/LESP_Obs_vs_Pred.png", plot=ObsPredPlot, 
       device="png", dpi=300, units="cm", width=20, height=20)

# Figures A4 & A5 ----

# posterior predictive check and observed vs. predicted counts for puffins

# Load ATPU workspace
load("../output/model_output/ATPU_wksp.RData")

# Plot results of Posterior predictive check
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

ggsave(filename="../output/figures/goodness_of_fit/ATPU_PPC.png", plot=PostPredCheckPlot, 
       device="png", dpi=300, units="cm", width=20, height=15)

# ~Plot observed counts versus estimated annual indices at each colony

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
annual_summary_colony <- annual_summary_colony %>% full_join(spdat)

ObsPredPlot <- ggplot(annual_summary_colony, 
                      aes(x = Count, xmin = Count - 1.96*SE_est, xmax = Count + 1.96*SE_est, 
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
        axis.text.x = element_text(size=12))

ObsPredPlot

ggsave(filename="output/figures/goodness_of_fit/ATPU_Obs_vs_Pred.png", plot=ObsPredPlot, 
       device="png", dpi=300, units="cm", width=20, height=20)