library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)
library(png)
library(grid)

rm(list=ls())

# -------------------------------------------------
# Set working directory and graphic options
# -------------------------------------------------

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Seabirds/Petrel_Puffin_Trend/code")

options(scipen = 999)
sf_use_s2(FALSE)

# **********************************************************
# **********************************************************
# Figure 1: Maps of colony locations
# **********************************************************
# **********************************************************

# Set plotting region and define a basemap
bbox <- st_bbox(c(xmin = -68, xmax = -51, ymin = 42.5, ymax = 58), crs = 4326)

# downloaded from https://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
coastline <- st_read('../data/spatial_data/Shoreline_Coastline_GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp') %>% 
  st_crop(bbox)

basemap2 <- ggplot(data=coastline) + 
  geom_sf(fill="gray90", col = "gray85", size=0.001) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), 
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"), 
        panel.grid.major = element_line(color = "gray80", linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "white")) 
basemap2 

# -------------------------------------------------
# Load relevant seabird data and prepare for plotting
# -------------------------------------------------

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

# -------------------------------------------------
# Construct maps for each species
# -------------------------------------------------

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

# ------
# LESP
# ------

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

# ------
# ATPU
# ------

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

# ------
# Combine into single figure and save
# ------

Fig1 <- Fig1A|Fig1B
Fig1

ggsave(filename="../output/figures/maps/Fig1_maps.png", plot=Fig1, 
       device="png", dpi=300, units="cm", width=30, height=15)


# **********************************************************
# **********************************************************
# Figure 2: Trajectories of each LESP colony
# **********************************************************
# **********************************************************

# - produced by script 2_LESP_analysis.R

# **********************************************************
# **********************************************************
# Figure 3: Trajectories of each ATPU colony
# **********************************************************
# **********************************************************

# - produced by script 3_ATPU_analysis.R

# **********************************************************
# **********************************************************
# Figure 4: Regional trajectories and trends for both species
# **********************************************************
# **********************************************************

# Load LESP workspace
load("../output/model_output/LESP_wksp.RData")

plot_margins <- theme(plot.margin = margin(0.1,0.1,0.5,0.5, "cm"))
Fig4A <- regional_trajectory_plot + annotate("text", label = "A", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins
Fig4C <- trend_5_yr_plot + annotate("text", label = "C", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins
Fig4E <- trend_3Gen_plot + annotate("text", label = "E", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins

# Load ATPU workspace
load("../output/model_output/ATPU_wksp.RData")

Fig4B <- regional_trajectory_plot + annotate("text", label = "B", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins
Fig4D <- trend_5_yr_plot + annotate("text", label = "D", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins
Fig4F <- trend_3Gen_plot + annotate("text", label = "F", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8) + plot_margins

# Combine into single 6-panel figure
Fig4 <- Fig4A + Fig4B + Fig4C + 
  Fig4D + Fig4E + Fig4F +
  plot_layout(nrow=3, heights = c(2, 2,1.5))

ggsave(filename="../output/figures/trajectory_and_trend_plots/Fig4.png", plot=Fig4, 
       device="png", dpi=300, units="cm", width=20, height=20)

# **********************************************************
# **********************************************************
# Figures for Appendices
# **********************************************************
# **********************************************************
