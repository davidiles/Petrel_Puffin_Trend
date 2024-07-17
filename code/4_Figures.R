library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)
library(sp)
library(ggspatial) # for scalebars on ggplot maps
library(ggsflabel) # devtools::install_github("yutannihilation/ggsflabel")
library(patchwork)
library(ggrepel)
library(mapview)
library(viridis)

rm(list=ls())

options(scipen = 999)

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Seabirds/Greg_Sarah_Petrel/Petrel_Puffin_Trend/code")

# bring in all ATPU colonies ----
ATPU_colonies <- read.csv("../input/ATPU_colony_coordinates_for.maps.csv")

ATPU_colonies <- st_as_sf(ATPU_colonies,coords = c("Lon", "Lat"),crs=4326) 

# remove the colonies with zero individuals
ATPU_colonies <- subset(ATPU_colonies, Individuals != 0)

# create a numeric recent_count from Individuals which will change "present" to NA
ATPU_colonies$recent_count <- as.numeric(ATPU_colonies$Individuals)

# now we need to replace add all of the monitored colonies with the most recent count and more accurate position
ATPU_colonies$sp <- "ATPU"
ATPU_colonies <- ATPU_colonies[,c("sp","Colony","recent_count","geometry")]
ATPU_colonies.mon <- ATPU_colonies.mon[,c("sp","Colony","recent_count","geometry")]


# unfortunately colony naming differs so we'll need to do add all of the monitored colonies then delete the duplicates
ATPU_colonies <- rbind(ATPU_colonies,ATPU_colonies.mon)
ATPU_colonies <- ATPU_colonies[-c(11,16,17,19,20,21,25:29,55,56,58,63,66,69,71,72,84,111),] # also remove the generic combo NS MBS in QC and we'll just circle the general area

# now make a factor where we can give NAs a "present" factor
ATPU_colonies <- ATPU_colonies %>% mutate(count_f = case_when(
  is.na(recent_count) ~ "present",
  recent_count %in% 0:1000 ~ "<1,000",
  recent_count %in% 1000:5000 ~ "1,000 to 5,000",
  recent_count %in% 5000:10000 ~ "5,000 to 10,000",
  recent_count %in% 10000:50000 ~ "10,000 to 50,000",
  recent_count %in% 50000:100000 ~ "50,000 to 100,000",
  recent_count %in% 100000:500000 ~ "100,000 to 500,000",
  recent_count %in% 500000:1000000 ~ "500,000 to 1,000,000",
  recent_count > 1000000 ~ "> 1,000,000"))
summary(as.factor(ATPU_colonies$count_f))
levels(as.factor(ATPU_colonies$count_f))
# make it an ordered factor
ATPU_colonies$count_f <- factor(ATPU_colonies$count_f, levels=c("present","<1,000",
                                                              "1,000 to 5,000",
                                                              "5,000 to 10,000",
                                                              "10,000 to 50,000",
                                                              "50,000 to 100,000",
                                                              "100,000 to 500,000",
                                                              "500,000 to 1,000,000",
                                                              "> 1,000,000"))
summary(ATPU_colonies$count_f)

# bring in all LESP colonies ----
LESP_colonies <- read.csv("../input/lesp_colony_coordinates_for.maps.csv")

LESP_colonies <- st_as_sf(LESP_colonies,coords = c("Longitude", "Latitude"),crs=4326) 

# remove the colonies with zero individuals
LESP_colonies <- subset(LESP_colonies, Individuals != 0)

# remove the commas from the counts
LESP_colonies$Individuals <- gsub(",", "", LESP_colonies$Individuals)

# create a numeric recent_count from Individuals which will change "present" to NA
LESP_colonies$recent_count <- as.numeric(LESP_colonies$Individuals)

hist(LESP_colonies$recent_count, breaks=100)

# now we need to replace add all of the monitored colonies with the most recent count and more accurate position
LESP_colonies$sp <- "LESP"
LESP_colonies <- LESP_colonies[,c("sp","Colony","recent_count","geometry")]
LESP_colonies.mon <- LESP_colonies.mon[,c("sp","Colony","recent_count","geometry")]
# unfortunately colony naming differs so we'll need to do add all of the monitored colonies then delete the duplicates
LESP_colonies <- rbind(LESP_colonies,LESP_colonies.mon)
row.names(LESP_colonies) <- c(1:nrow(LESP_colonies))
LESP_colonies <- LESP_colonies[-c(28,24,23,17,18,19,21,22,65,67,68),] 

# now make a factor where we can give NAs a "present" factor
LESP_colonies <- LESP_colonies %>% mutate(count_f = case_when(
  is.na(recent_count) ~ "present",
  recent_count %in% 0:1000 ~ "<1,000",
  recent_count %in% 1000:5000 ~ "1,000 to 5,000",
  recent_count %in% 5000:10000 ~ "5,000 to 10,000",
  recent_count %in% 10000:50000 ~ "10,000 to 50,000",
  recent_count %in% 50000:100000 ~ "50,000 to 100,000",
  recent_count %in% 100000:500000 ~ "100,000 to 500,000",
  recent_count %in% 500000:1000000 ~ "500,000 to 1,000,000",
  recent_count > 1000000 ~ "> 1,000,000"))

# make it an ordered factor
LESP_colonies$count_f <- factor(LESP_colonies$count_f, levels=c("present","<1,000",
                                                      "1,000 to 5,000",
                                                      "5,000 to 10,000",
                                                      "10,000 to 50,000",
                                                      "50,000 to 100,000",
                                                      "100,000 to 500,000",
                                                      "500,000 to 1,000,000",
                                                      "> 1,000,000"))

bbox <- st_bbox(c(xmin = -68, xmax = -51, ymin = 42.5, ymax = 58), crs = 4326)

# downloaded from https://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
coastline <- st_read('../data/spatial_data/Shoreline_Coastline_GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp') %>% st_crop(bbox)

basemap2 <- ggplot(data=coastline) + geom_sf(fill="gray95", size=0.001) + 
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), 
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"), 
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue")) 
basemap2 

# basemap <- basemap +
#   annotate(geom="text", x = -63.2, y=45.1, label = "Nova Scotia", size=4, colour="grey30") +
#   annotate(geom="text", x = -66.3, y=46, label = "New Brunswick", size=4, colour="grey30") +
#   annotate(geom="text", x = -56, y=48.5, label = "Newfoundland", size=4, colour="grey30") +
#   coord_sf(xlim = c(-68, -51), ylim = c(42.5, 55), expand=FALSE) 
# basemap

# map all colonies both species ----
# put them together in one dataframe

col.all <- rbind(LESP_colonies[,c("Colony","count_f","sp","geometry")], ATPU_colonies[,c("Colony","count_f","sp","geometry")])

col_pal <- viridis::plasma(10)
col_pal <- rev(col_pal)
col_pal <- col_pal[c(2:10)]

p1 <- basemap2 + geom_sf(data=subset(col.all, sp=="LESP"), aes(geometry=geometry, color=count_f, size=count_f), alpha=0.8, shape=20) + 
  scale_color_manual(values=col_pal, drop = FALSE) +
  scale_size_manual(values=c(3,3.5,4,4.5,5,5.5,6,6.5,7), guide="none") +
  geom_sf(data=subset(col, sp=="LESP"), aes(geometry=geometry), shape=1, size=6) + 
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE) +
  labs(color = "Most Recent Count") +
  guides(color = guide_legend(override.aes = list(size = c(3,3.5,4,4.5,5,5.5,6,6.5,7)))) +
  theme(legend.spacing.y = unit(1, "mm"), legend.direction="vertical",
        legend.box="vertical",
        legend.position="none", 
        legend.box.background = element_rect(color = "black",fill = "white"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))
p1

p2 <- basemap2 + geom_sf(data=subset(col.all, sp=="ATPU"), aes(geometry=geometry, color=count_f, size=count_f), alpha=0.8, shape=20) + 
  scale_color_manual(values=col_pal, drop = FALSE) +
  scale_size_manual(values=c(3,3.5,4,4.5,5,5.5,6,6.5,7), guide="none") +
  geom_sf(data=subset(col, sp=="ATPU"), aes(geometry=geometry), shape=1, size=6) + 
  coord_sf(xlim = c(-68, -51), ylim = c(42.5, 57), expand=FALSE) +
  labs(color = "Most Recent Count") +
  guides(color = guide_legend(override.aes = list(size = c(3,3.5,4,4.5,5,5.5,6,6.5,7)))) +
  theme(legend.spacing.y = unit(1, "mm"), legend.direction="vertical",
        legend.box="vertical",
        #legend.position="none", 
        legend.box.background = element_rect(color = "black",fill = "white"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))
p2

p1 <- p1 + annotate("text", label = "A", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8)
p2 <- p2 + annotate("text", label = "B", x=-Inf, y=Inf, vjust=2, hjust=-1, size=8)

p <- p1|p2
p

ggsave(filename="output/figures/maps/both.species.all.colonies.w.trend.colonies.png", plot=p, 
       device="png", dpi=300, units="cm", width=30, height=15)

# maybe we should try adding fun clipart... 
library(png)
library(grid)
atpu <- readPNG(here("input/ATPU_clipart.png"), native=TRUE)
atpu <- rasterGrob(atpu, interpolate=TRUE)
lesp <- readPNG(here("input/LESP_clipart.png"), native=TRUE)
lesp <- rasterGrob(lesp, interpolate=TRUE)

p2.w.pic <- p2 + annotation_custom(atpu, xmin=-67.5, xmax=-61, ymin = 51, ymax=55) 
p1.w.pic <- p1 + annotation_custom(lesp, xmin=-67, xmax=-62, ymin = 51, ymax=56) 

p.w.pic <- p1.w.pic|p2.w.pic
p.w.pic

ggsave(filename="output/figures/maps/both.species.all.colonies.w.trend.colonies.clipart.png", plot=p.w.pic, 
       device="png", dpi=300, units="cm", width=30, height=15)
