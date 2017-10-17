# INPUT FILES -------------------------------------------------------------
# mapcoord.RData


# OUTPUT FILES ------------------------------------------------------------
# SEER_covered_areas.png



# LOAD MAP COORDINATES ----------------------------------------------------

library(tidyverse)
load(file = "./data/public/data_geo/mapcoord.RData")

map_county <- map_county[, which(names(map_county) %in% c("STATEFP", "COUNTYFP", "long", "lat", "group"))]
map_state <- map_state[, which(names(map_state) %in% c("long", "lat", "group"))]

# LOAD POPDATA ------------------------------------------------------------

# SEER areas Population information 
# use non-modified original file to use in map
pop_map  <-  read_fwf(file = file.path("./data/public/us.1990_2015.19ages.adjusted.txt"), 
                      fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                                 col_names=c("Year", "State", "StateFIPS", "CountyFIPS", "Registry", "Race", 
                                             "Origin", "Sex", "Age", "Population")))

# choose 1 year: could be 1990-2015
year1 <- 2014 # enough for map purposes & this is the last year available now in SEER

# reduce file by selecting year of interest
pop_map <- pop_map[pop_map$Year %in% year1, ]
# http://seer.cancer.gov/popdata/popdic.html
# 99 = Registry for non-SEER area
# 29 (AK) = Alaska Natives
# 33 (AZ) = Arizona Indians (not available in SEER as a registry)
pop_map <- pop_map[!(pop_map$Registry %in% c(33, 99)) & !(pop_map$Race %in% c(1, 2, 4)), ] 
pop_map <- pop_map[ , which(names(pop_map) %in% c("State", "StateFIPS", "CountyFIPS"))]
names(pop_map) <-  c("State", "STATEFP", "COUNTYFP")
pop_map$STATEFP <- as.numeric(pop_map$STATEFP)
pop_map$COUNTYFP <- as.numeric(pop_map$COUNTYFP)


# MERGE DATA WITH MAP -----------------------------------------------------

pop_map <- left_join(map_county, pop_map, by=c("STATEFP", "COUNTYFP"))
pop_map$SEERregion <- NA
pop_map$SEERregion[!is.na(pop_map$State)] <- "Yes"
pop_map$SEERregion[is.na(pop_map$SEERregion)] <- "No"


# PLOT SEER COVERED REGIONS -----------------------------------------------
# areas with publicly available data

plot.seer_regions <- function(){
  ggplot() +
    geom_polygon(data = pop_map, aes(long, lat, group = group, fill = SEERregion)) +
    geom_path(data = map_county, aes(long, lat, group = group), color = "dark grey", size = 0.3) + 
    geom_path(data = map_state, aes(long, lat, group = group), color = "black", size = 0.4) + 
    # http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=6
    scale_fill_manual(name = "\n",
                      values = c("#FFFFFF", "#bdbdbd"),
                      breaks = c("No", "Yes"),
                      labels = c("Not SEER or\n not publicly available", "SEER")) +
    geom_text(data = centres, aes(x = long, y = lat, label = STUSPS), size = 3, color = "black") + 
    geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend), color ='black', size = 0.4) +
    theme_opts + 
    coord_equal(xlim = c(-1900000, 2350000), ylim = c(-2500000, 800000)) +
    theme(legend.key = element_rect(colour = "#bdbdbd"))
}

png(file = file.path("./figures/SEER_covered_areas.png"), 
    width = 8, height = 5, units = "in", res = 300, bg = "transparent")
print(plot.seer_regions())
dev.off()

