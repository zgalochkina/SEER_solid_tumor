# Note: If file 'mapcoord.RData' has already created, 
# then no need to rerun this part of the code.

# INPUT FILES -------------------------------------------------------------
# http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_5m.zip
# http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_5m.zip


# OUTPUT FILES ------------------------------------------------------------
# mapcoord.RData


# Template for the map of 50 states: --------------------------------------

# GIS files ---------------------------------------------------------------

url_county <- "http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_5m.zip"
url_state  <- "http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_5m.zip"

file_county <- basename(url_county)
file_state  <- basename(url_state)

if(! file.exists(file.path("./data/public/data_geo", file_county))){
  download.file(url_county, file.path("./data/public/data_geo", file_county), mode = "wb")
  utils::unzip(file.path("./data/public/data_geo", file_county), exdir = "./data/public/data_geo")
}

if(! file.exists(file.path("./data/public/data_geo", file_state))){
  download.file(url_state, file.path("./data/public/data_geo", file_state), mode = "wb")
  utils::unzip(file.path("./data/public/data_geo", file_state), exdir = "./data/public/data_geo")
}

# read GeoJSON files (long, lat) ------------------------------------------

# counties (to plot borders on the map)
UScounty <- readOGR(dsn = "./data/public/data_geo", layer = "cb_2013_us_county_5m") 
# states (to plot borders on the map)
USstate <- readOGR(dsn = "./data/public/data_geo", layer = "cb_2013_us_state_5m") 


# prepare DATA for MAPs ---------------------------------------------------

# projected map: convert map to Albers equal area
# The Albers projection is one of the standard projections. It is also used by the United States Census Bureau.
UScountyProj <- spTransform(UScounty, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
USstateProj <- spTransform(USstate, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))


# add ID column -----------------------------------------------------------

# Need 'id' column later. 
# From mirosa.org: "In order to join the two data frames, we need to have a column with the same elements.
# we are trying to link the country information to the polygons of each country by 'id' column" - note that was a world map.
UScountyProj@data$id <- rownames(UScountyProj@data)
USstateProj@data$id <- rownames(USstateProj@data)


# rearrange maps for Alaska & Hawaii, delete Puerto Rico & others ---------

# COUNTY level  -----------------------------------------------------------

# extract, then rotate, shrink & move Alaska (and reset projection)
alaska1 <- UScountyProj[UScountyProj$STATEFP=="02",]
alaska1 <- elide(alaska1, rotate=-50)
alaska1 <- elide(alaska1, scale=max(apply(bbox(alaska1), 1, diff)) / 2.3)
alaska1 <- elide(alaska1, shift=c(-2100000, -2500000))
proj4string(alaska1) <- proj4string(UScountyProj)

# extract, then rotate & shift Hawaii (and reset projection)
hawaii1 <- UScountyProj[UScountyProj$STATEFP=="15",]
hawaii1 <- elide(hawaii1, rotate=-35)
hawaii1 <- elide(hawaii1, shift=c(5400000, -1400000))
proj4string(hawaii1) <- proj4string(UScountyProj)

# remove states "02" (Alaska), "15" (Hawaii), "72" (Puerto Rico) and etc.;
# put new defined Alaska & Hawaii back in
UScountyProj <- UScountyProj[!UScountyProj$STATEFP %in% c("02", "15", "72", "60", "66", "69", "74", "78"), ]
UScountyProj <- rbind(UScountyProj, alaska1, hawaii1)


# STATE level -------------------------------------------------------------

# extract, then rotate, shrink & move Alaska (and reset projection)
alaska2 <- USstateProj[USstateProj$STATEFP=="02",]
alaska2 <- elide(alaska2, rotate=-50)
alaska2 <- elide(alaska2, scale=max(apply(bbox(alaska2), 1, diff)) / 2.3)
alaska2 <- elide(alaska2, shift=c(-2100000, -2500000))
proj4string(alaska2) <- proj4string(USstateProj)

# extract, then rotate & shift Hawaii (and reset projection)
hawaii2 <- USstateProj[USstateProj$STATEFP=="15",]
hawaii2 <- elide(hawaii2, rotate=-35)
hawaii2 <- elide(hawaii2, shift=c(5400000, -1400000))
proj4string(hawaii2) <- proj4string(USstateProj)

# remove states "02" (Alaska), "15" (Hawaii), "72" (Puerto Rico) and etc.;
# put new defined Alaska & Hawaii back in
USstateProj <- USstateProj[!USstateProj$STATEFP %in% c("02", "15", "72", "60", "66", "69", "74", "78"), ]
USstateProj <- rbind(USstateProj, alaska2, hawaii2)


# convert coordinats to data frame format ---------------------------------

# get ready to 'ggplot' maps: it's important to convert coordinats to data frame format
# R Documentation:  "Rather than using this function, ... use the broom package, which implements a much wider range of methods. fortify may be deprecated in the future."
map_county <- fortify(UScountyProj)
map_state <- fortify(USstateProj)

# now 'Regions defined for each Polygons'


# merge Counties' attibutes and map ---------------------------------------

# 1. merge county and state dataframes by 'STATEFP', then reduce # of columns
USstateProj@data$STATEFP <- as.numeric(as.character(USstateProj@data$STATEFP))
UScountyProj@data$STATEFP <- as.numeric(as.character(UScountyProj@data$STATEFP))
UScountyProj@data$COUNTYFP <- as.numeric(as.character(UScountyProj@data$COUNTYFP))

df_state_county <- left_join(USstateProj@data, UScountyProj@data, by="STATEFP")
head(df_state_county)
df_state_county <- df_state_county[ , c("STATEFP", "STUSPS", "NAME.x", "COUNTYFP", "NAME.y", "id.x", "id.y")]
names(df_state_county) <- c("STATEFP", "STUSPS", "STATEname", "COUNTYFP", "COUNTYname", "id_state", "id_county")
head(df_state_county)
dim(df_state_county) # 3143 x 7

# 2. merge county/state df with real county map (which has coordinates: long & lat) by 'id' variable
names(map_county)[which(names(map_county) == "id")] <- "id_county"
map_county <- left_join(df_state_county, map_county, by="id_county")
head(map_county)
dim(map_county) # 204643 x 13


# generic ggplot theme ----------------------------------------------------

# template theme for ggplot
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))


# define segments for ggplot ----------------------------------------------

# NH, VT, MA, RI, CT, NJ, DE, MD, DC (draw segments)
segment_data = data.frame(
  x = c(2222921, 2122598.2, 2288870.4, 2299139, 2218978.5, 2130364, 2066178, 1935994.4, 1960104.8),
  xend = c(rep(2450000, 9)), 
  y = c(279993.88, 249348.65, 102876.83, 29335.53, -13406.89, -215347.81, -342271.26, -383907.35+20000, -404959.42),
  yend = c(329993.9, 219348.6, 102876.83, -13406.89, -113406.9, -215347.81, -322271.3, -433907.3, -534959.4)
)


# shift some States' abbreviations on the map -----------------------------

fun.statelabels <- function(df){
  # MUST order in a right way!
  df <- df[order(df$STUSPS, df$order),]
  mid_range <- function(x) mean(range(x,na.rm=TRUE))
  centres <- plyr::ddply(df, .(STUSPS), colwise(mid_range, .(long,lat)))
  shift.place <- c("CA", "ID", "LA", "MI", "FL", "VA", "WV", "RI", "DC", "MD", "NJ", "MA", "CT", "VT", "NH", "DE", "AK")
  xy.shift <- centres[(centres$STUSPS %in% shift.place), ] # original X and Y coordinates

  # changing coordinates for some states
  centres[which(centres$STUSPS %in% "CA"), 2] <- -1784425
  centres[which(centres$STUSPS %in% "ID"), 3] <- 72637
  centres[which(centres$STUSPS %in% "LA"), 2] <- 740827
  centres[which(centres$STUSPS %in% "MI"), 2] <- 1194168
  centres[which(centres$STUSPS %in% "FL"), 2] <- 1841536
  centres[which(centres$STUSPS %in% "VA"), 2] <- 1911512
  centres[which(centres$STUSPS %in% "WV"), 2] <- 1657751
  centres[which(centres$STUSPS %in% c("NH", "VT", "MA", "RI", "CT", "NJ", "DE", "MD", "DC")), 2] <- 2500000
  centres[which(centres$STUSPS %in% "NH"), 3] <- 329994
  centres[which(centres$STUSPS %in% "VT"), 3] <- 219349
  centres[which(centres$STUSPS %in% "RI"), 3] <- -13407
  centres[which(centres$STUSPS %in% "CT"), 3] <- -113407
  centres[which(centres$STUSPS %in% "DE"), 3] <- -322271
  centres[which(centres$STUSPS %in% "MD"), 3] <- -433907
  centres[which(centres$STUSPS %in% "DC"), 3] <- -534960
  return(centres)
}

centres = fun.statelabels(map_county)

# Save data frames to .RData file -----------------------------------------
# do not save to .csv - had a problem

# save(map_county, map_state, theme_opts, segment_data, centres, 
#      file = "./data/public/data_geo/mapcoord.RData")

