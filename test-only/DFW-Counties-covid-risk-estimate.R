library(usmap)
library(ggplot2)

dfw_fips <- c(48085,48113, 48121, 48139, 48221, 48231, 48251, 48257, 48367, 48397, 48425, 48439, 48497)

testing_data <- tibble(rates=sample(2:20,13),fips=dfw_fips)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))


p <- plot_usmap("counties",color = "red", values="rates", alpha=0.25,include = c(dfw_fips), data = testing_data, labels = TRUE) +
  scale_fill_continuous(name = "Covid risk rate per county", label = scales::comma, low="white", high="red") + 
  theme(legend.position = "right")+
  labs(title = "Western US States",
       subtitle = "These are the states in the Pacific Timezone.")

suppressWarnings(ggplotly(p))

dfw_zipcodes <- county_geo %>% filter(STATE=="TX" & CWA=="FWD")
  
ggplot()+geom_tile(data=dfw_zipcodes,aes(x=LAT,y=LON))



p <- plot_usmap(data=dfw_zipcodes, "counties",color = "red", fill="blue",alpha=0.25,include = c(dfw_fips)) +
  #  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")+
  labs(title = "Western US States",
       subtitle = "These are the states in the Pacific Timezone.")


library(choroplethr)
library(choroplethrZip)


library(ggplot2)

choro = ZipChoropleth$new(df_pop_zip)
choro$title = "2012 ZCTA Population Estimates"
choro$ggplot_scale = scale_fill_brewer(name="Population", palette=2, drop=FALSE)
choro$set_zoom_zip(state_zoom="new york", county_zoom=NULL, msa_zoom=NULL, zip_zoom=NULL)
choro$render()



library(sf)

us_zipcode <- st_read("Articles/Yolande/cb_2018_us_zcta510_500k.kml")

plot(us_zipcode[1])

View(us_zipcode)

tarrant_zip <- c(76244,76063,76137,76010,76179,76051,76133,76116,76119,76017)

tibble(us_zipcode) %>% filter(grepl('^76', Name)) %>% head()

ggplot(us_zipcode$geometry)+geom_sf()+coord_sf()



##### Using tigris and ggplot

z <- zctas(cb = TRUE, starts_with = c("760", "761", "762"))

z <- z %>% dplyr::filter(ZCTA5CE10 %in% tarrant_zip)

zf <- fortify(z, region = "GEOID10", class="sf")

ggplot(zf) +
  geom_polygon(aes(x = long, y = lat, group = id),
               fill = NA, color = "black") +
  coord_map()


ggplot(zf) +
geom_sf(color = "black", data = zf) +
  geom_sf(fill = NA) +
  geom_sf_label(aes(label = GEOID10))




####

library(maptools)
library(gpclib)
library(sp)


shapefile <- readShapeSpatial('acs2018_5yr_B01003_86000US02648.shp', proj4string = CRS("+proj=longlat + datum=WGS84"))

data_map <- fortify(shapefile)


qmap('texas', zoom = 6, maptype = 'satellite') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data_map,
               colour = 'white', fill = 'black', alpha = .4, size = .3)



#### Leaflet

library(rgdal)
port <- readOGR(dsn = "data", layer = "Police_Districts_Portland")
crime <- readOGR(dsn = "data", layer = "NIJ_Nov2016_Crime")


#### Leaflet 
# https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom) # if you plot with ggplot and need to turn sp data into dataframes
options(stringsAsFactors = FALSE)

# download the data 
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
              destfile = 'coastlines.zip')

# unzip the file
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')

# load the data 
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")

# view spatial attributes
class(coastlines)


extent(coastlines)

crs(coastlines)

plot(coastlines, 
     main = "Global Coastlines")


# simplify with a lower tolerance value (keeping more detail)
coastlines_sim2 <- gSimplify(coastlines, 
                             tol = .1, 
                             topologyPreserve = TRUE)
plot(coastlines_sim2, 
     main = "Map of coastlines - simplified geometry\n tol=.1")

#######################################################
### With GGPLOT

# turn the data into a spatial data frame 
coastlines_sim2_df <- SpatialLinesDataFrame(coastlines_sim2,
                                            coastlines@data) 
#tidy(coastlines_sim2_df)

# plot the data 
ggplot() +
  geom_path(data = coastlines_sim2_df, aes(x = long, y = lat, group = group)) +
  labs(title = "Global Coastlines - using ggplot")


### Second plot

ggplot() +
  geom_path(data = coastlines_sim2_df, aes(x = long, y = lat, group = group)) + 
  coord_fixed() + 
  labs(title = "My awesome ggplot map of coastlines",
       subtitle = "my awesome subtitle",
       x = "", y = "")

#######################################################
# create leaflet 
mapview(coastlines_sim2)


# create a leaflet object
leaflet(coastlines_sim2) %>%
  addTiles() %>% # add basemap to your map
  # then add a lines layer
  addPolylines(color = "#444444", weight = 1, smoothFactor = 0.5,
               opacity = 1.0)


#######################################################
### Vector data with SF

library(sf)
# import the data - sf is much faster
coastlines_sf <- st_read("ne-coastlines-10m/ne_10m_coastline.shp")

# plotting is still a bit slow
plot(coastlines_sf[2])


ggplot() +
  geom_sf(data=coastlines_sf, aes(fill = featurecla))


plot(coastlines_sim2, 
     main = "Coastlines with the NE state boundaries")



#######################################################
### Zipcode Tabulation Area

test_data <- readOGR("Articles/Yolande/Texas/cb_2018_us_zcta510_500k/cb_2018_us_zcta510_500k.shp", layer = "cb2018")

plot(test_data, add=TRUE, col="purple")


### Filtering specific Area = Tarrant County

tarrant_zip <- read.csv("Articles/Yolande/Texas/cb_2018_us_zcta510_500k/tarrant.csv", col.names = "zipcode")

tarrant_county <- subset(test_data, GEOID10 %in% tarrant_zip$zipcode)

tarrant_county.wgs <- spTransform(tarrant_county, CRS("+proj=longlat +ellps=WGS84"))

# writeOGR(tarrant_county.wgs, dsn="tarrant_county.wgs.geojson", layer="cd_2018", driver="GeoJSON", check_exists = FALSE)

plot(tarrant_county.wgs)


#######################################################
### Leaflet again

library(mapview)

p <- mapview(test_data)
q <- mapview(coastlines_sf)

p+q

### more

# create a leaflet object
leaflet() %>%
  addTiles() %>% # add basemap to your map
  # then add a lines layer
  addPolylines(data = coastlines_sf, color = "#444444", weight = 1, smoothFactor = 0.5,
               opacity = 1.0) %>% 
  addPolygons(data = test_data, color = "#444444", fillColor = "green", weight = 1, smoothFactor = 0.5,
              opacity = 1.0)


#######################################################
###### Same Tarrant county map with ggplot ############
#######################################################
library(viridis)
library(plotly)

tr_cnt <- st_read(dsn="Articles/Yolande/Texas/cb_2018_us_zcta510_500k/cb_2018_us_zcta510_500k.shp", quiet = TRUE)

tr_cnt_county <- subset(tr_cnt, GEOID10 %in% tarrant_zip$zipcode)

## Simulating the risk per zipcode
tr_cnt_county$risk <- rnorm(63, mean = 10, sd=2)

## Plotting the risk

pp <- ggplot(data = tr_cnt_county, aes(fill=risk)) + geom_sf() +
  scale_fill_viridis(option = "magma") + theme_bw()+labs(fill="Covid Risk", title = "Tarrant County Covid risk by Zip code")

ggplotly(pp)

mapview(tr_cnt_county, fill='risk')


#######################################################
### With Leaflet

### Preparing colors for filling

qpal <- colorQuantile("Reds", tr_cnt_county$risk, n = 4)

# create a leaflet object
qq <- leaflet() %>%
  addTiles() %>% # add basemap to your map
  # then add a lines layer
  addPolylines(data = tr_cnt_county, color = "#444444", weight = 3, smoothFactor = 0.5,
               opacity = 1.0) %>%
  addPolygons(data = tr_cnt_county, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
              color =~qpal(risk))

qq

# Adding county maps
# tx_sch <- st_read(dsn="Articles/Yolande/Texas/cb_2018_48_unsd_500k/cb_2018_48_unsd_500k.shp", quiet = TRUE)

# qq %>% 
#  addPolygons(data = tx_cnt, color = "black", weight = 3, fillOpacity = 0.1)


#######################################################

# Loading list of schools

library(tabulizer)

pdf_loc <- "FWISD_school_directory.pdf"

tables <- extract_tables(pdf_loc)


### Extracting High Schools table

high_schools <- tables[[1]] %>% head(20) %>% as.data.frame()

high_schools <- high_schools[,1:7]

names(high_schools)<- c("Schools", "Number", "Phone", "Fax", "Address", "Zip", "Principal")

high_schools <- high_schools[-1,]

high_schools$Type <- "High"

### Extracting Middle Schools table

mid_schools <- tables[[1]][22:50,] %>% as.data.frame()

mid_schools <- mid_schools[,1:7]

names(mid_schools)<- c("Schools", "Number", "Phone", "Fax", "Address", "Zip", "Principal")

mid_schools <- mid_schools[-1,]

mid_schools$Type <- "Middle"

### Extracting Alternative Schools table

alt_schools <- tables[[1]][52:61,] %>% as.data.frame()

alt_schools <- alt_schools[,1:7]

names(alt_schools)<- c("Schools", "Number", "Phone", "Fax", "Address", "Zip", "Principal")

alt_schools$Type <- "Alternative"

### Extracting Elementary Schools table

elt_schools <- tables[[2]] %>% as.data.frame()

elt_schools <- elt_schools[,1:7]

names(elt_schools)<- c("Schools", "Number", "Phone", "Fax", "Address", "Zip", "Principal")

elt_schools <- elt_schools[-1,]

elt_schools$Type <- "Elementary"

### Combining full dataset

schools <- rbind(elt_schools, alt_schools, mid_schools, high_schools)

row.names(schools) <-1:nrow(schools)

### Completing the address of all schools by adding the DFW zipcode

fwisd <- tibble(
  address = schools$Address,
  title=schools$Schools,
  street_address= schools$Address,
  type=schools$Type
  )

fwisd$zipcode=paste0("TX 761",schools$Zip)
               
fwisd$address <- paste(fwisd$address,",Fort Worth", fwisd$zipcode)

### Generating lat and lon information for each school

fwisd_lat_lon <- fwisd %>% tidygeocoder::geocode(address = address, method = "osm")
  
# saveRDS(fwisd_lat_lon, "fwisd_schools_lat_lon.rds")

fwisd_lat_lon <- read.csv("fwisd_schools_lat_lon.csv")

##########################################
### plotting schools on the map ##########
##########################################
library(htmltools)

qq %>% 
  addMarkers(data = fwisd_lat_lon, popup = ~htmlEscape(title), group = as.factor(fwisd_lat_lon$type), lat=fwisd_lat_lon$lat,lng =fwisd_lat_lon$long) %>% 
  addPopups(
  data = Tarrant_county, 
  popup = ~paste0("<b>", title, "</b></br>", street_address)
)


