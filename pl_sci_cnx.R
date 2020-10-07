require(leaflet)
require(leafpop)
require(tabulizer)
require(Sf)

plot_ly(data = fb_sci_dfw, x=~factor(user_loc), y=~factor(fr_loc), z=~rel_cnx, type = "heatmap", color ="viridis")



#### Counties level map

### Get shapes data US wide
cnt_shp <- st_read(dsn="../../covid19/maps/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")


### Filter to Dallas Fort Worth counties

dfw_cnt_shp <- cnt_shp %>% filter(as.numeric(GEOID) %in% dfw_fips)


### Creating color quantiles based on connectedness

qpal <- colorQuantile("Reds", fb_sci_dfw$rel_cnx, n = 4)

### plotting counties of DFW using leaflet and adding

# create a leaflet object
qq <- leaflet() %>%
  addTiles() %>% # add basemap to your map
  # then add a lines layer
  addPolylines(data = dfw_cnt_shp, color = "black", 
               weight = 3, smoothFactor = 0.5,
               opacity = 1.0, fillOpacity = 1.0
               ) %>%
  addPolygons(data = dfw_cnt_shp, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8,
              color =~qpal(fb_sci_dfw[fb_sci_dfw$user_loc=="48439","rel_cnx"]))

qq

##########################################
# County map with ggplot

pl_cnt_cnx <- function(county_fips){

  # Merging to new data column
  df1 <- fb_sci_dfw[fb_sci_dfw$user_loc==county_fips,] %>% arrange(desc(GEOID))
  
  ## Calculating a connection ratio
  # df1$cnx_ratio <- fb_sci_dfw[fb_sci_dfw$user_loc==county_fips,"rel_cnx"]/sum(fb_sci_dfw[fb_sci_dfw$user_loc==county_fips,"rel_cnx"])
  
  # Transforming back the object to sf
  df2 <- dfw_cnt_shp %>% arrange(desc(GEOID))
  
  # Adding value column to an SF object for plotting
  df2$rate_cnx <- round(df1$rate_cnx,2)
  df2$label <- paste(df2$NAME, df2$rate_cnx, sep = "\n")
  
#  df <- tibble(merge(df1,df2))
  
  q1<- ggplot() +
    theme_bw()+
    geom_sf(data=df2, aes(fill = df1[,"rate_cnx"]), color="white")+
    scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
    geom_sf_text(data = df2, aes(label = label), colour = "white")+
    labs(title = paste(fips_to_name(county_fips), "Connectedness To Neighboring Counties"))
  
  
  ggplotly(q1)  
  
}



q1<- ggplot() +
  theme_bw()+
  geom_sf(data=dfw_cnt_shp, aes(fill = fb_sci_dfw[fb_sci_dfw$user_loc==county_fips,"rel_cnx"]), color="white")+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  geom_sf_text(data = dfw_cnt_shp, aes(label = NAME), colour = "white")


ggplotly(q1)  



##########################################
### Getting polygon shape information from different zip codes per county 

tx_cnt <- st_read(dsn="Articles/Yolande/Texas/cb_2018_us_zcta510_500k/cb_2018_us_zcta510_500k.shp", quiet = TRUE)

dfw_cnt <- subset(tx_cnt, GEOID10 %in% tarrant_zip$zipcode)


#######################################################
### With Leaflet

### Preparing colors for filling

qpal <- colorQuantile("Reds", tr_cnt_county$risk, n = 4)

# create a leaflet object
qq <- leaflet() %>%
  addTiles() %>% # add basemap to your map
  # then add a lines layer
  addPolylines(data = fb_sci_dfw, color = "#444444", weight = 3, smoothFactor = 0.5,
               opacity = 1.0) %>%
  addPolygons(data = fb_sci_dfw, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
              color =~qpal(risk))

qq



### Reading counties schools location
fwisd_lat_lon <- read.csv("data/fwisd_schools_lat_lon.csv")

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

