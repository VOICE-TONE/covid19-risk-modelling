### Getting Apple data
library(tidyr)

apple_mob <- read.csv("../../covid19/apple/applemobilitytrends-2020-10-02.csv")

apple_mob_dfw <- apple_mob %>% dplyr::filter(region %in% dfw_counties$county & sub.region=="Texas")

apple_mob_dfw <- gather(apple_mob_dfw, "date","value",6:length(names(apple_mob_dfw)))

apple_mob_dfw <- apple_mob_dfw[23:nrow(apple_mob_dfw),]

apple_mob_dfw$date <- gsub("X","",apple_mob_dfw$date)

apple_mob_dfw$date <- ymd(apple_mob_dfw$date)

apple_mob_dfw$value <- as.numeric(apple_mob_dfw$value)



apple_mob_dfw %>% filter(region == "Denton County") %>% plot_ly(x=~transportation_type, y=~value, type = "box")


apple_mob_dfw %>% filter(region == "Denton County") %>% 
  plot_ly(x=~date, color = ~transportation_type) %>%
  add_trace(y=~value, mode="lines")



apple_mob_dfw %>% 
  group_by(region) %>%
  plot_ly(x=~transportation_type, y=~value, type = "box")


apple_mob_dfw %>%
  plot_ly(x=~date, color = ~transportation_type) %>%
  add_trace(y=~value, mode="lines")



#### Creating variables driving, walking and transit

apple_driving <- apple_mob_dfw %>% 
  filter(tolower(transportation_type)=="driving") %>% 
  dplyr::select(date, county=region, type=transportation_type, driving =value)

apple_walking <- apple_mob_dfw %>% 
  filter(tolower(transportation_type)=="walking") %>% 
  dplyr::select(date, county=region, type=transportation_type, walking =value)

apple_transit <- apple_mob_dfw %>% 
  filter(tolower(transportation_type)=="transit") %>% 
  dplyr::select(date, county=region, type=transportation_type, transit =value)


drive_walk <- full_join(apple_driving, apple_walking, by=c("date", "county"))

apple_mob_new <- full_join(drive_walk, apple_transit, by=c("date", "county"))

apple_mob_new <- apple_mob_new %>% dplyr::select(date, county, driving, walking, transit)


#### Filtering apple data for Tarrant county only

apple_mob_tarrant <- apple_mob_new %>% filter(tolower(county) == "tarrant county")

### Computing 7 days moving average

library(zoo)

# Filling NA values with previous value
apple_mob_tarrant <- apple_mob_tarrant %>% fill(driving)
apple_mob_tarrant <- apple_mob_tarrant %>% fill(walking)
apple_mob_tarrant <- apple_mob_tarrant %>% fill(transit)


### Computing the relative change from the baseline of January 13th, 2020

apple_mob_tarrant$rel_driving <- (apple_mob_tarrant$driving-100)/100
apple_mob_tarrant$rel_walking <- (apple_mob_tarrant$walking-100)/100
apple_mob_tarrant$rel_transit <- (apple_mob_tarrant$transit-100)/100


# Calculating the 7days moving average
apple_mob_tarrant$rel_drv_ma <- rollmean(apple_mob_tarrant$rel_driving, 7, align = "right", na.pad = TRUE)
apple_mob_tarrant$rel_wlk_ma <- rollmean(apple_mob_tarrant$rel_walking, 7, align = "right", na.pad = TRUE)
apple_mob_tarrant$rel_trs_ma <- rollmean(apple_mob_tarrant$rel_transit, 7, align = "right", na.pad = TRUE)

# Plotting timeseries of walking, driving and transit
plot(cbind(ts(apple_mob_tarrant$rel_drv_ma),ts(apple_mob_tarrant$rel_wlk_ma),ts(apple_mob_tarrant$rel_trs_ma)))

plot_ly(data = apple_mob_tarrant, x=~date) %>%
  add_trace(y=~rel_drv_ma, mode="lines", name="driving") %>%
  add_trace(y=~rel_trs_ma, mode="lines", name="transit") %>%
  add_trace(y=~rel_wlk_ma, mode="lines", name="walking")

# 

#########################################################
# Adding a composite indicator on apple mobility types #
#########################################################


lm_comp1 <- lm(data = apple_mob_tarrant, rel_trs_ma ~ rel_wlk_ma+rel_drv_ma)
lm_comp2 <- lm(data = apple_mob_tarrant, rel_wlk_ma ~ rel_trs_ma+rel_drv_ma)
lm_comp3 <- lm(data = apple_mob_tarrant, rel_drv_ma ~ rel_wlk_ma+rel_trs_ma)


apple_mob_tarrant1 <- apple_mob_tarrant %>% mutate(safe_mob_indice = (rel_driving+rel_walking)/(rel_transit+rel_walking+rel_driving))


lm_comp <- lm(data = apple_mob_tarrant, safe_mob_indice~rel_driving+rel_walking+rel_transit)
