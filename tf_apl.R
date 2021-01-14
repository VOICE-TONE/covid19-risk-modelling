### Data Transformation main file

### Apple mobility data



### Running apple Transformation function
apple_mob_new <- trans_apl(df=apple_mob_dfw)

########## Filtering Apple data to county specific and transforming 

apl_county <- function(cnt, df=apple_mob_new, start=start, end=end){
  
  
  #### Filtering apple data for Tarrant county only
  
  apple_mob_tarrant <- df %>% filter(tolower(county) == tolower(cnt))
  
  ### Computing 7 days moving average
  
  
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
  
  
  apple_mob_tarrant1 <- apple_mob_tarrant %>% mutate(safe_mob_indice = (rel_driving+rel_walking)/(rel_transit+rel_walking+rel_driving))
  
  ###### Apple mobility combined ######
  
  apple_mob_tarrant <- apple_mob_tarrant1 %>% 
    dplyr::arrange(date) %>%
    dplyr::filter(dplyr::between(date, as.Date(start), as.Date(end)))
  
  
  return(apple_mob_tarrant) 
  
}


############### Below lines are part of the main app after county is selected #########
### Running to county filtering function for apple
apple_mob_tarrant <- apl_county(cnt=county, df=apple_mob_new, start=start, end=end)

### Some exploration
# Plotting timeseries of walking, driving and transit
#plot(cbind(ts(apple_mob_tarrant$rel_drv_ma),ts(apple_mob_tarrant$rel_wlk_ma),ts(apple_mob_tarrant$rel_trs_ma)))

#plot_ly(data = apple_mob_tarrant, x=~date) %>%
#  add_trace(y=~rel_drv_ma, mode="lines", name="driving") %>%
#  add_trace(y=~rel_trs_ma, mode="lines", name="transit") %>%
#  add_trace(y=~rel_wlk_ma, mode="lines", name="walking")

# 

#########################################################
# Adding a composite indicator on apple mobility types #
#########################################################
