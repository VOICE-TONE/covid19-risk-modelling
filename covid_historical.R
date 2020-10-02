######################################################
####### Pulling historical data ######################

######## Pulling google data ########################
library(lubridate)

start=today()-60
end=today()

########################################################
# Getting Live data from all sources                   #
########################################################
#### Get Google Data by county #####
gg_data_list <- function(start_date, end_date){
  
  #### Getting Google mobility data ####
  print("Starting Google mobility data collection")
  
  # u <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"
  
  # google_url <- getURL(url = u, ssl.verifypeer=TRUE)
  
  #google_data <- read.csv(text = google_url)
  
  google_data <- read.csv("data/apple_mobility.csv",header =TRUE)
  
  #google_data <- google_data[,c(3,4,8,10,11)]
  
  #names(google_data) <- c("state", "county", "date","grocery.and.pharmacy", "parks")
  
  google_data$date <- mdy(google_data$date)
  
#  google_data <- google_data %>% filter(grepl(tolower(county_name), tolower(county)) & between(date, as.Date(start_date), as.Date(end_date)))
  
  google_data <- google_data %>% filter(between(date, as.Date(start_date), as.Date(end_date)))
  
  ### For Apple
  # google_data <- google_data %>% filter(between(date, as.Date(start), as.Date(end)))
  
  return(google_data)
  
}

google_data <- gg_data_list(start_date = start, end_date = end)

google_data_dfw <- google_data %>% filter(tolower(state)=="texas" & tolower(county) %in% tolower(dfw_counties$county))

saveRDS(google_data_dfw, "data/google_data_dfw.rds")



#p <- google_data_dfw %>% 
#  ggplot()+
#  geom_line(data=google_data_dfw,aes(x=as.Date(date),y=grocery.and.pharmacy))+
#  theme_bw()+
#  facet_wrap(~county)


# ggplotly(p)


#########################################################
### Pulling and Exploring fb data ####


fb_data_dfw <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_cli",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)

saveRDS(fb_data_dfw, "data/fb_data_dfw.rds")

#p <- fb_data_dfw %>% 
#  ggplot(aes(x=time_value, y=value))+
#  geom_line(color="steelblue")+
#  theme_bw()+
#  facet_wrap(~fips_to_name(geo_value))

#ggplotly(p)

# saveRDS(p, "plot_fb_by_county.rds")


#########################################################
### Pulling and exploring sg data ###

sg_data_dfw <- covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)

saveRDS(sg_data_dfw, "data/sg_data_dfw.rds")

#p <- sg_data_dfw %>% 
#  ggplot(aes(x=time_value, y=value))+
#  geom_line(color="steelblue")+
#  theme_bw()+
#  facet_wrap(~fips_to_name(geo_value))

# ggplotly(p)

#saveRDS(p, "plot_fb_by_county.rds")