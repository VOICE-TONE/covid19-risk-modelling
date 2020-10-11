##############################################
### Dallas-Fort Worth-Arlington, USA, county list
dfw_fips <- c(48085,48113, 48121, 48139, 48221, 48231, 48251, 48257, 48367, 48397, 48425, 48439, 48497)

state_code = noquote(str_pad(1:56, width = 2 ,pad = "0"))

get_census_data <- function(fips_code=dfw_fips, year="2019",dataset="pep/population", variables=c("CBSA","POP", "DENSITY"), state=48){
  
  # apis <- listCensusApis()
  
  # View(apis)
  
  #pop_data_vars <- listCensusMetadata(
  #    vintage = year,
  #    name = dataset, 
  #   type = variables)
  
  pop_data_2019 <- getCensus(
    vintage = year,
    name = dataset,
    vars =variables,
    region = "county:*",
    regionin = paste0("state:",state)
  )
  
  head(pop_data_2019)
  
  pop_data_2019$fips <- noquote(paste0(pop_data_2019$state, pop_data_2019$county))
  pop_data_2019$county <- fips_to_name(pop_data_2019$fips)
  
  data <- pop_data_2019 %>% filter(fips %in% fips_code)
  
  return(data)
}

##############################################

##############################################
### Function collecting for any county ###
covid_get <- function(county_name, start_date, end_date){
  
  ### Facebook daily smoothed survey data on Covid Like Infection symptoms
  
  print("Starting Facebook individual data collection")
  
  fb_data <- suppressMessages(
    covidcast_signal(data_source = "fb-survey", signal = "smoothed_cli",
                     start_day = start_date, end_day = end_date,
                     geo_type = "county",
                     geo_values = name_to_fips(county_name))
  )
  
  fb_data_val <- fb_data %>% select(cli_perc = value)
  
  
  
  #### Facebook community data
  
#  print("Starting Facebook Community data collection")
  
#fbc_data <- suppressMessages(
  #  covidcast_signal(data_source = "fb-survey", signal = "smoothed_hh_cmnty_cli",
  #                   start_day = start_date, end_day = end_date,
  #                   geo_type = "county",
  #                   geo_values = name_to_fips(county_name))
  # )
  
# fbc_data_val <- fbc_data %>% select(clic_perc = value)
  
  
  ### Safegraph data 6Hr+ full time work away from home
#  print("Starting Safegraph data collection")
  
 # sg_data <- suppressMessages(
 #    covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop",
 #                     start_day = start_date, end_day = end_date,
 #                     geo_type = "county",
 #                    geo_values = name_to_fips(county_name))
 #  )
  
 # sg_data_val <- sg_data %>% select(ft_workaway = value)
  
  
  #### Getting Google mobility data ####
 # print("Starting Google mobility data collection")
  
  u <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/1c211682a94b0fc605ad3b733a5edf52280c72c5/google_reports/mobility_report_US.csv"
  
  google_url <- getURL(url = u, ssl.verifypeer=TRUE)
  
  google_data <- read.csv(text = google_url)
  
  google_data$date <- ymd(google_data$date)
  
  google_data <- google_data %>% filter(grepl(tolower(county_name), tolower(county)) & between(date, as.Date(start_date), as.Date(end_date)))
  
  #data <- cbind(google_data, fb_data_val, fbc_data_val, sg_data_val)
  data <- cbind(google_data, fb_data_val)
  
  
  return(data)
  
}


##########################################################

########### Scatter plot of merge datasey#################

covid_viz <- function(data_new=tarrant_data){
  
  p1 <- ggscatter(data = data_new, x = "Google_park_7d_Avg", y = "Percent_positivity_7d_Avg", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  label = rownames(data_new),
                  #color = "label",
                  palette = c("#00AFBB", "#FC4E07"),
                  xlab = "Google Park Mobility (%)", ylab = "Percent positivity Rate (%)")
  
  p2 <- ggscatter(data = data_new, x = "Google_grocery_7d_Avg", y = "Percent_positivity_7d_Avg",
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  label = rownames(data_new),
                  #color = "label",
                  palette = c("#00AFBB", "#FC4E07"),
                  xlab = "Google grocery Mobility (%)", ylab = "Percent positivity Rate (%)")
  
  p3 <- ggscatter(data = data_new, x = "fb_cli_perc_7d_Avg", y = "Percent_positivity_7d_Avg", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  label = rownames(data_new),
                  #color = "label",
                  palette = c("#00AFBB", "#FC4E07"),
                  xlab = "Facebook Covid Like Illness (%)", ylab = "Percent positivity Rate (%)")
  
  p4 <- ggscatter(data = data_new, x = "fb_clic_perc_7d_Avg", y = "Percent_positivity_7d_Avg", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  label = rownames(data_new),
                  #color = "label",
                  palette = c("#00AFBB", "#FC4E07"),
                  xlab = "Facebook Community Covid Like Illness (%)", ylab = "Percent positivity Rate (%)")
  
  p5 <- ggscatter(data = data_new, x = "ft_workaway_7d_Avg", y = "Percent_positivity_7d_Avg", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  label = rownames(data_new),
                  #color = "label",
                  palette = c("#00AFBB", "#FC4E07"),
                  xlab = "Safegraph FT work away from home (%)", ylab = "Percent positivity Rate (%)")
  
  return(suppressMessages(list(p1,p2,p3,p4,p5)))
}

##############################################

### Calculating 7 Days moving average for the full dataset

covid_7d_ma <- function(dataset=tarrant_data, has_response=TRUE){
  
  if(isTRUE(has_response)){
    
    ### Calculating 7 days moving average with response variable and filtering useful variables ###
    dataset <- dataset %>%
      mutate(Google_park_7d_Avg = round(rollmean(parks,7, fill = NA, align = "right"),1),
             Google_grocery_7d_Avg = round(rollmean(grocery.and.pharmacy, 7, fill = NA, align = "right"),1),
             fb_cli_perc_7d_Avg = round(rollmean(cli_perc*100, 7, fill = NA, align = "right"),1),
             #fb_clic_perc_7d_Avg = round(rollmean(clic_perc, 7, fill = NA, align = "right"),1),
             #ft_workaway_7d_Avg = round(rollmean(ft_workaway, 7, fill = NA, align = "right"),3),
             Percent_positivity_7d_Avg = round(rollmean(Percent_positivity, 7, fill = NA, align = "right"),2))
     
      # dplyr::select(date, state, county, 
      #              Google_park_7d_Avg, 
       #             Google_grocery_7d_Avg, 
        #            fb_cli_perc_7d_Avg, 
              #      fb_clic_perc_7d_Avg, 
              #      ft_workaway_7d_Avg, 
         #           Percent_positivity_7d_Avg) %>%
      #na.omit() 
    
  }else{
    
    ### Calculating 7 days moving average without response variable and filtering useful variables ###
    dataset <- dataset %>%
      mutate(Google_park_7d_Avg = round(rollmean(parks,7, fill = NA, align = "right"),1),
             Google_grocery_7d_Avg = round(rollmean(grocery.and.pharmacy, 7, fill = NA, align = "right"),1),
             fb_cli_perc_7d_Avg = round(rollmean(cli_perc*100, 7, fill = NA, align = "right"),1)
             #fb_clic_perc_7d_Avg = round(rollmean(clic_perc, 7, fill = NA, align = "right"),1),
             #ft_workaway_7d_Avg = round(rollmean(ft_workaway, 7, fill = NA, align = "right"),3)
             ) %>%
      #dplyr::select(date, state, county, 
      #              Google_park_7d_Avg, 
      #              Google_grocery_7d_Avg, 
      #              fb_cli_perc_7d_Avg 
              #      fb_clic_perc_7d_Avg, 
              #     ft_workaway_7d_Avg
      #          ) %>%
      # na.omit() 
  }
  
  return(dataset)
  
}











#########################################################
#PREDICT FROM COUNTIES WITHOUT TEST Positivity Rate #
#########################################################

covid_predic <- function(county = "Denton", response=FALSE, startdate="2020-06-25", enddate="2020-08-31"){
  
  data <- covid_get(county_name = county, start_date = startdate, end_date = enddate)
  
  data <- covid_7d_ma(dataset = data, has_response = FALSE)
  
  data_pred_res <- predict(tarrant_lm, data)
  
  data_pred_res <- tibble(date=data$date, Fitted=data_pred_res)
  
  data_final <<- tibble(date=data$date, Fitted=predict(tarrant_lm, data)) 
  
  # ggplot(data=data_final, aes(x=date, y=Fitted))+
  #  geom_point(color="blue")+
  #  theme_bw()+
  #  labs(title = "COVID positivity rates prediction", subtitle = county)
  
  # ggplotly(q) 
  
  return(data_final)
  
}











########################################################
#                                                      #
# FUNCTIONS TO EXTRACT FRESH DATA ONLINE FOR PREDICTION#
#                                                      #
#### Functions to get Extract data                  ####  

####fb data extraction
get_fb_data <- function(fips, startday = "2020-01-01", endday = "2020-08-31"){
  suppressMessages(
    df <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_cli",
                           start_day = startday, end_day = endday,
                           geo_type = "county",
                           geo_values = fips)
  )
  
  
  print(paste(fips_to_name(fips), "Completed"))
  
  return(df)
}

####################################################################


### Safegraph data 6Hr+ full time work away from home
print("Starting Safegraph data collection")
get_sg_data <- function(fips, startday = "2020-01-01", endday = "2020-08-31"){
  
  suppressMessages(
    
    df <- covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop",
                           start_day = startday, end_day = endday,
                           geo_type = "county",
                           geo_values = fips)
  )
  
  #  print(fips_to_name(fips), "data retrieved.")
  
  return(df)
  
}



####################################################################


#### Get Google Data by county #####
gg_data_list <- function(county, start_date, end_date){
  
  #### Getting Google mobility data ####
  print("Starting Google mobility data collection")
  
  u <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"
  
  google_url <- getURL(url = u, ssl.verifypeer=TRUE)
  
  google_data <- read.csv(text = google_url)
  
  google_data$date <- ymd(google_data$date)
  
  google_data <- google_data %>% filter(grepl(tolower(county_name), tolower(county)) & between(date, as.Date(start_date), as.Date(end_date)))
  
  return(google_data)
  
}
