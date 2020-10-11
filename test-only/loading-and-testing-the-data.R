#setwd("C:/Users/mfondoum/Dropbox/COVID/Covid-risk-early-detection")

dfw_fips <- c(48085,48113, 48121, 48139, 48221, 48231, 48251, 48257, 48367, 48397, 48425, 48439, 48497)

dfw_counties <- tibble(county=fips_to_name(dfw_fips))

shift_days <- 90



######################################################
##### Loading saved dataset #########################

covid_data_load <- function(){

  ### Loading facebook survey data
  fb_data_dfw <- readRDS("data/fb_data_dfw.rds")
  
  ### Loading safegraph survey data
  sg_data_dfw <- readRDS("data/sg_data_dfw.rds")
  
  ### Loading google survey data
  google_data_dfw <- readRDS("data/google_data_dfw.rds")
  
  
  ### Selecting only essential data
  
  fb_data <<- fb_data_dfw %>% dplyr::select(date=time_value, county=geo_value,fb_survey=value) %>% mutate(county=fips_to_name(county))
  
  sg_data <<- sg_data_dfw %>% dplyr::select(date=time_value, county=geo_value,sg_ftwork=value) %>% mutate(county=fips_to_name(county))
  
  gl_data <<- google_data_dfw %>% dplyr::select(date, county, grocery.and.pharmacy, parks) %>% mutate(date=as.Date(date))
  
}

covid_data_load()

####################################################
#### Calculating the data availability
####################################################

covid_data_avail <- function(start_date=today()-60){
  
  start_date <- as.Date(start_date)  
  
  data_period <- difftime(strptime(today(), format = "%Y-%m-%d"),strptime(start_date, format = "%Y-%m-%d"), units = "days") %>% as.numeric()
  
  gl_park_avail<-gl_data %>% 
    filter(between(date, start_date, today())&!is.na(parks)) %>% 
    group_by(county)%>%
    dplyr::select(date, county, parks) %>%
    dplyr::summarise(parks_avail=length(parks)/data_period)
  
  gl_gro_avail<-gl_data %>% 
    filter(between(date, start_date, today())&!is.na(grocery.and.pharmacy)) %>% 
    group_by(county)%>%
    dplyr::select(date, county, grocery.and.pharmacy) %>%
    dplyr::summarise(gro_avail=length(grocery.and.pharmacy)/data_period)
  
  
  fb_avail<-fb_data %>% 
    filter(between(date, start_date, today())&!is.na(fb_survey)) %>% 
    group_by(county)%>%
    dplyr::summarise(fb_avail=length(fb_survey)/data_period)
  
  
  sg_avail<-sg_data %>% 
    filter(between(date, start_date, today())&!is.na(sg_ftwork)) %>% 
    group_by(county)%>%
    dplyr::summarise(sg_avail=length(sg_ftwork)/data_period)
  
  
  ### Merging of the availability dataset
  
  data_avail <- suppressWarnings(
    Reduce(function(x,y) full_join(x=x,y=y,by="county"), list(dfw_counties,gl_park_avail, gl_gro_avail, fb_avail, sg_avail))
  )
  
  data_avail[is.na(data_avail)]=0
  
  return(data_avail)
}

## Calculating data availability over the past 60 days

# data_avail <- covid_data_avail()


####################################################
# State wide model based on last 60 days of data   #
####################################################

# shift_days <- 60

covid_data_60 <- function(start_date=today()-shift_days-6){
  
  start_date <- today()-shift_days-6
  
  start_date <- as.Date(start_date)  
  
  data_period <- seq(today()-shift_days, today(), by="days")
  
  data_period <- tibble(date=data_period)
    
  gl_park<-gl_data %>% 
    filter(between(date, start_date, today())) %>% 
    group_by(county)%>%
    dplyr::select(date, county, parks)
  
  gl_gro<-gl_data %>% 
    filter(between(date, start_date, today())) %>% 
    group_by(county)%>%
    dplyr::select(date, county, grocery.and.pharmacy)
  
  fb_surv<-fb_data %>% 
    filter(between(date, start_date, today())&!is.na(fb_survey)) %>% 
    group_by(county)

  
  sg_mob<-sg_data %>% 
    filter(between(date, start_date, today())&!is.na(sg_ftwork)) %>% 
    group_by(county)
  
  ### Merging of the availability dataset
  
  data_full <- suppressWarnings(
    Reduce(function(x,y) full_join(x=x,y=y,by=c("date","county")), list(gl_park, gl_gro, fb_surv, sg_mob))
  )
  
  data_full <- full_join(data_period, data_full, by="date")
  
  data_full <- arrange(data_full, date)
  
  
#  data_avail[is.na(data_avail)]=0
  
  return(data_full)
}

 data_60 <- covid_data_60()


####################################################


####################################################
# Getting Tarrant county positivity rates          #
####################################################

get_tarrant_df <- function(){

  ### Getting positivity rates of Tarrant county as predictor
  tarrant_pos <- read.csv("data/tarrant_county_data.csv")
  #tarrant_pos <<- read.csv("tarrant-data-covidactnow.csv")
  
  tarrant_pos <- tarrant_pos[,c(1,2)]
  
  names(tarrant_pos) <- c("date","daily_pos_rate")
  
  tarrant_pos$date <- mdy(tarrant_pos$date)
  
  tarrant_pos <- tarrant_pos %>% filter(between(date, today()-shift_days-6, today()))
  
  
  ### Merging response and predictors by date
  
  tarrant_predictors <- data_60 %>% filter(county=="Tarrant County")
  
  tarrant_data <- merge(tarrant_predictors, tarrant_pos, by="date")
  
  
  ####################################################
  # Calculating the 7 days moving average of all vars#
  ####################################################
  
  ### Calculating 7Days moving average
  
  tarrant_data_ma <- round(rollmean(tarrant_data[,-c(1,2)],7, fill = NA, align = "right"),2)
  
  tarrant_data_ma <- cbind(tarrant_data[,c(1,2)],tarrant_data_ma)
  names(tarrant_data_ma) <- c("date", "county", "parks_visit", "grocery_phar_visit","covid_like_symp","work_mobility","positivity_rate_7d")
  
 # all(complete.cases(tarrant_data_ma))
  
  return(tarrant_data_ma)
}

## Running the get function for Tarrant County

tarrant_data_ma <- get_tarrant_df()


####################################################
# Building the model based on Tarrant positivity   #
####################################################

fit_model <- function(df=tarrant_data_ma){
  
  lm_t1 <- lm(positivity_rate_7d~work_mobility+parks_visit+grocery_phar_visit+covid_like_symp, data = df)
  
  return(lm_t1)
  
}

lm_t1 <- fit_model()

pred_tarrant_model <- function(model=lm_t1, df=tarrant_data_ma){
  
  act_fit <- tibble(df[as.numeric(names(model$fitted.values)),c('date','positivity_rate_7d')],Fitted=model$fitted.values)
  
  names(act_fit) <- c("date", "Actual", "Fitted")
  
  return(act_fit)
}

tarrant_pred <- pred_tarrant_model()

####################################################
# Plotting the actual versus the fitted values     #
####################################################

plot_tarrant_pred <- function(df=tarrant_pred){
  
  p <- ggplot()+
    geom_point(data = df, aes(x=date, y=Actual), color="steelblue")+
    geom_line(data = df, aes(x=date, y=Actual), color="steelblue")+
    geom_point(data = df, aes(x=date, y=Fitted), color="#E69F00")+
    geom_line(data = df, aes(x=date, y=Fitted), color="#E69F00")+
    theme_bw()
  
  #ggplotly(p)
  
  return(p)
  
}

#plot_tarrant_pred()

####################################################
#  Prediction on another county data for a period  #
####################################################


get_county_test <- function(county_name="Denton County"){

  time_period <- tibble(date=seq(today()-shift_days-6, today()-7, by="days"))
  
  test <- data_60 %>% filter(county==county_name & between(as.Date(date), today()-shift_days-6, today()))
  
  test_ma <- round(rollmean(test[,-c(1,2)],7, fill = NA, align = "right"),2)
  
  test_ma <- cbind(test[,c(1,2)],test_ma)
  
  names(test_ma) <- c("date", "county", "parks_visit", "grocery_phar_visit","covid_like_symp","work_mobility")
  
  return(test_ma)

}


#### Predicts county values

pred_county <- function(model=lm_t1, df=test_ma){
  
  test_pred <- predict(lm_t1, df[,-c(1,2)])
  
  test_pred <- tibble(date=df[,'date'], Fitted = test_pred)
  
  return(test_pred)

}


#### plots county prediction

plot_pred <- function(df=test_pred){

  q <- ggplot(data = df, aes(x=date, y=Fitted))+
    geom_point(color="red")+
    geom_line(color="steelblue")+
    theme_bw()
  
  # ggplotly(q)
  
  return(q)
  
}



##########################################################
###### Do the actual preduction and store it for later use


get_county_last_pred <- function(county1){
  
  test_ma <- get_county_test(county_name = county1)
  
  test_pred <- pred_county(model = lm_t1, df=test_ma)
  
  test_pred <- na.omit(test_pred)
  
  as.numeric(test_pred$Fitted[length(test_pred$Fitted)])
  
}

all_counties_test <- lapply(dfw_counties$county, function(x) get_county_test(county_name = x))

all_counties_pred <- lapply(all_counties_test, function(x) pred_county(df=x))

### converting names
for(d in names(all_counties_pred)){
  
  names(all_counties_pred[[d]])[2]<-d
  
}

### Combiningdata frame

all_counties_pred <- Reduce(merge,all_counties_pred) %>% as_tibble()

