
#############################################################
########## Functions for building the final dataset #########
#############################################################


## This function load Tarrant county positivity rates 

load_county <- function(){
  
  ### Positivity rates Tarrant County
  
  pos_tarrant <- read.csv("data/tarrant_county_data.csv", header = T)
  
  pos_tarrant$Day <- dmy(pos_tarrant$Day)
  
  pos_tarrant <- pos_tarrant %>% 
    arrange(Day) %>%
    dplyr::filter(dplyr::between(Day, as.Date(start)-6, as.Date(end))) %>% 
    dplyr::select(date=Day,pos=Percent_positivity) %>%
    dplyr::arrange(date, county)
  
  ### Calculating 7days moving average
  pos_tarrant$pos_ma <- rollmean(pos_tarrant$pos, 7, align = "right", na.pad = TRUE)
  
  pos_tarrant <- na.omit(pos_tarrant)
  
  pos_tarrant$pos_ma <- pos_tarrant$pos_ma
  
  return(pos_tarrant)
  
}

#########################################################
####### combining facebook CLI and Community CLI #######
#########################################################

fb_combined <- function(df1=fb_cli, df2=fb_cmnty_cli, start=start, end=end){
  
  ### Facebook combined
  fb_cli_df <- df1 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, cli=value) %>%
    dplyr::select(fips, date, cli)
  
  
  fb_cmnt_cli_df <- df2 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, cmnt_cli=value) %>%
    dplyr::select(fips, date, cmnt_cli)
  
  
  fb_df <- full_join(fb_cli_df, fb_cmnt_cli_df, by=c("fips","date"))
  
  return(fb_df)
  
}



### Facebook Connectedness Data Transformation
# Fb social Connectedness

trans_fb_sci <- function(){
  
  ## Filtering values of reference columns or diagonal elements
  fb_sci_self <- fb_sci_dfw %>% filter(user_loc==fr_loc)
  
  ## Setting the reference of each data point
  fb_sci_dfw$ref <- lapply(fb_sci_dfw$user_loc, function(x) fb_sci_self[fb_sci_self$user_loc==x, "scaled_sci"]) %>% unlist()
  
  ## Calcularing the total connectedness (Total friends count per county)
  tmp_df <- fb_sci_dfw %>% dplyr::group_by(user_loc) %>% summarise(Total=sum(scaled_sci), .groups="drop")
  
  
  fb_sci_dfw <- left_join(fb_sci_dfw,tmp_df, by="user_loc")
  
  rm(tmp_df)
  
  ## Relative connectedness (counties relatively to their reference)
  fb_sci_dfw$rel_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$ref
  
  ## Connectedness proportion based on total county connectedness
  
  fb_sci_dfw$rate_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$Total
  
  
  fb_sci_dfw$GEOID <- as.character(fb_sci_dfw$fr_loc)
  
  return(fb_sci_dfw)
  
}

#########################################################
####### combining Safegraph Fulltimne and Partime #######
#########################################################
### Safegraph combined

sg_combined <- function(df1=sg_mob_ft, df2=sg_mob_pt, start=start, end=end){
  
  ## This function combines safegraph fulltime and parttime data
  sg_ft_df <- df1 %>% 
    dplyr::arrange(geo_value, time_value) %>% 
    dplyr::filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    dplyr::mutate(fips=geo_value, date=time_value, fulltime=value) %>%
    dplyr::select(fips, date, fulltime)
  
  
  sg_pt_df <- df2 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, parttime=value) %>%
    dplyr::select(fips, date, parttime)
  
  
  sg_df <- full_join(sg_ft_df, sg_pt_df, by=c("fips","date"))
  
  return(sg_df)
  
}




#########################################################
####### Transforming Apple mobility data          #######
#########################################################


trans_apl <- function(df=apple_mob_dfw){
  
  
  #### Creating variables driving, walking and transit
  
  apple_driving <- df %>% 
    filter(tolower(transportation_type)=="driving") %>% 
    dplyr::select(date, county=region, type=transportation_type, driving =value)
  
  apple_walking <- df %>% 
    filter(tolower(transportation_type)=="walking") %>% 
    dplyr::select(date, county=region, type=transportation_type, walking =value)
  
  apple_transit <- df %>% 
    filter(tolower(transportation_type)=="transit") %>% 
    dplyr::select(date, county=region, type=transportation_type, transit =value)
  
  
  drive_walk <- full_join(apple_driving, apple_walking, by=c("date", "county"))
  
  apple_mob_new <- full_join(drive_walk, apple_transit, by=c("date", "county"))
  
  apple_mob_new <- apple_mob_new %>% dplyr::select(date, county, driving, walking, transit)
  
  # Removing temp dataframe created
  rm(drive_walk)
  
  
  return(apple_mob_new)
  
}






### Function used to combined Facebook, Safegraph, Apple mobility and Tarrant county data into a unified dataset
### named df_final to be used for model building

comb_final <- function(df=pos_tarrant){
  
  df1 <- left_join(pos_tarrant[,-2], fb_df_tarrant[,-1])
  df2 <- left_join(df1,sg_df_tarrant[,-1])
  df_final <- left_join(df2, apple_mob_tarrant[,-c(2,13)])
  
  df_final$comp_indice <- df_final$transit/(df_final$transit+df_final$driving)
  
  df_final <- na.omit(df_final)
  
  
  df_final <- na.omit(df_final[,c("date","pos_ma","cli","cmnt_cli","fulltime","parttime","comp_indice")])
  
  return(df_final)
}




#############################################################
################## Forecasting USING VAR MODEL ##############
#############################################################

### Prediction based on the VAR model 

var_pred <- function(model, df=df_test_ts, ahead=14){
  
  
  pred_var1 <- predict(model, new_data=df ,n.ahead=ahead)
  
  ### Error Handling
  tryCatch(
    
    pred_7d <- tibble(Actual=tail(df_final,ahead)$pos_ma, 
                      Fitted=pred_var1$fcst$pos_ma[1:ahead,"fcst"], 
                      date=tail(df_final$date, ahead), 
                      lower=pred_var1$fcst$pos_ma[1:ahead,"lower"],
                      upper=pred_var1$fcst$pos_ma[1:ahead,"upper"]),
    
    warning=function(w){print("The test dataframe length needs to match the actual!!!");
    },
    
    
    error=function(e){print(paste("The period of forecasting selected is longer than the actual. defaulting to",nrow(df)));
      
      pred_7d <- tibble(Actual=df_final_ts_sta[1:nrow(df),"pos_ma"], 
                        Fitted=pred_var1$fcst$pos_ma[1:nrow(df),"fcst"], 
                        date=df_final_ts_sta[1:nrow(df),"date"], 
                        lower=pred_var1$fcst$pos_ma[1:nrow(df),"lower"],
                        upper=pred_var1$fcst$pos_ma[1:nrow(df),"upper"])
      
    }
    
  ) 
  
 p <- plot_ly(data = pred_7d, x=~date) %>%
    add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
    add_trace(y=~Fitted, name="Fitted", type="scatter",mode="lines+markers") %>%
    add_ribbons(data = pred_7d,
               x=~date,
               ymin = ~lower,
               ymax = ~upper,
               color=I("orange"),
               name = "95% confidence",
               opacity=0.2)
 
   
   # add_trace(y=~lower, name="Lower", type="scatter",mode="lines+markers") %>%
  #  add_trace(y=~upper, name="Upper", type="scatter",mode="lines+markers") %>%
  #  layout()
  
 return(p)
  
}

### Applying the prediction function for original VAR model
# var_pred(model = VAR_pos, df=df_test_ts, ahead=20)






#####################################################################
###### Forecasting ahead without information (Purely autoregressive)
#####################################################################

var_forecast<- function(model, ahead=4, actual_df=pos_tarrant){
  
  ### Forecast 4 days ahead
  forecast_1 <- predict(model, n.ahead=ahead)
  
  ## plot of the forecast
  
  tibble(date=tail(round(actual_df$date,4),ahead), 
         Actual=tail(round(actual_df$pos_ma,4),ahead), 
         Fitted=round(forecast_1$fcst$pos_ma[,"fcst"],4),
         Lower=round(forecast_1$fcst$pos_ma[,"lower"],4),
         Upper=round(forecast_1$fcst$pos_ma[,"upper"],4)) %>% 
    plot_ly(x=~date) %>% 
    add_trace(y=~Actual, name="Actual", mode="lines") %>% 
    add_trace(y=~Fitted, name="Fitted", mode="lines") %>%
    add_trace(y=~Lower, name="Lower", mode="lines") %>%
    add_trace(y=~Upper, name="Upper", mode="lines")
  
  
  
}

### Applying the prediction function for original VAR model
# var_forecast(model = VAR_pos)



