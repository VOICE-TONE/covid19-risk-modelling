##############################################################
#                   NEW PHASE                                #
############## Vector Error correction Model #################
require(bvartools)
require(urca)
require(vars)
require(dynlm)
require(forecast)

k <- nrow(df_final)-14


##############################################################
####  Making the dataset stationary by first differencing ####

df_final_ts <- ts(df_final[1:k,-1])
df_final_ts_sta <- diff(df_final_ts,1)


##############################################################
############## Creating the test timeseries ##################
df_test <- na.omit(df_final[(k+1):nrow(df_final),])

df_test_ts <- ts(df_test[,-c(1:2)])

### Stationarizing the test dataset
df_test_ts_sta <- diff(df_test_ts,1)


##############################################################
################## VAR MODEL BUILDING ########################

#Lag selection
var_lag <- VARselect(df_final_ts, lag.max = 8, type = "both", season = 7)

var_lag$selection

## Option 1: VAR Model
VAR_pos <- VAR(df_final_ts, lag.max = 8, type = "both", season = 7)


summary(VAR_pos$varresult$pos_ma)

coeftest(VAR_pos$varresult$pos_ma)

VAR_pos$p

#############################################################
################## Forecasting USING VAR MODEL ##############

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
  
  plot_ly(data = pred_7d, x=~date) %>%
    add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
    add_trace(y=~Fitted, name="Fitted", type="scatter",mode="lines+markers") %>%
    add_trace(y=~lower, name="Lower", type="scatter",mode="lines+markers") %>%
    add_trace(y=~upper, name="Upper", type="scatter",mode="lines+markers") %>%
    layout()

  
}

### Applying the prediction function for original VAR model
var_pred(model = VAR_pos, df=df_test_ts, ahead=20)


#####################################################################
###### Forecasting ahead without information (Purely autoregressive)

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
var_forecast(model = VAR_pos)










##############################################################
################## VECM MODEL BUILDING #######################

k_order <- VAR_pos$p

vecm_model1 <- ca.jo(df_final_ts, ecdet = "none", type = "trace", K=k_order, spec = "transitory")

vecm_model2 <- ca.jo(df_final_ts, ecdet = "none", type = "trace", K=k_order, spec = "transitory", season = 7)

summary(vecm_model2)

### Converting back VECM to new VAR model

VAR_pos_R1 <- vec2var(vecm_model1, r=1)
VAR_pos_R2 <- vec2var(vecm_model1, r=2)
VAR_pos_R3 <- vec2var(vecm_model1, r=3)
VAR_pos_R4 <- vec2var(vecm_model1, r=4)
VAR_pos_R5 <- vec2var(vecm_model1, r=5)


###########################################
#                                         #
### Prediction using the VECM_TO_VAR_R1 ###

var_pred(model = VAR_pos_R1, df=df_test_ts, ahead=14)

var_pred(model = VAR_pos_R2,  df=df_test_ts, ahead=14)

var_pred(model = VAR_pos_R3,  df=df_test_ts, ahead=14)

var_pred(model = VAR_pos_R4,  df=df_test_ts, ahead=14)

var_pred(model = VAR_pos_R5,  df=df_test_ts, ahead=14)

### Forecasting auto-regressively

var_forecast(model = VAR_pos_R1, ahead=14, actual_df=df_final)
var_forecast(model = VAR_pos_R2, ahead=14, actual_df=df_final)
var_forecast(model = VAR_pos_R3, ahead=14, actual_df=df_final)

################### The End #####################





### Full forecast

var_pred(model = VAR_pos_R1, df=na.omit(df_final[,c(2:3,5:6)]), ahead = nrow())

forecast_full <- predict(VAR_pos_R1, new_data=df_final[,c(2:3,5:6)], n.ahead=14)

plot(forecast_full)


############# Model accuracy ##############

mape(pred_r1$fcst$pos_ma[,"fcst"], df_test$pos_ma)


#var_accuracy <- function(model){
  
#  forecast_full <- predict(model, new_data=df_test_ts, n.ahead=14)
  
#    res <- residuals(model)
#    fits <- fitted(model)
    
#    for(i in 1:14)
#    {
#      fc <- structure(list(mean=forecast_full$fcst[[i]][,"fcst"], x=df_final_ts[,i],
#                           fitted=c(NA,NA,fits[,i])),class="forecast")
#      suppressWarnings(
#        print(accuracy(fc,df_test_ts[,i]))
#      )
#    }
    
    
  
#}

# var_accuracy(model = VAR_pos_R2)



## Dynamic model
#VAR_pos1 <- dynlm(pos_ma_ts~cli_ts+L(cli_ts,c(1:14))+L(cmnt_cli_ts,c(1:14))+L(pos_ma_ts,c(1:14))+L(safe_mob_ts,c(1:14)))
#summary(VAR_pos1)


## coefficient test
#coeftest(VAR_pos1,vcov. = sandwich)


#VAR_p14 <- VAR(y=df_final_ts[,c(1:5,15)], p=14)

#VAR_p14


## R squared
#summary(VAR_p14$varresult$pos_ma)$adj.r.squared





#################################################
#                 PLOTS                         #
################ Plotting #######################
### For plotting
#df_1 <- df_final[1:k,-1]

#names(df_1) <- c("Positivity_rate", "Covid_Like_Illness", "Community_CLI", "Fulltime_Work", "Partime_work", "Apple_comp_index")

#plot(ts(df_1), main="Figure 1: Time series plot non-stationary")

## plotting
#plot.ts(df_final_ts, main="Figure 1: Time series plot non-stationary")


##############################################################
### Just for plotting
### Df created for plot
#df_2 <- cbind(Positivity_rate=diff(pos_ma_ts,1),
#              Covid_Like_Illness=diff(cli_ts,1),
#              Community_CLI=diff(cmnt_cli_ts,1),
#              Fulltime_Work=diff(fulltime_ts,1),
#              Partime_work=diff(parttime_ts,1),
#              Apple_comp_index=diff(comp_indice,1))

# names(df_2) <- c("Positivity_rate", "Covid_Like_Illness", "Community_CLI", "Fulltime_Work", "Partime_work", "Apple_comp_index")

# plot(df_2, main="Figure 2: Time series plot - Stationary")
##############################################################
