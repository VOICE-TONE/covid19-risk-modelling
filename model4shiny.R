####################################################################
################ Forecasting with test data available ##############
####################################################################

##### Loadong all VECM models used for forecasting
load("VAR_pos_R.rda")

load("VAR_pos_R_seas.rda")

##### Setting the number of days to predict to 14
ndays <- 14

##### Predict for the number of days selected
VAR_pos_R_pred <- lapply(VAR_pos_R, function(x) predict(x, new_data=df_test_ts, ahead=ndays))

##### Prediction with Seasonality
VAR_pos_R_seas_pred <- lapply(VAR_pos_R_seas, function(x) predict(x, new_data=df_test_ts, ahead=ndays))


### Compute the  MAPE of all models and select the lowest MAPE
#####
#####

####################################################################
################ Plotting predictions in Shiny Serve r##############
####################################################################

plt_R <- lapply(VAR_pos_R, function(x) var_pred(model = x, df=df_test_ts, ahead=ndays))

plt_R_seas <- lapply(VAR_pos_R_seas, function(x) var_pred(model = x, df=df_test_ts, ahead=ndays))

plt_R[[1]]



####################################################################
################ Forecasting without available test data ###########
####################################################################

load("VAR_pos_Rs.rda")

load("VAR_pos_Rs_seas.rda")

### PRediction without Seasonality
VAR_pos_Rs_pred <- lapply(VAR_pos_Rs, function(x) predict(x,  ahead=14))

### Prediction with Seasonality
VAR_pos_R_seas_pred <- lapply(VAR_pos_Rs_seas, function(x) predict(x, new_data=df_full_ts, ahead=ndays))


### Forecasting ahead without data based on the model above with the lowest MAPE

VAR_forecast <- predict(VAR_pos_Rs[[2]], n.ahead=14)
####################################################################
############           Plotting the forecast        ################
####################################################################
### Creating the vector of days ahead corresponding to the forecast
days_ahead <- last(df_final$date)+days(0:ndays)[-1]
fcst_ahead <- VAR_forecast$fcst$pos_ma[,"fcst"]

### only the data part of the forecast ndays ahead
df_fcst_only <- tibble(date=days_ahead, 
                       value=VAR_forecast$fcst$pos_ma[,"fcst"], 
                       lower=VAR_forecast$fcst$pos_ma[,"lower"],
                       upper=VAR_forecast$fcst$pos_ma[,"upper"])

### Binding the dates of the actual to the days ahead and the Actual values to the prediction ahead
df_fcst <- tibble(date=c(df_final$date, days_ahead), value=c(df_final$pos_ma,VAR_forecast$fcst$pos_ma[,"fcst"]))

plot_ly() %>%
  add_lines(data = df_fcst, x=~date, y=~value, name="Positivity rate") %>%
  add_ribbons(data = df_fcst_only, 
              x=~date,
              ymin = ~lower,
              ymax = ~upper,
              color=I("orange"),
              name = "95% confidence")
