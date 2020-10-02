########################################################
#             NEW PHASE                                #
########### Vector Error correction Model ##############
library(bvartools)
library(urca)

begin_date= as.Date("2020-08-05")
ending_date = as.Date("2020-09-08")

## Mobility datae from Feb 15 to Aug 31
data1 <- read.csv("data/mobility_merged.csv")

data1 <- data1 %>% dplyr::select(date, Google_park_7d_Avg, 
                                 Google_grocery_7d_Avg, 
                                 Apple_driving_7d_Avg,
                                 Apple_transit_7d_Avg,
                                 Apple_walking_7d_Avg) %>% na.omit()

names(data1) <- c("date","park","grocery","driving","transit","walking")

## Facebook data

fb_cli<-readRDS("../fb_data_dfw.rds") %>% 
  filter(geo_value == name_to_fips("Tarrant County")) %>% 
  dplyr::select(date=time_value, cli_value=value)


pos_rate <- tarrant_data_ma %>% dplyr::select(date,positivity_rate_7d) %>% na.omit()


## Composing timeseries 
pos_ts1 <- pos_rate %>% filter(between(date, begin_date, ending_date)) %>% dplyr::select(positivity_rate_7d) %>% ts()

fb_ts1 <- fb_cli %>% filter(between(date, begin_date, ending_date)) %>% dplyr::select(cli_value) %>% ts()

mob_ts1 <- data1 %>% filter(between(mdy(date), begin_date, ending_date)) %>% dplyr::select(-date) %>% ts()

df_ts1 <- cbind(pos_ts1, fb_ts1, mob_ts1)

dates <- data1 %>% filter(between(mdy(date), begin_date, ending_date)) %>% dplyr::select(date)


### building the model
## Lag Order selection
var1_lag <- VARselect(df_ts1[1:28,c("pos_ts1","fb_ts1","mob_ts1.transit")], lag.max = 14, type = "both")


## building the VAr modle to get the order for the VECM model

var1.aic <- VAR(df_ts1[1:28,c("pos_ts1","fb_ts1","mob_ts1.transit")], type = "none", lag.max = 15, ic="AIC")

summary(var1.aic)


### Prediction based on the VAR model 
pred_var1 <- predict(var1.aic, new_data=df_ts1[29:35,c("fb_ts1","mob_ts1.transit")], n.ahead=7)


pred_7d <- tibble(Actual=df_ts1[29:35,c("pos_ts1")], 
                  Fitted=pred_var1$fcst$pos_ts1[,"fcst"], 
                  date=dates$date[29:35], 
                  lower=pred_var1$fcst$pos_ts1[,"lower"],
                  upper=pred_var1$fcst$pos_ts1[,"upper"])


plot_ly(data = pred_7d, x=~date) %>%
  add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
  add_trace(y=~Fitted, name="Fitted", type="scatter",mode="lines+markers") %>%
  add_trace(y=~lower, name="Lower", type="scatter",mode="lines+markers") %>%
  add_trace(y=~upper, name="Upper", type="scatter",mode="lines+markers")


### forecasting 14 Days ahead blind
pred_var14 <- predict(var1.aic, n.ahead=10)

pred_14d <- tibble(Actual=tail(tarrant_data_ma$positivity_rate_7d,23)[1:10], 
                  Fitted=pred_var14$fcst$pos_ts1[,"fcst"], 
                  date=tail(tarrant_data_ma$date,23)[1:10], 
                  lower=pred_var14$fcst$pos_ts1[,"lower"],
                  upper=pred_var14$fcst$pos_ts1[,"upper"])


plot_ly(data = pred_14d, x=~date) %>%
  add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
  add_trace(y=~Fitted, name="Fitted", type="scatter",mode="lines+markers") %>%
  add_trace(y=~lower, name="Lower", type="scatter",mode="lines+markers")
  #add_trace(y=~upper, name="Upper", type="scatter",mode="lines+markers")




#### Correcting the model using the VECM

## Extracting the order to be used in the VECM modem from the fitted VAR model
k1_order = var1.aic$p

## building the VECM
vec1_model <- ca.jo(df_ts1[1:28,], ecdet = "none", type = "trace", K=k1_order, spec = "transitory")

summary(vec1_model)


### Converting back VECM to new VAR model

var1_model <- vec2var(vec1_model, r=1)


### Prediction based on New data

forecast_var1 <- predict(var1_model,new_data=df_ts1[29:35,],  n.ahead=7)

pred_7d_vecm <- tibble(Actual=df_ts1[29:35,c("pos_ts1")], Fitted=forecast_var1$fcst$pos_ts1[,"fcst"], date=dates$date[29:35])

p7d <- plot_ly(data = pred_7d_vecm, x=~date) %>%
  add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
  add_trace(y=~Fitted, name="VECM_Fitted", type="scatter",mode="lines+markers", text=~Fitted) %>%
  add_trace(data=pred_7d, y=~Fitted, name="VAR_Fitted", type="scatter",mode="lines+markers", text=~Fitted)



p7d

####### Policy Effect Simulation##############
## Based on the converted VAR model ##

## impulse response function with VAR model before Error Correction

ir_test <- irf(var1.aic, n.ahead = 7,
                impulse = c("fb_ts1", 
                            "mob_ts1.park", 
                            "mob_ts1.grocery",
                            "mob_ts1.driving",
                            "mob_ts1.transit",
                            "mob_ts1.walking"), 
                response = "pos_ts1", 
                ortho = FALSE, 
                runs = 1000)

plot(ir_test)



## impulse response function with VAR model after Error Correction

ir1_test <- irf(var1_model, n.ahead = 7,
                impulse = c("fb_ts1", 
                            "mob_ts1.park", 
                            "mob_ts1.grocery",
                            "mob_ts1.driving",
                            "mob_ts1.transit",
                            "mob_ts1.walking"), 
                response = "pos_ts1", 
                ortho = FALSE, 
                runs = 1000)

plot(ir1_test)


#### plotting interactive impulse response

irf_data <- tibble(days=c(1:8),
                   fb_cli_mean=ir1_test$irf$fb_ts1, 
                   fb_cli_low=ir1_test$Lower$fb_ts1, 
                   fb_cli_up=ir1_test$Upper$fb_ts1)


plot_ly(data = irf_data, x=~days) %>% 
  add_trace(y=~fb_cli_mean, name="Mean effect", mode="lines+markers") %>%
  add_trace(y=~fb_cli_low, name="Lower", mode="lines+markers") %>%
  add_trace(y=~fb_cli_up, name="Upper", mode="lines+markers") %>%
  layout(title="Facebook CLI 1 point predicted effect on Positivity Rate over 7 days (CI 95%)", color="blue")


## Prediction

new_data <- read.csv("../google_data.csv")

test_data <- new_data %>% filter(county=="Tarrant County", between(ymd(date), ymd("2020-09-01"), ymd("2020-09-08")))

var_pred <- predict(var1.aic, new_data=test_data, n.ahead=7,ci=0.95,dumvar=NULL)

pred_new_var <- tibble(date=mdy(df_1$date),
                       Actual=c(var_pred$endog[,"pos_ts1"], df_ts1[,"pos_ts1"][29:35]), 
                       Fitted=c(var_pred$endog[,"pos_ts1"],var_pred$fcst$pos_ts1[,"fcst"]), 
                       Lower=c(var_pred$endog[,"pos_ts1"],var_pred$fcst$pos_ts1[,"lower"]),
                       Upper=c(var_pred$endog[,"pos_ts1"],var_pred$fcst$pos_ts1[,"upper"]))

plot_ly(data = pred_new_var, x=~date) %>%
  add_trace(y=~Fitted, name="Prediction", type="scatter",mode="lines") %>%
  #add_trace(y=~Lower, name="Lower", type="scatter",mode="lines+markers") %>%
  #add_trace(y=~Upper, name="Upper", type="scatter",mode="lines+markers") %>%
  add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines") %>%
  layout(title="Positivity Rates Actual versus Predicton")
