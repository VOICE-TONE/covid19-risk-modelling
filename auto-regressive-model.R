
#### Building a autoregressive model

library(dLagM)
library(vars)
#adl_model <- ardlBound(data = tarrant_data_ma[7:76,], formula = positivity_rate_7d~parks_visit+grocery_phar_visit+covid_like_symp+work_mobility)

train_ts <- na.omit(train_data[,c(1,5:7)])

### Transforming into timeseries

data_ts_fb <- ts(train_ts$covid_like_symp,frequency = 7)
#data_ts_apt <- ts(train_ts$covid_like_symp,frequency = 7)
#data_ts_sg <- ts(train_ts$work_mobility,frequency = 7)
#data_ts_glp <- ts(train_ts$parks_visit,frequency = 7)
#data_ts_glg <- ts(train_ts$grocery_phar_visit,frequency = 7)
data_ts_pos <- ts(train_ts$positivity_rate_7d,frequency = 7)

#plot.ts(cbind(data_ts_fb, data_ts_sg, data_ts_glp, data_ts_glg, data_ts_pos))
plot.ts(cbind(data_ts_fb,data_ts_pos))

train_ts <- cbind(data_ts_pos, data_ts_fb)
var.aic <- VAR(na.omit(train_ts), type = "none", lag.max = 15, ic="AIC")

summary(var.aic)


data_ts_fb_test <- ts(test_data$covid_like_symp, start = c(2020,9,11))

data_ts_pos_test <- ts(test_data$positivity_rate_7d, start = c(2020,9,11))

#plot(cbind(data_ts_pos_test,data_ts_fb_test))


#p1 <- decompose(cbind(data_ts_fb,data_ts_pos))

#p2 <- decompose(cbind(data_ts_pos,data_ts_pos))

#plot_ly(data = test_data, x=~date) %>%
#  add_trace(y=~positivity_rate_7d, name="POsitivity Rate") %>%
#  add_trace(y=~covid_like_symp, name="Facebook CLI")


# p1$trend

# plot(decompose(cbind(data_ts_fb,data_ts_pos)))


#### Forecast

fc_pos <- predict(var.aic, n.ahead=14, ci=0.99)

### plotting the forecast results

df_pred <- tibble(date=test_data$date, Actual=test_data$positivity_rate_7d, Fitted=fc_pos$fcst$data_ts_pos[,1], Fitted_lwr=fc_pos$fcst$data_ts_pos[,2], Fitted_upr=fc_pos$fcst$data_ts_pos[,3])

plot_ly(data = df_pred, x=~date) %>% 
  add_trace(y=~Actual, mode="lines+markers", name="Actual") %>%
#  add_trace(y=~Fitted, mode="lines+markers", name="Fitted") %>%
#  add_trace(y=~Fitted_lwr, mode="lines", name="Lwr_Pred_boundary") %>%
  add_trace(y=~Fitted_upr, mode="lines", name="Upr_Pred_boundary")


### plotting Fanchart

fanchart(fc_pos, names = "data_ts_pos")


### plotting Full period

df_pred <- tibble(date=test_data$date, Actual=test_data$positivity_rate_7d, Fitted=fc_pos$fcst$data_ts_pos[,1], Fitted_lwr=fc_pos$fcst$data_ts_pos[,2], Fitted_upr=fc_pos$fcst$data_ts_pos[,3])

plot_ly(data = df_pred, x=~date) %>% 
  add_trace(y=~Actual, mode="lines+markers", name="Actual") %>%
  add_trace(y=~Fitted, mode="lines+markers", name="Fitted") %>%
#  add_trace(y=~Fitted_lwr, mode="lines", name="Lwr_Pred_boundary", dash="dash") %>%
#  add_trace(y=~Fitted_upr, mode="lines", name="Upr_Pred_boundary", dash="dash") %>%
  add_trace(data=train_data, x=~date, y=~positivity_rate_7d, mode="lines+markers", name="Actual") %?%
  add_trace(data=train_data, x=~date, y=~positivity_rate_7d, mode="lines+markers", name="Actual")
  



### Plottting Actual and fitted





################### VAR with stationary timeseries FB only ###########
### Stationarizing 

train_data_ts <- na.omit(train_data[,c("date","covid_like_symp","positivity_rate_7d")])


### conversion to timeseries
pos_fd <- ts(train_data_ts$positivity_rate_7d, frequency = 7)

cli_fd <- ts(train_data_ts$covid_like_symp, frequency = 7)

## Detrending
cli_fd <- diff(cli_fd, lag = 7, differences = 1)
pos_fd <- diff(pos_fd, lag = 7, differences = 1)

## Deseasonalizing
library(TTR)

cli_fd_seas <- SMA(cli_fd, n=7)
pos_fd_seas <- SMA(pos_fd, n=7)

cli_fd_adj <- cli_fd - cli_fd_seas
pos_fd_adj <- pos_fd - pos_fd_seas

plot(decompose(cli_fd))
plot(cli_fd_adj)
plot(pos_fd_adj)


#### Running VAR model 

train1_ts <- cbind(pos_fd_adj, cli_fd_adj)

var1.aic <- VAR(na.omit(train1_ts), type = "none", lag.max = 14, ic="AIC")

summary(var.aic)

pacf(pos_fd)
acf(cli_fd)








##### Auto Arima with FB only and detrend + deseas
library(forecast)
arima_model0 <- auto.arima(na.omit(pos_fd), xreg = na.omit(cli_fd))

summary(arima_model0)

cli_fb_test <- ts(test_data$covid_like_symp, frequency = 7)

cli_fb_test <- diff(cli_fb_test, lag = 1, differences = 1)


pos_fd_pred <- forecast(arima_model0, xreg = cli_fb_test)



plot(pos_fd_pred$fitted)





### Auto Arima model
library(forecast)
library(tsm)
library(vars)
library(mFilter)

arima_model <- auto.arima(data_ts_pos[1:50], xreg = cbind(data_ts_fb[1:50],data_ts_glg[1:50], data_ts_glp[1:50], data_ts_sg[1:50]))

summary(arima_model)


arima_fc <- forecast(arima_model, xreg = cbind(data_ts_fb[51:55],data_ts_glg[51:55], data_ts_glp[51:55], data_ts_sg[51:55]))

arima_fc


df1 <- tibble(Actual=data_ts_pos[1:50],Fitted=arima_model$fitted)

df2 <- tibble(Actual=data_ts_pos[51:55] ,Fitted=as.numeric(arima_fc$mean))

df12 <- bind_rows(df1,df2)

df12$index <- as.numeric(rownames(df12))

### plotting the model
plot_ly(data = df12, x=~index) %>% add_trace(y=~Actual, mode="markers+lines") %>% add_trace(y=~Fitted, mode="markers+lines")









##### AUtorregressive Arima (FB) only

arima_model <- auto.arima(data_ts_pos[1:30], xreg = data_ts_fb[1:30])

summary(arima_model)


arima_fc <- forecast(arima_model, xreg = data_ts_fb[31:37])

arima_fc


df1 <- tibble(Actual=data_ts_pos[1:30],Fitted=arima_fc$fitted)

df2 <- tibble(Actual=data_ts_pos[31:37] ,Fitted=arima_fc$mean)

df12 <- bind_rows(df1,df2)

df12$index <- as.numeric(rownames(df12))

### plotting the model
plot_ly(data = df12, x=~index) %>% add_trace(y=~Actual, mode="markers+lines") %>% add_trace(y=~Fitted, mode="markers+lines")







#### A Package for VAR Model

# Integrated timeseries
#df3 <- cbind(data_ts_pos[1:50],data_ts_fb[1:50],data_ts_glg[1:50], data_ts_glp[1:50], data_ts_sg[1:50])
#colnames(df3) <- c("positivity","cli","grocery","parks","working_dist")

## with only 2 variables
df3 <- cbind(data_ts_pos[1:50],data_ts_fb[1:50],data_ts_glg[1:50])

colnames(df3) <- c("positivity","cli","grocery")

#data_ts <- na.omit(train_data[,3:7])

lagselect <- VARselect(df3,lag.max = 15, type = "const")

lagselect$selection

#var_model <- VAR(df3, p=7, type = "none", season = 7, lag.max = 14, ic="AIC")

var_model <- VAR(df3, p=7, type = "const", season = c(7), lag.max = 14, ic="AIC")

summary(var_model)














plot_ly()%>%
  add_trace(type="choropleth",
            geojson=counties,
            locations=fb_data_dfw$geo_value,
            z=fb_data_dfw$value,
            colorscale="Viridis",
            zmin=0,
            zmax=3,
            marker=list(line=list(width=0)
                        )
            )



url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)







#############

lm_t1 <- lm(positivity_rate_7d~work_mobility+parks_visit+grocery_phar_visit+covid_like_symp, data = df)







##################### Working Part with data #############
## Preparing the df_ts timeseries dataframe
fb_ts <- ts(na.omit(data$cli_perc_7d_Avg))
glp_ts <- ts(na.omit(data$Google_park_7d_Avg))
glg_ts <- ts(na.omit(data$Google_grocery_7d_Avg))
pos_ts <- ts(na.omit(data$Percent_positivity_7d_Avg))


df_ts <- cbind(pos_ts, fb_ts, glg_ts, glp_ts)

# fb only prediction
#df_ts <- cbind(pos_ts, fb_ts)

## Lag Order selecttion
var_lag <- VARselect(df_ts, lag.max = 14, type = "both")


## building the VAr modle to get the order for the VECM model

var.aic1 <- VAR(df_ts, type = "both", lag.max = 8, ic="AIC", season = 7)

summary(var.aic1)

## Extracting the order
k_order = var.aic1$p

## building the VECM
vec_model <- ca.jo(df_ts, ecdet = "trend", type = "trace", K=k_order, spec = "transitory", season = 7)

summary(vec_model)

vec_model1 <- VECM(df_ts, lag = 8, r=1)

predict(vec_model1, n.ahead=5)

#Below values tell that r=1 and the first column of estimates cointegration coefficient Beta 
# and loading matrix Alpha are the Maximum Likelihood estimator of Lutkepohl

#Values of teststatistic and critical values of test:

# test 10pct  5pct  1pct
# r <= 3 |  2.62  6.50  8.18 11.65
# r <= 2 |  8.72 15.66 17.95 23.52
# r <= 1 | 19.43 28.71 31.52 37.22
# r = 0  | 48.28 45.23 48.28 55.43
#


## Beta
round(vec_model@V,5)

## Alpha
round(vec_model@W,5)

## Coefficients of Non-Cointegration part of the model
round(vec_model@GAMMA,5)

## obtaining constants and seasonal dummies
library(tsDyn)

#seas <- gen_vec(data=df_ts, p=8, r=1, const = "unrestricted", seasonal = "unrestricted")


## Converting back themodle to VAR

var_model <- vec2var(vec_model, r=1)

var_model


## impulse response function

ir_test <- irf(var_model, n.ahead = 14, 
               impulse = c("fb_ts", "glg_ts", "glp_ts"), 
               response = "pos_ts", 
               ortho = FALSE, 
               runs = 500)

plot(ir_test)


## Prediction

new_data <- read.csv("../google_data.csv")

test_data <- new_data %>% filter(county=="Tarrant County", between(ymd(date), ymd("2020-09-01"), ymd("2020-09-08")))

var_pred <- predict(var.aic1, n.ahead=7,ci=0.95,dumvar=NULL)

plot(var_pred)








###################################################
### Trying Linear Model correction using SARIMA ###

library(astsa)

df_1 <- df_ts1 %>% tbl_df()

df_1 <- cbind(dates,df_1)

names(df_1) <- c("date", "pos", "fb_ts1", "park", "grocery", "driving","transit","walking")

# data_nona <- na.omit(data)

## All variables
lm_df1 <- lm(pos~fb_ts1+park+grocery+driving+transit+walking,
            data = df_1)

acf2(residuals(lm_df1))

## FB only variables

#lm_fb <- lm(Percent_positivity_7d_Avg~cli_perc_7d_Avg, data = data_nona)
#acf2(residuals(lm_fb))

## Adjusting LM by incorporating residuals to outcome of ACF and PACF on residual

ar1res <- sarima(residuals(lm_df1), 0,1,8, no.constant = TRUE)

## Prediction with SARIMA(0,1,8)

# Creating lag 8 for 

library(dynlm)

new_lm_t1 <- dynlm(pos~ L(pos,14) + fb_ts1,
                data = df_1)

summary(new_lm_t1)

plot(df_1$pos, type="l")
lines(fitted(new_lm_t1), col=2)







### Linear model Composite var

df_2 <- df_1 %>% mutate(comp_app1 = transit/(driving), comp_app2 = transit/(driving+transit))

fb_l5 <- fb_cli %>% filter(between(date, begin_date-5, ending_date-5)) %>% dplyr::select(date,cli_value)

fb_l5$date <- df_2$date

df_2 <- merge(df_2, fb_l5, by="date")

## Verification
plot(cbind(ts(df_2$fb_l5), ts(df_2$fb_ts1)), type="l")


## Model

lm_full <- lm(data = df_2[,-c(1,4:9,11)], pos~.)

summary(lm_full)

acf2(residuals(lm3))


stepAIC(lm_full, direction = "backward")


lm_final <- lm(data = df_2, pos ~ comp_app2+fb_l5)


# plot(lm_final)


### Prediction

train_pred <- tibble(date=df_2$date,Actual=df_2$pos, Fitted=lm_final$fitted.values)

plot_ly(data = train_pred, x=~date)%>%
  add_trace(y=~Actual, name="Actual", type="scatter", mode="lines+markers") %>%
  add_trace(y=~Fitted, name="Prediction", type="scatter", mode="lines+markers")



pp <- ggplot(data = train_pred, aes(x=Fitted, y=Actual))+geom_point()+geom_smooth(formula=y~x ,method = "lm")



ggplotly(pp)

### 

### Prediction of another county



##### Loading apple mobilty all time data


#u <-"https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/apple_reports/applemobilitytrends.csv"

u <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv"

#u <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/summary_reports/summary_report_US.csv"

google_url <- getURL(url = u, ssl.verifypeer=TRUE)

google_full <- read.csv(text = google_url)


google_full %>% dplyr::filter(tolower(state)=="texas") %>% head()
