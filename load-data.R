source("libraries.R")

##### Loading data for shiny app for 60 days prediction #####

### Setting the number of days to go back ####
shift_days <- 90


## Loading data ##

source("loading-and-testing-the-data.R")


## Preparing Tarrant County's data for building the model##

tarrant_data_ma <- get_tarrant_df()

#### Splitting into train test

train_index <- index(tarrant_data_ma)[1:round(nrow(tarrant_data_ma)*0.75)]

train_data <- tarrant_data_ma[train_index,]

test_data <- tarrant_data_ma[-train_index,]

# nrow(test_data)+nrow(train_data) == nrow(tarrant_data_ma)




########################################
##### fit  or load the base model #####
#########################################

lm_t1 <- fit_model(df=na.omit(train_data))





##### Tarrant county prediction #####

tarrant_pred_train <- pred_tarrant_model(model = lm_t1, df=na.omit(train_data))

#tarrant_pred_test <- predict(lm_t1, na.omit(test_data[,3:6])) # Backup

tarrant_pred_test <- predict(lm_t1, na.omit(test_data[,c("parks_visit","grocery_phar_visit","covid_like_symp","work_mobility")]))

### Combining test data Actual vs Prediction 
tarrant_test_actual <- test_data[which(rownames(test_data) %in% names(tarrant_pred_test)),c("date", "positivity_rate_7d")] %>% rename(Actual=positivity_rate_7d)

tarrant_pred_test <- tibble(tarrant_test_actual, Fitted=tarrant_pred_test)

### Combining Train and Test Predictions results

tarrant_pred <- bind_rows(na.omit(tarrant_pred_train), tarrant_pred_test)


### Test Accuracy

#test_accuracy <- tarrant_pred %>% dplyr::filter(between(date, as.Date("2020-08-25"), as.Date("2020-09-08"))) %>% dplyr::mutate(Res_sq=(Actual-Fitted)^2, Tot_seq=(Actual-mean(Actual))^2)


### Test accuracy

#tab <- table(tarrant_pred_test, na.omit(test_data)$positivity_rate_7d)

#1-sum(diag(tab))/sum(tab)

#test_accuracy = sum(diag(tab))/sum(tab)


### Train accuracy

#tab1 <- table(na.omit(tarrant_pred_train)$Fitted, na.omit(tarrant_pred_train)$Actual)

#train_accuracy = sum(diag(tab1))/sum(tab1)


## plotting Tarrant County prediction ##

#colors <- c("Actual"="steelblue", "Fitted"="#E69F00")

#p <- ggplot(data = tarrant_pred, aes(x=date))+
#  geom_point(aes(y=Actual), color="steelblue")+
#  geom_line(aes(y=Actual), color="steelblue")+
#  geom_point(aes(y=Fitted), color="#E69F00")+
#  geom_line(aes(y=Fitted), color="#E69F00")+
#  scale_x_date(date_breaks = "weeks")+
#  geom_vline(xintercept = as.numeric(tarrant_pred[tarrant_pred$date==as.Date("2020-08-25"),"date"]),color="red", linetype="dashed")+
#  annotate(geom = "segment", x=c(as.Date("2020-09-02"), as.Date("2020-09-02")), xend = c(as.Date("2020-09-04"),as.Date("2020-09-04")), y=c(19, 18), yend = c(19, 18), color=c("steelblue", "#E69F00"), size=1)+
#  annotate(geom = "text", x=c(as.Date("2020-08-13"),as.Date("2020-08-21"), as.Date("2020-09-07"), as.Date("2020-09-07"),as.Date("2020-08-13"),as.Date("2020-08-30")), y=c(19.5, 19.5, 19, 18,16,16), label=c("R-Squared:",round(summary(lm_t1)$r.squared,2), "Actual", "Fitted","Train data", "Test data"), size=4, color="blue")+
#  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.subtitle = element_text(color="blue"), plot.title = element_text(size=12,colour = "black", face = "bold"))+
#  labs(title = "Tarrant County daily risk",subtitle = "(MyPHI Covid Analysis)", x="Date", y="Positivity rates")+
#  scale_color_manual(name="Legend", values = colors)

# ggplotly(p)