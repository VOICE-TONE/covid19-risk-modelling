#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)

##### Loadong all VECM models used for forecasting
load("models/VAR_pos_R.rda")

load("models/VAR_pos_R_seas.rda")

load("models/VAR_pos_Rs.rda")

load("models/VAR_pos_Rs_seas.rda")


##### Setting the number of days to predict to 14
ndays <- 14


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

    
    ### filtering the df_final dataset based on the InputDateRange selected
#    df_out <- reactive({
        
#        df <- df_final %>% 
#            filter(between(date, input$date_range[1], input$date_range[2]))
        
        
#    })
    
    
    ### Using the filtered dataset for display of the data period selected
    output$df <- renderDataTable({
        
        df <- df_final
       # df <- df_out()
        
    })
    
    
 
    ### Plotting the forecast 14 days
    output$forecasting <- renderPlotly({
        
        ### PRediction without Seasonality
        VAR_pos_Rs_pred <- lapply(VAR_pos_Rs, function(x) predict(x,  ahead=14))
        
        ### Prediction with Seasonality
        VAR_pos_R_seas_pred <- lapply(VAR_pos_Rs_seas, function(x) predict(x, new_data=df_full_ts, ahead=ndays))
        
        ### Forecasting ahead without data based on the model above with the lowest MAPE
        VAR_forecast <- predict(VAR_pos_Rs[[3]], n.ahead=14)
        
        
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
                        name = "95% confidence") %>%
            layout(title=paste(input$county, "risk prediction."), yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
    })
    
    
    
    
    
    ### Plotting the forecast 14 days
    output$forecasting2 <- renderPlotly({
        
        ### PRediction without Seasonality
        VAR_pos_Rs_pred <- lapply(VAR_pos_Rs, function(x) predict(x,  ahead=14))
        
        ### Prediction with Seasonality
        VAR_pos_R_seas_pred <- lapply(VAR_pos_Rs_seas, function(x) predict(x, new_data=df_full_ts, ahead=ndays))
        
        ### Forecasting ahead without data based on the model above with the lowest MAPE
        VAR_forecast <- predict(VAR_pos_Rs[[3]], n.ahead=14)
        
        
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
            add_lines(data = tail(df_fcst,14), x=~date, y=~value, name="Positivity rate", line= list(color="Red",dash="dash")) %>%
            add_ribbons(data = tail(df_fcst_only,14), 
                        x=~date,
                        ymin = ~lower,
                        ymax = ~upper,
                        color=I("orange"),
                        opacity=0.3,
                        name = "95% confidence") %>%
            layout(title=paste(input$county, "covid risk prediction (14 days ahead)."), yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
    })
    
    
    
    
    
    
    ### Plotting the predicted vs Actual 14 days
    output$prediction <- renderPlotly({
        
        ##### Predict for the number of days selected
        VAR_pos_Rs_pred <- lapply(VAR_pos_Rs, function(x) predict(x, new_data=df_test_ts, ahead=ndays))
        
        ##### Prediction with Seasonality
        VAR_pos_Rs_seas_pred <- lapply(VAR_pos_Rs_seas, function(x) predict(x, new_data=df_test_ts, ahead=ndays))
        
        ##### Plotting the models
        
        plt_R <- lapply(VAR_pos_Rs, function(x) var_pred(model = x, df=df_test_ts, ahead=ndays))
        
        plt_R_seas <- lapply(VAR_pos_Rs_seas, function(x) var_pred(model = x, df=df_test_ts, ahead=ndays))
        
        plt_R[[3]]%>%
            layout(title="Prediction on test data", yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
        
    })
    
    
})
